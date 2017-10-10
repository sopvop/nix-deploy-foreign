{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Prelude                       hiding (FilePath)

import           Control.Monad.IO.Class        (liftIO)
import           Data.Foldable                 (foldrM, for_)
import           Data.List                     (isPrefixOf)
import qualified Data.Map                      as Map
import           Data.Maybe                    (mapMaybe)
import           Data.Semigroup                ((<>))
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Traversable              (for)
import           System.FilePath               (splitExtension, splitFileName,
                                                takeDirectory)

import           System.Environment            (getExecutablePath)
import           System.IO                     (BufferMode (LineBuffering),
                                                hSetBuffering, stdout)
import           System.IO.Error               (annotateIOError, catchIOError,
                                                ioError)
import           System.Posix.Files.ByteString (readSymbolicLink)

import           Data.String                   (fromString)
import           Options.Applicative           (Parser, ParserInfo, execParser,
                                                flag, help, helper, info, long,
                                                metavar, short, strArgument)
import qualified Options.Applicative           as OA

import           Shelly                        hiding (path)

newtype ProfilePath = ProfilePath
    { unProfilePath :: FilePath
    } deriving (Eq, Ord, Show)

newtype ClosurePath = ClosurePath
    { unClosurePath :: FilePath
    } deriving (Eq, Ord, Show)


data Args = Args
  { argsRollingBack :: !Bool
  , argsProfile     :: !ProfilePath
  , argsClosure     :: !ClosurePath
  } deriving (Show, Eq)

newtype RollbackArgs = RollbackArgs
  { rollbackProfile :: ProfilePath
  } deriving (Show, Eq)

data Command = Activate Args
             | Rollback RollbackArgs
             deriving (Show)

rollingBackFlag :: Parser Bool
rollingBackFlag = flag False True $
  help "Is rolling back, don't rollback on error"
  <> long "rollingback"
  <> short 'r'

profileArg :: Parser ProfilePath
profileArg =  fmap (ProfilePath . fromString) . strArgument $
   metavar "PROFILEPATH"
   <> help "Path to nix profile"

closureArg :: Parser ClosurePath
closureArg = fmap (ClosurePath . fromString) . strArgument $
   metavar "STOREPATH"
   <> help "Path to closure inside nix store"

activateArgs :: Parser Args
activateArgs = Args
               <$> rollingBackFlag
               <*> profileArg
               <*> closureArg

rollbackArgs :: Parser RollbackArgs
rollbackArgs = RollbackArgs <$> profileArg

parseArgs :: Parser Command
parseArgs = OA.hsubparser $
  OA.command "activate" (info (Activate <$> activateArgs) mempty)
  <> OA.command "rollback" (info (Rollback <$> rollbackArgs) mempty)

options :: ParserInfo Command
options = info (helper <*> parseArgs) mempty

main :: IO ()
main = do
  args <- execParser options

  hSetBuffering stdout LineBuffering
  prog args

prog :: Command -> IO ()
prog (Activate args@Args{..}) = shelly . print_commands False $
  if argsRollingBack
  then rollingback args
  else rollforward args
prog (Rollback args) = shelly . print_commands False $ rollbackCmd args

rollbackCmd :: RollbackArgs -> Sh ()
rollbackCmd  RollbackArgs{..} = do
  mprof <- findProfileTarget rollbackProfile
  self <- liftIO getExecutablePath
  case mprof of
    Nothing -> terror "Profile not found"
    Just closure -> do
      run_ "nix-env"
          [ "-p"
          , toTextArg (unProfilePath rollbackProfile)
          , "--rollback" ]
      run_ (fromString self)
          [ "--rollingback"
          , toTextArg (unProfilePath rollbackProfile)
          , toTextArg (unClosurePath closure) ]

rollingback :: Args -> Sh ()
rollingback Args{..} = do
  oldClosureMay <- findProfileTarget argsProfile
  case oldClosureMay of
    Nothing         -> terror "Can't read profile while rolling back"
    Just oldClosure -> activate (Just argsClosure) oldClosure

rollforward :: Args -> Sh ()
rollforward Args{..} = do
  oldClosure <- findProfileTarget argsProfile
  setProfile argsProfile argsClosure
  handleany_sh  (onError oldClosure)
               $ activate oldClosure argsClosure
  where
    onError old e = do
      unless argsRollingBack $ do
        echo $ "Got error, rolling back: " <> Text.pack (show e)
        rollbackActivation argsProfile argsClosure old
      terror $ "Rolled: " <> Text.pack (show e)


activate :: Maybe ClosurePath -> ClosurePath -> Sh ()
activate oldClosure newClosure = do
  oldDeploy <- maybe (pure (Deployment [] [])) readDeployment oldClosure
  newDeploy <- readDeployment newClosure
  let newServices = deploymentServices newDeploy

  running <- getInterestingServices $
               deploymentServices newDeploy
            <> deploymentServices oldDeploy

  checkFilePaths (deploymentFiles oldDeploy)
  checkFilePaths (deploymentFiles newDeploy)

  let (toStart, toRestart, toStop) = serviceActions newServices running

  unlinkPaths oldDeploy
  linkPaths newDeploy

  reloadUpstart

  unless (null toStop) $
    stopServices toStop

  unless (null toRestart) $
    restartServices toRestart

  unless (null toStart) $
    startServices toStart

  echo "ok"
  pure ()


deactivate :: ClosurePath -> Sh ()
deactivate closure = do
  newDeploy <- readDeployment closure

  checkFilePaths (deploymentFiles newDeploy)

  running <- getInterestingServices (deploymentServices newDeploy)
  unless (null running) $
    stopServices (map serviceName running)
  unlinkPaths newDeploy


findProfileTarget :: ProfilePath -> Sh (Maybe ClosurePath)
findProfileTarget (ProfilePath path) = do
  exists <- test_e path
  if not exists
   then pure Nothing
   else
     do isSym <- test_s path
        unless isSym $
          terror "Profile is not a symlink"
        fmap (Just . ClosurePath) $ readSymlink path >>= readSymlink

data Service = Service
  { serviceName      :: !Text
  , serviceStorePath :: !FilePath
  } deriving (Eq, Ord, Show)

data File = File
  { fileTarget    :: !FilePath
  , fileStorePath :: !FilePath
  } deriving (Eq, Ord, Show)

data Deployment = Deployment
  { deploymentServices :: ![Service]
  , deploymentFiles    :: ![File]
  } deriving (Eq, Ord, Show)

readClosure :: ClosurePath -> Sh [(FilePath, FilePath)]
readClosure (ClosurePath path) =  print_stdout False $ do
  paths <- go path []
  relArgs <- traverse (\x -> (,)
                             <$> ((("/"::FilePath) </>) <$> relativeTo path x)
                             <*> canonicalize x) paths
  pure relArgs
  where
    go p acc = do
      isDir <- test_d p
      if not isDir
        then do
          exists <- test_e p
          pure $ if exists then p:acc else acc
        else do
          xs <- ls p
          foldrM go acc xs

addService :: Deployment -> Service -> Deployment
addService d s = d { deploymentServices = deploymentServices d <> [s] }

addFile :: Deployment -> File -> Deployment
addFile d s = d { deploymentFiles = deploymentFiles d <> [s] }

findServices :: [(FilePath, FilePath)] -> Deployment
findServices = foldr go (Deployment [] [])
  where
    toPath = Text.unpack . toTextArg
    go (nm, nixpath) acc
        | dir == "/etc/init/" && ext == ".conf" =
            addService acc $ Service (Text.pack name) nixpath
        | "/bin/" `isPrefixOf` dir =  acc
        | otherwise = addFile acc (File nm nixpath)
      where
        path = toPath nm
        (dir, fname) = splitFileName path
        (name, ext) = splitExtension fname

readDeployment :: ClosurePath -> Sh Deployment
readDeployment closure = findServices <$> readClosure closure


getInterestingServices :: [Service] -> Sh [Service]
getInterestingServices interesting  = do
  running <- readRunningServices
  checkRunningServices (intersect running)
  where
    intersect running = Set.toList $
       Set.fromList (map serviceName interesting)
      `Set.intersection` Set.fromList running


checkRunningServices :: Traversable t => t Text -> Sh (t Service)
checkRunningServices names =
  for names $ \name -> do
    let
      path = ("/etc/init" :: FilePath) </> fromText name <.> "conf"
    issym <- test_s path
    unless issym .
       terror $ "Service is not a symlink: " <> toTextArg path
    target <- readSymlink path
    unless (Text.isPrefixOf "/nix/store" (toTextArg target)) .
       terror $ "Service is not a symlink to store"
                  <> toTextArg target

    pure $ Service name target

checkFilePaths :: Foldable t => t File -> Sh ()
checkFilePaths files =
  for_ files $ \(File target _) -> do
     ex <- test_e target
     when ex $ do
       isSym <- test_s target
       unless isSym .
         terror $ "Target path is not a symlink: " <> toTextArg target
       echo $ "Reading" <> toTextArg target
       storeTarget <- readSymlink target
       unless (isStorePath storeTarget) .
          terror $ "Target path is not a store path: " <> toTextArg target


readRunningServices :: Sh [Text]
readRunningServices = print_stdout False $
     mapMaybe (go . Text.strip) . Text.lines
           <$> run "/sbin/initctl" ["list"]
  where
    go x = case Text.break (== ' ') x of
             (name, status)
                 | Text.isPrefixOf "start/running" (Text.strip status) -> Just name
                 | otherwise -> Nothing

setProfile :: ProfilePath -> ClosurePath -> Sh ()
setProfile profile closure = do
  target <- findProfileTarget profile
  unless (target == Just closure) doLink
  where
   profilePath = unProfilePath profile
   doLink = run_ "nix-env" [ "-p", toTextArg profilePath
                           , "--set", toTextArg (unClosurePath closure) ]


rollbackActivation :: ProfilePath -> ClosurePath -> Maybe ClosurePath -> Sh ()
rollbackActivation profile newClosure oldClosure =
  case oldClosure of
    Nothing -> rm_f (unProfilePath profile)
               *> deactivate newClosure
    Just _ -> do
      run_ "nix-env"
          [ "-p"
          , toTextArg (unProfilePath profile)
          , "--rollback" ]
      self <- liftIO getExecutablePath
      run_ (fromString self)
          [ "--rollingback"
          , toTextArg (unProfilePath profile)
          , toTextArg (unClosurePath newClosure) ]

serviceActions :: [Service] -> [Service] -> ([Text], [Text], [Text])
serviceActions newSet runningSet = (start, restart, stop)
  where
    (restart, stop) = foldr go ([],[]) runningSet
    go (Service name path) acc@(restarts, stops) =
      case Map.lookup name new of
        Nothing -> (restarts, name:stops)
        Just newPath
             | newPath == path -> acc
             | otherwise -> (name:restarts, stops)
    new = Map.fromList
          $ map ((,) <$> serviceName <*> serviceStorePath) newSet

    start = Set.toList $
            Set.fromList (map serviceName newSet)
            `Set.difference`
             Set.fromList (map serviceName runningSet)

readSymlink :: FilePath -> Sh FilePath
readSymlink path =
   liftIO $ (dir </>) . fromText . Text.decodeUtf8 <$>
           (readSymbolicLink bsPath
            `catchIOError` (ioError . annotate))
 where
   tpath = toTextArg path
   bsPath = Text.encodeUtf8 tpath
   dir = fromString $ takeDirectory (Text.unpack tpath) :: FilePath
   annotate e =
     annotateIOError e "" Nothing (Just $ Text.unpack tpath)

isStorePath :: FilePath -> Bool
isStorePath p = Text.isPrefixOf "/nix/store/" (toTextArg p)

linkPaths :: Deployment -> Sh ()
linkPaths d = do
  for_ (deploymentFiles d) $
    \File{..} -> suLN  fileStorePath fileTarget

  for_ (deploymentServices d) $
    \(Service name nixpath) ->
       suLN nixpath (("/etc/init" :: FilePath) </> name <.> "conf")

unlinkPaths :: Deployment -> Sh ()
unlinkPaths d = do
  for_ (deploymentFiles d) $
    \File{..} -> do
    ex <- test_e fileTarget
    when ex $ suUnlink fileTarget

  for_ (deploymentServices d) $
    \(Service name _) -> do
       let tgt = ("/etc/init" :: FilePath) </> name <.> "conf"
       ex <- test_e tgt
       when ex $ suUnlink tgt

suLN :: FilePath -> FilePath -> Sh ()
suLN p t = run_ "sudo" ["ln", "-sfT", toTextArg p, toTextArg t]

suUnlink :: FilePath -> Sh ()
suUnlink p = run_ "sudo" ["unlink", toTextArg p]

reloadUpstart :: Sh ()
reloadUpstart = run_ "sudo" ["/sbin/initctl", "reload-configuration"]

stopServices :: [Text] -> Sh ()
stopServices names =
 run_ "sudo" ("/sbin/initctl":"stop":names)

startServices :: [Text] -> Sh ()
startServices names =
 run_ "sudo" ("/sbin/initctl":"start":names)

restartServices :: [Text] -> Sh ()
restartServices names =
 run_ "sudo" ("/sbin/initctl":"restart":names)


