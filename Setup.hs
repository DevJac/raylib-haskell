import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Types.HookedBuildInfo
import System.Environment

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {preConf = runMakeOnRaylib}

runMakeOnRaylib :: Args -> ConfigFlags -> IO HookedBuildInfo
runMakeOnRaylib _args flags = do
    cflags <- lookupEnv "CLFAGS" >>= return . maybe "" id
    setEnv "CFLAGS" $ cflags ++ " -fPIC"
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "env" ["make", "--directory=raylib/src"]
    return emptyHookedBuildInfo
