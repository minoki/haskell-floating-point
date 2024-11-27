{-# LANGUAGE CPP #-}
import           Distribution.Simple
import           Distribution.Simple.Configure (configure)
import           Distribution.Simple.PackageIndex (allPackages)
import           Distribution.Types.BuildInfo (BuildInfo (includeDirs))
import qualified Distribution.Types.InstalledPackageInfo as InstalledPackageInfo (includeDirs)
import           Distribution.Types.Library (Library (libBuildInfo))
import           Distribution.Types.LocalBuildInfo (LocalBuildInfo (installedPkgs, localPkgDescr))
import           Distribution.Types.PackageDescription (PackageDescription (library))
#if MIN_VERSION_Cabal(3, 14, 0)
import           Distribution.Utils.Path (makeSymbolicPath)
#endif

{-
We want to access "ghcconfig.h" from assembly source file (.S),
but GHC does not pass the include directory to the assembler.
So we need to set include-dirs to include the path to "ghcconfig.h"
-}

main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }
  where
    -- myConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
    myConfHook a cf  = do
      localBuildInfo <- configure a cf
      let extraIncludeDirs :: [String]
          extraIncludeDirs = concatMap InstalledPackageInfo.includeDirs (allPackages $ installedPkgs localBuildInfo)
          updateBuildInfo :: BuildInfo -> BuildInfo
#if MIN_VERSION_Cabal(3, 14, 0)
          updateBuildInfo bi = bi { includeDirs = includeDirs bi ++ map makeSymbolicPath extraIncludeDirs }
#else
          updateBuildInfo bi = bi { includeDirs = includeDirs bi ++ extraIncludeDirs }
#endif
          updateLibrary :: Library -> Library
          updateLibrary lib = lib { libBuildInfo = updateBuildInfo (libBuildInfo lib) }
          updatePkgDescr :: PackageDescription -> PackageDescription
          updatePkgDescr pd = pd { library = updateLibrary <$> library pd }
      return localBuildInfo { localPkgDescr = updatePkgDescr (localPkgDescr localBuildInfo) }
