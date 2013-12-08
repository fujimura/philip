{-# LANGUAGE NamedFieldPuns #-}
module Philip.DSL
    ( desc
    , author
    , copyright
    , maintainer
    , name
    , version
    , licenseFile
    , stability
    , homepage
    , packageUrl
    , bugReports
    , synopsis
    , description
    , category
    , buildDepends
    , buildType
    , dataFiles
    , dataDir
    , extraSrcFiles
    , extraTmpFiles
    , extraDocFiles
    ) where

import           Control.Monad.State
import           Data.List.Split                 (splitOn)
import           Data.Version                    (Version (..))
import qualified Distribution.Compat.ReadP       as Parse
import           Distribution.Package            (Dependency (..),
                                                  PackageIdentifier (..),
                                                  PackageName (..))
import           Distribution.PackageDescription (PackageDescription,
                                                  emptyPackageDescription,
                                                  BuildType(..))
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Text               as T
import           Distribution.Version            (VersionRange (..))

desc :: State PackageDescription a -> PackageDescription
desc = flip execState emptyPackageDescription

author, copyright, maintainer, version, name, licenseFile, stability,
  homepage, packageUrl, bugReports, synopsis, description, category
  :: String -> State PackageDescription ()
author x      = state $ \pd -> ((), pd { PD.author      = x })
copyright x   = state $ \pd -> ((), pd { PD.copyright   = x })
maintainer x  = state $ \pd -> ((), pd { PD.maintainer  = x })
licenseFile x = state $ \pd -> ((), pd { PD.licenseFile = x })
stability x   = state $ \pd -> ((), pd { PD.stability   = x })
homepage x    = state $ \pd -> ((), pd { PD.homepage    = x })
packageUrl x  = state $ \pd -> ((), pd { PD.pkgUrl      = x })
bugReports x  = state $ \pd -> ((), pd { PD.bugReports  = x })
synopsis x    = state $ \pd -> ((), pd { PD.synopsis    = x })
description x = state $ \pd -> ((), pd { PD.description = x })
category x    = state $ \pd -> ((), pd { PD.category    = x })
version x    = state $ \pd -> ((), pd { PD.package    = update x (PD.package pd) })
  where
    -- TODO Update version tags
    update v PackageIdentifier{pkgName} = PackageIdentifier pkgName $ Version (parse v) []
    parse xs = map (\x -> read x :: Int) $ splitOn "." xs
name x = state $ \pd -> ((), pd { PD.package = update x (PD.package pd) })
  where
    update x PackageIdentifier{pkgVersion} = PackageIdentifier (PackageName x) pkgVersion

dataDir :: FilePath -> State PackageDescription()
dataDir x = state $ \pd -> ((), pd { PD.dataDir = x })

dataFiles, extraSrcFiles, extraTmpFiles, extraDocFiles
  :: [FilePath] -> State PackageDescription ()
dataFiles xs = state $ \pd -> ((), pd { PD.dataFiles = xs })
extraSrcFiles xs = state $ \pd -> ((), pd { PD.extraSrcFiles = xs })
extraTmpFiles xs = state $ \pd -> ((), pd { PD.extraTmpFiles = xs })
extraDocFiles xs = state $ \pd -> ((), pd { PD.extraDocFiles = xs })

buildDepends :: [(String, String)] -> State PackageDescription()
buildDepends deps = state $ \pd -> ((), pd { PD.buildDepends = map toDependency deps})
  where
    toDependency :: (String, String) -> Dependency
    toDependency (depName,ver) = Dependency (PackageName depName) (parse ver)
      where
        parse :: String -> VersionRange
        parse str = case [ p | (p, _) <- Parse.readP_to_S T.parse str ] of
          [] -> error $ "Error: Parsing version range of " ++ depName ++ "was failed (" ++ str ++ ")"
          xs -> last xs

buildType :: BuildType -> State PackageDescription ()
buildType x = state $ \pd -> ((), pd { PD.buildType = Just x })
