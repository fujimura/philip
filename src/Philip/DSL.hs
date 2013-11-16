module Philip.DSL
    ( desc
    , author
    , copyright
    , maintainer
    ) where

import           Control.Monad.State
import           Distribution.PackageDescription (PackageDescription,
                                                  emptyPackageDescription)
import qualified Distribution.PackageDescription as PD

desc :: State PackageDescription a -> PackageDescription
desc = flip execState emptyPackageDescription

author, copyright, maintainer :: String -> State PackageDescription ()
author x     = state $ \pd -> ((), pd { PD.author     = x })
copyright x  = state $ \pd -> ((), pd { PD.copyright  = x })
maintainer x = state $ \pd -> ((), pd { PD.maintainer = x })
