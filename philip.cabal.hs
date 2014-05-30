import           Control.Applicative
import           Data.Time.Calendar  (toGregorian)
import           Data.Time.Clock     (getCurrentTime, utctDay)
import           Philip.DSL

philip = undefined
getYear = do
  (year,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
  return year

main :: IO ()
main = do
    let me = "Fujimura Daisuke<me@fujimuradaisuke.com>"
        deps = [ ("mtl", "-any")
               , ("split", "-any")
               , ("time", "-any")
               , ("Cabal", ">= 1.18")
               ]
    year <- getYear

    philip $ do
      name "philip"
      version "0.0.1"
      author me
      maintainer me
      ghcOptions ["-Wall"]
      copyright $ "(c) " ++ show year ++ " " ++ me
      licenseFile "LICENSE"
      srcDirs <- sourceDirectories ["src"]
      stability "stable"
      buildDepends deps
      testSuite "spec" $ do
        sourceDirectories $ srcDirs ++ ["test"]
        buildDepends $ deps ++ [("hspec", ">= 1.9.2")]
