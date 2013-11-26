module PhilipSpec (main, spec) where
import           Philip.DSL

import           Control.Monad.State
import           Data.Version                    (Version (..))
import           Distribution.Package            (Dependency (..),
                                                  PackageIdentifier (..),
                                                  PackageName (..))
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Version            as V

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $
    it "should work fine" $ do
      let me = "Fujimura Daisuke"
      let d = desc $ do
            name "foo"
            version "1.2.3"
            author me
            maintainer me
            copyright $ "(c) 2013 " ++ me
            licenseFile "LICENSE"
            stability "stable"
            buildDepends [("dep1", "== 1.0.0 && < 1.2.0"), ("dep2", ">= 2.0.0 && <= 2.2.0")]
            dataFiles ["g", "h"]
            dataDir "data/directory"
            extraSrcFiles ["a", "b"]
            extraTmpFiles ["c", "d"]
            extraDocFiles ["e", "f"]

      PD.author d `shouldBe` "Fujimura Daisuke"
      PD.maintainer d `shouldBe` "Fujimura Daisuke"
      PD.copyright d `shouldBe` "(c) 2013 Fujimura Daisuke"
      PD.package d `shouldBe` PackageIdentifier (PackageName "foo") (Version [1,2,3] [])
      PD.licenseFile d `shouldBe` "LICENSE"
      PD.stability d `shouldBe` "stable"
      PD.extraSrcFiles d `shouldBe` ["a", "b"]
      PD.extraTmpFiles d `shouldBe` ["c", "d"]
      PD.extraDocFiles d `shouldBe` ["e", "f"]
      PD.dataFiles d `shouldBe` ["g", "h"]
      PD.dataDir d `shouldBe` "data/directory"
      let ver1 = V.intersectVersionRanges
                (V.thisVersion $ Version [1,0,0] [])
                (V.earlierVersion $ Version [1,2,0] [])
      let ver2 = V.intersectVersionRanges
                (V.thisVersion $ Version [2,0,0] [])
                (V.thisVersion $ Version [2,2,0] [])
      PD.buildDepends d `shouldBe` [ Dependency (PackageName "dep1") ver1
                                   , Dependency (PackageName "dep2") ver2
                                   ]
