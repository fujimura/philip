module Philip.DSLSpec (main, spec) where

import           Philip.DSL

import           Control.Monad.State
import           Data.Version                    (Version (..))
import           Distribution.Package            (Dependency (..),
                                                  PackageIdentifier (..),
                                                  PackageName (..))
import           Distribution.PackageDescription (PackageDescription)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Version            as V

import           Test.Hspec

main :: IO ()
main = hspec spec

testDescription :: PackageDescription
testDescription =
    let me = "Fujimura Daisuke" in
    desc $ do
      name "foo"
      version "1.2.3"
      author me
      maintainer me
      copyright $ "(c) 2013 " ++ me
      licenseFile "LICENSE"
      stability "stable"
      buildDepends [("dep1", "== 1.0.0 && < 1.2.0"), ("dep2", "== 2.0.0")]
      dataFiles ["g", "h"]
      dataDir "data/directory"
      extraSrcFiles ["a", "b"]
      extraTmpFiles ["c", "d"]
      extraDocFiles ["e", "f"]

spec :: Spec
spec = do
  describe "DSL" $
    it "should write proper fields" $ do
      let d = testDescription

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
      let ver2 = V.thisVersion $ Version [2,0,0] []
      PD.buildDepends d `shouldBe` [ Dependency (PackageName "dep1") ver1
                                   , Dependency (PackageName "dep2") ver2
                                   ]