module PhilipSpec (main, spec) where
import           Philip.DSL

import           Control.Monad.State
import           Data.Version                    (Version (..))
import           Distribution.Package            (PackageIdentifier (..),
                                                  PackageName (..))
import qualified Distribution.PackageDescription as PD

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


      PD.author d `shouldBe` "Fujimura Daisuke"
      PD.maintainer d `shouldBe` "Fujimura Daisuke"
      PD.copyright d `shouldBe` "(c) 2013 Fujimura Daisuke"
      PD.package d `shouldBe` PackageIdentifier (PackageName "foo") (Version [1,2,3] [])
      PD.licenseFile d `shouldBe` "LICENSE"
      PD.stability d `shouldBe` "stable"
