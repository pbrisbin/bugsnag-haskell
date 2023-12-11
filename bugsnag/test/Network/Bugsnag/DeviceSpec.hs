module Network.Bugsnag.DeviceSpec
  ( spec
  ) where

import           Prelude

import           Data.Bugsnag           (Device (..))
import           Network.Bugsnag.Device
import           Test.Hspec

spec :: Spec
spec = do
  describe "bugsnagDeviceFromUserAgent" $ do
    it "best-effort sniffs from UserAgent" $ do
      let device =
            bugsnagDeviceFromUserAgent
              "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.119 Safari/537.36"

      device_osName device `shouldBe` Just "Linux"
      device_osVersion device `shouldBe` Nothing
      device_browserName device `shouldBe` Just "Chrome"
      device_browserVersion device `shouldBe` Just "64.0.3282"
