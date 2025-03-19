module TestCountdownSpec (spec) where

import Countdown
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.List (all)

prop_split :: [Int] -> Bool
prop_split xs =  all id [xs == ys ++ zs | (ys, zs) <- split xs]

spec :: Spec
spec = do
    describe "icebreaker" $ do
        prop "icebreaker" $
            property prop_split

