module Web3.TypesSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Event's FromJSON instance" $
        it "decodes Event JSON"
            pending
