{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term (..), bound, free, fresh)
import           Test.Hspec
import Data.Set


main :: IO ()
main = hspec $ 
    describe "Fresh test" testFresh

testFresh :: SpecWith ()
testFresh = do
    let conflicts = fromList ["a", "x", "x1"]
    it "should generate fresh names" $ 
        toList conflicts `shouldNotContain` [fresh conflicts]
    
