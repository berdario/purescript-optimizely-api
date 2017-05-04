module Test.Common where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Either (Either, either)
import Control.Monad.Except (Except, ExceptT(..), runExcept, runExceptT, withExcept, withExceptT)
import Test.Unit (Test, TestSuite, failure, success, test)

assertExceptT :: forall e a eff. (Show e) => ExceptT e (Aff eff) a -> Test eff
assertExceptT result = do
    rez <- (runExceptT $ withExceptT show result)
    assertEither rez

assertEither :: forall a eff. Either String a -> Test eff
assertEither = either failure (const success)
