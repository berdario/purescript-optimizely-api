module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (Except, runExcept, withExcept)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readJSON)
import Data.Optimizely (Project)
import Test.Unit (Test, TestSuite, failure, success, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Type.Proxy (Proxy(..))

type TestEffs = (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR)

main :: Eff TestEffs Unit
main = runTest do
  checkDeserialize "Project" (Proxy :: Proxy Project) """{
    "id": 859720118,
    "account_id": 555650815,
    "code_revision": 12,
    "project_name": "My even newer project name",
    "project_status": "Active",
    "created": "2014-04-16T21:33:34.408430Z",
    "last_modified": "2014-06-10T22:12:21.707170Z",
    "library": "jquery-1.6.4-trim",
    "include_jquery": false,
    "js_file_size": 23693,
    "project_javascript": "someFunction = function () {\n //Do cool reusable stuff \n}",
    "enable_force_variation": false,
    "exclude_disabled_experiments": false,
    "exclude_names": null,
    "ip_anonymization": false,
    "ip_filter": "1.2.3.4",
    "socket_token": "AABBCCDD~123456789",
    "dcp_service_id": 121234
  }"""

checkDeserialize :: forall e. IsForeign e => String -> Proxy e -> String -> TestSuite TestEffs
checkDeserialize name _ json =
    test ("Check that we can deserialize a " <> name) $ do
        assertExcept (readJSON json :: F e)

assertExcept :: forall e a. Show e => Except e a -> Test TestEffs
assertExcept result = assertEither $ runExcept $ withExcept show result
    where
      assertEither = either failure (const success)

