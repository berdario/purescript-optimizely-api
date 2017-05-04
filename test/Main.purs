module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (Except)
import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readJSON)
import Network.HTTP.Affjax (AJAX)
import Node.Process (PROCESS, lookupEnv)
import Test.Unit (Test, TestSuite, test, suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Type.Proxy (Proxy(..))

import Data.Optimizely (Audience, Dimension, Experiment, Goal, Project, Result, Schedule, TargetingList, Variation)
import Data.Optimizely.DCP as DCP
import Network.Optimizely.Auth (Auth(..))
import Network.Optimizely.Internal (generalize)
import Test.Integration (integration)
import Test.Common (assertExceptT)

type TestEffs = (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, process :: PROCESS, ajax :: AJAX, now :: NOW)

main :: Eff TestEffs Unit
main = do
  key <- lookupEnv "OPTIMIZELY_KEY"
  runTest $ do
    deserializations
    integration $ map ClassicToken key


deserializations :: TestSuite TestEffs
deserializations = suite "Deserializations" $ do
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

  checkDeserialize "Experiment" (Proxy :: Proxy Experiment) """{
    "id": 791495413,
    "percentage_included": 10000,
    "display_goal_order_lst": [],
    "is_multivariate": false,
    "project_id": 754864960,
    "variation_ids": [
      800227656,
      800227657
    ],
    "status": "Not started",
    "url_conditions": [
      {
        "index": 0,
        "match_type": "simple",
        "created": "2014-04-12T19:10:53.806640Z",
        "value": "http://blog.optimizely.com/2014/04/11/10-reasons-why-your-agency-should-offer-optimization/",
        "last_modified": "2014-04-12T19:10:53.806650Z",
        "negate": false
      }
    ],
    "description": "Wordpress: 10 Reasons Why Your Agency Should Offer Optimization ",
    "last_modified": "2014-04-12T19:10:53.806650Z",
    "activation_mode": "immediate",
    "details": "Experiment to test out blog post.",
    "custom_css": "",
    "created": "2014-04-12T19:10:53.588450Z",
    "custom_js": "",
    "primary_goal_id": null,
    "experiment_type": "ab",
    "shareable_results_link": "https://www.optimizely.com/results?experiment_id=791495413&token=fh3lk2hrlk",
    "edit_url": "http://blog.optimizely.com/2014/04/11/10-reasons-why-your-agency-should-offer-optimization/",
    "audience_ids": []
  }"""

  checkDeserialize "Schedule" (Proxy :: Proxy Schedule) """{
    "status": "ACTIVE",
    "start_time": "2015-01-01T08:00:00Z",
    "stop_time": null,
    "experiment_id": 5678,
    "id": 1234
  }"""

  checkDeserialize "Variation" (Proxy :: Proxy Variation) """{
    "is_paused": false,
    "description": "Variation #2",
    "weight": 5000,
    "created": "2014-04-17T00:47:06.388650Z",
    "section_id": null,
    "js_component": "alert('It works!');",
    "experiment_id": 854484703,
    "project_id": 859720118,
    "id": 859611684
  }"""

  checkDeserialize "Goal" (Proxy :: Proxy Goal) """{
    "is_editable": null,
    "target_to_experiments": true,
    "archived": false,
    "description": "Confirming if the navigation is used more or less. #nav",
    "id": 543071054,
    "target_urls": [],
    "title": "Navigation button clicks",
    "event": "nav_button_clicks",
    "url_match_types": [],
    "project_id": 547944643,
    "goal_type": 0,
    "experiment_ids": [
      561450169
    ],
    "selector": ".portal-navigation > button",
    "created": "2014-01-09T23:47:51.042343Z",
    "last_modified": "2014-12-08T12:33:27.045543Z",
    "target_url_match_types": [],
    "urls": []
  }"""

  checkDeserialize "Audience" (Proxy :: Proxy Audience) """{
    "description": "People from Canada",
    "project_id": 1234,
    "id": 567,
    "name": "Canadians",
    "created": "2014-05-24T00:13:52.784580Z",
    "conditions": "[\"and\", {\"type\":\"browser\", \"value\":\"gc\"}, {\"type\":\"query\", \"name\":\"utm_campaign\", \"value\":\"true\"}]",
    "last_modified": "2014-06-10T22:12:21.707170Z",
    "segmentation": false,
    "archived": false
  }"""

  checkDeserialize "TargetingList" (Proxy :: Proxy TargetingList) """{
    "name": "List_1",
    "description": "Description of List_1",
    "list_type": 2,
    "key_fields": "user_id",
    "id": 123,
    "project_id": 456,
    "account_id": 789,
    "format": "csv"
  }"""

  checkDeserialize "Dimension" (Proxy :: Proxy Dimension) """{
    "name": "My Dimension",
    "last_modified": "2015-01-01T00:00:00.000000Z",
    "client_api_name": "my_dimension_api_name",
    "project_id": 5678,
    "id": 1234,
    "description": "Description of my dimension."
  }"""

  checkDeserialize "Result" (Proxy :: Proxy Result) """{
    "variation_id": "925781903",
    "variation_name": "My Variation",
    "goal_id": 820360058,
    "goal_name": "Engagement",
    "baseline_id": "924521605",
    "begin_time": "2014-07-25T20:30:00Z",
    "end_time": "2014-07-25T20:38:09Z",
    "visitors": 853,
    "conversions": 204,
    "conversion_rate": 0.239,
    "status": "inconclusive",
    "improvement": 0.014,
    "statistical_significance": 0.631,
    "difference": 0.014,
    "difference_confidence_interval_min": 0.008,
    "difference_confidence_interval_max": 0.020,
    "visitors_until_statistically_significant": 100,
    "is_revenue": false
  }"""

  checkDeserialize "DCP.Service" (Proxy :: Proxy DCP.Service) """{
    "id": 567,
    "account_id": 123456,
    "archived": false,
    "aws_access_key": "123423asfakedf12vh451234",
    "aws_secret_key": "1234fake12341asdfas234zc",
    "created": "2015-08-01T11:50:37.864010Z",
    "last_modified": "2015-08-18T21:38:55.927670Z",
    "name": "My DCP Service",
    "s3_path": "dcp/567"
  }"""

  checkDeserialize "DCP.Datasource" (Proxy :: Proxy DCP.Datasource) """{
    "id": 678,
    "archived": false,
    "attributes": [],
    "aws_access_key": "AKfakekeyV8SH8XTJBUPO",
    "aws_secret_key": "ailb234vK/fakekeyc8SH8SeGCh2leiuX",
    "created": "2015-08-20T23:26:08.414110Z",
    "dcp_service_id": 567,
    "description": "First party data from my Data Warehouse",
    "is_optimizely": false,
    "keyfield_locator_name": "_my_hashedEmailcookie",
    "keyfield_locator_type": "cookie",
    "last_modified": "2015-08-20T23:26:08.414140Z",
    "name": "My Data Warehouse",
    "s3_path": "dcp/567/678"
  }"""

  checkDeserialize "DCP.DatasourceAttribute" (Proxy :: Proxy DCP.DatasourceAttribute) """{
    "archived": false,
    "created": "2015-08-18T21:38:55.927670Z",
    "datatype": "long",
    "dcp_datasource_id": 678,
    "description": "Predicted LTV, per growth team",
    "format": null,
    "id": 789,
    "is_value_public": false,
    "last_modified": "2015-08-18T21:38:55.927680Z",
    "name": "Life-time value"
  }"""

checkDeserialize :: forall e. IsForeign e => String -> Proxy e -> String -> TestSuite TestEffs
checkDeserialize name _ json =
    test ("Check that we can deserialize a " <> name) $ do
        assertExcept (readJSON json :: F e)

assertExcept :: forall e a. Show e => Except e a -> Test TestEffs
assertExcept = assertExceptT <<< generalize
