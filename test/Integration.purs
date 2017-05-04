module Test.Integration where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, now, nowDateTime)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array (head)
import Data.DateTime (adjust)
import Data.DateTime.Foreign (DateTime(..))
import Data.DateTime.Instant (unInstant)
import Data.DateTime.Locale (LocalValue(..))
import Data.Foreign.Null (Null(..))
import Data.Foreign.Undefined (Undefined(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..), singleton)
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Time.Duration (class Duration, Days(..), Minutes(..))
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX)
import Test.Common (assertExceptT)
import Test.Unit (TestSuite, test, suite)
import Test.Unit.Console (TESTOUTPUT)

import Data.Optimizely hiding (LocatorType, locatorName)
import Data.Optimizely.DCP as DCP
import Data.Optimizely.Project (emptyProject)
import Data.Optimizely.Audience (EditTargetingList, ListType(..), emptyAudience, emptyDimension, RootCondition(..), Condition(..), emptyRule)
import Data.Optimizely.Audience.Internal (ListName, listName)
import Data.Optimizely.Experiment (ExperimentStatus(..), NewGoal(..), NewGoalFields(..), emptyExperiment, emptyGoal, emptyVariation)
import Data.Optimizely.Experiment.Internal (MatchType(..), UrlMatchType(..))
import Network.Optimizely as Api
import Network.Optimizely.Auth (Auth)
import Network.Optimizely.Internal (OptimizelyError(..))


type TestEffs eff = (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, ajax :: AJAX, now :: NOW | eff)



integration :: forall eff. Maybe Auth -> TestSuite (TestEffs eff)
integration Nothing = pure unit
integration (Just tkn) = suite "Integration tests" $ do
  test "Project CRUD" $ assertExceptT $ tkn # (runReaderT $ do
    (Project prj) <- ReaderT $ Api.newProject emptyProject{project_name="foo"}
    ReaderT Api.listProjects
    ReaderT $ Api.updateProject prj.id emptyProject{project_name=(Undefined (Just "foobar"))})

  test "Experiment CRUD" $ assertExceptT $ do
    projects <- Api.listProjects tkn
    (Project prj) <- first projects
    let createExperiment = Api.newExperiment prj.id emptyExperiment{description="Test Description", edit_url="http://example.com"} tkn
    (Experiment exp) <- createExperiment
    Api.listExperiments prj.id tkn
    Api.updateExperiment exp.id emptyExperiment tkn
    Api.deleteExperiment exp.id tkn
    createExperiment

  let retrieveFirstProject = unwrap <$> (first =<< Api.listProjects tkn)
      retrieveExperiment = unwrap <$> (first =<< flip Api.listExperiments tkn =<< _.id <$> retrieveFirstProject)

  test "Schedule CRUD" $ assertExceptT $ do
    exp <- retrieveExperiment
    beginning <- 15.0 # Minutes # fromNow
    (Schedule sched) <- Api.newSchedule exp.id (Api.start beginning) tkn
    Api.listSchedules exp.id tkn
    conclusion <- 3.0 # Days # fromNow
    Api.updateSchedule sched.id (Api.fromTo beginning conclusion) tkn
    Api.deleteSchedule sched.id tkn

  test "Variation CRUD" $ assertExceptT $ do
    exp <- retrieveExperiment
    (Variation var) <- Api.newVariation exp.id emptyVariation{description="Test Variation"} tkn
    Api.listVariations exp.id tkn
    Api.updateVariation var.id emptyVariation{description=(Undefined (Just "foo"))} tkn
    Api.deleteVariation var.id tkn

  let goal1 :: NewGoal
      goal1 = (NewGoal "click goal 1" (NewClick "div.selector" Nothing))

      goal2 :: NewGoal
      goal2 = (NewGoal "click goal 2"
                       (NewClick "div.selector"
                                 (Just { target_urls: ["http://example.org"]
                                       , target_url_match_types: [Simple]})))

      goal3 :: NewGoal
      goal3 = (NewGoal "pageview goal"
                       (NewPageView (singleton "http://example.org")
                                    (singleton $ UrlMatchType Exact)))

      goal4 :: NewGoal
      goal4 = (NewGoal "custom goal" (NewCustom "eventname"))

      goals :: NonEmptyList NewGoal
      goals = NonEmptyList $ goal1 :| goal2 : goal3 : goal4 : Nil


  test "Goal CRUD" $ assertExceptT $ do
    prj <- retrieveFirstProject
    createdGoals@(NonEmptyList (created1 :| createdOthers)) <- traverse (\g -> unwrap <$> Api.newGoal prj.id g tkn) goals
    Api.listGoals prj.id tkn
    Api.updateGoal created1.id emptyGoal{description=Undefined (Just "First goal")} tkn
    traverse (\{id} -> Api.deleteGoal id tkn) createdGoals

  let firstSession :: Condition
      firstSession = Rule emptyRule{type="first_session"}

      condition :: RootCondition
      condition = RootCondition $ Just $ And $ NonEmptyList $ firstSession :| Not (Not firstSession) : Nil

  test "Audience CRUD" $ assertExceptT $ do
    prj <- retrieveFirstProject
    tstamp <- currentTimeStamp
    (Audience aud) <- Api.newAudience prj.id emptyAudience{name="Test Audience" <> tstamp} tkn
    Api.listAudiences prj.id tkn
    Api.updateAudience aud.id emptyAudience{ name=Undefined (Just ("Renamed Audience" <> tstamp))
                                           , conditions=Undefined (Just condition)} tkn

  test "TargetingList CRUD" $ assertExceptT $ do
    prj <- retrieveFirstProject
    lname <- listName' "Test_list1"
    (TargetingList tlst) <- Api.newTargetingList prj.id (tLstFields lname) tkn
    Api.listTargetingLists prj.id tkn
    Api.updateTargetingList tlst.id (tLstFields lname){description=Undefined (Just "Test List")} tkn
    Api.deleteTargetingList tlst.id tkn

  test "Dimension CRUD" $ assertExceptT $ do
    prj <- retrieveFirstProject
    tstamp <- currentTimeStamp
    (Dimension dim) <- Api.newDimension prj.id emptyDimension{name="Test Dimension" <> tstamp} tkn
    Api.listDimensions prj.id tkn
    Api.updateDimension dim.id emptyDimension{name=Undefined (Just ("Renamed Dimension" <> tstamp))} tkn
    Api.deleteDimension dim.id tkn

  test "Retrieve stats" $ assertExceptT $ do
    exp <- retrieveExperiment
    Api.updateExperiment exp.id emptyExperiment{status=Undefined (Just Running)} tkn
    Api.getStats exp.id tkn

-- These tests are not run, since a free developer account doesn't have permission to access these endpoints
integrationDCP :: forall eff. Auth -> TestSuite (TestEffs eff)
integrationDCP tkn = do
  test "DCPService CRUD" $ assertExceptT $ do
    (DCP.Service svc) <- Api.newDCPService "Test Service" tkn
    Api.listDCPServices tkn
    Api.updateDCPService svc.id "Renamed Service" tkn
    Api.deleteDCPService svc.id tkn
    Api.newDCPService "Test Service" tkn

  test "DCPDatasource CRUD" $ assertExceptT $ do
    (DCP.Service svc) <- first =<< Api.listDCPServices tkn
    lname <- locatorName "attributeKey"
    (DCP.Datasource dsrc) <- Api.newDCPDatasource svc.id (dSrcFields lname) tkn
    Api.listDCPDatasources svc.id tkn
    Api.updateDCPDatasource dsrc.id DCP.emptyDatasource{name=Undefined (Just "Renamed Datasource")} tkn
    Api.deleteDCPDatasource dsrc.id tkn
    Api.newDCPDatasource svc.id (dSrcFields lname) tkn

  test "DCPDatasourceAttribute CRUD" $ assertExceptT $ do
    (DCP.Service svc) <- first =<< Api.listDCPServices tkn
    (DCP.Datasource dsrc) <- first =<< Api.listDCPDatasources svc.id tkn
    (DCP.DatasourceAttribute dsattr) <- Api.newDCPDatasourceAttribute dsrc.id (DCP.NewAttribute "flag" DCP.NewBool) tkn
    Api.listDCPDatasourceAttributes dsrc.id tkn
    Api.updateDCPDatasourceAttribute dsattr.id "Description" tkn
    Api.deleteDCPDatasourceAttribute dsattr.id tkn


tLstFields :: ListName -> EditTargetingList
tLstFields lname =
    { name : lname
    , list_type : Cookie
    , list_content : "1,2,3"
    , key_fields : "uid"
    , description : Undefined Nothing
    }

dSrcFields :: DCP.LocatorName -> DCP.NewDatasource
dSrcFields lname =
    { name : ""
    , keyfield_locator_type : DCP.Cookie
    , keyfield_locator_name : Null (Just lname)
    , description : Undefined Nothing
    }

currentTimeStamp :: forall m eff. MonadEff (now :: NOW | eff) m => m String
currentTimeStamp = liftEff $ show <<< unwrap <<< unInstant <$> now

listName' :: forall m. (MonadError OptimizelyError m) => String -> m ListName
listName' = maybe' (const $ throwError $ Other "Incorrect ListName") pure <<< listName

locatorName :: forall m. (MonadError OptimizelyError m) => String -> m DCP.LocatorName
locatorName = maybe' (const $ throwError $ Other "Incorrect LocatorName") pure <<< DCP.locatorName

fromNow :: forall m eff d. (Duration d, MonadError OptimizelyError m, MonadEff (now :: NOW | eff) m) => d -> m DateTime
fromNow duration = do
  (LocalValue _ current) <- liftEff nowDateTime
  maybe' (const $ throwError $ Other "Invalid datetime adjustment") (pure <<< DateTime) $ adjust duration current

first :: forall m a. (MonadError OptimizelyError m) => Array a -> m a
first = maybe' (const $ throwError $ Other "No values returned") pure <<< head

