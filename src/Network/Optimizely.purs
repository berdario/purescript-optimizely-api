module Network.Optimizely
    ( module Network.Optimizely
    , module Network.Optimizely.Internal
    ) where

import Prelude
import Data.Optimizely
import Control.Monad.Aff (Aff)
import Control.Monad.Except (ExceptT(..), Except, mapExceptT, withExcept, throwError)
import Control.Monad.Trans.Class (lift)
import Data.DateTime.Foreign (DateTime)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, MultipleErrors)
import Data.Foreign.Class (class IsForeign, read)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Maybe (maybe)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.These (These(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxRequest, AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.StatusCode (StatusCode(..))

import Data.Optimizely.Common (Id(..))
import Data.Optimizely.Project (Project(..), NewProject, MkNewProject(..), PutProject, MkPutProject(..))
import Data.Optimizely.Experiment (Experiment(..), NewExperiment, PutExperiment, MkNewExperiment(..), MkPutExperiment(..), Variation(..), NewVariation, PutVariation, MkNewVariation(..), MkPutVariation(..), Goal(..), NewGoal, PutGoal, putNewGoal, MkPutGoal(..))
import Data.Optimizely.Schedule (Schedule(..), EditSchedule(..))
import Data.Optimizely.Audience (Audience, NewAudience, MkNewAudience(..), PutAudience, MkPutAudience(..), EditTargetingList, mkEditTargetingList, TargetingList, Dimension, NewDimension, MkNewDimension(..), PutDimension, MkPutDimension(..))
import Data.Optimizely.Result (Result)
import Data.Optimizely.DCP as DCP
import Network.Optimizely.Auth (Auth, toHeader)
import Network.Optimizely.Internal (OptimizelyError(..), H, get, post, put, delete, executeRequest, executeGet, executePost, executePut, executeDelete)


getProject :: forall eff. Id Project -> Auth -> H eff Project
getProject id = executeGet ("projects/" <> show id)

newProject :: forall eff. NewProject -> Auth -> H eff Project
newProject content = executePost "projects/" (MkNewProject content)

updateProject :: forall eff. Id Project -> PutProject -> Auth -> H eff Project
updateProject id content = executePut ("projects/" <> show id) (MkPutProject content)

listProjects :: forall eff. Auth -> H eff (Array Project)
listProjects = executeGet "projects/"

getExperiment :: forall eff. Id Experiment -> Auth -> H eff Experiment
getExperiment id = executeGet ("experiments/" <> show id)

newExperiment :: forall eff. Id Project -> NewExperiment -> Auth -> H eff Experiment
newExperiment projectId content = executePost ("projects/" <> show projectId <> "/experiments") (MkNewExperiment content)

updateExperiment :: forall eff. Id Experiment -> PutExperiment -> Auth -> H eff Experiment
updateExperiment id content = executePut ("experiments/" <> show id) (MkPutExperiment content)

deleteExperiment :: forall eff. Id Experiment -> Auth -> H eff (AffjaxResponse Unit)
deleteExperiment id = executeDelete ("experiments/" <> show id)

listExperiments :: forall eff. Id Project -> Auth -> H eff (Array Experiment)
listExperiments id = executeGet ("projects/" <> show id <> "/experiments")

getSchedule :: forall eff. Id Schedule -> Auth -> H eff Schedule
getSchedule id = executeGet ("schedules/" <> show id)

type StartDate = DateTime
type EndDate = DateTime

start :: StartDate -> These StartDate EndDate
start = This
end :: EndDate -> These StartDate EndDate
end = That
fromTo :: StartDate -> EndDate -> These StartDate EndDate
fromTo = Both

newSchedule :: forall eff. Id Experiment -> These StartDate EndDate -> Auth -> H eff Schedule
newSchedule experimentId content = executePost ("experiments/" <> show experimentId <> "/schedules") (EditSchedule content)

updateSchedule :: forall eff. Id Schedule -> These StartDate EndDate -> Auth -> H eff Schedule
updateSchedule id content = executePut ("schedules/" <> show id) (EditSchedule content)

deleteSchedule :: forall eff. Id Schedule -> Auth -> H eff (AffjaxResponse Unit)
deleteSchedule id = executeDelete ("schedules/" <> show id)

listSchedules :: forall eff. Id Experiment -> Auth -> H eff (Array Schedule)
listSchedules id = executeGet ("experiments/" <> show id <> "/schedules")

getVariation :: forall eff. Id Variation -> Auth -> H eff Variation
getVariation id = executeGet ("variations/" <> show id)

newVariation :: forall eff. Id Experiment -> NewVariation -> Auth -> H eff Variation
newVariation experimentId content = executePost ("experiments/" <> show experimentId <> "/variations") (MkNewVariation content)

updateVariation :: forall eff. Id Variation -> PutVariation -> Auth -> H eff Variation
updateVariation id content = executePut ("variations/" <> show id) (MkPutVariation content)

deleteVariation :: forall eff. Id Variation -> Auth -> H eff (AffjaxResponse Unit)
deleteVariation id = executeDelete ("variations/" <> show id)

listVariations :: forall eff. Id Experiment -> Auth -> H eff (Array Variation)
listVariations id = executeGet ("experiments/" <> show id <> "/variations")

getGoal :: forall eff. Id Goal -> Auth -> H eff Goal
getGoal id = executeGet ("goals/" <> show id)

newGoal :: forall eff. Id Project -> NewGoal -> Auth -> H eff Goal
newGoal projectId content = executePost ("projects/" <> show projectId <> "/goals") (putNewGoal content)

updateGoal :: forall eff. Id Goal -> PutGoal -> Auth -> H eff Goal
updateGoal id content = executePut ("goals/" <> show id) (MkPutGoal content)

deleteGoal :: forall eff. Id Goal -> Auth -> H eff (AffjaxResponse Unit)
deleteGoal id = executeDelete ("goals/" <> show id)

listGoals :: forall eff. Id Project -> Auth -> H eff (Array Goal)
listGoals id = executeGet ("projects/" <> show id <> "/goals")

getAudience :: forall eff. Id Audience -> Auth -> H eff Audience
getAudience id = executeGet ("audiences/" <> show id)

newAudience :: forall eff. Id Project -> NewAudience -> Auth -> H eff Audience
newAudience projectId content = executePost ("projects/" <> show projectId <> "/audiences") (MkNewAudience content)

updateAudience :: forall eff. Id Audience -> PutAudience -> Auth -> H eff Audience
updateAudience id content = executePut ("audiences/" <> show id) (MkPutAudience content)

listAudiences :: forall eff. Id Project -> Auth -> H eff (Array Audience)
listAudiences id = executeGet ("projects/" <> show id <> "/audiences")

getTargetingList :: forall eff. Id TargetingList -> Auth -> H eff TargetingList
getTargetingList id = executeGet ("targetingLists/" <> show id)

newTargetingList :: forall eff. Id Project -> EditTargetingList -> Auth -> H eff TargetingList
newTargetingList projectId content = executePost ("projects/" <> show projectId <> "/targetingLists") (mkEditTargetingList content)

updateTargetingList :: forall eff. Id TargetingList -> EditTargetingList -> Auth -> H eff TargetingList
updateTargetingList id content = executePut ("targetingLists/" <> show id) (mkEditTargetingList content)

deleteTargetingList :: forall eff. Id TargetingList -> Auth -> H eff (AffjaxResponse Unit)
deleteTargetingList id = executeDelete ("targetingLists/" <> show id)

listTargetingLists :: forall eff. Id Project -> Auth -> H eff (Array TargetingList)
listTargetingLists id = executeGet ("projects/" <> show id <> "/targetingLists")

getDimension :: forall eff. Id Dimension -> Auth -> H eff Dimension
getDimension id = executeGet ("dimensions/" <> show id)

newDimension :: forall eff. Id Project -> NewDimension -> Auth -> H eff Dimension
newDimension projectId content = executePost ("projects/" <> show projectId <> "/dimensions") (MkNewDimension content)

updateDimension :: forall eff. Id Dimension -> PutDimension -> Auth -> H eff Dimension
updateDimension id content = executePut ("dimensions/" <> show id) (MkPutDimension content)

deleteDimension :: forall eff. Id Dimension -> Auth -> H eff (AffjaxResponse Unit)
deleteDimension id = executeDelete ("dimensions/" <> show id)

listDimensions :: forall eff. Id Project -> Auth -> H eff (Array Dimension)
listDimensions id = executeGet ("projects/" <> show id <> "/dimensions")

getStats :: forall eff. Id Experiment -> Auth -> H eff (Array Result)
getStats id = executeGet ("experiments/" <> show id <> "/stats")

getDCPService :: forall eff. Id DCP.Service -> Auth -> H eff DCP.Service
getDCPService id = executeGet ("dcp_services/" <> show id)

newDCPService :: forall eff. String -> Auth -> H eff DCP.Service
newDCPService content = executePost "dcp_services/" (DCP.EditService content)

updateDCPService :: forall eff. Id DCP.Service -> String -> Auth -> H eff DCP.Service
updateDCPService id content = executePut ("dcp_services/" <> show id) (DCP.EditService content)

deleteDCPService :: forall eff. Id DCP.Service -> Auth -> H eff (AffjaxResponse Unit)
deleteDCPService id = executeDelete ("dcp_services/" <> show id)

listDCPServices :: forall eff. Auth -> H eff (Array DCP.Service)
listDCPServices = executeGet "dcp_services/"

getDCPDatasource :: forall eff. Id DCP.Datasource -> Auth -> H eff DCP.Datasource
getDCPDatasource id = executeGet ("dcp_datasources/" <> show id)

newDCPDatasource :: forall eff. Id DCP.Service -> DCP.NewDatasource -> Auth -> H eff DCP.Datasource
newDCPDatasource serviceId content = executePost ("dcp_services/" <> show serviceId <> "/dcp_datasources") (DCP.MkNewDatasource content)

updateDCPDatasource :: forall eff. Id DCP.Datasource -> DCP.PutDatasource -> Auth -> H eff DCP.Datasource
updateDCPDatasource id content = executePut ("dcp_datasources/" <> show id) (DCP.MkPutDatasource content)

deleteDCPDatasource :: forall eff. Id DCP.Datasource -> Auth -> H eff (AffjaxResponse Unit)
deleteDCPDatasource id = executeDelete ("dcp_datasources/" <> show id)

listDCPDatasources :: forall eff. Id DCP.Service -> Auth -> H eff (Array DCP.Datasource)
listDCPDatasources id = executeGet ("dcp_services/" <> show id <> "/dcp_datasources")

getDCPDatasourceAttribute :: forall eff. Id DCP.DatasourceAttribute -> Auth -> H eff DCP.DatasourceAttribute
getDCPDatasourceAttribute id = executeGet ("dcp_datasource_attributes/" <> show id)

newDCPDatasourceAttribute :: forall eff. Id DCP.Datasource -> DCP.NewAttribute -> Auth -> H eff DCP.DatasourceAttribute
newDCPDatasourceAttribute datasourceId content = executePost ("dcp_datasources/" <> show datasourceId <> "/dcp_datasource_attributes") content

updateDCPDatasourceAttribute :: forall eff. Id DCP.DatasourceAttribute -> String -> Auth -> H eff DCP.DatasourceAttribute
updateDCPDatasourceAttribute id content = executePut ("dcp_datasource_attributes/" <> show id) (DCP.EditAttribute content)

deleteDCPDatasourceAttribute :: forall eff. Id DCP.DatasourceAttribute -> Auth -> H eff (AffjaxResponse Unit)
deleteDCPDatasourceAttribute id = executeDelete ("dcp_datasource_attributes/" <> show id)

listDCPDatasourceAttributes :: forall eff. Id DCP.Datasource -> Auth -> H eff (Array DCP.DatasourceAttribute)
listDCPDatasourceAttributes id = executeGet ("dcp_datasources/" <> show id <> "/dcp_datasource_attributes")
