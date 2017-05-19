module Data.Optimizely.Experiment where

import Prelude
import Data.Array (fromFoldable)
import Data.DateTime.Foreign (DateTime)
import Data.Foreign (ForeignError(..), fail, readString)
import Data.Foreign.Class (class IsForeign, class AsForeign, write)
import Data.Foreign.Generic (toForeignGeneric, readGeneric)
import Data.Foreign.Null (Null(..))
import Data.Foreign.Undefined (Undefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax.Request (class Requestable)

import Data.Optimizely.Project (Project)
import Data.Optimizely.Audience (Audience)
import Data.Optimizely.Common (Id, Section, foreignOptions, foreignToRequest)
import Data.Optimizely.Experiment.Internal (GoalType(..), MatchType, UrlMatchType)


data ExperimentStatus = Running | Paused | NotStarted | Archived
derive instance genericExperimentStatus :: Generic ExperimentStatus _

instance foreignExperimentStatus :: IsForeign ExperimentStatus where
    read value = parseStatus =<< readString value
        where
            parseStatus "Running" = pure Running
            parseStatus "Paused" = pure Paused
            parseStatus "Not started" = pure NotStarted
            parseStatus "Archived" = pure Archived
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected Running, Paused, \"Not started\", Archived, found " <> val

instance asForeignExperimentStatus :: AsForeign ExperimentStatus where
    write Running = write "Running"
    write Paused = write "Paused"
    write NotStarted = write "Not started"
    write Archived = write "Archived"

instance showExperimentStatus :: Show ExperimentStatus where
    show = genericShow

newtype Condition = Condition
    { match_type :: MatchType
    , value :: URL
    , negate :: Boolean
    }
derive instance genericCondition :: Generic Condition _

instance foreignCondition :: IsForeign Condition where
    read = readGeneric foreignOptions

instance asForeignCondition :: AsForeign Condition where
    write = toForeignGeneric foreignOptions

instance showCondition :: Show Condition where
    show = genericShow


data ActivationMode = Immediate | Manual | Conditional
derive instance genericActivationMode :: Generic ActivationMode _

instance foreignActivationMode :: IsForeign ActivationMode where
    read value = parseStatus =<< readString value
        where
            parseStatus "immediate" = pure Immediate
            parseStatus "manual" = pure Manual
            parseStatus "conditional" = pure Conditional
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected immediate, manual, conditional, found " <> val

instance asForeignActivationMode :: AsForeign ActivationMode where
    write Immediate = write "immediate"
    write Manual = write "manual"
    write Conditional = write "conditional"

instance showActivationMode :: Show ActivationMode where
    show = genericShow

data ExperimentType = AB | MultiVariate | MultiPage
derive instance genericExperimentType :: Generic ExperimentType _


instance foreignExperimentType :: IsForeign ExperimentType where
    read value = parseStatus =<< readString value
        where
            parseStatus "ab" = pure AB
            parseStatus "multivariate" = pure MultiVariate
            parseStatus "multipage" = pure MultiPage
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected ab, multivariate, multipage, found " <> val

instance showExperimentType :: Show ExperimentType where
    show = genericShow



newtype Experiment = Experiment
    { id :: Id Experiment
    , percentage_included :: Int
    , display_goal_order_lst :: Array (Id Goal)
    , is_multivariate :: Boolean
    , project_id :: Id Project
    , variation_ids :: Array (Id Variation)
    , status :: ExperimentStatus
    , url_conditions :: Array Condition
    , description :: String
    , last_modified :: DateTime
    , activation_mode :: Null ActivationMode
    , details :: String
    , custom_css :: String
    , created :: DateTime
    , custom_js :: String
    , primary_goal_id :: Null (Id Goal)
    , experiment_type :: ExperimentType
    , shareable_results_link :: URL
    , edit_url :: URL
    , audience_ids :: Array (Id Audience)
    }
derive instance genericExperiment :: Generic Experiment _
derive instance newtypeExperiment :: Newtype Experiment _

instance foreignExperiment :: IsForeign Experiment where
    read = readGeneric foreignOptions

instance showExperiment :: Show Experiment where
    show = genericShow

type EditExperiment desc edit_url =
    { description :: desc
    , edit_url :: edit_url
    , audience_ids :: Undefined (Array (Id Audience))
    , activation_mode :: Undefined (Null ActivationMode)
    , status :: Undefined ExperimentStatus
    , custom_css :: Undefined String
    , custom_js :: Undefined String
    , percentage_included :: Undefined Int
    , url_conditions :: Undefined (Array Condition)
    }

type NewExperiment = EditExperiment String URL
type PutExperiment = EditExperiment (Undefined String) (Undefined URL)

newtype MkNewExperiment = MkNewExperiment NewExperiment
derive instance genericNewExperiment :: Generic MkNewExperiment _

instance asForeignNewExperiment :: AsForeign MkNewExperiment where
    write = toForeignGeneric foreignOptions

instance requestableNewExperiment :: Requestable MkNewExperiment where
    toRequest = foreignToRequest

newtype MkPutExperiment = MkPutExperiment PutExperiment
derive instance genericPutExperiment :: Generic MkPutExperiment _

instance asForeignPutExperiment :: AsForeign MkPutExperiment where
    write = toForeignGeneric foreignOptions

instance requestablePutExperiment :: Requestable MkPutExperiment where
    toRequest = foreignToRequest

emptyExperiment :: PutExperiment
emptyExperiment =
    { description : Undefined Nothing
    , edit_url : Undefined Nothing
    , audience_ids : Undefined Nothing
    , activation_mode : Undefined Nothing
    , status : Undefined Nothing
    , custom_css : Undefined Nothing
    , custom_js : Undefined Nothing
    , percentage_included : Undefined Nothing
    , url_conditions : Undefined Nothing
    }

newtype Variation = Variation
    { is_paused :: Boolean
    , description :: String
    , weight :: Null Int
    , created :: DateTime
    , section_id :: Null (Id Section)
    , js_component :: Null String
    , experiment_id :: Id Experiment
    , project_id :: Id Project
    , id :: Id Variation
    }
derive instance genericVariation :: Generic Variation _
derive instance newtypeVariation :: Newtype Variation _

instance foreignVariation :: IsForeign Variation where
    read = readGeneric foreignOptions

instance showVariation :: Show Variation where
    show = genericShow

type EditVariation desc =
    { description :: desc
    , is_paused :: Undefined Boolean
    , js_component :: Undefined String
    , weight :: Undefined Int
    }

type NewVariation = EditVariation String
type PutVariation = EditVariation (Undefined String)

newtype MkNewVariation = MkNewVariation NewVariation
derive instance genericNewVariation :: Generic MkNewVariation _

instance asForeignNewVariation :: AsForeign MkNewVariation where
    write = toForeignGeneric foreignOptions

instance requestableNewVariation :: Requestable MkNewVariation where
    toRequest = foreignToRequest

newtype MkPutVariation = MkPutVariation PutVariation
derive instance genericPutVariation :: Generic MkPutVariation _

instance asForeignPutVariation :: AsForeign MkPutVariation where
    write = toForeignGeneric foreignOptions

instance requestablePutVariation :: Requestable MkPutVariation where
    toRequest = foreignToRequest

emptyVariation :: PutVariation
emptyVariation =
    { description : Undefined Nothing
    , is_paused : Undefined Nothing
    , js_component : Undefined Nothing
    , weight : Undefined Nothing
    }



newtype Goal = Goal
    { is_editable :: Null Boolean
    , target_to_experiments :: Null Boolean
    , archived :: Boolean
    , description :: String
    , id :: Id Goal
    , target_urls :: Array String
    , title :: String
    , event :: Null String
    , url_match_types :: Array UrlMatchType
    , project_id :: Id Project
    , goal_type :: GoalType
    , experiment_ids :: Array (Id Experiment)
    , selector :: Null String
    , created :: DateTime
    , last_modified :: DateTime
    , target_url_match_types :: Array MatchType
    , urls :: Array URL
    }
derive instance genericGoal :: Generic Goal _
derive instance newtypeGoal :: Newtype Goal _

instance foreignGoal :: IsForeign Goal where
    read = readGeneric foreignOptions

instance showGoal :: Show Goal where
    show = genericShow

data NewGoal = NewGoal String NewGoalFields

data NewGoalFields
    = NewClick String
               (Maybe { target_urls :: Array String
                      , target_url_match_types :: Array MatchType})
    | NewPageView (NonEmptyList URL)
                  (NonEmptyList UrlMatchType)
    | NewCustom String

type PutGoal =
    { archived :: Undefined Boolean
    , description :: Undefined String
    , experiment_ids :: Undefined (Array (Id Experiment))
    , goal_type :: Undefined GoalType
    , selector :: Undefined String
    , target_to_experiments :: Undefined Boolean
    , target_urls :: Undefined (Array String)
    , target_url_match_types :: Undefined (Array MatchType)
    , title :: Undefined String
    , urls :: Undefined (Array URL)
    , url_match_types :: Undefined (Array UrlMatchType)
    , event :: Undefined String
    }

emptyGoal :: PutGoal
emptyGoal =
    { archived : Undefined Nothing
    , description : Undefined Nothing
    , experiment_ids : Undefined Nothing
    , goal_type : Undefined Nothing
    , selector : Undefined Nothing
    , target_to_experiments : Undefined Nothing
    , target_urls : Undefined Nothing
    , target_url_match_types : Undefined Nothing
    , title : Undefined Nothing
    , urls : Undefined Nothing
    , url_match_types : Undefined Nothing
    , event : Undefined Nothing
    }

just :: forall a. a -> Undefined a
just = Undefined <<< Just

putNewGoal :: NewGoal -> MkPutGoal
putNewGoal (NewGoal title newgoal) = newGoalFields emptyGoal{title=just title} newgoal

newGoalFields :: PutGoal -> NewGoalFields -> MkPutGoal
newGoalFields record (NewClick selector Nothing)
    = MkPutGoal record{ goal_type=just Click
                      , selector=just selector
                      , target_to_experiments=just true
                      }
newGoalFields record (NewClick selector (Just {target_urls, target_url_match_types}))
    = MkPutGoal record{ goal_type=just Click
                      , selector=just selector
                      , target_to_experiments=just false
                      , target_urls=just target_urls
                      , target_url_match_types=just target_url_match_types
                      }
newGoalFields record (NewPageView urls url_match_types)
    = MkPutGoal record{ goal_type=just PageViews
                      , urls=just $ fromFoldable urls
                      , url_match_types=just $ fromFoldable url_match_types
                      }
newGoalFields record (NewCustom event)
    = MkPutGoal record{ goal_type=just CustomEvent
                      , event=just event
                      }

newtype MkPutGoal = MkPutGoal PutGoal
derive instance genericPutGoal :: Generic MkPutGoal _

instance asForeignPutGoal :: AsForeign MkPutGoal where
    write = toForeignGeneric foreignOptions

instance requestablePutGoal :: Requestable MkPutGoal where
    toRequest = foreignToRequest
