module Data.Optimizely.Experiment where

import Prelude
import Data.DateTime as D
import Data.List.NonEmpty as NEL
import Control.Bind ((=<<))
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (throwError)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.DateTime.Foreign (DateTime)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum, fromEnum)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readInt, readString)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (toForeignGeneric, defaultOptions, readGeneric)
import Data.Foreign.Null (Null)
import Data.Foreign.Undefined (Undefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe')
import Network.HTTP.Affjax (URL)

import Data.Optimizely.Common (MatchType)

foreignOptions = defaultOptions{unwrapSingleConstructors=true}

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

instance showExperimentStatus :: Show ExperimentStatus where
    show = genericShow

newtype Condition = Condition
    { index :: Int
    , match_type :: MatchType
    , created :: DateTime
    , value :: URL
    , last_modified :: DateTime
    , negate :: Boolean
    }
derive instance genericCondition :: Generic Condition _

instance foreignCondition :: IsForeign Condition where
    read = readGeneric foreignOptions

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
    { id :: Number
    , percentage_included :: Int
    , display_goal_order_lst :: Array Number
    , is_multivariate :: Boolean
    , project_id :: Number
    , variation_ids :: Array Number
    , status :: ExperimentStatus
    , url_conditions :: Array Condition
    , description :: String
    , last_modified :: DateTime
    , activation_mode :: ActivationMode
    , details :: String
    , custom_css :: String
    , created :: DateTime
    , custom_js :: String
    , primary_goal_id :: Null Number
    , experiment_type :: ExperimentType
    , shareable_results_link :: URL
    , edit_url :: URL
    , audience_ids :: Array Number
    }
derive instance genericExperiment :: Generic Experiment _

instance foreignExperiment :: IsForeign Experiment where
    read = readGeneric foreignOptions

instance showExperiment :: Show Experiment where
    show = genericShow
