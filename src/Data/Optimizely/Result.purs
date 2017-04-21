module Data.Optimizely.Result where

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
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (URL)

import Data.Optimizely.Common (Id(..), foreignOptions)
import Data.Optimizely.Experiment (Goal(..), Variation(..))


data ResultStatus = Winner | Loser | Inconclusive | Baseline
derive instance genericResultStatus :: Generic ResultStatus _

instance foreignResultStatus :: IsForeign ResultStatus where
    read value = parseStatus =<< readString value
        where
            parseStatus "winner" = pure Winner
            parseStatus "loser" = pure Loser
            parseStatus "inconclusive" = pure Inconclusive
            parseStatus "baseline" = pure Baseline
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected winner, loser, inconclusive, baseline, found " <> val

instance showResultStatus :: Show ResultStatus where
    show = genericShow

type StringId = String

newtype Result = Result
    { variation_id :: Id Variation
    , variation_name :: String
    , goal_id :: Id Goal
    , goal_name :: String
    , baseline_id :: Id Variation -- String ?
    , begin_time :: DateTime
    , end_time :: DateTime
    , visitors :: Number
    , conversions :: Number
    , conversion_rate :: Number
    , status :: ResultStatus
    , improvement :: Number
    , statistical_significance :: Number
    , difference :: Number
    , difference_confidence_interval_max :: Number
    , difference_confidence_interval_min :: Number
    , visitors_until_statistically_significant :: Number
    , is_revenue :: Boolean
    }
derive instance genericResult :: Generic Result _
derive instance newtypeResult :: Newtype Result _

instance foreignResult :: IsForeign Result where
    read = readGeneric foreignOptions

instance showResult :: Show Result where
    show = genericShow



