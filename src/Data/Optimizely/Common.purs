module Data.Optimizely.Common where

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
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write)
import Data.Foreign.Generic (toForeignGeneric, defaultOptions, readGeneric)
import Data.Foreign.Null (Null)
import Data.Foreign.Undefined (Undefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe')
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Optimizely.Internal (readBoundedEnum)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax.Request (RequestContent)
import Unsafe.Coerce (unsafeCoerce)


foreignOptions = defaultOptions{unwrapSingleConstructors=true}

foreignToRequest :: forall a. AsForeign a => a -> Tuple (Maybe MediaType) RequestContent
foreignToRequest x = Tuple (Just applicationJSON) $ unsafeCoerce $ unsafeStringify $ write x

newtype Variation = Variation
    { is_paused :: Boolean
    , description :: String
    , weight :: Int
    , created :: DateTime
    , section_id :: Null Number
    , js_component :: String
    , experiment_id :: Number
    , project_id :: Number
    , id :: Number
    }
derive instance genericVariation :: Generic Variation _

instance foreignVariation :: IsForeign Variation where
    read = readGeneric foreignOptions

instance showVariation :: Show Variation where
    show = genericShow

data ScheduleStatus = Active | Inactive
derive instance genericScheduleStatus :: Generic ScheduleStatus _

instance foreignScheduleStatus :: IsForeign ScheduleStatus where
    read value = parseStatus =<< readString value
        where
            parseStatus "ACTIVE" = pure Active
            parseStatus "INACTIVE" = pure Inactive
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected ACTIVE or INACTIVE, found " <> val

instance showScheduleStatus :: Show ScheduleStatus where
    show = genericShow

newtype Schedule = Schedule
    { status :: ScheduleStatus
    , start_time :: Null DateTime
    , stop_time :: Null DateTime
    , experiment_id :: Number
    , id :: Number
    }
derive instance genericSchedule :: Generic Schedule _

instance foreignSchedule :: IsForeign Schedule where
    read = readGeneric foreignOptions

instance showSchedule :: Show Schedule where
    show = genericShow

data GoalType = Click | CustomEvent | Engagement | PageViews | Revenue
derive instance genericGoalType :: Generic GoalType _
derive instance eqGoalType :: Eq GoalType
derive instance ordGoalType :: Ord GoalType

instance boundedGoalType :: Bounded GoalType where
  bottom = Click
  top = Revenue

instance enumGoalType :: Enum GoalType where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumGoalType :: BoundedEnum GoalType where
  cardinality = Cardinality 5
  toEnum = case _ of
    0 -> Just Click
    1 -> Just CustomEvent
    2 -> Just Engagement
    3 -> Just PageViews
    4 -> Just Revenue
    _ -> Nothing
  fromEnum = case _ of
    Click -> 0
    CustomEvent -> 1
    Engagement -> 2
    PageViews -> 3
    Revenue -> 4

instance foreignGoalType :: IsForeign GoalType where
    read = readBoundedEnum

instance showGoalType :: Show GoalType where
    show = genericShow


data MatchType = Simple | Regex | Exact | Substring
derive instance genericMatchType :: Generic MatchType _
derive instance eqMatchType :: Eq MatchType
derive instance ordMatchType :: Ord MatchType

instance foreignMatchType :: IsForeign MatchType where
    read value = parseStatus =<< readString value
        where
            parseStatus "simple" = pure Simple
            parseStatus "regex" = pure Regex
            parseStatus "exact" = pure Exact
            parseStatus "substring" = pure Substring
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected simple, regex, exact, substring, found " <> val

instance showMatchType :: Show MatchType where
    show = genericShow


newtype UrlMatchType = UrlMatchType MatchType
derive newtype instance ordUrlMatchType :: Ord UrlMatchType
derive newtype instance showUrlMatchType :: Show UrlMatchType

instance boundedUrlMatchType :: Bounded UrlMatchType where
  bottom = UrlMatchType Exact
  top = UrlMatchType Substring

instance enumUrlMatchType :: Enum UrlMatchType where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumUrlMatchType :: BoundedEnum UrlMatchType where
  cardinality = Cardinality 4
  toEnum = case _ of
    1 -> Just $ UrlMatchType Exact
    2 -> Just $ UrlMatchType Regex
    3 -> Just $ UrlMatchType Simple
    4 -> Just $ UrlMatchType Substring
    _ -> Nothing
  fromEnum = case _ of
    (UrlMatchType Exact) -> 1
    (UrlMatchType Regex) -> 2
    (UrlMatchType Simple) -> 3
    (UrlMatchType Substring) -> 4

instance foreignUrlMatchType :: IsForeign UrlMatchType where
    read = readBoundedEnum


newtype Goal = Goal
    { is_editable :: Null Boolean
    , target_to_experiments :: Boolean
    , archived :: Boolean
    , description :: String
    , id :: Number
    , target_urls :: Array String
    , title :: String
    , event :: String
    , url_match_types :: Array UrlMatchType
    , project_id :: Number
    , goal_type :: GoalType
    , experiment_ids :: Array Number
    , selector :: String
    , created :: DateTime
    , last_modified :: DateTime
    , target_url_match_types :: Array UrlMatchType
    , urls :: Array URL
    }
derive instance genericGoal :: Generic Goal _

instance foreignGoal :: IsForeign Goal where
    read = readGeneric foreignOptions

instance showGoal :: Show Goal where
    show = genericShow
