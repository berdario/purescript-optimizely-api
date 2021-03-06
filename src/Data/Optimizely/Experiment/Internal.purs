module Data.Optimizely.Experiment.Internal where

import Prelude
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum, fromEnum)
import Data.Foreign (ForeignError(..), fail, readString)
import Data.Foreign.Class (class AsForeign, class IsForeign, write)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

import Data.Optimizely.Internal (readBoundedEnum)


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

instance asForeignGoalType :: AsForeign GoalType where
    write = write <<< fromEnum

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

instance asForeignMatchType :: AsForeign MatchType where
    write Simple = write "simple"
    write Regex = write "regex"
    write Exact = write "exact"
    write Substring = write "substring"

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

instance asForeignUrlMatchType :: AsForeign UrlMatchType where
    write = write <<< fromEnum