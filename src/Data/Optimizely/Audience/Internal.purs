module Data.Optimizely.Audience.Internal where

import Prelude

import Data.Foreign.Class (class IsForeign, class AsForeign)
import Data.Foreign.Generic (readGeneric, toForeignGeneric)
import Data.Foreign.Undefined (Undefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Data.Optimizely.Common (foreignOptions)


type RuleRecord =
    { type :: String
    , value :: Undefined String
    , match_type :: Undefined String
    , name :: Undefined String
    }

newtype Rule = Rule RuleRecord

derive instance genericRule :: Generic Rule _

instance isForeignRule :: IsForeign Rule where
    read = readGeneric foreignOptions

instance asForeignRule :: AsForeign Rule where
    write = toForeignGeneric foreignOptions

instance showRule :: Show Rule where
    show = genericShow

