module Data.Optimizely.Audience.Internal
    ( RuleRecord
    , Rule(..)
    , ListName
    , listName
    ) where

import Prelude
import Data.Foreign (ForeignError(..), fail)
import Data.Foreign.Class (class IsForeign, read, class AsForeign, write)
import Data.Foreign.Generic (readGeneric, toForeignGeneric)
import Data.Foreign.Undefined (Undefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe')
import Data.Optimizely.Common (foreignOptions)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)


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

newtype ListName = ListName String
derive newtype instance showListName :: Show ListName

listNameRegex :: String
listNameRegex = "^[a-zA-Z0-9\\-_]+$"

testlistName :: String -> Boolean
testlistName = test $ unsafeRegex listNameRegex noFlags

listName :: String -> Maybe ListName
listName s =
    case testlistName s of
        true -> Just $ ListName s
        false -> Nothing

instance isForeignlistName :: IsForeign ListName where
    read val = do
        s <- read val
        maybe' (wrongName s) pure $ listName s
        where
            wrongName s _ = fail $ ForeignError $ s <> " does not match regex: " <> listNameRegex

instance asForeignListName :: AsForeign ListName where
    write (ListName s) = write s