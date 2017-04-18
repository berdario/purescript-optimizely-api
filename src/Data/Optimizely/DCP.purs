module Data.Optimizely.DCP where
    -- ( Service(..)
    -- , LocatorName
    -- , unsafeLocatorName
    -- , locatorName
    -- , LocatorType(..)
    -- , Datasource(..)
    -- , AttributeType(..)
    -- , DateTimeFormat(..)
    -- , DatasourceAttribute(..)

    -- ) where

import Prelude
import Data.DateTime as D
import Data.List.NonEmpty as NEL
import Control.Bind ((=<<))
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (throwError)
import DOM.HTML.HTMLTableHeaderCellElement (abbr)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.DateTime.Foreign (DateTime)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum, fromEnum)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readInt, readString)
import Data.Foreign.Class (class IsForeign, read, readJSON)
import Data.Foreign.Generic (toForeignGeneric, defaultOptions, readGeneric)
import Data.Foreign.Null (Null(..))
import Data.Foreign.Undefined (Undefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe')
import Data.Optimizely.Common (Account, Id(..), foreignOptions)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Network.HTTP.Affjax (URL)

newtype Service = Service
    { id :: Id Service
    , account_id :: Id Account
    , archived :: Boolean
    , aws_access_key :: String
    , aws_secret_key :: String
    , created :: DateTime
    , last_modified :: DateTime
    , name :: String
    , s3_path :: String
    }
derive instance genericService :: Generic Service _

instance foreignService :: IsForeign Service where
    read = readGeneric foreignOptions

instance showService :: Show Service where
    show = genericShow

newtype LocatorName = LocatorName String
derive newtype instance showLocatorName :: Show LocatorName

locatorNameRegex :: String
locatorNameRegex = "^[a-zA-Z_][a-zA-Z_0-9$]*$"

testLocatorName :: String -> Boolean
testLocatorName = test $ unsafeRegex locatorNameRegex noFlags

unsafeLocatorName :: String -> LocatorName
unsafeLocatorName = LocatorName

locatorName :: String -> Maybe LocatorName
locatorName s =
    case testLocatorName s of
        true -> Just $ LocatorName s
        false -> Nothing

instance isForeignLocatorName :: IsForeign LocatorName where
    read val = do
        s <- readString val
        case testLocatorName s of
            true -> pure $ LocatorName s
            false -> fail $ ForeignError $ s <> " does not match regex: " <> locatorNameRegex

data LocatorType = Cookie | QueryParameter | JsVariable | UID
derive instance genericLocatorType :: Generic LocatorType _

instance foreignLocatorType :: IsForeign LocatorType where
    read value = parseStatus =<< readString value
        where
            parseStatus "cookie" = pure Cookie
            parseStatus "query parameter" = pure QueryParameter
            parseStatus "js_variable" = pure JsVariable
            parseStatus "uid" = pure UID
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected cookie, query parameter, js_variable, uid, found " <> val

instance showLocatorType :: Show LocatorType where
    show = genericShow

newtype Datasource = Datasource
    { id :: Id Datasource
    , archived :: Boolean
    , attributes :: Array String
    , aws_access_key :: String
    , aws_secret_key :: String
    , created :: DateTime
    , dcp_service_id :: Id Service
    , description :: String
    , is_optimizely :: Boolean
    , keyfield_locator_name :: Null LocatorName
    , keyfield_locator_type :: LocatorType
    , last_modified :: DateTime
    , name :: String
    , s3_path :: String
    }
derive instance genericDatasource :: Generic Datasource _

instance foreignDatasource :: IsForeign Datasource where
    read = readGeneric foreignOptions

instance showDatasource :: Show Datasource where
    show = genericShow

data AttributeType t = AString | ABool | ALong | ADouble | ADateTime
derive instance genericAttributeType :: Generic (AttributeType t) _

instance foreignAttributeTypeOther :: IsForeign (AttributeType OtherType') where
    read value = parseStatus =<< readString value
        where
            parseStatus "string" = pure AString
            parseStatus "bool" = pure ABool
            parseStatus "long" = pure ALong
            parseStatus "double" = pure ADouble
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected string, bool, long, double, found " <> val

instance foreignAttributeTypeDate :: IsForeign (AttributeType DateTimeType') where
    read value = parseStatus =<< readString value
        where
            parseStatus "datetime" = pure ADateTime
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected datetime, found " <> val

instance showAttributeType :: Show (AttributeType t) where
    show = genericShow

data DateTimeFormat = DateFormat | DateTimeFormat | EpochFormat
derive instance genericDateTimeFormat :: Generic DateTimeFormat _

instance foreignDateTimeFormat :: IsForeign DateTimeFormat where
    read value = parseStatus =<< readString value
        where
            parseStatus "yyyy-mm-dd" = pure DateFormat
            parseStatus "yyyy-mm-ddThh:mm:ssZ" = pure DateTimeFormat
            parseStatus "epoch" = pure EpochFormat
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected yyyy-mm-dd, yyyy-mm-ddThh:mm:ssZ, epoch, found " <> val

instance showDateTimeFormat :: Show DateTimeFormat where
    show = genericShow

class DSF at f | at -> f

data DateTimeType'
data OtherType'

instance dsfDateTime :: DSF DateTimeType' DateTimeFormat
instance dsfOther :: DSF OtherType' (Null Void)

newtype DatasourceAttribute at f = DatasourceAttribute
    { archived :: Boolean
    , created :: DateTime
    , datatype :: AttributeType at
    , dcp_datasource_id :: Id Datasource
    , description :: String
    , format :: f
    , id :: Id (DatasourceAttribute at f)
    , is_value_public :: Boolean
    , last_modified :: DateTime
    , name :: String
    }
derive instance genericDatasourceAttribute :: Generic (DatasourceAttribute at f) _

instance foreignDatasourceAttribute :: (DSF at f, IsForeign (AttributeType at), IsForeign f) => IsForeign (DatasourceAttribute at f) where
    read = readGeneric foreignOptions

instance showDatasourceAttribute :: (Show f) => Show (DatasourceAttribute at f) where
    show = genericShow
