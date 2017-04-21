module Data.Optimizely.DCP
    ( Service(..)
    , EditService(..)
    , LocatorName
    , unsafeLocatorName
    , locatorName
    , LocatorType(..)
    , Datasource(..)
    , EditDatasource
    , NewDatasource
    , MkNewDatasource(..)
    , PutDatasource
    , MkPutDatasource(..)
    , AttributeType(..)
    , DateTimeFormat(..)
    , DatasourceAttribute(..)
    , EditAttribute(..)
    , NewAttribute(..)
    , NewAttributeFields(..)
    ) where

import Prelude
import Data.DateTime.Foreign (DateTime)
import Data.Foreign (ForeignError(..), fail, readString, writeObject)
import Data.Foreign.Class (class IsForeign, writeProp, class AsForeign, write)
import Data.Foreign.Generic (toForeignGeneric, readGeneric)
import Data.Foreign.Null (Null(..))
import Data.Foreign.Undefined (Undefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Network.HTTP.Affjax.Request (class Requestable)

import Data.Optimizely.Common (Account, Id, foreignOptions, foreignToRequest)


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
derive instance newtypeService :: Newtype Service _

instance foreignService :: IsForeign Service where
    read = readGeneric foreignOptions

instance showService :: Show Service where
    show = genericShow

newtype EditService = EditService String

instance asForeignEditService :: AsForeign EditService where
    write (EditService name) = writeObject [writeProp "name" name]

instance requestableEditService :: Requestable EditService where
    toRequest = foreignToRequest

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

instance asForeignLocatorName :: AsForeign LocatorName where
    write (LocatorName val) = write val

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

instance asForeignLocatorType :: AsForeign LocatorType where
    write Cookie = write "cookie"
    write QueryParameter = write "query parameter"
    write JsVariable = write "js_variable"
    write UID = write "uid"

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
derive instance newtypeDatasource :: Newtype Datasource _

instance foreignDatasource :: IsForeign Datasource where
    read = readGeneric foreignOptions

instance showDatasource :: Show Datasource where
    show = genericShow

type EditDatasource name locType locName =
    { name :: name
    , keyfield_locator_type :: locType
    , keyfield_locator_name :: locName
    , description :: Undefined String
    }

type NewDatasource = EditDatasource String LocatorType (Null LocatorName)
type PutDatasource = EditDatasource (Undefined String) (Undefined LocatorType) (Undefined (Null LocatorName))

newtype MkNewDatasource = MkNewDatasource NewDatasource
derive instance genericNewDatasource :: Generic MkNewDatasource _

instance asForeignNewDatasource :: AsForeign MkNewDatasource where
    write = toForeignGeneric foreignOptions

instance requestableNewDatasource :: Requestable MkNewDatasource where
    toRequest = foreignToRequest

newtype MkPutDatasource = MkPutDatasource PutDatasource
derive instance genericPutDatasource :: Generic MkPutDatasource _

instance asForeignPutDatasource :: AsForeign MkPutDatasource where
    write = toForeignGeneric foreignOptions

instance requestablePutDatasource :: Requestable MkPutDatasource where
    toRequest = foreignToRequest

data AttributeType = AString | ABool | ALong | ADouble | ADateTime
derive instance genericAttributeType :: Generic AttributeType _

instance foreignAttributeType :: IsForeign AttributeType where
    read value = parseStatus =<< readString value
        where
            parseStatus "string" = pure AString
            parseStatus "bool" = pure ABool
            parseStatus "long" = pure ALong
            parseStatus "double" = pure ADouble
            parseStatus "datetime" = pure ADateTime
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected string, bool, long, double, datetime, found " <> val

instance asForeignAttributeType :: AsForeign AttributeType where
    write AString = write "string"
    write ABool = write "bool"
    write ALong = write "long"
    write ADouble = write "double"
    write ADateTime = write "datetime"

instance showAttributeType :: Show AttributeType where
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

instance asForeignDateTimeFormat :: AsForeign DateTimeFormat where
    write DateFormat = write "yyyy-mm-dd"
    write DateTimeFormat = write "yyyy-mm-ddThh:mm:ssZ"
    write EpochFormat = write "epoch"

instance showDateTimeFormat :: Show DateTimeFormat where
    show = genericShow


newtype DatasourceAttribute = DatasourceAttribute
    { archived :: Boolean
    , created :: DateTime
    , datatype :: AttributeType
    , dcp_datasource_id :: Id Datasource
    , description :: String
    , format :: Null DateTimeFormat
    , id :: Id DatasourceAttribute
    , is_value_public :: Boolean
    , last_modified :: DateTime
    , name :: String
    }
derive instance genericDatasourceAttribute :: Generic DatasourceAttribute _
derive instance newtypeDatasourceAttribute :: Newtype DatasourceAttribute _

instance foreignDatasourceAttribute :: IsForeign DatasourceAttribute where
    read = readGeneric foreignOptions

instance showDatasourceAttribute :: Show DatasourceAttribute where
    show = genericShow

data NewAttribute = NewAttribute String NewAttributeFields

data NewAttributeFields
    = NewString
    | NewBool
    | NewLong
    | NewDouble
    | NewDateTime DateTimeFormat

newAttrRecord :: NewAttribute -> {name:: String, datatype :: AttributeType, format :: Null DateTimeFormat}
newAttrRecord (NewAttribute name NewString) = {name: name, datatype: AString, format: Null Nothing}
newAttrRecord (NewAttribute name NewBool) = {name: name, datatype: ABool, format: Null Nothing}
newAttrRecord (NewAttribute name NewLong) = {name: name, datatype: ALong, format: Null Nothing}
newAttrRecord (NewAttribute name NewDouble) = {name: name, datatype: ADouble, format: Null Nothing}
newAttrRecord (NewAttribute name (NewDateTime fmt)) = {name: name, datatype: ADateTime, format: Null (Just fmt)}

instance asForeignNewAttribute :: AsForeign NewAttribute where
    write  = writeAttr <<< newAttrRecord
        where
        writeAttr {name, datatype, format}
            = writeObject [ writeProp "name" name
                          , writeProp "datatype" datatype
                          , writeProp "format" format
                          ]

instance requestableNewAttribute :: Requestable NewAttribute where
    toRequest = foreignToRequest

newtype EditAttribute = EditAttribute String

instance asForeignEditAttribute :: AsForeign EditAttribute where
    write (EditAttribute description) = writeObject [writeProp "description" description]

instance requestableEditAttribute :: Requestable EditAttribute where
    toRequest = foreignToRequest