module Data.Optimizely.Project where

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
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Foreign.Undefined (Undefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (class Newtype)

import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax.Request (class Requestable, toRequest)

import Data.Optimizely.DCP as DCP
import Data.Optimizely.Common (Account, Id, foreignOptions, foreignToRequest)


data ProjectStatus = Active | Archived
derive instance genericProjectStatus :: Generic ProjectStatus _


instance foreignProjectStatus :: IsForeign ProjectStatus where
    read value = parseStatus =<< readString value
        where
            parseStatus "Active" = pure Active
            parseStatus "Archived" = pure Archived
            parseStatus val = error val
            error val = fail $ ForeignError $ "Expected Active or Archived, found " <> val

instance asForeignProjectStatus :: AsForeign ProjectStatus where
    write Active = write "Active"
    write Archived = write "Archived"

instance showProjectStatus :: Show ProjectStatus where
    show = genericShow

newtype Project = Project
    { account_id :: Id Account
    , cache_ttl :: Undefined Int
    , code_revision :: Int
    , created :: DateTime
    , dcp_service_id :: Null (Id DCP.Service)
    , default_timezone :: NullOrUndefined String
    , enable_force_variation :: Boolean
    , exclude_disabled_experiments :: Boolean
    , exclude_names :: Null Boolean -- null ?
    , id :: Id Project
    , include_jquery :: Boolean
    , ip_anonymization :: Boolean
    , ip_filter :: Null String
    , is_shared :: Undefined Boolean
    , js_file_size :: Int
    , last_modified :: DateTime
    , library :: String
    , opted_into_apps_beta :: Undefined Boolean
    , project_javascript :: Null String
    , project_name :: String
    , project_status :: ProjectStatus
    , recommender_service_ids :: Undefined (Array Int)
    , socket_token :: String
    }
derive instance genericProject :: Generic Project _
derive instance newtypeProject :: Newtype Project _

instance foreignProject :: IsForeign Project where
    read = readGeneric foreignOptions

instance showProject :: Show Project where
    show = genericShow

type EditProject name =
    { project_name :: name
    , project_status :: Undefined ProjectStatus
    , include_jquery :: Undefined Boolean
    , project_javascript :: Undefined (Null String)
    , enable_force_variation :: Undefined Boolean
    , exclude_disabled_experiments :: Undefined Boolean
    , exclude_names :: Undefined Boolean
    , ip_anonymization :: Undefined Boolean
    , ip_filter :: Undefined (Null String)
    , dcp_service_id :: Undefined (Null (Id DCP.Service))
    }

type NewProject = EditProject String
type PutProject = EditProject (Undefined String)

newtype MkNewProject = MkNewProject NewProject
derive instance genericNewProject :: Generic MkNewProject _

instance asForeignNewProject :: AsForeign MkNewProject where
    write = toForeignGeneric foreignOptions

instance requestableNewProject :: Requestable MkNewProject where
    toRequest = foreignToRequest

newtype MkPutProject = MkPutProject PutProject
derive instance genericPutProject :: Generic MkPutProject _

instance asForeignPutProject :: AsForeign MkPutProject where
    write = toForeignGeneric foreignOptions

instance requestablePutProject :: Requestable MkPutProject where
    toRequest = foreignToRequest

emptyProject :: PutProject
emptyProject =
    { project_name : Undefined Nothing
    , project_status : Undefined Nothing
    , include_jquery : Undefined Nothing
    , project_javascript : Undefined Nothing
    , enable_force_variation : Undefined Nothing
    , exclude_disabled_experiments : Undefined Nothing
    , exclude_names : Undefined Nothing
    , ip_anonymization : Undefined Nothing
    , ip_filter : Undefined Nothing
    , dcp_service_id : Undefined Nothing
    }

