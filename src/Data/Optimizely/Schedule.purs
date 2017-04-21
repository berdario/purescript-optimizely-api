module Data.Optimizely.Schedule where

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
import Data.Foreign (F, Foreign, ForeignError(..), fail, readInt, readString, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write, writeProp)
import Data.Foreign.Generic (toForeignGeneric, defaultOptions, readGeneric)
import Data.Foreign.Null (Null(..))
import Data.Foreign.Undefined (Undefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe')
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype)
import Data.These (These, theseLeft, theseRight)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax.Request (RequestContent)
import Network.HTTP.Affjax.Request (class Requestable, toRequest)
import Unsafe.Coerce (unsafeCoerce)

import Data.Optimizely.Common (Id(..), foreignOptions, foreignToRequest)
import Data.Optimizely.Experiment (Experiment(..))

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
    , experiment_id :: Id Experiment
    , id :: Id Schedule
    }
derive instance genericSchedule :: Generic Schedule _
derive instance newtypeSchedule :: Newtype Schedule _

instance foreignSchedule :: IsForeign Schedule where
    read = readGeneric foreignOptions

instance showSchedule :: Show Schedule where
    show = genericShow

newtype EditSchedule = EditSchedule (These DateTime DateTime)

instance asForeignEditSchedule :: AsForeign EditSchedule where
    write (EditSchedule dates) =
        writeObject [ writeProp "start_time" (Null (theseLeft dates))
                    , writeProp "stop_time" (Null (theseRight dates))
                    ]

instance requestableEditSchedule :: Requestable EditSchedule where
    toRequest = foreignToRequest