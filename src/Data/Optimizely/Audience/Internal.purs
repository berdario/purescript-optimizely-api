module Data.Optimizely.Audience.Internal where

import Prelude
import Data.DateTime as D
import Data.List.NonEmpty as NEL
import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (throwError)
import Data.Array (toUnfoldable, fromFoldable)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.DateTime.Foreign (DateTime)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum, fromEnum)
import Data.Foldable (class Foldable)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readInt, readString, toForeign)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Foreign.Null (Null)
import Data.Foreign.Undefined (Undefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe')
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Traversable (traverse)
import Network.HTTP.Affjax (URL)
import Unsafe.Coerce (unsafeCoerce)

foreignOptions = defaultOptions{unwrapSingleConstructors=true}


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

instance showRule :: Show Rule where
    show = genericShow

