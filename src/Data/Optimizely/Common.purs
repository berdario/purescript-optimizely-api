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
import Data.Int53 (Int53, fromNumber, toNumber, toString)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax.Request (RequestContent)
import Unsafe.Coerce (unsafeCoerce)

import Data.Optimizely.Internal (readBoundedEnum)

foreignOptions = defaultOptions{unwrapSingleConstructors=true}

foreignToRequest :: forall a. AsForeign a => a -> Tuple (Maybe MediaType) RequestContent
foreignToRequest x = Tuple (Just applicationJSON) $ unsafeCoerce $ unsafeStringify $ write x

newtype Id a = Id Int53
derive newtype instance eqId :: Eq (Id a)
derive newtype instance ordId :: Ord (Id a)

instance showId :: Show (Id a) where
    show (Id x) = toString x

instance isForeignId :: IsForeign (Id a) where
    read val = readId =<< read val
        where
        readId :: Number -> F (Id a)
        readId num = maybe (fail $ ForeignError $ show num <> " is too big for a js int") (pure <<< Id) $ fromNumber num

instance asForeignId :: AsForeign (Id a) where
    write = write <<< toNumber <<< (\(Id x) -> x)

data Account
data Section


