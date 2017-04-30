module Data.Optimizely.Common where

import Prelude
import Control.Alt ((<|>))
import Data.Foreign (F, ForeignError(..), Foreign, fail)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write, readJSON)
import Data.Foreign.Generic (defaultOptions)
import Data.Foreign.Generic.Types (Options)
import Data.Int53 (Int53, fromNumber, toNumber, toString)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax.Request (RequestContent)
import Unsafe.Coerce (unsafeCoerce)

foreignOptions :: Options
foreignOptions = defaultOptions{unwrapSingleConstructors=true}

foreignToRequest :: forall a. AsForeign a => a -> Tuple (Maybe MediaType) RequestContent
foreignToRequest x = Tuple (Just applicationJSON) $ unsafeCoerce $ unsafeStringify $ write x

newtype Id a = Id Int53
derive newtype instance eqId :: Eq (Id a)
derive newtype instance ordId :: Ord (Id a)
derive instance newtypeId :: Newtype (Id a) _

instance showId :: Show (Id a) where
    show (Id x) = toString x

instance isForeignId :: IsForeign (Id a) where
    read val = readId =<< (read val <|> readNumberAsString val)
        where
        readNumberAsString :: Foreign -> F Number
        readNumberAsString value = readJSON =<< read value
        readId :: Number -> F (Id a)
        readId num = maybe (fail $ ForeignError $ show num <> " is too big for a js int") (pure <<< Id) $ fromNumber num

instance asForeignId :: AsForeign (Id a) where
    write = write <<< toNumber <<< (\(Id x) -> x)

data Account
data Section


