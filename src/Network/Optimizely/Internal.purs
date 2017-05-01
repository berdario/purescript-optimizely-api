module Network.Optimizely.Internal where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Except (ExceptT, Except, mapExceptT, withExcept, throwError, class MonadError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class IsForeign, read)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxRequest, AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.StatusCode (StatusCode(..))
import Network.Optimizely.Auth (Auth, toHeader)

data OptimizelyError = Decode String MultipleErrors | HTTP StatusCode String
derive instance genericOptimizelyError :: Generic OptimizelyError _

instance showOptimizelyError :: Show OptimizelyError where
    show = genericShow


type H eff = ExceptT OptimizelyError (Aff (ajax :: AJAX | eff))

optiRequest :: forall a. Requestable a => Method -> String -> a -> Auth -> AffjaxRequest a
optiRequest method endpoint content auth = defaultRequest {
    method = Left method
  , url = "https://www.optimizelyapis.com/experiment/v1/" <> endpoint
  , content = Just content
  , headers = [toHeader auth]
}

get :: String -> Auth -> AffjaxRequest Unit
get endpoint = optiRequest GET endpoint unit
post :: forall a. Requestable a => String -> a -> Auth -> AffjaxRequest a
post = optiRequest POST
put :: forall a. Requestable a => String -> a -> Auth -> AffjaxRequest a
put = optiRequest PUT
delete :: String -> Auth -> AffjaxRequest Unit
delete endpoint auth = (optiRequest DELETE endpoint unit auth){content=Nothing}

whenInvalid :: forall a. AffjaxResponse a -> Maybe (AffjaxResponse a)
whenInvalid r@{status:StatusCode x} | x < 200 || x >= 300 = Just r
whenInvalid _ = Nothing

checkStatus :: forall eff a. (Respondable a) => Affjax eff a -> H eff (AffjaxResponse a)
checkStatus request = do
    response <- lift request
    maybe (pure response) httpError $ whenInvalid response

httpError :: forall m a b. MonadError OptimizelyError m => AffjaxResponse a -> m b
httpError {response, status} = throwError $ HTTP status $ unsafeStringify response

-- hoist generalize from mmorph
generalize :: forall e m a. (Monad m) => Except e a -> ExceptT e m a
generalize = mapExceptT (pure <<< unwrap)

readResponse :: forall eff a. (IsForeign a) => AffjaxResponse Foreign -> H eff a
readResponse {response} = generalize $ withExcept (Decode json) $ read $ response
    where
    json = unsafeStringify response

executeRequest :: forall a b eff. (Requestable a, IsForeign b) => AffjaxRequest a -> H eff b
executeRequest = readResponse <=< checkStatus <<< affjax

executeGet :: forall a eff. (IsForeign a) => String -> Auth -> H eff a
executeGet endpoint = executeRequest <<< get endpoint

executePost :: forall a b eff. (Requestable a, IsForeign b) => String -> a -> Auth -> H eff b
executePost endpoint content = executeRequest <<< post endpoint content

executePut :: forall a b eff. (Requestable a, IsForeign b) => String -> a -> Auth -> H eff b
executePut endpoint content = executeRequest <<< put endpoint content

executeDelete :: forall eff. String -> Auth -> H eff (AffjaxResponse Unit)
executeDelete endpoint = checkStatus <<< affjax <<< delete endpoint
