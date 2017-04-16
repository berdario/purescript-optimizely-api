module Optimizely where

import Prelude
import Data.Optimizely
import Data.Optimizely.Project (NewProject, MkNewProject(..))
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (class IsForeign, read)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxRequest, AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))

optiRequest method endpoint content auth = defaultRequest {
    method = Left method
  , url = "https://www.optimizelyapis.com/experiment/v1/" <> endpoint
  , content = Just content
  , headers = [RequestHeader "Token" "sample"]
}

get endpoint = optiRequest GET endpoint unit
post = optiRequest POST
put = optiRequest PUT
delete endpoint = optiRequest DELETE endpoint unit

-- type Endpoint a = Method -> String -> AffjaxRequest a

-- --get :: forall a. Respondable a => Endpoint a -> String -> AffjaxRequest a
-- get request auth = (request GET auth) {content = Just unit}

-- -- post :: forall a b. (Requestable a, Respondable b)  => Endpoint b -> a -> String -> AffjaxRequest b
-- post request content auth = (request POST auth) {content = Just content}

readResponse :: forall eff a. (IsForeign a) => Affjax eff Foreign -> Aff (ajax :: AJAX | eff) (F a)
readResponse request = do
    {response} <- request
    pure $ read response

projects = optiRequest "projects/"

getProjects :: forall eff. String -> Aff (ajax :: AJAX | eff) (F (Array Project))
getProjects = readResponse <<< affjax <<< get "projects/"

-- postProject :: forall t eff. {|t} -> String -> Aff (ajax :: AJAX | eff) (AffjaxResponse Unit)
-- postProject content = affjax <<< post "projects/" content

postProject :: forall eff. NewProject -> String -> Aff (ajax :: AJAX | eff) (AffjaxResponse Unit)
postProject content = affjax <<< post "projects/" (MkNewProject content)