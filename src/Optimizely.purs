module Optimizely where

import Prelude
import Data.Optimizely
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (class IsForeign, read)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxRequest, AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)

import Data.Optimizely.Common (Id(..))
import Data.Optimizely.Project (NewProject, MkNewProject(..))
import Network.Optimizely.Auth (Auth, toHeader)

optiRequest :: forall a b. Requestable a => Method -> String -> a -> Auth -> AffjaxRequest a
optiRequest method endpoint content auth = defaultRequest {
    method = Left method
  , url = "https://www.optimizelyapis.com/experiment/v1/" <> endpoint
  , content = Just content
  , headers = [toHeader auth]
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

getProject :: forall eff. Id Project -> Auth -> Aff (ajax :: AJAX | eff) (F Project)
getProject id = readResponse <<< affjax <<< get ("projects/" <> show id)

listProjects :: forall eff. Auth -> Aff (ajax :: AJAX | eff) (F (Array Project))
listProjects = readResponse <<< affjax <<< get "projects/"

-- postProject :: forall t eff. {|t} -> String -> Aff (ajax :: AJAX | eff) (AffjaxResponse Unit)
-- postProject content = affjax <<< post "projects/" content

newProject :: forall eff. NewProject -> Auth -> Aff (ajax :: AJAX | eff) (AffjaxResponse Unit)
newProject content = affjax <<< post "projects/" (MkNewProject content)

