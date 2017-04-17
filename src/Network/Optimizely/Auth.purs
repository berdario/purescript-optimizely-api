module Network.Optimizely.Auth where

import Prelude
import Network.HTTP.RequestHeader (RequestHeader(..))

data Auth = ClassicToken String | OAuthBearer String

toHeader :: Auth -> RequestHeader
toHeader (ClassicToken tkn) = RequestHeader "Token" tkn
toHeader (OAuthBearer tkn) = RequestHeader "Authorization" $ "Bearer " <> tkn