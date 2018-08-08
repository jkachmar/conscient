module Conscient.Routes where

import           ClassyPrelude

import           Data.Proxy          (Proxy (..))
import           Data.Swagger        (Swagger, URL (..), description, info,
                                      license, title, url, version)
import           GHC.Generics        ()
import           Lens.Micro          ((&), (.~), (?~))
import           Servant.API         ((:>), Capture, Description, Get, JSON, Post, Put,
                                      ReqBody, Summary)
import           Servant.API.Generic ((:-), ToServantApi, genericApi)
import           Servant.Swagger     (toSwagger)

import           Conscient.Models    (User)

--------------------------------------------------------------------------------
-- | The API contract for all routes associated with a `User`.
-- |
-- | An API's route specification is defined as a record of type-level route
-- | definitions.
-- |
-- | A brief summary of the symbols involved:
-- |
-- | (:-) - Boilerplate that helps the framework resolve the type following it
-- | as either a standalone Route or an internal Link to another resource.
-- |
-- | (:>) - A generic resource separator, this symbol is used to separate logical
-- | components of the API type, such as path segments, capture groups, query
-- | parameters, request body, etc.
-- |
-- | ('[JSON, HTML]) - A list of MIME types that the route can handle, either as
-- | a request or a response.
data UserRoutes route
    = UserRoutes
    -- "/users", HTTP GET, JSON response body containing a list of Users
    { _getUsers :: route
        :- Summary "Get a list of all users."
        :> Description "Get a list of all users currently registered with this \
                       \service."
        :> "users" :> Get '[JSON] [User]

    -- "/users/:name", HTTP GET, JSON response body containing a single User
    , _getUser :: route
        :- Summary "Get a specific user by name."
        :> "users" :> Capture "name" Text :> Get '[JSON] User

    -- "/users", HTTP POST, JSON request body containing a single user, JSON
    -- response body containing a single user
    , _createUser :: route
        :- Summary "Create a user."
        :> Description "Create a new user from the `User` record supplied by \
                       \request body."
        :> "users" :> ReqBody '[JSON] User :> Post '[JSON] User

    -- "/users", HTTP PUT, JSON request body containing a single user, JSON
    -- response body containing a single user
    , _updateUser :: route
        :- Summary "Update a user."
        :> Description "Update an existing user by completely replacing their \
                       \`User` record with a new one supplied by the request \
                       \body."
        :> "users" :> ReqBody '[JSON] User :> Put '[JSON] User
    } deriving Generic

-- | The API contract for all routes associated with this specification.
-- |
-- | Note that the `Routes` record can, itself, contain route specifications.
-- | This allows users to factor out components of the API type into different
-- | modules as necessary (e.g. to separate concerns, version sub-routes, etc.)
data Routes route
    = Routes
    { _v1Routes :: route :- "v1" :> (ToServantApi UserRoutes)
    } deriving Generic

-- | A value "containing" the API contract as defined by the `Routes` type.
routes :: Proxy (ToServantApi Routes)
routes = genericApi (Proxy @Routes)

-- | A value "containing" the Swagger specification for the API contract.
-- |
-- | This lets us annotate the API contract with additional metadata that will
-- | be useful when generating the specification.
-- |
-- | This function uses lenses to modify fields with the following operators:
-- |
-- | (&) - Flipped function application, ensures that all expressions on the
-- | left of the operator are executed first, then passed to the expressions on
-- | the right of the operator
-- |
-- | (.~) - Sets a field that contains a non-optional value.
-- |
-- | (?~) - Sets a field that contains an optional value.
routesSpecification :: Swagger
routesSpecification = toSwagger routes
    & info.title       .~ "Conscient API"
    & info.version     .~ "0.1.0"
    & info.license     ?~ ("MIT" & url ?~ URL "http://mit.com")
    & info.description ?~ "This is an API that demonstrates how one might take\
                          \ a 'code-first' approach to deriving API\
                          \ specifications with Haskell and Servant."
