module Conscient.Models where

import           ClassyPrelude

import           Data.Aeson                (FromJSON (..), ToJSON (..), Value,
                                            camelTo2, object, withObject, (.:),
                                            (.=))
import           Data.Aeson.Types          (Parser)
import           Data.Swagger              (Definitions, NamedSchema, Schema,
                                            ToSchema (..), defaultSchemaOptions,
                                            description, example,
                                            fieldLabelModifier,
                                            genericDeclareNamedSchema, schema)
import           Data.Swagger.Declare      (Declare)
import           GHC.Generics              ()
import           Lens.Micro                (mapped, (&), (?~))
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------
-- | Datatype describing a user to be used within our API.
data User
    = User
    { username     :: Text
    , password     :: Text
    , emailAddress :: Text
    , createdAt    :: UTCTime
    , updatedAt    :: UTCTime
    } deriving (Eq, Generic, Show)

-- | Provide an `Arbitrary` instance for the `User` datatype; this allows any
-- | value of type `User` to be randomly generated using QuickCheck.
-- |
-- | This instance provides two useful methods:
-- |
-- | arbitrary - Randomly generates a `User` by randomly generating its
-- | constitutent types and building up a record with them.
-- |
-- | shrink - Generates "shrinks" for the data type, which help QuickCheck
-- | narrow down the failing case to a much smaller sample that still reproduces
-- | the error.
instance Arbitrary User where
    arbitrary :: Gen User
    arbitrary = do
        username     <- arbitrary
        password     <- arbitrary
        emailAddress <- arbitrary
        createdAt    <- arbitrary
        updatedAt    <- arbitrary
        return User{..}

    shrink :: User -> [User]
    shrink User{..} = do
        username'     <- shrink username
        password'     <- shrink password
        emailAddress' <- shrink emailAddress
        createdAt'    <- shrink createdAt
        updatedAt'    <- shrink updatedAt
        return (User username' password' emailAddress' createdAt' updatedAt')

-- | Provide a `FromJSON` instance for the `User` datatype; this allows any
-- | value of type `User` to be deserialized from JSON.
instance FromJSON User where
    parseJSON :: Value -> Parser User
    parseJSON = withObject "User" $ \obj -> do
        userObj      <- obj     .: "user"
        username     <- userObj .: "username"
        password     <- userObj .: "password"
        emailAddress <- userObj .: "email_address"
        createdAt    <- userObj .: "created_at"
        updatedAt    <- userObj .: "updated_at"
        return User{..}

-- | Provide a `ToJSON` implementation for the `User` datatype; this allows any
-- | value of type `User` to be serialized to JSON.
-- |
-- | Note how we're selectively eliding the `password` field.
instance ToJSON User where
    toJSON :: User -> Value
    toJSON User{..} =
        let userObj = object [ "username"      .= username
                             , "email_address" .= emailAddress
                             , "created_at"    .= createdAt
                             , "updated_at"    .= updatedAt
                             ]
        in object [ "user" .= userObj ]

-- | Provide a `ToSchema` implementation for the `User` datatype; this allows
-- | applications to derive Swagger specifications from values of type `User`.
instance ToSchema User where
    declareNamedSchema
        :: proxy User
        -> Declare (Definitions Schema) NamedSchema
    declareNamedSchema user =
        let
          -- Options that can be overridden to modify the generated Swagger
          -- specification.
          --
          -- The `User` record field labels are camel cased, as this is idiomatic
          -- in Haskell; here we pass a function to `fieldLabelModifier` that
          -- will convert camel case'd strings to snake case, which is more
          -- idiomatic in the specification format.
          schemaOptions = defaultSchemaOptions { fieldLabelModifier = camelTo2 '_' }

          -- An arbitrary user datatype that can be serialized to JSON and
          -- provided as an example in the Swagger specification.
          timestamp = UTCTime (fromGregorian 2018 1 1) 0
          exampleUser =
            User "aroberts" "p@ssw0rd" "aroberts@example.com" timestamp timestamp

        -- Take a generic named schema, with the schema options defined earlier
        -- and user type that was passed in, apply some annotations to enrich
        -- the generated specification.
        in genericDeclareNamedSchema schemaOptions user
               & mapped.schema.description ?~ "A Conscient user"
               & mapped.schema.example ?~ toJSON exampleUser
