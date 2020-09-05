{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.OpenAPI.UI
-- Copyright   :  (C) 2016-2020 Oleg Grenrus
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Provides 'OpenApiSchemaUI' and corresponding 'openapiSchemaUIServer' to embed
-- <http://swagger.io/swagger-ui/ swagger ui> into the application.
--
-- All of the UI files are embedded into the binary.
--
-- /An example:/
--
-- @
-- -- | Actual API.
-- type BasicAPI = Get '[PlainText, JSON] Text
--     :\<|> "cat" :> Capture ":name" CatName :> Get '[JSON] Cat
--
-- -- | API type with bells and whistles, i.e. schema file and swagger-ui.
-- type API = 'OpenApiSchemaUI' "swagger-ui" "swagger.json"
--     :\<|> BasicAPI
--
-- -- | Servant server for an API
-- server :: Server API
-- server = 'openapiSchemaUIServer' swaggerDoc
--     :\<|> (pure "Hello World" :\<|> catEndpoint)
--   where
--     catEndpoint name = pure $ Cat name False
-- @

module Servant.OpenAPI.UI (
    -- * Swagger UI API
    OpenApiSchemaUI,
    openapiSchemaUIServer,

    -- ** Official swagger ui
    swaggerUiIndexTemplate,
    swaggerUiFiles,
    ) where

import Servant.Swagger.UI.Core
import Servant.Swagger.UI

import Data.OpenApi    (OpenApi)
import GHC.TypeLits    (Symbol)
import Servant

-- | OpenAPI schema + ui api.
type OpenApiSchemaUI (dir :: Symbol) (schema :: Symbol) = GenericSwaggerSchemaUI dir schema OpenApi

-- | Serve Swagger UI on @/dir@ using @api@ as a OpenAPI spec source.
--
-- @
-- openapiSchemaUIServer :: OpenAPI -> Server (OpenApiSchemaUI schema dir)
-- @
openapiSchemaUIServer
    :: (Server api ~ Handler OpenApi)
    => OpenApi -> Server (SwaggerSchemaUI' dir api)
openapiSchemaUIServer =
    swaggerSchemaUIServerImpl swaggerUiIndexTemplate swaggerUiFiles
