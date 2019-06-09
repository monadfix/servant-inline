{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Inline
    (
        Endpoint(..),
    )
    where

import GHC.TypeLits
import Data.Kind
import Servant.API
import Servant.Server

----------------------------------------------------------------------------
-- Endpoint
----------------------------------------------------------------------------

data Endpoint
    (synopsis :: Symbol)
    (modifiers :: Modifiers')
    (route :: Route')
    (params :: Params')
    (result :: Result')
    = Endpoint (MkHandler params result)

data Modifiers' = Modifiers [Type]

data Route' = Route [RoutePiece]
data RoutePiece = Piece Symbol | Arg Symbol

data Params' = Params [Param]
data Param = Type := Type

data Result' = Result Type Type

----------------------------------------------------------------------------
-- Endpoint -> handler type
----------------------------------------------------------------------------

type family MkHandler
    (params :: Params')
    (result :: Result')
    :: Type
  where
    MkHandler (Params '[]) (Result x _) = x
    MkHandler (Params ((t := _) ': ps)) r = t -> MkHandler (Params ps) r

----------------------------------------------------------------------------
-- Endpoint -> API type
----------------------------------------------------------------------------

-- type family MkApi (Endpoint synopsis)

----------------------------------------------------------------------------
-- Example
----------------------------------------------------------------------------

updateClient :: Endpoint "PUT /clients/:client"
    (Modifiers
         '[ Summary "Update a registered client."
          -- , ErrorResponse 200 "Client updated"
          -- , ErrorMalformedPrekeys
          ])
    (Route
         '[ Piece "category", Arg "client" ])
    (Params
         '[ UserId       := Header "Z-User" UserId
          , ClientId     := Capture' '[Description "Client ID"] "client" ClientId
          , UpdateClient := ReqBody '[JSON] UpdateClient ])
    (Result
         (Handler ())
         (Put '[JSON] NoContent))
updateClient = Endpoint $ \usr clt req -> pure ()

type UserId = String
type ClientId = String
data UpdateClient = UpdateClient {foo,bar :: Int}
