module Eio
(
  Eio,
)
where

import Eio.Prelude


newtype Eio err res =
  Eio (IO res)
  deriving (Functor, Applicative, Monad, MonadFail)

instance MonadIO (Eio SomeException) where
  liftIO = Eio

{-| Low-level helper. -}
mapIO :: (IO a -> IO b) -> Eio oldErr a -> Eio newErr b
mapIO mapper =
  coerce . mapper . coerce
