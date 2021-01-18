module Eio
(
  Eio,
  runEio,
  catch,
  throw,
)
where

import Eio.Prelude hiding (catch, throw, throwIO)
import qualified Eio.Prelude as Prelude


runEio :: Eio Void res -> IO res
runEio (Eio io) = io

newtype Eio err res =
  Eio (IO res)
  deriving (Functor, Applicative, Monad, MonadFail)

instance Exception err => MonadIO (Eio err) where
  liftIO io =
    Eio $ Prelude.catch io $ \ (e :: err) ->
      Prelude.throwIO (unsafeCoerce e :: EioException)

instance Bifunctor Eio where
  second = fmap
  first mapper =
    mapIO $ \ io ->
      Prelude.catch io $ \ (EioException e) ->
        Prelude.throwIO (unsafeCoerce (mapper e) :: EioException)

newtype EioException =
  EioException (forall a. a)

instance Show EioException where
  show _ = "Internal Eio exception. You shouldn't be seeing this"

instance Exception EioException

{-| Low-level helper. -}
mapIO :: (IO a -> IO b) -> Eio oldErr a -> Eio newErr b
mapIO mapper =
  coerce . mapper . coerce

catch :: Eio a res -> (a -> Eio b res) -> Eio b res
catch (Eio io) handler =
  Eio $ Prelude.catch io $ \ (EioException e) ->
    case handler e of Eio io -> io

throw :: err -> Eio err res
throw e =
  Eio (Prelude.throwIO (unsafeCoerce e :: EioException))

liftExceptionlessIO :: IO res -> Eio err res
liftExceptionlessIO = Eio

liftIOMappingErr :: Exception exc => (exc -> err) -> IO res -> Eio err res
liftIOMappingErr mapper io =
  Eio $ Prelude.catch io $ \ exc ->
    Prelude.throwIO (unsafeCoerce (mapper exc) :: EioException)
