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


{-|
Execute an effect, with all errors handled.
If any lifted IO actions throw unhandled exceptions, they will be propagated.
-}
runEio :: Eio Void res -> IO res
runEio (Eio io) = io

newtype Eio err res =
  Eio (IO res)
  deriving (Functor, Applicative, Monad, MonadFail)

instance Exception err => MonadIO (Eio err) where
  liftIO =
    handleIO throw

instance Bifunctor Eio where
  second = fmap
  first mapper =
    mapIO $ \ io ->
      Prelude.catch io $ \ (AnyException e) ->
        Prelude.throwIO (unsafeCoerce (mapper e) :: AnyException)

newtype AnyException =
  AnyException (forall a. a)

instance Show AnyException where
  show _ = "Internal EIO exception. You shouldn't be seeing this"

instance Exception AnyException

{-| Low-level helper. -}
mapIO :: (IO a -> IO b) -> Eio oldErr a -> Eio newErr b
mapIO mapper =
  coerce . mapper . coerce

catch :: Eio a res -> (a -> Eio b res) -> Eio b res
catch (Eio io) handler =
  Eio $ Prelude.catch io $ \ (AnyException e) ->
    case handler e of Eio io -> io

throw :: err -> Eio err res
throw e =
  Eio (Prelude.throwIO (unsafeCoerce e :: AnyException))

bracket :: Eio e a -> (a -> Eio e b) -> (a -> Eio e c) -> Eio e c
bracket acquire release use =
  do
    resource <- acquire
    join (catch
      (fmap (\ res -> release resource $> res) (use resource))
      (\ e -> return (release resource *> throw e)))

liftExceptionlessIO :: IO res -> Eio err res
liftExceptionlessIO = Eio

liftIOMappingErr :: Exception exc => (exc -> err) -> IO res -> Eio err res
liftIOMappingErr mapper io =
  Eio $ Prelude.catch io $ \ exc ->
    Prelude.throwIO (unsafeCoerce (mapper exc) :: AnyException)

handleIO :: Exception exc => (exc -> Eio err res) -> IO res -> Eio err res
handleIO handler io =
  Eio $ Prelude.catch io $ \ exc ->
    case handler exc of Eio io -> io
