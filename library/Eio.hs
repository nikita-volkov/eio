module Eio
(
  Eio,
  runEio,
  handle,
  throw,
)
where

import Eio.Prelude hiding (handle, throw, throwIO)
import qualified Eio.Prelude as Prelude


-- * Exception
-------------------------

newtype CapturedException =
  CapturedException (forall a. a)

instance Show CapturedException where
  show _ = "Internal EIO exception. You shouldn't be seeing this"

instance Exception CapturedException


-- * IO
-------------------------

{-|
Execute an effect, with all errors handled.
If any lifted IO actions throw unhandled exceptions, they will be propagated.
-}
runEio :: Eio Void res -> IO res
runEio (Eio io) = io


-- * EIO
-------------------------

{-|
IO action with explicitly typed error.
-}
newtype Eio err res =
  Eio { run :: IO res }
  deriving (Functor, Applicative, Monad, MonadFail)

instance Exception err => MonadIO (Eio err) where
  liftIO =
    handleIO throw

instance Bifunctor Eio where
  second = fmap
  first mapper = handle (throw . mapper)

{-| Low-level helper. -}
mapIO :: (IO a -> IO b) -> Eio oldErr a -> Eio newErr b
mapIO mapper =
  coerce . mapper . coerce

handle :: (a -> Eio b res) -> Eio a res -> Eio b res
handle handler (Eio io) =
  Eio $ Prelude.catch io $ \ (CapturedException e) ->
    case handler e of Eio io -> io

throw :: err -> Eio err res
throw e =
  Eio (Prelude.throwIO (unsafeCoerce e :: CapturedException))

bracket :: Eio e a -> (a -> Eio e b) -> (a -> Eio e c) -> Eio e c
bracket acquire release use =
  Eio $ Prelude.mask $ \ unmask -> do
    resource <- run acquire
    join (Prelude.catch
      (fmap (\ result -> run (release resource) $> result) (unmask (run (use resource))))
      (\ (e :: SomeException) -> return (run (release resource) *> Prelude.throwIO e)))

{-|
Lift an IO action without handling the exceptions that may be thrown in it.
-}
liftExceptionlessIO :: IO res -> Eio err res
liftExceptionlessIO = Eio

liftIOMappingErr :: Exception exc => (exc -> err) -> IO res -> Eio err res
liftIOMappingErr mapper =
  handleIO (throw . mapper)

handleIO :: Exception exc => (exc -> Eio err res) -> IO res -> Eio err res
handleIO handler io =
  Eio $ Prelude.catch io $ \ exc ->
    case handler exc of Eio io -> io

liftIONarrowing :: (SomeException -> Maybe err) -> IO res -> Eio err res
liftIONarrowing narrower =
  handleIO $ \ someException ->
    case narrower someException of
      Just err -> throw err
      Nothing -> Eio (Prelude.throwIO someException)
