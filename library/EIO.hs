module EIO
(
  runEIO,
  EIO,
  capture,
  handle,
  throw,
  bracket,
)
where

import EIO.Prelude hiding (handle, throw, throwIO, bracket)
import qualified EIO.Prelude as Prelude


-- * Exception
-------------------------

data CapturedException

instance Show CapturedException where
  show _ = "Internal EIO exception. You shouldn't be seeing this"

instance Exception CapturedException


-- * IO
-------------------------

{-|
Execute an effect, with all explicit errors handled.
If any lifted IO actions throw unhandled exceptions, they will be propagated.
-}
runEIO :: EIO Void res -> IO res
runEIO (EIO io) = io


-- * EIO
-------------------------

{-|
IO action with explicitly typed error.
-}
newtype EIO err res =
  EIO { run :: IO res }
  deriving (Functor, Applicative, Monad, MonadFail)

{-|
Lifts an IO action without handling the exceptions that may be thrown in it.

For capturing exceptions raised in it combine 'liftIO' with 'capture'.
-}
instance MonadIO (EIO err) where
  liftIO = EIO

instance Bifunctor EIO where
  second = fmap
  first mapper = handle (throw . mapper)

{-| Low-level helper. -}
mapIO :: (IO a -> IO b) -> EIO oldErr a -> EIO newErr b
mapIO mapper =
  coerce . mapper . coerce

{-|
Narrow down any exception that may be raised in the lifted IO
into an explicit error representation if you want it captured.

Producing @Nothing@ in the passed in matching function means that
the exception will be propagated up to 'runEIO' if not captured later.

Use the 'fromException' function to match against specific exception types.
-}
capture :: (SomeException -> Maybe err) -> EIO err res -> EIO err res
capture narrower =
  mapIO $ Prelude.handle $ \ someException ->
    case narrower someException of
      Just err -> Prelude.throwIO (unsafeCoerce err :: CapturedException)
      Nothing -> Prelude.throwIO someException

handle :: (a -> EIO b res) -> EIO a res -> EIO b res
handle handler (EIO io) =
  EIO $ Prelude.catch io $ \ (e :: CapturedException) ->
    case handler (unsafeCoerce e) of EIO io -> io

throw :: err -> EIO err res
throw e =
  EIO (Prelude.throwIO (unsafeCoerce e :: CapturedException))

bracket :: EIO e a -> (a -> EIO e b) -> (a -> EIO e c) -> EIO e c
bracket acquire release use =
  EIO $ Prelude.mask $ \ unmask -> do
    resource <- run acquire
    join (Prelude.catch
      (fmap (\ result -> run (release resource) $> result) (unmask (run (use resource))))
      (\ (e :: SomeException) -> return (run (release resource) *> Prelude.throwIO e)))
