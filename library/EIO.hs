module EIO
(
  EIO,
  runEIO,
  handle,
  throw,
  liftExceptionlessIO,
)
where

import EIO.Prelude hiding (handle, throw, throwIO)
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
Execute an effect, with all errors handled.
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

instance Exception err => MonadIO (EIO err) where
  liftIO =
    handleIO throw

instance Bifunctor EIO where
  second = fmap
  first mapper = handle (throw . mapper)

{-| Low-level helper. -}
mapIO :: (IO a -> IO b) -> EIO oldErr a -> EIO newErr b
mapIO mapper =
  coerce . mapper . coerce

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

{-|
Lift an IO action without handling the exceptions that may be thrown in it.
-}
liftExceptionlessIO :: IO res -> EIO err res
liftExceptionlessIO = EIO

liftIOMappingErr :: Exception exc => (exc -> err) -> IO res -> EIO err res
liftIOMappingErr mapper =
  handleIO (throw . mapper)

handleIO :: Exception exc => (exc -> EIO err res) -> IO res -> EIO err res
handleIO handler io =
  EIO $ Prelude.catch io $ \ exc ->
    case handler exc of EIO io -> io

liftIONarrowing :: (SomeException -> Maybe err) -> IO res -> EIO err res
liftIONarrowing narrower =
  handleIO $ \ someException ->
    case narrower someException of
      Just err -> throw err
      Nothing -> EIO (Prelude.throwIO someException)
