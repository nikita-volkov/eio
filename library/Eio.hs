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


newtype Eio err res =
  Eio (IO res)
  deriving (Functor, Applicative, Monad, MonadFail)

instance MonadIO (Eio err) where
  liftIO =
    Eio

newtype EioException =
  EioException (forall a. a)

instance Show EioException where
  show _ = "Error as exception"

instance Exception EioException

{-| Low-level helper. -}
mapIO :: (IO a -> IO b) -> Eio oldErr a -> Eio newErr b
mapIO mapper =
  coerce . mapper . coerce

catch :: Eio a res -> (a -> Eio b res) -> Eio b res
catch (Eio io) handler =
  Eio $ Prelude.catch io $ \ someException ->
    let
      err = 
        case fromException someException of
          Just (EioException err) -> unsafeCoerce err
          Nothing -> unsafeCoerce someException
      in case handler err of Eio io -> io

throw :: err -> Eio err res
throw e =
  Eio (Prelude.throwIO (unsafeCoerce e :: EioException))

runEio :: Eio Void res -> IO res
runEio (Eio io) = io

liftTotalIO :: IO res -> Eio err res
liftTotalIO = Eio

handleIO :: (SomeException -> err) -> IO res -> Eio err res
handleIO = error "TODO"
