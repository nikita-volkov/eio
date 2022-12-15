module Main where

import Prelude hiding (catch, throw, bracket, handle, try)
import qualified Prelude
import EIO
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck


main =
  defaultMain $ testGroup "All" $ [
    testCase "Exception raised in IO gets caught" $ do
      ref <- newIORef @(Maybe IOException) Nothing
      runEIO $ do
        handle
          (\e -> liftIO (writeIORef ref (Just e)))
          (capture fromException (fail "ABC"))
      assertEqual "" (Just "user error (ABC)") =<< fmap (fmap show) (readIORef ref)
    ,
    testCase "Custom error gets caught" $ do
      ref <- newIORef @Int 0
      runEIO $ do
        liftIO (modifyIORef' ref succ)
        handle
          (\e -> liftIO (modifyIORef' ref (+ e)))
          (throw 2)
      val <- readIORef ref
      assertEqual "" 3 val
    ,
    testCase "Action preceding throwing has effect" $ do
      ref <- newIORef @Int 0
      runEIO $ do
        let
          main = do
            liftIO (modifyIORef' ref succ)
            throw 2
          handler e =
            liftIO (modifyIORef' ref (+ e))
          in handle handler main
      val <- readIORef ref
      assertEqual "" 3 val
    ,
    testCase "Uncaptured exceptions get propagated" $ do
      res <- Prelude.try @IOException (runEIO (liftIO (fail "ABC")))
      case res of
        Left exc -> assertEqual "" "user error (ABC)" (show exc)
        Right _ -> assertFailure "Didn't fail"
    ,
    testCase "Non-exception gets processed" $ do
      res <- runEIO $ try @Text @() $ throw "abc"
      assertEqual "" (Left "abc") res
    ]
