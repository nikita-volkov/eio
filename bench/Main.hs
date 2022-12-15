module Main where

import Prelude
import Criterion.Main
import qualified EIO


data CustomException =
  CustomException
  deriving (Show, Typeable, Generic)

instance NFData CustomException

instance Exception CustomException

main =
  do
    ref <- newIORef @Int 0
    defaultMain [
      bgroup "Success" [
        bench "EIO" $ nfIO $ EIO.runEIO $ do
          replicateM_ 10 $ liftIO (writeIORef ref 0)
          a <- liftIO (readIORef ref)
          liftIO (writeIORef ref $! succ a)
        ,
        bench "IO" $ nfIO $ do
          replicateM_ 10 $ liftIO (writeIORef ref 0)
          a <- readIORef ref
          writeIORef ref $! succ a
        ]
        ,
        bench "ExceptT" $ nfIO $ runExceptT @() $ do
          replicateM_ 10 $ liftIO (writeIORef ref 0)
          a <- liftIO (readIORef ref)
          liftIO (writeIORef ref $! succ a)
      ,
      bgroup "Handling failure" [
        bench "EIO" $ nfIO $ EIO.runEIO $ do
          EIO.try
            (do
              replicateM_ 10 $ liftIO (writeIORef ref 0)
              EIO.throw CustomException
              liftIO (writeIORef ref 1)
              )
        ,
        bench "IO" $ nfIO $ do
          try @CustomException (do
            replicateM_ 10 $ liftIO (writeIORef ref 0)
            throwIO CustomException
            liftIO (writeIORef ref 1)
            )
        ],
        -- This one proves to be much faster than both EIO and IO,
        -- because it avoids the instructions in 'catch'.
        bench "ExceptT" $ nfIO $ runExceptT $ do
          catchError
            (do
              replicateM_ 10 $ liftIO (writeIORef ref 0)
              throwError CustomException
              liftIO (writeIORef ref 1)
              return (Right ())
              )
            (return . Left)
      ]
