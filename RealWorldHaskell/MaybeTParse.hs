{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module MaybeTParse
    (
      Parse
    , evalParse
    ) where

import           Control.Monad.State  (MonadState, State, evalState, get, put)
import           Control.Monad.Trans  (MonadTrans, lift)
import qualified Data.ByteString.Lazy as L
import           Data.Int             (Int64)
import           MyMaybeT             (MaybeT, runMaybeT)

data ParseState = ParseState
    { string :: L.ByteString
    , offset :: Int64
    }
    deriving (Show)

newtype Parse a = P {
      runP :: EitherT String (State ParseState) a
    } deriving (Functor, Applicative, Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Either String a
evalParse m s = evalState (runEitherT (runP m)) (ParseState s 0)

{-
  Our Parse monad is not a perfect replacement for its earlier counterpart.
  Because we are using Maybe instead of Either to represent a result, we can't
  report any useful information if a parse fails.

  Create an EitherT sometype monad transformer, and use it to implement a more
  capable Parse monad that can report an error message if parsing fails.
-}

newtype EitherT a m b = EitherT {
  runEitherT :: m (Either a b)
}

instance (Functor m) => Functor (EitherT a m) where
  fmap f = EitherT . (fmap . fmap) f . runEitherT

instance (Monad m, Applicative m) => Applicative (EitherT a m) where
  pure = EitherT . pure . Right
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  etf <*> etab = EitherT $ do
    ef <- runEitherT etf
    case ef of
      Left e -> return $ Left e
      Right f -> do
        eab <- runEitherT etab
        return $ fmap f eab

instance (Monad m) => Monad (EitherT a m) where
  return = EitherT . return . Right
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  ma >>= f = EitherT $ do
    ea <- runEitherT ma
    case ea of
      Left e  -> return $ Left e
      Right a -> runEitherT $ f a

instance MonadTrans (EitherT a) where
  -- lift :: (MonadTrans t, Monad m) => m a -> t m a
  lift = EitherT . fmap Right

instance (MonadState s m) => MonadState s (EitherT a m) where
  get = lift get
  put = lift . put
