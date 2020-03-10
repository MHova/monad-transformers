{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- http://book.realworldhaskell.org/read/monad-transformers.html

module Main (
  main
) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (MonadState, StateT, get, gets, put, runStateT)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad.Writer (MonadWriter, WriterT, tell, execWriterT)

listDirectory :: FilePath -> IO [String]
listDirectory = fmap (filter notDots) . getDirectoryContents
  where
    notDots p = p /= "." && p /= ".."

countEntries1 :: FilePath -> IO [(FilePath, Int)]
countEntries1 path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntries1 newName
              else return []
  return $ (path, length contents) : concat rest

countEntries2 :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries2 path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries2 newName

myName :: String -> Reader String String
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)

newtype AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

newtype AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

type App1 = WriterT [(FilePath, Int)] (ReaderT AppConfig (StateT AppState IO))
type App2 = StateT AppState (ReaderT AppConfig IO)

newtype MyApp1 a = MyApp1 {
  runA1 :: WriterT [(FilePath, Int)] (ReaderT AppConfig (StateT AppState IO)) a
} deriving (Monad, MonadIO, MonadReader AppConfig, MonadWriter [(FilePath, Int)],
                MonadState AppState, Applicative, Functor)

newtype MyApp2 a = MyApp2 {
  runA2 :: StateT AppState (ReaderT AppConfig IO) a
} deriving (Monad, MonadIO, MonadReader AppConfig,
                MonadState AppState, Applicative, Functor)

implicitGet :: App1 AppState
implicitGet = get

explicitAsk :: MyApp1 AppConfig
explicitAsk = MyApp1 $ lift ask

countEntries3 :: FilePath -> Int -> MyApp1 ()
countEntries3 path currentDepth = do
  _wtf <- MyApp1 $ lift ask
  maxDepth <- asks cfgMaxDepth
  when (currentDepth <= maxDepth) $
    do
      deepest <- gets stDeepestReached
      when (currentDepth > deepest) (put $ AppState currentDepth)
      contents <- liftIO . listDirectory $ path
      tell [(path, length contents)]
      forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO $ doesDirectoryExist newName
        when isDir (countEntries3 newName (currentDepth + 1))

-- countEntries4 :: FilePath -> Int -> MyApp2 [(FilePath, Int)]
-- countEntries4 path currentDepth = do
--   maxDepth <- cfgMaxDepth <$> ask
--   if currentDepth > maxDepth
--     then return []
--     else do
--       deepest <- stDeepestReached <$> get
--       when (currentDepth > deepest) (put $ AppState currentDepth)
--       contents <- liftIO . listDirectory $ path
--       rest <- forM contents $ \name -> do
--                 let newName = path </> name
--                 isDir <- liftIO $ doesDirectoryExist newName
--                 if isDir
--                   then countEntries4 newName (currentDepth + 1)
--                   else return []
--       return $ (path, length contents) : concat rest

runApp1 :: MyApp1 a -> Int -> IO ([(FilePath, Int)], AppState)
runApp1 app maxDepth =
  let
    config = AppConfig maxDepth
    state = AppState 0
  in
    runStateT (runReaderT (execWriterT (runA1 app)) config) state

runApp2 :: MyApp2 a -> Int -> IO (a, AppState)
runApp2 app maxDepth =
  let
    config = AppConfig maxDepth
    state = AppState 0
  in
    runReaderT (runStateT (runA2 app) state) config

main :: IO ()
main = do
  let path = "/home/michael/git/mhova/monad-transformers"
  print =<< execWriterT (countEntries2 path)
  print =<< runApp1 (countEntries3 path 0) 10
  -- print =<< runApp2 (countEntries4 path 0) 10
