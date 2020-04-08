{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Polysemy
import System.Random (randomIO)

data Console m a where
  PrintLine :: String -> Console m ()
  ReadLine :: Console m String

makeSem ''Console

runConsoleIO ::
  Member (Embed IO) r =>
  Sem (Console ': r) a ->
  Sem r a
runConsoleIO = interpret $ \case
  PrintLine line -> embed $ putStrLn line
  ReadLine -> embed getLine

runConsolePure :: String -> Sem (Console ': r) a -> Sem r a
runConsolePure input = interpret $ \case
  PrintLine _ -> pure ()
  ReadLine -> pure input

--printLine :: Member Console r => String -> Sem r ()
--readLine :: Member Console r => Sem r String

data Random v m a where
  NextRandom :: Random v m v

makeSem ''Random

runRandomIO :: Member (Embed IO) r => Sem (Random Int ': r) a -> Sem r a
runRandomIO = interpret $ \case
  NextRandom -> embed randomIO

runRandomPure :: Int -> Sem (Random Int ': r) a -> Sem r a
runRandomPure v = interpret $ \case
  NextRandom -> pure v

program ::
  Member Console r =>
  Member (Random Int) r =>
  Sem r Int
program = do
  printLine "Insert your number:"
  i1 <- readLine
  i2 <- nextRandom
  pure (read i1 + i2)

main :: IO ()
main = print a
  where
    --execute = runM . runRandomIO . runConsoleIO $ program
    a = run . runConsolePure "10" . runRandomPure 20 $ program
