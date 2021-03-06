{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Emulator.IO where

import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Trans (lift, MonadIO)

import Emulator.Monad
import Memory

newtype IOEmulator a = IOEmulator (ReaderT (Memory RealWorld) IO a)
    deriving (Monad, MonadIO)

instance Emulator IOEmulator where
    load address = IOEmulator $ do
        memory <- ask
        lift $ stToIO $ Memory.read memory address
    store address word = IOEmulator $ do
        memory <- ask
        lift $ stToIO $ Memory.write memory address word
    swap address address' = IOEmulator $ do
        memory <- ask
        lift $ stToIO $ Memory.swap memory address address'

runIOEmulator :: IOEmulator a -> IO a
runIOEmulator (IOEmulator reader) = do
    memory <- stToIO Memory.new
    runReaderT reader memory
