{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Main where

import Emulator
import Emulator.IO
import Emulator.Monad
import Instruction
import Memory

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS
import Graphics.UI.GLUT
import Data.IORef
import Control.Concurrent

splitFunction :: [String] -> [String]
splitFunction str
    | length str > 10 = (\x -> [unwords . fst $ x] ++ (splitFunction . snd) x) $ splitAt 10 str
    | otherwise = [unwords str]


main :: IO ()
main = do
    x <- BS.readFile "blargh"
    reference <- newIORef [""]
    renderPos <- newIORef $ Vector4 
    runIOEmulator $ do
        loadProgram x
        liftIO $ forkIO $ initiateDisplay reference
        printRange (liftIO . (writeIORef reference) . splitFunction) 0x0 0x31

initiateDisplay :: IORef [String] -> IO ()
initiateDisplay ref = do
    (progname, _) <- getArgsAndInitialize 
    createWindow "DCPU-16 Emulator"
    windowSize $= (Size 1280 768)
    drawPos <- newIORef (0.0::GLfloat, 0.0::GLfloat)
    displayCallback $= (display ref drawPos) 
    mainLoop

display :: IORef [String] -> IORef (GLfloat, GLfloat) -> IO ()
display ref drawPos = do
    clear [ColorBuffer]
    datArr <- get ref
    pos <- get drawPos
    preservingMatrix $ do 
        mapM_ (stringAt pos) $ zip3 datArr (repeat 0 :: [GLfloat]) ([-0.05,-0.1..]::[GLfloat])
    flush


stringAt :: (GLfloat, GLfloat) -> (String, GLfloat, GLfloat) -> IO ()
stringAt (x, y) (str,dx,dy) = do
    color $ Color3 (1::GLfloat) (1::GLfloat) (1::GLfloat)
    rasterPos $ Vertex2 (x+dx) (y+dy)
    renderString Fixed8By13 str