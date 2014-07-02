{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Prelude hiding (lookup)

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Text (Text)
import           Data.Word
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testCase "load" loadTest
    , testCase "types" typesTest
    , testCase "interp" interpTest
    , testCase "scoped-interp" scopedInterpTest
    , testCase "import" importTest
    , testCase "reload" reloadTest
    ]

withLoad :: FilePath -> (Config -> IO ()) -> IO ()
withLoad name t = do
    mb <- try $ load (testFile name)
    case mb of
        Left (err :: SomeException) -> assertFailure (show err)
        Right cfg -> t cfg

withReload :: FilePath -> ([Maybe FilePath] -> Config -> IO ()) -> IO ()
withReload name t = do
    tmp   <- getTemporaryDirectory
    temps <- forM (testFile name) $ \f -> do
        exists <- doesFileExist (worth f)
        if exists
            then do
                (p,h) <- openBinaryTempFile tmp "test.cfg"
                L.hPut h =<< L.readFile (worth f)
                hClose h
                return (p <$ f, Just p)
            else do
                return (f, Nothing)
    flip finally (mapM_ removeFile (catMaybes (map snd temps))) $ do
        mb <- try $ autoReload autoConfig (map fst temps)
        case mb of
            Left (err :: SomeException) -> assertFailure (show err)
            Right (cfg, tid) -> t (map snd temps) cfg >> killThread tid

testFile :: FilePath -> [Worth FilePath]
testFile name = [Required $ "tests" </> "resources" </> name]

takeMVarTimeout :: Int -> MVar a -> IO (Maybe a)
takeMVarTimeout millis v = do
    w <- newEmptyMVar
    tid <- forkIO $ do
        putMVar w . Just =<< takeMVar v
    forkIO $ do
        threadDelay (millis * 1000)
        killThread tid
        tryPutMVar w Nothing
        return ()
    takeMVar w

loadTest :: Assertion
loadTest =
  withLoad "pathological.cfg" $ \cfg -> do
    aa  <- lookup cfg "aa"
    assertEqual "int property" aa $ (Just 1 :: Maybe Int)

    ab  <- lookup cfg "ab"
    assertEqual "string property" ab (Just "foo" :: Maybe Text)

    acx <- lookup cfg "ac.x"
    assertEqual "nested int" acx (Just 1 :: Maybe Int)

    acy <- lookup cfg "ac.y"
    assertEqual "nested bool" acy (Just True :: Maybe Bool)

    ad <- lookup cfg "ad"
    assertEqual "simple bool" ad (Just False :: Maybe Bool)

    ae <- lookup cfg "ae"
    assertEqual "simple int 2" ae (Just 1 :: Maybe Int)

    af <- lookup cfg "af"
    assertEqual "list property" af (Just (2,3) :: Maybe (Int,Int))

    deep <- lookup cfg "ag.q-e.i_u9.a"
    assertEqual "deep bool" deep (Just False :: Maybe Bool)

typesTest :: Assertion
typesTest =
  withLoad "pathological.cfg" $ \cfg -> do
    asInt <- lookup cfg "aa" :: IO (Maybe Int)
    assertEqual "int" asInt (Just 1)

    asInteger <- lookup cfg "aa" :: IO (Maybe Integer)
    assertEqual "int" asInteger (Just 1)

    asWord <- lookup cfg "aa" :: IO (Maybe Word)
    assertEqual "int" asWord (Just 1)

    asInt8 <- lookup cfg "aa" :: IO (Maybe Int8)
    assertEqual "int8" asInt8 (Just 1)

    asInt16 <- lookup cfg "aa" :: IO (Maybe Int16)
    assertEqual "int16" asInt16 (Just 1)

    asInt32 <- lookup cfg "aa" :: IO (Maybe Int32)
    assertEqual "int32" asInt32 (Just 1)

    asInt64 <- lookup cfg "aa" :: IO (Maybe Int64)
    assertEqual "int64" asInt64 (Just 1)

    asWord8 <- lookup cfg "aa" :: IO (Maybe Word8)
    assertEqual "word8" asWord8 (Just 1)

    asWord16 <- lookup cfg "aa" :: IO (Maybe Word16)
    assertEqual "word16" asWord16 (Just 1)

    asWord32 <- lookup cfg "aa" :: IO (Maybe Word32)
    assertEqual "word32" asWord32 (Just 1)

    asWord64 <- lookup cfg "aa" :: IO (Maybe Word64)
    assertEqual "word64" asWord64 (Just 1)

    asTextBad <- lookup cfg "aa" :: IO (Maybe Text)
    assertEqual "bad text" asTextBad Nothing

    asTextGood <- lookup cfg "ab" :: IO (Maybe Text)
    assertEqual "good text" asTextGood (Just "foo")

    asStringGood <- lookup cfg "ab" :: IO (Maybe String)
    assertEqual "string" asStringGood (Just "foo")

    asInts <- lookup cfg "xs" :: IO (Maybe [Int])
    assertEqual "ints" asInts (Just [1,2,3])

    asChar <- lookup cfg "c" :: IO (Maybe Char)
    assertEqual "char" asChar (Just 'x')

interpTest :: Assertion
interpTest =
  withLoad "pathological.cfg" $ \cfg -> do
    home    <- getEnv "HOME"
    cfgHome <- lookup cfg "ba"
    assertEqual "home interp" (Just home) cfgHome

scopedInterpTest :: Assertion
scopedInterpTest = withLoad "interp.cfg" $ \cfg -> do
    home    <- getEnv "HOME"

    lookup cfg "myprogram.exec"
        >>= assertEqual "myprogram.exec" (Just $ home++"/services/myprogram/myprogram")

    lookup cfg "myprogram.stdout"
        >>= assertEqual "myprogram.stdout" (Just $ home++"/services/myprogram/stdout")

    lookup cfg "top.layer1.layer2.dir"
        >>= assertEqual "nested scope" (Just $ home++"/top/layer1/layer2")

importTest :: Assertion
importTest =
  withLoad "import.cfg" $ \cfg -> do
    aa  <- lookup cfg "x.aa" :: IO (Maybe Int)
    assertEqual "simple" aa (Just 1)
    acx <- lookup cfg "x.ac.x" :: IO (Maybe Int)
    assertEqual "nested" acx (Just 1)

reloadTest :: Assertion
reloadTest =
  withReload "pathological.cfg" $ \[Just f] cfg -> do
    aa <- lookup cfg "aa"
    assertEqual "simple property 1" aa $ Just (1 :: Int)

    dongly <- newEmptyMVar
    wongly <- newEmptyMVar
    subscribe cfg "dongly" $ \ _ _ -> putMVar dongly ()
    subscribe cfg "wongly" $ \ _ _ -> putMVar wongly ()
    L.appendFile f "\ndongly = 1"
    r1 <- takeMVarTimeout 2000 dongly
    assertEqual "notify happened" r1 (Just ())
    r2 <- takeMVarTimeout 2000 wongly
    assertEqual "notify not happened" r2 Nothing
