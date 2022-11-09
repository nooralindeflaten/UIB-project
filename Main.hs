import System.IO
import System.Environment
import Text.Read (readMaybe)

import Data.List
import NGram
import Model

import qualified Data.Map as Map
import System.Random

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Codec.Compression.GZip as GZip


-- Print the usage instructions for the program.
printUsage
 = putStrLn
 $  "Usage: ramble <COMMAND> <MODELFILE> [SAMPLEFILE]\n"
 ++ "where COMMAND is one of:\n"
 ++ "   create:  Create a new model from samplefile or stdin\n"
 ++ "   on <STARTPRASE> <LENGTH>:\n"
 ++ "                Generate text from model starting\n"
 ++ "                from STARTPHRASE until total length\n"
 ++ "                reaches LENGTH.\n\n"
 ++ "Examples:\n"
 ++ "    ramble create alice.mod alice.txt\n"
 ++ "    ramble on \"Alice went\" alice.mod"

-- A global n for our n-grams.
-- If you play around with it, remember that it
-- has to be the same when creating and using a model.
-- All example models are generated with gramLen = 7
gramLen :: (Num a) => a
gramLen = 7

-- Pick out an element from a weighted list by
-- going through the list until a certain treshold has been
-- reached.
pick :: [(a,Weight)] -> Weight -> a
pick weights treshold =
-- Pick a random element from a weighted list with a given
-- total weight.
pickRandom :: [(a,Weight)] -> Weight -> IO a
pickRandom wl total = randomRIO(1,(total-1)) >>= (\x -> return $ pick wl x)
   
-- Generate a fixed amount of text from a model starting from a given
-- start string
generate :: TextModel -> String -> Integer -> IO String
generate model start amount = tail (grams start)

-- Helper function which generates n-grams from a model
generate' :: TextModel -> NGram -> Integer -> IO [NGram]
generate' model start amount = case nextDistribution model start of
   Nothing -> return []
   Just v -> do
      nextstart <- pickRandom (fst v) (snd v)
      genNew <- generate' model nextstart (amount -1)
      return (nextstart : genNew)
 
-- Serialize a text model and write a handle.
writeModel :: TextModel -> Handle -> IO ()
writeModel model h
 = ByteString.hPut h $ GZip.compress
                     $ UTF8.fromString
                     $ show model

-- Read a text model from a handle.
readModel :: Handle -> IO TextModel
readModel h = do
   lines <- ByteString.hGetContents h
      $ GZip.decompress
      $ UTF8.toString
      $ read model



main = do
   args <- getArgs
   case args of
     ["create",modelFile] -> do
        modelh <- openFile modelFile WriteMode
        sample <- hGetContents stdin
        let model = createModel gramLen sample
        writeModel model modelh
        hClose modelh
     ["create",modelFile,sampleFile] -> do
        modelh <- openFile modelFile WriteMode
        sampleh <- openFile sampleFile ReadMode
        sample <- hGetContents sampleh
        let model = createModel gramLen sample
        putStrLn $ "Created model with: " ++ show (Map.size model) ++ " n-grams"
        writeModel model modelh
        hClose modelh
        hClose sampleh
     ["on",startPhrase,sLength,modelFile] -> do
        modelh <- openFile modelFile ReadMode
        model <- readModel modelh
        case readMaybe sLength of
           (Just outlength)
                  ->  generate model startPhrase outlength >>= putStrLn
           Nothing -> printUsage
        hClose modelh
     _ -> printUsage

