module Main where

import Criterion.Config (defaultConfig)
import Criterion.Main (defaultMainWith, bench, nf, parseArgs, defaultOptions, Benchmark)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Text.PrettyPrint.HughesPJ (render, Doc)
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Pretty (document)

import Language.C (parseCFile)
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.Pretty (pretty)
import Language.C.System.GCC (newGCC)

import qualified Data.ByteString.Char8 as BS

parse :: FilePath -> IO CTranslUnit
parse fp = do
  out <- parseCFile (newGCC "gcc") Nothing ["-I."] fp
  case out of
    Left e  -> error (show e)
    Right x -> return x

createDoc :: FilePath -> IO Doc
createDoc file = case takeExtension file of
  ".c" -> do
    translationUnit <- parse file
    return (pretty translationUnit)
  ".xml" -> do
    contents <- readFile file
    return $ document $ xmlParse file contents
  x -> error $ "unsupported file extension: " ++ x

createBench :: FilePath -> Doc -> Benchmark
createBench file doc = bench file $ nf render doc

main = do
  args <- getArgs
  (config, files) <- parseArgs defaultConfig defaultOptions args
  docs <- mapM createDoc files
  let bs = zipWith createBench files docs
  defaultMainWith config (return ()) bs
