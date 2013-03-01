module Main where

import Control.DeepSeq (($!!), NFData(rnf))
import Criterion.Config (defaultConfig)
import Criterion.Main (defaultMainWith, bench, nf, parseArgs, defaultOptions, Benchmark)
import Data.Maybe (catMaybes)
import Language.C (parseCFile)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.System.GCC (newGCC)
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Pretty (document)

import PrettyTestVersion (render, Doc(..))

instance NFData Doc where
  rnf (Empty           ) = ()
  rnf (NilAbove d      ) = rnf d
  rnf (TextBeside _ _ d) = rnf d -- avoiding strict fields
  rnf (Nest _ d        ) = rnf d -- avoiding strict fields
  rnf (Union d1 d2     ) = rnf d1 `seq` rnf d2
  rnf (NoDoc           ) = ()
  rnf (Beside d1 b d2  ) = rnf d1 `seq` rnf b `seq` rnf d2
  rnf (Above d1 b d2   ) = rnf d1 `seq` rnf b `seq` rnf d2

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
createBench file doc = bench file (nf render $!! doc)

main = do
  args <- getArgs
  (config, files) <- parseArgs defaultConfig defaultOptions args
  docs <- mapM createDoc files
  let bs = zipWith createBench files docs
  defaultMainWith config (return ()) bs
