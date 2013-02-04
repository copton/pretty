module Main where

import Criterion.Config (defaultConfig)
import Criterion.Main (defaultMainWith, bench, nf, parseArgs, defaultOptions)
import System.Environment (getArgs)
import Text.PrettyPrint.HughesPJ (render)

import Language.C (parseCFile)
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.Data (initPos)
import Language.C.Pretty (pretty)
import Language.C.System.GCC (newGCC)

import qualified Data.ByteString.Char8 as BS

parse :: FilePath -> IO CTranslUnit
parse fp = do
  out <- parseCFile (newGCC "gcc") Nothing ["-I."] fp
  case out of
    Left e  -> error (show e)
    Right x -> return x

main = do
  args <- getArgs
  (config, files) <- parseArgs defaultConfig defaultOptions args
  print files
  tus <- mapM parse files
  let docs = map pretty tus
  let bs = zipWith (\f d -> bench f $ nf render d) files docs
  defaultMainWith config (return ()) bs
