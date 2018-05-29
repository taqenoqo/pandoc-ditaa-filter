{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Pandoc.JSON
import System.IO.Temp
import System.FilePath
import System.Console.GetOpt
import System.Process (readProcess)
import System.Environment (getArgs)
import System.Directory
import Data.Maybe (fromMaybe)
import Data.Reflection (Given, give, given)
import Text.Parsec
import Control.Monad (void)

pattern DitaaBlock code <- CodeBlock (_, ["ditaa"], _) code

data Config = Config {
  cfgDitaaCmd   :: String,
  cfgDitaaOpts  :: String,
  cfgDitaaOptsP :: [String],
  cfgImgDir     :: Maybe FilePath,
  cfgAppID      :: String,
  cfgImgDirRel  :: Maybe FilePath
  } deriving Show

defaultConfig :: Config
defaultConfig = Config {
  cfgDitaaCmd   = "ditaa",
  cfgDitaaOpts  = "",
  cfgDitaaOptsP = [],
  cfgImgDir     = Nothing,
  cfgAppID      = "ditaa-filter",
  cfgImgDirRel  = Nothing
  }

options :: [OptDescr (Config -> Config)]
options = [
  Option [] ["ditaa-cmd"]
    (ReqArg (\s cfg -> cfg { cfgDitaaCmd = s }) "CMD")
    "ditaa command",
  Option [] ["ditaa-opts"]
    (ReqArg (\s cfg -> cfg { cfgDitaaOpts = s }) "OPTS")
    "ditaa options",
  Option [] ["img-dir"]
    (ReqArg (\s cfg -> cfg { cfgImgDir = Just s }) "DIR")
    "image output directory",
  Option [] ["img-dir-relative"]
    (ReqArg (\s cfg -> cfg { cfgImgDirRel = Just s }) "DIR")
    "relative path of image output directory"
  ]

getOptions :: IO (Config, [String])
getOptions = do
  args <- getArgs
  case getOpt Permute options args of
    (c, n, []) -> return (foldl (flip id) defaultConfig c, n)
    (_, _, e)  -> ioError $ userError $ "error: " ++ concat e

getConfig :: IO Config
getConfig = do
  (cfg, _) <- getOptions
  postprocessDitaaOpts cfg
  where
    postprocessDitaaOpts cfg = case splitDitaaOptions $ cfgDitaaOpts cfg of
      Left err -> ioError $ userError $ show err
      Right [""] -> return $ cfg { cfgDitaaOptsP = [] }
      Right opts -> return $ cfg { cfgDitaaOptsP = opts }

main :: IO ()
main = do
  cfg <- getConfig
  give cfg $ do
    imgDir <- createImgDir
    toJSONFilter $ convertPandoc imgDir

createImgDir :: Given Config => IO FilePath
createImgDir = case cfgImgDir given of
  Nothing -> do
    sysTmpDir <- getCanonicalTemporaryDirectory
    createTempDirectory sysTmpDir $ cfgAppID given
  Just imgDir -> do
    pwd <- getCurrentDirectory
    let imgDirPath = pwd </> imgDir
    createDirectoryIfMissing False imgDirPath
    return imgDirPath

convertPandoc :: Given Config => FilePath -> Pandoc -> IO Pandoc
convertPandoc imgDir (Pandoc meta blocks) = do
  newBlocks <- mapM (ditaaBlockToImg imgDir title) $ numberDitaaBlocks blocks 1
  return $ Pandoc meta newBlocks
  where
    title = case docTitle meta of
      [Str t] -> t
      _ -> ""
    numberDitaaBlocks [] !_ = []
    numberDitaaBlocks (b@(DitaaBlock _) : bs) !i =
      (b, i) : numberDitaaBlocks bs (i + 1)
    numberDitaaBlocks (b:bs) !i = (b, i) : numberDitaaBlocks bs i

ditaaBlockToImg :: Given Config => FilePath -> String -> (Block, Int) -> IO Block
ditaaBlockToImg imgDir title (DitaaBlock code, i) = do
  writeFile txtPath code
  readProcess ditaaCmd ditaaArgs ""
  return $ Para [Image nullAttr [] (imgLink, "fig:" ++ imgTitle)]
  where
    imgTitle = cfgAppID given ++ show i
    ditaaCmd = cfgDitaaCmd given
    ditaaArgs = cfgDitaaOptsP given ++ [txtPath, imgPath]
    basename = case title of
      "" -> show i
      t -> t ++ "-" ++ show i
    txtPath = imgDir </> basename <.> "txt"
    imgPath = imgDir </> basename <.> "png"
    imgLink = case cfgImgDirRel given of
      Nothing -> imgPath
      Just imgRelDir -> imgRelDir </> basename <.> "png"
ditaaBlockToImg _ _ (b, _) = return b

splitDitaaOptions :: String -> Either ParseError [String]
splitDitaaOptions s = parse ditaaOpts "ditaa options" s where
  ditaaOpts = ditaaOpt `sepBy` spaces1
  ditaaOpt = do
    ss <- manyTill (try escape <|> nonEscape) $ eof <|> lookAhead spaces1
    return $ concat ss
  nonEscape = manyTill anyChar $ eof <|> lookAhead (void escape) <|> lookAhead spaces1
  spaces1 = skipMany1 space
  escape = try escapedSlosh <|> try escapedSpace
  escapedSlosh = string "\\\\" >> return "\\"
  escapedSpace = do
    string "\\"
    sp <- space
    return [sp]

