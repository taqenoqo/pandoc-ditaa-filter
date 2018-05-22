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

pattern DitaaBlock code <- CodeBlock (_, ["ditaa"], _) code

data Config = Config
  { cfgDitaaCmd :: String
  , cfgImgDir :: Maybe FilePath
  , cfgAppID :: String
  , cfgImgDirRel :: Maybe FilePath
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { cfgDitaaCmd = "ditaa"
  , cfgImgDir = Nothing
  , cfgAppID = "ditaa-filter"
  , cfgImgDirRel = Nothing
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option [] ["ditta-cmd"] (ReqArg (\s cfg -> cfg { cfgDitaaCmd = s }) "CMD") "ditaa command"
  , Option [] ["img-dir"] (ReqArg (\s cfg -> cfg { cfgImgDir = Just s }) "DIR") "image output directory"
  , Option [] ["img-dir-relative"] (ReqArg (\s cfg -> cfg { cfgImgDirRel = Just s }) "DIR") "relative path of image output directory"
  ]

getOptions :: IO (Config, [String])
getOptions = do
  args <- getArgs
  case getOpt Permute options args of
    (c, n, []) -> return (foldl (flip id) defaultConfig c, n)
    (_, _, e)  -> ioError $ userError $ "error: " ++ concat e

main :: IO ()
main = do
  (cfg, _) <- getOptions
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
  newBlocks <- mapM (ditaaBlockToImg imgDir) numberedBlocks
  return $ Pandoc meta newBlocks
  where
    numberedBlocks = numberDitaaBlocks blocks 1

ditaaBlockToImg :: Given Config => FilePath -> (Block, Int) -> IO Block
ditaaBlockToImg imgDir (DitaaBlock code, i) = do
  writeFile txtPath code
  readProcess ditaaCmd [txtPath, imgPath] ""
  return $ Para [Image nullAttr [Str imgTitle] (imgLink, "fig:" ++ imgTitle)]
  where
    imgTitle = cfgAppID given ++ show i
    ditaaCmd = cfgDitaaCmd given
    txtPath = imgDir </> show i <.> "txt"
    imgPath = imgDir </> show i <.> "png"
    imgLink = case cfgImgDirRel given of
      Nothing -> imgPath
      Just imgRelDir -> imgRelDir </> show i <.> "png"
ditaaBlockToImg _ (b, _) = return b

numberDitaaBlocks :: [Block] -> Int -> [(Block, Int)]
numberDitaaBlocks [] !_ = []
numberDitaaBlocks (b@(DitaaBlock _) : bs) !i =
  (b, i) : numberDitaaBlocks bs (i + 1)
numberDitaaBlocks (b:bs) !i = (b, i) : numberDitaaBlocks bs i

