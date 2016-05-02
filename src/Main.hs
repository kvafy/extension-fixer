module Main where
import Control.Exception (evaluate)
import Control.Monad(filterM)
import Data.Char(toLower)
import Data.List(isPrefixOf, findIndices, intercalate)
import Data.Maybe(isJust, fromJust)
import Data.Monoid(mconcat, First(..), getFirst)
import Data.Word(Word8)
import System.Directory(listDirectory, doesFileExist, renameFile)
import System.Environment(getArgs)
import qualified System.IO as IO
-- Using lazy to have nice and simple file format detection
-- for magic sequences of arbitrary length.
import qualified Data.ByteString.Lazy as B

data FileFormat = FileFormat { extensions :: [String]
                             , magicNumbers :: [[Word8]]
                             }

knownFormats :: [FileFormat]
knownFormats = [ -- images
                 FileFormat ["jpg","jpeg"] [[0xFF,0xD8,0xFF,0xDB],[0xFF,0xD8,0xFF,0xE0],[0xFF,0xD8,0xFF,0xE1]]
               , FileFormat ["gif"] [[0x47,0x49,0x46,0x38,0x37,0x61],[0x47,0x49,0x46,0x38,0x39,0x61]]
               , FileFormat ["png"] [[0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A]]
               , FileFormat ["bmp","dib"] [[0x42,0x4D]]
               , FileFormat ["ico"] [[0x00,0x00,0x01,0x00]]
               , FileFormat ["tiff", "tif"] [[0x49,0x49,0x2A,0x00],[0x4D,0x4D,0x00,0x2A]]
               -- audio
               , FileFormat ["mp3"] [[0xFF,0xFB,49,0x44,0x33]]
               , FileFormat ["ogg","oga","ogv"] [[0x4F,0x67,0x67,0x53]]
               , FileFormat ["wmv","wma","asf"] [[0x30,0x26,0xB2,0x75,0x8E,0x66,0xCF,0x11,0xA6,0xD9,0x00,0xAA,0x00,0x62,0xCE,0x6C]]
               , FileFormat ["midi","mid"] [[0x4D,0x54,0x68,0x64]]
               , FileFormat ["flac"] [[0x66,0x4C,0x61,0x43]]
               -- video
               , FileFormat ["avi"] [[0x52,0x49,0x46,0x46],[0x41,0x56,0x49,0x20]]
               , FileFormat ["mkv","mka","mks","mk3d","webm"] [[0x1A,0x45,0xDF,0xA3]]
               , FileFormat ["3gp","3g2"] [[0x66,0x74,0x79,0x70,0x33,0x67]]
               -- archives
               , FileFormat ["zip","jar","odt","ods","odp","docx","xlsx","pptx","epub","vsdx","apk"] [[0x50,0x4B,0x03,0x04],[0x50,0x4B,0x05,0x06],[0x50,0x4B,0x07,0x08]]
               , FileFormat ["rar"] [[0x52,0x61,0x72,0x21,0x1A,0x07,0x00],[0x52,0x61,0x72,0x21,0x1A,0x07,0x01,0x00]]
               , FileFormat ["gz","tar.gz"] [[0x1F,0x8B]]
               , FileFormat ["tar"] [[0x75,0x73,0x74,0x61,0x72,0x00,0x30,0x30]]
               , FileFormat ["7z"] [[0x37,0x7A,0xBC,0xAF,0x27,0x1C]]
               , FileFormat ["bz2"] [[0x42,0x5A,0x06]]
               , FileFormat ["z","tar.z"] [[0x1F,0x9D],[0x1F,0xA0]]
               ]

instance Show FileFormat where
  show fmt = head . extensions $ fmt

mainExtension :: FileFormat -> String
mainExtension fmt = head . extensions $ fmt


identifyFormats :: [FilePath] -> IO [(FilePath, Maybe FileFormat)]
identifyFormats files = mapM (\fp -> do {ff <- identifyFormatIO fp; return (fp, ff)}) files

identifyFormatIO :: FilePath -> IO (Maybe FileFormat)
identifyFormatIO file = do
  h <- IO.openFile file IO.ReadMode
  bytes <- B.hGetContents h
  fmt <- evaluate $ identifyFormat (B.unpack bytes)
  IO.hClose h
  return fmt

identifyFormat :: [Word8] -> Maybe FileFormat
identifyFormat fileData = getFirst . mconcat . map (\fmt -> First $ hasFormat fmt) $ knownFormats
  where hasFormat fmt = if any (`isPrefixOf` fileData) (magicNumbers fmt)
                          then Just fmt
                          else Nothing

hasCorrectExt :: FilePath -> FileFormat -> Bool
hasCorrectExt fp fmt = any (== ext) (extensions fmt)
  where ext = map toLower . snd $ fileNameAndExt fp

fileNameAndExt :: FilePath -> (String, String)
fileNameAndExt fp = (\(n,e) -> (n, removeDot e)) (splitAt dotIdx fp)
  where dotIdxs = findIndices (== '.') fp
        dotIdx = if not (null dotIdxs) then last dotIdxs else length fp
        removeDot = drop 1

fixExtension :: String -> FilePath -> IO ()
fixExtension ext fp = do
    let basename = fst (fileNameAndExt fp)
        fp' = basename ++ "." ++ ext
    nameTaken <- doesFileExist fp'
    if not nameTaken
      then do
        putStrLn $ "Renaming " ++ fp ++ "  -->  " ++ fp'
        renameFile fp fp'
      else error $ "Cannot rename " ++ fp ++ ". Target file " ++ fp' ++ " already exists."

prompt :: String -> [String] -> IO String
prompt title opts = do
  putStr $ title ++ " (" ++ intercalate "/" opts ++ ") "
  IO.hFlush IO.stdout
  input <- getLine
  if input `elem` opts then return input
                       else prompt title opts

printUsage :: IO ()
printUsage = do
  putStrLn ""
  putStrLn "File extension fixer."
  putStrLn "Scans all files in given directory (non-recursively) and fixes extensions"
  putStrLn "of files that are named incorrectly."
  putStrLn ""
  putStrLn "  Usage: executable <directory>"
  putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
    then do
      putStrLn "Error: Incorrect parameters"
      printUsage
    else do
      let dir = last args

      files <- listDirectory dir >>= filterM doesFileExist
      filesWithMaybeFormats <- identifyFormats files
      let filesWithFormats = map (\(f,fmt) -> (f, fromJust fmt)) . filter (isJust . snd) $ filesWithMaybeFormats

      let misNamedFiles = filter (\(fp,fmt) -> not $ fp `hasCorrectExt` fmt) filesWithFormats
      if null misNamedFiles
        then
          putStrLn $ "All " ++ show (length filesWithFormats) ++ " recognized files have correct extension."
        else do
          putStrLn "Following files are named incorrectly:"
          mapM_ putStrLn $ map (\(f,fmt) ->
                                  " * " ++ f ++ " - detected format is " ++ intercalate "/" (extensions fmt))
                               misNamedFiles
          proceed <- prompt "Do you want to rename the files?" ["y","n"]
          if proceed == "y"
            then do
              mapM_ (\(fp,fmt) -> fixExtension (mainExtension fmt) fp) misNamedFiles
            else
              return ()
