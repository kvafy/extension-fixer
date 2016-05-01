module Main where
import Control.Monad(filterM)
import Data.Char(toLower)
import Data.List(isPrefixOf, findIndices, intercalate)
import Data.Maybe(isJust, fromJust)
import Data.Monoid(mconcat, First(..), getFirst)
import Data.Word(Word8)
import System.Directory(listDirectory, doesFileExist, renameFile)
import System.Environment(getArgs)
-- Using lazy to have nice and simple file format detection
-- for magic sequences of arbitrary length.
import qualified Data.ByteString.Lazy as B

data FileFormat = FileFormat { extensions :: [String]
                             , magicNumbers :: [[Word8]]
                             }

knownFormats :: [FileFormat]
knownFormats = [ FileFormat ["jpg", "jpeg"] [[0xFF,0xD8,0xFF,0xDB],[0xFF,0xD8,0xFF,0xE0],[0xFF,0xD8,0xFF,0xE1]]
               , FileFormat ["gif"] [[0x47,0x49,0x46,0x38,0x37,0x61],[0x47,0x49,0x46,0x38,0x39,0x61]]
               , FileFormat ["png"] [[0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A]]
               ]

instance Show FileFormat where
  show fmt = head . extensions $ fmt

mainExtension :: FileFormat -> String
mainExtension fmt = head . extensions $ fmt


identifyFormats :: [FilePath] -> IO [(FilePath, Maybe FileFormat)]
identifyFormats files = mapM (\fp -> do {ff <- identifyFormatIO fp; return (fp, ff)}) files

identifyFormatIO :: FilePath -> IO (Maybe FileFormat)
identifyFormatIO file = do
  bytes <- B.readFile file
  return $ identifyFormat (B.unpack bytes)

identifyFormat :: [Word8] -> Maybe FileFormat
identifyFormat fileData = getFirst . mconcat . map (\fmt -> First $ hasFormat fmt) $ knownFormats
  where hasFormat fmt = if any (`isPrefixOf` fileData) (magicNumbers fmt)
                          then Just fmt
                          else Nothing

hasCorrectExt :: FilePath -> FileFormat -> Bool
hasCorrectExt fp fmt = any (== ext) (extensions fmt)
  where ext = snd (fileNameAndExt fp)

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
                                  " * " ++ f ++ " - detected format is " ++ mainExtension fmt)
                               misNamedFiles
          proceed <- prompt "Do you want to rename the files?" ["y","n"]
          if proceed == "y"
            then do
              mapM_ (\(fp,fmt) -> fixExtension (mainExtension fmt) fp) misNamedFiles
            else
              return ()
