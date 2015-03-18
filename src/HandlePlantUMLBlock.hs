import Text.Pandoc.JSON
import Data.ByteString.Lazy (hGetContents, hPut)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.IO (hClose, hPutStr, IOMode(..), openBinaryFile)
import System.Process
import System.Environment

processBlocks :: String ->  Block -> IO Block
processBlocks f b =
  case b of
    CodeBlock (_ , ["plantuml"], _) content -> plantUMLToImg f content
    _ -> return b

plantUMLToImg :: String -> String -> IO Block
plantUMLToImg f content =  do
  path <- renderImage f content
  return $ Para [Image [] (path, "")]

renderImage :: String -> String -> IO String
renderImage f content = do
  let path = uniqueName content ++ "." ++ f
  (Just hIn, Just hOut, _, _) <-
    createProcess (proc "plantuml" ["-pipe", "-t" ++ f]){ std_in = CreatePipe,
                                                          std_out = CreatePipe }
  hPutStr hIn content
  hClose hIn

  hFile <- openBinaryFile path WriteMode
  img <- hGetContents hOut
  hPut hFile img

  hClose hFile
  hClose hOut

  return path

uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

getOutputFormat :: [String] ->  String
getOutputFormat args =
  if length args /= 1
  then error "Invalid arguments specified"
  else imageFormatFromOutputFormat (head args)

imageFormatFromOutputFormat :: String ->  String
imageFormatFromOutputFormat f =
  case f of
	  "latex" -> "eps"
	  "html" -> "svg"
	  "markdown" -> "utxt"
	  _ -> error "Unsupported document format"

main :: IO ()
main = do
  args <-  getArgs
  let outputFormat = getOutputFormat args
  toJSONFilter $ processBlocks outputFormat 
