import Text.Pandoc.JSON
import Data.ByteString.Lazy (hGetContents, hPut)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.IO (hClose, hPutStr, IOMode(..), openBinaryFile)
import System.Process

processBlocks :: Block -> IO Block
processBlocks b =
  case b of
    CodeBlock (_ , ["plantuml"], _) content -> plantUMLToImg content
    _ -> return b

plantUMLToImg :: String -> IO Block
plantUMLToImg content =  do
  path <- renderImage content
  return $ Para [Image [] (path, "")]

renderImage :: String -> IO String
renderImage content = do
  let path = uniqueName content ++ ".png"
  (Just hIn, Just hOut, _, _) <-
    createProcess (proc "plantuml" ["-pipe", "-tpng"]){ std_in = CreatePipe,
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

main :: IO ()
main = toJSONFilter processBlocks
