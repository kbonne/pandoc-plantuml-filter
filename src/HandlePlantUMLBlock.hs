-- | Support PlantUML code blocks in PanDoc
-- |
-- | Copyright (C) 2014, 2015 Kurt Bonne (kurt.bonne@gmail.com)
-- | Copyright (C) 2015 Jeremy O'Donoghue (jeremy.odonoghue@gmail.com)
-- |
-- | This program is free software; you can redistribute it and/or modify
-- | it under the terms of the GNU General Public License as published by
-- | the Free Software Foundation; either version 2 of the License, or
-- | (at your option) any later version.
-- |
-- | This program is distributed in the hope that it will be useful,
-- | but WITHOUT ANY WARRANTY; without even the implied warranty of
-- | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- | GNU General Public License for more details.
-- |
-- | You should have received a copy of the GNU General Public License along
-- | with this program; if not, write to the Free Software Foundation, Inc.,
-- | 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-- | 
-- | Usage:
-- |
-- | ~~~ {.plantuml #label caption="A sequence chart" format="eps"}
-- | @startuml
-- | Alice -> Bob: Authentication Request
-- | Bob --> Alice: Authentication Response
-- | @enduml
-- | ~~~~
-- |
-- | The parameters that can be given:
-- | "label", if defined, is used to define a label for the generated image.
-- | "caption", if defined, is used to generate a caption for the generated image.
-- | "format", if defined, allows the output format of the image to be controlled.
-- | The supported formats are "png", "svg" and "eps", with "png" being the default.

import Text.Pandoc.Builder (text, toList)
import Text.Pandoc.JSON
import Data.ByteString.Lazy (hGetContents, hPut)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (toLower)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.IO (hClose, hPutStr, IOMode(..), openBinaryFile)
import System.Process

-- | Selected output format
data ImgType = PNG | EPS | SVG

-- | Process incoming PanDoc blocks. We are only interested in CodeBlock - all
-- | others are returned unmodified.
-- |
-- | In the CodeBlock, id provides an identifier for use as a reference target
-- | (i.e. a LaTeX label). The namevals field is a list of key value pairs
-- | e.g. ("caption", "A Caption").
-- |
-- | We support the following key values for plantuml: "caption" - where the
-- | value is any String; "format" - where the value is "png", "eps" or "svg".
-- | Output defaults to "png" if no format is given.
-- |
-- | Input:  CodeBlock Attr String
-- | Output: Para [Image [String] (img_filename, "fig:")]
-- | 
-- | Where id is defined, we append a LaTeX \label{id} to the caption to allow
-- | easy LaTeX cross-reference (as PanDOc does implicitly with e.g. Section
-- | headings). Rendering to non-LaTeX targets is supposed to remove LaTeXisms.
-- | In other words, output is:
-- | Para [Image [String, Space, RawInline (Format "tex") "\\label{id}"](img, "fig:")
processBlocks :: Block -> IO Block
processBlocks b =
  case b of
    CodeBlock (id , ["plantuml"], namevals) content -> 
         do
            let label   = if length id > 0
                          then RawInline (Format "tex") ("\\label{" ++ id ++ "}")
                          else Space
                caption = case lookup "caption" namevals of
                            Just c  -> (toList $ text c) ++ [label]
                            Nothing -> [Space]
                img_typ = case lookup "format" namevals of
                            Just f  -> case map toLower f of
                                         "eps" -> EPS
                                         "svg" -> SVG
                                         _     -> PNG
                            Nothing -> PNG
            path <- renderImage img_typ content
            return $ Para [Image caption (path, "fig:")]
    _ -> return b

-- | Render the a PlantUML image in the requested format to a given filename.
renderImage :: ImgType -> String -> IO String
renderImage typ content = do
  let ft   = case typ of
               PNG -> "png"
               EPS -> "eps"
               SVG -> "svg"
      path = uniqueName content ++ "." ++ ft
  (Just hIn, Just hOut, _, _) <-
    createProcess (proc "plantuml" ["-pipe", "-t" ++ ft]){ std_in = CreatePipe,
                                                           std_out = CreatePipe }
  hPutStr hIn content
  hClose hIn

  hFile <- openBinaryFile path WriteMode
  img <- hGetContents hOut
  hPut hFile img

  hClose hFile
  hClose hOut

  return path

-- | Generate a unique name for the generated image. Here we use the SHA1 hash
-- | of the content as a sufficiently unique name.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

main :: IO ()
main = toJSONFilter processBlocks
