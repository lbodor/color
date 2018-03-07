{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Lens       (over, (^?))
import           Data.List.Split    (chunksOf)
import           Numeric            (readHex, showHex)
import           System.Environment (getArgs)
import           System.IO          (IOMode (..), hPutStrLn, withFile)
import           Text.Regex.Lens    (matchedString, regex)
import           Text.Regex.PCRE    (Regex, makeRegex, (=~))
import           Text.Regex.Quote   (r)

main :: IO ()
main = do
    [inputFile, outputFile] <- getArgs
    contents <- lines <$> readFile inputFile
    withFile outputFile WriteMode $ \f ->
        mapM_ (hPutStrLn f . scaleLine 0.5) contents

scaleLine :: Float -> String -> String
scaleLine factor = over (regex [r|(?i)#[0-9a-f]{6}|] . matchedString) (scaleRGB factor)

scaleRGB :: Float -> String -> String
scaleRGB factor (_:rgb) =
    "#" ++ concatMap (showHex' . scale' . readHex') (chunksOf 2 rgb)
  where
    scale' = min 255 . round . (* factor) . fromIntegral

readHex' :: String -> Integer
readHex' = fst . head . readHex

showHex' :: Integer -> String
showHex' n = case length hex of
    2 -> hex
    1 -> '0':hex
  where
    hex = showHex n ""

