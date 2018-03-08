{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Lens               (over, (^?))
import           Data.Int                   (Int64)
import           Data.List                  (unfoldr)
import           Data.Monoid                ((<>))
import           Numeric                    (readHex, showHex)
import           System.Environment         (getArgs)
import           System.IO                  (IOMode (..), hPutStrLn, withFile)
import           Text.Regex.Lens            (matchedString, regex)
import           Text.Regex.PCRE            (Regex, makeRegex, (=~))
import           Text.Regex.Quote           (r)

import qualified Data.ByteString.Lazy.Char8 as BSL

main :: IO ()
main = do
    [inputFile, read -> factor, outputFile] <- getArgs
    contents <- BSL.lines <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \f ->
        mapM_ (BSL.hPutStrLn f . scaleLine factor) contents

scaleLine :: Float -> BSL.ByteString -> BSL.ByteString
scaleLine factor = over (regex [r|(?i)#[\da-f]{6}|] . matchedString) (scaleRGB factor)

scaleRGB :: Float -> BSL.ByteString -> BSL.ByteString
scaleRGB factor (BSL.tail -> rgb) =
    "#" <> BSL.concat (map (showHex' . scale' . readHex') (chunksOf 2 rgb))
  where
    scale' = min 255 . round . (* factor) . fromIntegral

chunksOf :: Int64 -> BSL.ByteString -> [BSL.ByteString]
chunksOf n = unfoldr (\bs -> if BSL.null bs then Nothing else Just (BSL.splitAt n bs))

readHex' :: BSL.ByteString -> Integer
readHex' = fst . head . readHex . BSL.unpack

showHex' :: Integer -> BSL.ByteString
showHex' n = case BSL.length hex of
    2 -> hex
    1 -> '0' `BSL.cons` hex
  where
    hex = BSL.pack (showHex n "")
