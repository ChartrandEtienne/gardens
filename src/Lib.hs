{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( someFunc
  ) where

import qualified Codec.Picture as CP
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector as V
import qualified Control.Monad.ST as ST
import qualified Codec.Picture.Types as M
import qualified Codec.Picture.Gif as Gif
import qualified Codec.Picture.Png as Png
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

someFunc :: IO ()
someFunc = do
  fileContent <- BS.readFile "smol.gif"
  case Gif.decodeGifWithPaletteAndMetadata fileContent of
    Left error -> print $ "error: " ++ error
    Right (palette, meta) -> dealWithPaletteImage palette

finally :: M.Image M.Pixel8 -> M.Palette' M.PixelRGB8 -> IO ()
finally image palette = do
  let p = M.palettedAsImage palette
  let array = [(p, 50, image), (p, 50, timeWarp image 4 2)]
  case Gif.encodeGifImages Gif.LoopingForever array of
    Left error -> print $ "error: " ++ error
    Right buffer -> BS.writeFile "loop.gif" (BSL.toStrict buffer)

dealWithPaletteImage :: M.PalettedImage -> IO ()
dealWithPaletteImage img = do
  case img of
    M.TrueColorImage _ -> print "TrueColorImage"
    M.PalettedY8 _ _ -> print "PalettedY8"
    M.PalettedRGB8 image palette -> finally image palette
    M.PalettedRGBA8 _ _ -> print "PalettedRGBA8"
    M.PalettedRGB16 _ _ -> print "PalettedRGB16"
  print "er"

timeWarp :: CP.Image CP.Pixel8 -> Int -> Int -> CP.Image CP.Pixel8
timeWarp img@CP.Image {..} size nframes = ST.runST $ do
  mimg <- M.newMutableImage imageWidth imageHeight
  let go k j
        | k > imageWidth  = go 1 (j + 1)
        | j > imageHeight = M.unsafeFreezeImage mimg
        | j <= 2 ^ (nframes - 1) && k <= 2 ^ (nframes - 1) = do
            CP.writePixel mimg
              (k - 1)
              (j - 1)
              (CP.pixelAt img ((2 * j) - 1) ((2 * k) - 1))
            go (k + 1) j

        | j <= 2 ^ (nframes - 1) && k > 2 ^ (nframes - 1) = do
            CP.writePixel mimg
              (k - 1)
              (j - 1)
              (CP.pixelAt img ((2 * j) - 1) (2 * ((2 ^ nframes) - k + 1) - 1))
            go (k + 1) j

        | j > 2 ^ (nframes - 1) && k <= 2 ^ (nframes - 1) = do
            CP.writePixel mimg
              (k - 1)
              (j - 1)
              (CP.pixelAt img ((2 * (2 ^ nframes - j + 1)) - 1) ((2 * k) - 1))
            go (k + 1) j

        | j > 2 ^ (nframes - 1) && k > 2 ^ (nframes - 1) = do
            CP.writePixel mimg
              (k - 1)
              (j - 1)
              (CP.pixelAt img ((2 * (2 ^ nframes - j + 1)) - 1) ((2 * (2 ^ nframes - k + 1)) - 1))
            go (k + 1) j

        | otherwise = go (k + 1) j
  go 1 1

