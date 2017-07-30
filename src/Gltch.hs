{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Gltch
  ( someFunc
  ) where

import qualified Codec.Picture as CP
import qualified Control.Monad.ST as ST
import qualified Codec.Picture.Types as M

someFunc :: IO ()
someFunc = do
  eitherImage <- CP.readImage "nudes.jpg"
  case eitherImage of
    Left error -> print $ "error: " ++ error
    Right dynamicImage -> do
      let image = CP.convertRGB8 dynamicImage
      let gltchd = gltch image
      CP.savePngImage "after.png" (CP.ImageRGB8 gltchd)
  print "er"

gltch :: CP.Image CP.PixelRGB8 -> CP.Image CP.PixelRGB8
gltch image@CP.Image {..} = ST.runST $ do
  -- mimg <- M.newMutableImage imageWidth imageHeight
  mimg <- M.createMutableImage imageWidth imageHeight (CP.PixelRGB8 0 0 0)
  let mvector = M.mutableImageData mimg
  -- why * 3
  -- Iunno
  let max = imageWidth * imageHeight
  -- let erm = M.unsafePixelAt imageData 1 :: CP.PixelBaseComponent CP.PixelRGB8
  -- M.unsafeWritePixel mvector 1 erm
  let shit = imageWidth
  let go i offset
        | i >= max = M.unsafeFreezeImage mimg
        | otherwise = do
          let argh = M.pixelAt image ((i + offset) `rem` imageHeight) ((i + offset) `div` imageHeight)
          -- let blergh = case (rem i 1000) of
          --               0 -> 1
          --               _ -> 0
          let rn = M.pixelAt image (i `rem` imageHeight) (i `div` imageHeight)
          let blergh = case rn of
                      -- M.PixelRGB8 0 0 0 -> 10
                      M.PixelRGB8 a b 127 -> 10
                      _ -> 0
          M.writePixel mimg (i `rem` imageHeight) (i `div` imageHeight) argh
          go (i + 1) (offset - blergh)
  go 0 0

{-

width = 4
heigth = 5

     x 0  1  2  3
  y
     ____________
  0  | 0  1  2  3
     |
  1  | 4  5  6  7
     |
  2  | 8  9  10 11
     |
  3  | 12 13 14 15
     |
  4  | 16 17 18 19

x = (div/rem) 4
0 = (0/0)
1 = (0/1)
2 = (0/2)
3 = (0/3)
4 = (1/0)

x = (div/rem) 5
0 = (0/0)
1 = (0/1)
2 = (0/2)
3 = (0/3)
4 = (0/4)
5 = (1/0)

-}
