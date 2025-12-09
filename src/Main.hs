module Main where

import Data.Massiv.Array as A hiding (zip, mapM_, mapM, take)
import Data.Massiv.Array.IO as AIO
import Data.Word (Word8) 

import Text.Printf (printf)
import Data.Complex ( cis, Complex(..) )
import System.Directory (createDirectoryIfMissing)

import Orbit ( escapeTime )
import Polynomial (Poly(..))
import Ring ()

width, height :: Int
width = 800
height = 600

maxIter :: Int
maxIter = 255

pixelToComplex :: Int -> Int -> Complex Double
pixelToComplex x y = (scale :+ 0) * ((xf - w/2) :+ (yf - h/2))
  where
    xf = fromIntegral x
    yf = fromIntegral y
    w  = fromIntegral width
    h  = fromIntegral height
    scale = 4.0 / w

renderFrameMassiv :: Int -> Complex Double -> IO ()
renderFrameMassiv frameIndex c = do
    let filename = printf "output/frame_%04d.png" frameIndex
    putStrLn $ "Rendering " ++ filename ++ "..."

    let poly = Poly [c, 0, 1] :: Poly (Complex Double)

    let img :: Image S (SRGB NonLinear) Word8
        img = A.makeArrayR S Par (Sz (height :. width)) $ \(y :. x) ->
            let z = pixelToComplex x y
                t = escapeTime poly maxIter z
                val = fromIntegral t :: Word8
            in
                if t >= maxIter
                then PixelSRGB 0 0 0
                else PixelSRGB val val val

    writeImage filename img


main :: IO ()
main = do
    createDirectoryIfMissing True "output"

    let totalFrames = 240
    let radius = 0.7885
    let angles = [0, (2 * pi / fromIntegral totalFrames) .. (2 * pi)]

    let params = zip [0..] (take totalFrames angles)

    mapM_ (\(idx, angle) -> renderFrameMassiv idx (radius * cis angle)) params

    putStrLn "Done. Run FFmpeg."