module Main where

import System.IO ( hPutStrLn, withFile, IOMode(WriteMode) )
import Text.Printf (printf, hPrintf)
import Control.Monad (forM_)
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

colorToString :: Int -> String
colorToString steps
    | steps >= maxIter = "0 0 0"
    | otherwise        = unwords (show <$> take 3 (iterate (const  steps) 0))

renderFramePPM :: Int -> Complex Double -> IO ()
renderFramePPM frameIndex c = do
    let filename = printf "output/frame_%04d.ppm" frameIndex
    putStrLn $ "Rendering " ++ filename ++ "..."

    withFile filename WriteMode $ \h -> do
        hPutStrLn h "P3"
        hPrintf h "%d %d\n" width height
        hPutStrLn h "255"

        let poly = Poly [c, 0, 1] :: Poly(Complex Double)

        forM_ [0 .. height - 1] $ \y -> do
            forM_ [0 .. width - 1] $ \x -> do
                let z = pixelToComplex x y
                let t = escapeTime poly maxIter z
                hPutStrLn h (colorToString t)

main :: IO ()
main = do
    createDirectoryIfMissing True "output"

    let totalFrames = 300
    let radius = 0.7885
    let angles = [0, (2 * pi / fromIntegral totalFrames) .. (2 * pi)]

    let params = zip [0..] (take totalFrames angles)

    mapM_ (\(idx, angle) -> renderFramePPM idx (radius * cis angle)) params

    putStrLn "Done. Run FFmpeg."
    putStrLn "ffmpeg -framerate 30 -i output/frame_%04d.ppm -c:v libx264 -pix_fmt yuv420p julia_movie.mp4"