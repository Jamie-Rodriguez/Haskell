#!/usr/bin/env stack

import System.Random -- mkStdGen, randoms, randomRs
import Data.Random.Normal(mkNormals')

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)



data LineEq = LineEq { theta_0 :: Double, theta_1 :: Double } deriving (Show)
data NoiseProps = NoiseProps { mu :: Double, sigma :: Double } deriving (Show)



-- Create infinite list of random values to be used as source of noise
generateNoise :: (Double, Double) -> Int -> [Double]
generateNoise (mean, sigma) seed = randNums
    where randNums = mkNormals' (mean :: Double, sigma) seed


-- Create infinite list of points from a linear equation
createCleanSignal :: LineEq -> [(Double, Double)]
createCleanSignal LineEq { theta_0 = t0, theta_1 = t1 }
                  = [ (x, line x) | x <- [0..] ]
    where line = \x -> t0 + t1 * x


-- Create an infinite list of points of a linear equation with noise added
createNoisySignal :: LineEq -> NoiseProps -> Int -> [(Double, Double)]
createNoisySignal LineEq { theta_0 = t0, theta_1 = t1 }
                  NoiseProps { mu = m, sigma = s }
                  seed
                  = [ (i, noisySignal i) | i <- [0..] ]
    where
        line = \x -> t0 + t1 * x
        noiseList = mkNormals' (m, s) seed
        getNoise = \x -> noiseList !! x
        noisySignal = \x -> line x + getNoise (round x :: Int)


-- Add 'n' number of outliers to signal
addOutliers :: NoiseProps -> Int -> Int -> [(Double, Double)] -> [(Double, Double)]
addOutliers noise n seed datapoints = map applyOutliers indexedDatapoints
    where
        -- Create seeds for the randomly selected indexes to be chosen as
        -- outliers and for the (random) values they will be transformed into
        seeds = randoms $ mkStdGen seed :: [Int]
        indexesSeed = seeds !! 0
        valuesSeed = seeds !! 1

        -- Create infinite list of (random) possible indexes to select as outliers
        indexes = randomRs (0, length datapoints) $ mkStdGen indexesSeed :: [Int]
        -- Create infinite list of (random) outlier values
        -- Still want the outliers to use a gaussian (normal) distribution
        -- instead of uniform
        values = mkNormals' (mu noise, sigma noise) valuesSeed :: [Double]

        -- Get a list of 'n' number of outliers in the form (index, value) pairs
        outliers = take n $ zip indexes values
        -- Index the original datapoints into the form (index, (x, y))
        indexedDatapoints = zip [0..] datapoints

        -- TODO: Move the below to separate functions

        -- Retrieves the outliers from a list of (index, value) pairs
        getOutliersForIndex = \i o -> filter (\(x, z) -> x == i) o
        -- Go through list of datapoints and check if there is an outlier for
        -- that index then add outlier value
        applyOutliers = \(i, (x, y)) ->
            let os = getOutliersForIndex i outliers
            in
                if not $ null os then
                    let (_, z) = head os
                    in (x, y + z)
                else
                    (x, y)


printData :: [(Double, Double)] -> IO ()
printData xs = mapM_ ( \(x, y) -> putStrLn $ (show x ++ ", " ++ show y) ) xs



-- TODO: Replace lists with vectors
main :: IO ()
main = do
    putStrLn "Starting..."
    let seed = 615466154656
    let cleanSignal = LineEq 4 3
    let noiseProps = NoiseProps 0 8
    let outliers = NoiseProps 4 60 -- Increase the mean because in general we want more positive values
    let numOutliers = 7

    -- To make the data in this particular case replicate a real-life sensor output unsigned int values
    let makeDataPositive = \(x, y) -> if y > 0 then (x, y) else (x, 0)

    let noisySignal = createNoisySignal cleanSignal noiseProps seed

    let cleanSamples = take 50 $ createCleanSignal cleanSignal
    let noisySamples = take 50 noisySignal
    let noisySamplesWithOutliers = addOutliers outliers numOutliers seed noisySamples

    putStrLn "Begin plotting..."

    toFile def "mychart.svg" $ do
    layout_title .= "Noisy Data Simulation"
    setColors [opaque green, opaque blue, opaque red]
    plot (line "Original Signal" [cleanSamples]) -- [[(Double, Double)]]
    plot (points "Noisy Signal" $ map makeDataPositive noisySamples)
    plot (points "Noisy Signal with Outliers" $ map makeDataPositive noisySamplesWithOutliers)
