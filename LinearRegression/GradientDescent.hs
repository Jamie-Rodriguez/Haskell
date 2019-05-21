{-# OPTIONS_GHC -Wall #-}
import Data.List -- genericLength



-- (x, y)
data Point = Point {pointX :: Double, pointY :: Double} deriving (Show)
--  Parameters for h_theta(x) = theta_0 + theta_1 * x
data HypothesisParams = HypothesisParams {theta_0 :: Double, theta_1 :: Double} deriving (Show)



-- h_theta(x) = theta_0 + theta_1 * x
linearHypothesis :: HypothesisParams -> Double -> Double
linearHypothesis hypothesis x = t0 + t1 * x
    where
        t0 = theta_0 hypothesis
        t1 = theta_1 hypothesis


-- Loss function = squared error = (y_hat - y)^2
squaredError :: HypothesisParams -> Point -> Double
squaredError hypothesis Point {pointX = x, pointY = y} = (prediction - y)^(2 :: Integer)
    where prediction = linearHypothesis hypothesis x


-- J = 1/n * sum[i=1 -> n](prediction - y_i)^2
meanSquaredError :: HypothesisParams -> [Point] -> Double
meanSquaredError hypothesis points = (1/numPoints) * (sum squaredErrors)
    where
        calcSquaredError = squaredError hypothesis
        squaredErrors = map calcSquaredError points
        numPoints = genericLength points


-- dJ/dtheta_0 = 2/n * sum[i=1 -> n](theta_0 + theta_1 * x_i - y_i)
--             = 2/n * sum[i=1 -> n](prediction - y_i)
dJ_dTheta0 :: HypothesisParams -> [Point] -> Double
dJ_dTheta0 hypothesis points = (2/numPoints) * sum predictionMinusObservations
    where
        numPoints = genericLength points
        calcPrediction = linearHypothesis hypothesis
        f = \p -> 
                let x = pointX p
                    y = pointY p
                in calcPrediction x - y
        predictionMinusObservations = map f points


-- dJ/dtheta_1 = 2/n * sum[i=1 -> n](theta_0 + theta_1 * x_i - y_i) * x_i
--             = 2/n * sum[i=1 -> n]((prediction - y_i) * x_i)
dJ_dTheta1 :: HypothesisParams -> [Point] -> Double
dJ_dTheta1 hypothesis points = (2/numPoints) * sum (map f points)
    where
        numPoints = genericLength points
        calcPrediction = linearHypothesis hypothesis
        f = \p -> 
                let x = pointX p
                    y = pointY p
                in (calcPrediction x - y) * x


-- Compute one iteration of batch gradient descent
-- theta_0’ = theta_0 - learningRate * dJ/dtheta_0
-- theta_1’ = theta_1 - learningRate * dJ/dtheta_1
batchGradientDescent :: Double -> HypothesisParams -> [Point] -> HypothesisParams
batchGradientDescent learningRate hypothesis points = HypothesisParams newTheta0 newTheta1
    where
        theta0 = theta_0 hypothesis
        theta1 = theta_1 hypothesis
        newTheta0 = theta0 - learningRate * dJ_dTheta0 hypothesis points
        newTheta1 = theta1 - learningRate * dJ_dTheta1 hypothesis points


-- Helper for determining when convergence has been achieved
-- Gets the differences between two sets of theta_0 and theta_1
-- returns whichever differences is largest
largestParamsDelta :: HypothesisParams -> HypothesisParams -> Double
largestParamsDelta p1 p2 = maximum [diffTheta0s, diffTheta1s]
    where
        t10 = theta_0 p1
        t11 = theta_1 p1
        t20 = theta_0 p2
        t21 = theta_1 p2
        diffTheta0s = abs (t10 - t20)
        diffTheta1s = abs (t11 - t21)


-- Keep computing gradient descent until the difference between two iterations of
-- the hypothesis parameters are less than the convergence threshold
convergeGradientDescent :: Double -> Double -> HypothesisParams -> [Point] -> HypothesisParams
convergeGradientDescent threshold learningRate params points =
    if largestParamsDelta params newParams < threshold 
        then newParams
    else
        convergeGradientDescent threshold learningRate newParams points  
    where
        newParams = batchGradientDescent learningRate params points



main :: IO ()
main = do
    let convergenceThreshold = 1e-32 -- The precision to converge to
    let learningRate = 0.1
    let startingHypothesis = HypothesisParams 3 2
    let dataPoints = [(Point 1 7), (Point 2 10), (Point 3 13)] -- f(x) = 3x + 4
    let finalParams = convergeGradientDescent convergenceThreshold learningRate startingHypothesis dataPoints
    putStrLn $ "Linear equation parameters:\n\t" ++ show finalParams
    putStrLn $ "Mean squared error:\n\t" ++ show (meanSquaredError finalParams dataPoints)
