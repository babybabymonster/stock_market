module Order where

import Types

-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--

makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders (cash, holdings) history = case history of
    [] -> []
    (s,p):sps -> case length p <= 180 of
        True -> []
        False -> case k30Min > 0 of
            True
                | k60 > 0 -> case k180 > 0 of
                    True -> case
                     -- find a relative lower price to buy.
                        minPriceIn5days >= currentPrice of
                        True-> myOrder $ 2 * buyQuantity
                        False
                            | quantityHeld > 0 -> case
                                  -- find a relative higher price to sell
                                   maxPriceIn5days <= currentPrice of
                                   True -> myOrder sellQuantity
                                   False -> myOrder buyQuantity
                            | otherwise -> []
                    False -> case minPriceIn5days >= currentPrice of
                        True -> myOrder buyQuantity
                        False
                            | maxPriceIn5days < currentPrice -> myOrder sellQuantity
                            | otherwise -> []
                | otherwise -> case k10 > 0 of
                        True
                            | 1.1 * minPriceIn5days >= currentPrice -> myOrder buyQuantity
                            | otherwise -> []
                        False -> []
            False -> case k30Max > 0 of
                        True
                            | k10 > 0 -> myOrder buyQuantity
                            | otherwise -> case quantityHeld > 0 of
                                True -> myOrder quantityHeld
                                False -> []
                        False -> case k180 < 0 of
                                   -- short sell
                            True
                                | 0.9 * minPriceIn5days >= currentPrice -> myOrder sellQuantity
                                | otherwise -> case quantityHeld < 0 of
                                            True -> myOrder quantityHeld
                                            False -> []
                            False -> case quantityHeld >= 0 of
                                True
                                    | 0.9 * predictPrice <= currentPrice -> myOrder sellQuantity
                                    | otherwise -> []
                                False -> []

            where quantityHeld
                       | s `notElem` map fst holdings = 0
                       | otherwise = fromIntegral (snd $ head $ filter (\z -> fst z == s) holdings)
                  currentPrice = head $ p
                  -- made up a list of points: (days, corresponding price).
                  listOfPoints = zip [1..] (reverse p)
                  -- choose n points.
                  selectNDays n = take n $ reverse listOfPoints
                  -- let x = days, to calculate the corresponding price using simple linear regression;
                  -- then compare the value we got with actual price;
                  -- the predict function is to draw a regression line using the points given. ???
                  predictPrice = predict (fromIntegral $ length $ p) selectedMaxPoints30
                  selectedMinPoints30 = selectMinPoints $ selectNDays 30
                  selectedMaxPoints30 = selectMaxPoints $ selectNDays 30
                  -- the slope of the selected 6 points in 30 days, use only the minimum price every 5 points.
                  k30Min = calSlopeOfLine selectedMinPoints30
                  -- the slope of the selected 6 points in 30 days, use only the maximum price every 5 points.
                  k30Max = calSlopeOfLine selectedMaxPoints30
                  -- the slope of 60 points.
                  k60 = calSlopeOfLine $ selectNDays 60
                  -- the slope of 180 points
                  k180 = calSlopeOfLine $ selectNDays 180
                  -- the slope of recent 10 points
                  k10 = calSlopeOfLine $ selectNDays 10
                  minPriceIn5days = minimum $ take 5 p
                  maxPriceIn5days = maximum $ take 5 p
                  buyQuantity = floor (0.02 * cash / currentPrice)
                  sellQuantity = (-floor (0.5 * (0.02 * cash / currentPrice)))
                  -- update cash
                  updateCash q = cash + (fromInteger q) * currentPrice -- fromInteger???
                  -- makeOrders
                  myOrder qty = [Order s qty] ++ makeOrders (updateCash qty, updateHoldings (s, qty) holdings) sps


-- Helper Functions

-- update holdings in portfolio.
updateHoldings :: Holding -> Holdings -> Holdings
updateHoldings (stock, quantity) hold = case hold of
    [] -> [(stock, quantity)]
    h : hs
        | fst h == stock && snd h == -quantity -> hs
        | fst h == stock -> (stock, abs $ quantity + snd h) : hs
        | otherwise -> h : updateHoldings (stock, quantity) hs

-- select a list of points which is the largest one of every five points in 30 days.
selectMaxPoints :: Ord a => [(a, a)] -> [(a, a)]
selectMaxPoints opts = map findMaxPoint $ groupEveryFive opts

selectMinPoints :: Ord a => [(a, a)] -> [(a, a)]
selectMinPoints opts = map findMinPoint $ groupEveryFive opts

-- group every five points in a list of 30 points.
groupEveryFive :: [a] -> [[a]]
groupEveryFive list = case list of
    [] -> []
    _ -> take 5 list : (groupEveryFive $ drop 5 list)

-- find out the point that has the largest value of
-- y(the second value in a tuple) in the list.
findMaxPoint :: Ord a => [(a, a)] -> (a, a)
findMaxPoint l = head $ filter (\z -> snd z == (maximum $ map snd l)) l

-- find out the point whose snd is the smallest in the list
findMinPoint :: Ord a => [(a, a)] -> (a, a)
findMinPoint l = head $ filter (\z -> snd z == (minimum $ map snd l)) l

-- predict x, x is the current day.
predict :: (Fractional a, Floating a) => a -> [(a, a)] -> a
predict day points = (calSlopeOfLine points) * day + (calculateB points)

-- calculate the slope of the regression line.
calSlopeOfLine :: (Fractional a, Floating a) => [(a, a)] -> a
calSlopeOfLine pts = (correlation pts) * dy / dx
    where (dx, dy) = sdOfPoints pts

-- calculate the value of b in equation y = kx + b.
calculateB :: (Fractional a, Floating a) => [(a, a)] -> a
calculateB ptss = meany - (calSlopeOfLine ptss) * meanx
    where (meanx, meany) = meanOfPoints ptss

-- calculate the mean of a given list.
mean :: Fractional a => [a] -> a
mean xs = (sum xs) / fromIntegral (length xs)

-- calculate the mean of all x and all y.
meanOfPoints :: Fractional a => [(a, a)] -> (a, a)
meanOfPoints ys = (mean (fst (unzip ys)), mean (snd (unzip ys)))

-- calculate the standard deviation of a list.
staDeviation :: Floating a => [a] -> a
staDeviation x = sqrt  (sum (map (\z -> (z - mean x)**2) x)) / fromIntegral (length x)

-- calculate the standard deviation of all x and all y.
sdOfPoints :: Floating a => [(a, a)] -> (a, a)
sdOfPoints lst = (staDeviation (fst (unzip lst)), staDeviation (snd (unzip lst)))

-- find out the relation between these x and y using the deviation score formula.
correlation :: Floating a => [(a, a)] -> a
correlation list = xy / sqrt (xx * yy)
        where xy = sum (map (\z -> fst z * snd z) list)
              xx = sum (map (\z -> fst z * fst z) list)
              yy = sum (map (\z -> snd z * snd z) list)
