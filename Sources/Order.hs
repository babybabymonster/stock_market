module Order where

import Types
import Data.List

-- | quickCheck
-- prop> prop_groupEveryFive
-- prop> prop_findMaxPoint
-- prop> prop_findMinPoint

-- | selectMaxPoints
-- >>> selectMaxPoints [(2,4),(0,100),(200,0),(-10,-300)]
-- [(0,100)]
--
-- | selectMinPoints
-- >>> selectMinPoints [(1,29),(-30,300),(200,0),(-10,-490)]
-- [(-10,-490)]
--
-- | predict
-- >>> predict 2 [(1.62,49.99),(1.54,43.12),(1.32,23.11),(1.35,41.82)]
-- 81.61918239493019
--
-- | calSlopOfLine
-- >>> calSlopeOfLine [(1.47,52.21),(1.50,53.12),(1.52,54.48),(1.55,55.84)]
-- 47.165266231662734
--
-- | calculateB
-- >>> calculateB [(1.62,47.23),(2.14,42.13),(1.82,28.11),(1.95,43.14)]
-- -29.89201019091216
--
-- | staDeviation
-- >>> staDeviation [-9,0,27.9,-19.2,19.2,30]
-- 7.634270320949693
--
-- | sdOfPoints
-- >>> sdOfPoints [(89.0,100),(101,22),(-70,28),(0,-100)]
-- (34.8407089480108,35.91918011313733)
--
-- | correlation
-- >>> correlation [(1.47,52.21),(1.50,53.12),(1.52,54.48),(1.55,55.84)]
-- 0.9999760008749387
--
-- | mean
-- >>> mean [4.5,0,-100,300.9]
-- 51.349999999999994
--
-- | meanOfPoints
-- >>> meanOfPoints [(99.8, 0),(122,9),(-20,1),(-2,-9)]
-- (49.95,0.25)


prop_groupEveryFive :: Eq a => [a] -> Bool
prop_groupEveryFive glist = glist == (foldl (++) [] $ groupEveryFive glist)

prop_findMaxPoint :: (Num a, Eq a, Ord a) => [(a, a)] -> Bool
prop_findMaxPoint [] = (findMaxPoint []) == (0,0)
prop_findMaxPoint flist = (snd $ findMaxPoint flist) == (last $ sort $ map snd flist)

prop_findMinPoint :: (Num a, Eq a, Ord a) => [(a, a)] -> Bool
prop_findMinPoint [] = (findMinPoint []) == (0,0)
prop_findMinPoint fmlist = (snd $ findMinPoint fmlist) == (head $ sort $ map snd fmlist)

-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--

makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders port@(cash, holdings) history = case history of
    [] -> []
    (s,p):sps -> case length p <= 180 of
        True -> []
        -- check the trend of s stock in 30 days, using the minimum price every 5 days
        -- instead of average price every 5 days is to make sure the general trend of
        -- this stock in 30 days is increasing.
        False -> case k30Min > 0 of
            True
                | k60 > 0 -> case k180 > 0 of
                    -- s stock keeps increasing in 180 days, then it is relatively stable,
                    -- it is safe to invest.
                    True -> case
                     -- find a relatively lower price to buy.
                        minPriceIn5days >= currentPrice of
                        True-> myOrder $ 2 * buyQuantity
                        False
                            | quantityHeld > 0 -> case
                                  -- find a relatively higher price to sell
                                   maxPriceIn5days <= currentPrice of
                                   True -> myOrder sellQuantity
                                   False -> myOrder buyQuantity
                            | otherwise -> skipThisStock
                    -- s stock is increasing in 30 and 60 days, but not in 180 days, this stock
                    -- starts to climb back.
                    False -> case minPriceIn5days >= currentPrice of
                        True -> myOrder buyQuantity
                        False
                            | maxPriceIn5days < currentPrice -> myOrder sellQuantity
                            | otherwise -> skipThisStock
                -- k60 <= 0, the stock starts to climb back;
                -- check the slope of recent 10 days to check whether it is safe to buy.
                | otherwise -> case k10 > 0 of
                        True
                            | 1.1 * minPriceIn5days >= currentPrice -> myOrder buyQuantity
                            | otherwise -> skipThisStock
                        False -> skipThisStock
            -- k30Min < 0, then use maximum price every 5 days,
            -- if this regression line is decreasing, then this stock must decrease.
            False -> case k30Max > 0 of
                        True
                            | k10 > 0 -> myOrder buyQuantity
                            | otherwise -> case quantityHeld > 0 of
                                True -> myOrder (-quantityHeld)
                                False -> skipThisStock
                        -- k30Max < 0, the prices in 30 days is decreasing,
                        -- check if it is decreasing in 180 days, if so, we know the price
                        -- are highly likely to continue decrease in the future,
                        -- thus use short sell.
                        False -> case k180 < 0 of
                                -- use short sell
                            True
                                | 0.9 * minPriceIn5days >= currentPrice -> myOrder sellQuantity
                                | otherwise -> case quantityHeld < 0 of
                                            True -> myOrder quantityHeld
                                            False -> skipThisStock
                                -- if we still have this stock, then find a
                                -- relatively higher price to sell.
                                -- if not, then skip it.
                            False -> case quantityHeld >= 0 of
                                True
                                    | 0.9 * predictPrice <= currentPrice -> myOrder sellQuantity
                                    | otherwise -> skipThisStock
                                False -> skipThisStock

            where quantityHeld
                       | s `notElem` map fst holdings = 0
                       | otherwise = fromIntegral (snd $ head $ filter (\z -> fst z == s) holdings)
                  currentPrice = head $ p
                  -- made up a list of points: (x = days, y = corresponding price).
                  listOfPoints = zip [1..] (reverse p)
                  -- choose n points.
                  selectNDays n = take n $ reverse listOfPoints
                  -- calculate the corresponding price with respect to
                  -- current day using simple linear regression.
                  predictPrice = predict (fromIntegral $ length $ p) selectedMaxPoints30
                  selectedMinPoints30 = selectMinPoints $ selectNDays 30
                  selectedMaxPoints30 = selectMaxPoints $ selectNDays 30
                  -- the slope of the selected 6 points in 30 days, use only the minimum price every 5 points.
                  k30Min = calSlopeOfLine selectedMinPoints30
                  -- the slope of the selected 6 points in 30 days, use only the maximum price every 5 points.
                  k30Max = calSlopeOfLine selectedMaxPoints30
                  -- the slope of 60 points.
                  k60 = calSlopeOfLine $ selectNDays 60
                  -- the slope of 180 points.
                  k180 = calSlopeOfLine $ selectNDays 180
                  -- the slope of recent 10 points.
                  k10 = calSlopeOfLine $ selectNDays 10
                  minPriceIn5days = minimum $ take 5 p
                  maxPriceIn5days = maximum $ take 5 p
                  buyQuantity = floor (0.02 * cash / currentPrice)
                  sellQuantity = (-floor (0.5 * (0.02 * cash / currentPrice)))
                  -- update cash.
                  updateCash q = cash + (fromInteger q) * currentPrice
                  -- makeOrders.
                  myOrder qty = [Order s qty] ++ makeOrders (updateCash qty, updateHoldings (s, qty) holdings) sps
                  skipThisStock = [] ++ makeOrders port sps


-- Helper Functions

-- update holdings in portfolio.
updateHoldings :: Holding -> Holdings -> Holdings
updateHoldings (stock, quantity) hold = case hold of
    [] -> [(stock, quantity)]
    h : hs
        | fst h == stock && snd h == -quantity -> hs
        | fst h == stock -> (stock, abs $ quantity + snd h) : hs
        | otherwise -> h : updateHoldings (stock, quantity) hs

-- select the point that has the largest y in every 5 points.
selectMaxPoints :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
selectMaxPoints opts = map findMaxPoint $ groupEveryFive opts

selectMinPoints :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
selectMinPoints opts = map findMinPoint $ groupEveryFive opts

-- group every five points in a list of 30 points.
groupEveryFive :: [a] -> [[a]]
groupEveryFive list = case list of
    [] -> []
    _ -> take 5 list : (groupEveryFive $ drop 5 list)

-- find out the point that has the largest value of
-- y(the second value in a tuple) in the list.
findMaxPoint :: (Num a, Ord a) => [(a, a)] -> (a, a)
findMaxPoint [] = (0,0)
findMaxPoint l = head $ filter (\z -> snd z == (maximum $ map snd l)) l

-- find out the point whose snd is the smallest in the list
findMinPoint :: (Num a, Ord a) => [(a, a)] -> (a, a)
findMinPoint [] = (0,0)
findMinPoint l = head $ filter (\z -> snd z == (minimum $ map snd l)) l

-- draw a regression line using the points given and find out the
-- corresponding y with respect to the given x.
predict :: (Fractional a, Floating a) => a -> [(a, a)] -> a
predict day points = (calSlopeOfLine points) * day + (calculateB points)

-- Helper functions that will be used when drawing a regression line:
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
-- although the input here will not be [] in this case, we just arrange a random
-- coordinate here in order to use quickCheck to test it without error.
meanOfPoints :: Fractional a => [(a, a)] -> (a, a)
meanOfPoints [] = (0, 0)
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



