module Order where

import Types

-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--

makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders (cash, holdings) history = case history of
    [] -> []
    (s,p):sps -> case length p <= 31 of
        True -> []
        False -- deal with shortSell situation fist to lower the risk of shortSelling.
            | quantityHeld < 0 -> [Order s (-quantityHeld)] ++ makeOrders (cash - (fromInteger quantityHeld) * currentPrice, updateHoldings (s, (-quantityHeld)) holdings) sps
              -- if correlation of selected points > 0, that means the line is increasing;
            | otherwise -> case (correlation selectedMinPoints30) >= 0.1 && (correlation selectedMinPoints100) > 0 of
                True ->  case (calSlopeOfLine average180) > 0 of
                        False
                            | underValue -> [Order s buyQuantity] ++ makeOrders (cash - (fromInteger buyQuantity) * currentPrice, updateHoldings (s, buyQuantity) holdings) sps
                            | overValue -> [Order s (-sellQuantity)] ++ makeOrders (cash - (fromInteger (-sellQuantity))*currentPrice, updateHoldings (s, (-sellQuantity)) holdings) sps
                            | otherwise -> []
                        True
                            | currentPrice >= 1.2 * maxPriceIn300Days -> [Order s (-quantityHeld)] ++ makeOrders (cash + (fromInteger quantityHeld) * currentPrice, updateHoldings (s, (-quantityHeld)) holdings) sps
                            | otherwise -> [Order s (2 * buyQuantity)] ++ makeOrders (cash - (fromInteger (2 * buyQuantity)) * currentPrice, updateHoldings (s, (2 * buyQuantity)) holdings) sps
                    --
                False -> case calSlopeOfLine average180 > 0 of
                        False -> case calSlopeOfLine selectedMaxPoints30 > 0 of
                            True -> case sellPointIncrease of
                                  True -> [Order s (-quantityHeld)] ++
                                          makeOrders (cash + (fromInteger quantityHeld)*(head p), updateHoldings (s, -quantityHeld) holdings) sps
                                  False -> []
                            False -> case sellPointDecrease of
                                  True -> [Order s (-quantityHeld)] ++
                                          makeOrders (cash + (fromInteger quantityHeld)*(head p), updateHoldings (s, -quantityHeld) holdings) sps
                                  False -> []
                        True -> [Order s (-shortSellQ)] ++ makeOrders (cash + ((fromInteger (-shortSellQ)) * currentPrice), updateHoldings (s, (-shortSellQ)) holdings) sps

            where
                                        --shortSellAmount = floor(0.01 * cash / head p)
            currentPrice = head $ historyPrices
            historyPrices = snd $ head $ filter(\a -> fst a == s) history
            selectedMinPoints30 = selectMinPoints $ selectNDays 30
            selectedMinPoints100 = selectMinPoints $ selectNDays 100
            average180 = map meanOfPoints $ groupEveryFive $ selectNDays 30
            underValue = 0.7 * predictPrice >= currentPrice
            overValue = 1.1 * predictPrice <= currentPrice
            maxPriceIn300Days = maximum $ snd $ unzip $ take 300 (reverse listOfPoints)
            quantityHeld
                | s `notElem` map fst holdings = 0
                | otherwise = fromIntegral (snd $ head $ filter (\z -> fst z == s) holdings)
                                        -- [(1,p1),(2,p2),...,(n-1,pn-1),(n,pn)]
            listOfPoints = zip [1..4502] (reverse historyPrices)
                                        -- [(n-1,pn-1),...,(n-29, pn-29)]
            selectNDays n = tail $ take (n+1) (reverse listOfPoints)
            -- select30Days = tail $ take 31 (reverse listOfPoints)
            selectedMaxPoints30 = selectMaxPoints $ selectNDays 30

                                        -- pn
            buyQuantity = floor (0.02 * cash / currentPrice)
            sellQuantity = floor (0.5 * (0.02 * cash / currentPrice))
            sellPointIncrease = predictPrice <= currentPrice
            sellPointDecrease = 0.9 * predictPrice >= currentPrice
            predictPrice = predict (fromIntegral $ length $ historyPrices) (selectNDays 30)
            shortSellQ = floor(0.01 * cash / head p)

                      -- currentWealth = calculateWealth (cash, holdings) history


calculateWealth :: Portfolio -> [StockHistory] -> Cash
calculateWealth (cash, stock) his = cash + sum (map currentValue stock)
    where currentValue :: (Stock, Quantity) -> Cash
          currentValue (sname, q) = getStockPrice sname his * fromIntegral q

getStockPrice :: Stock -> [StockHistory] -> Price
getStockPrice stock shis = head $ snd $ head $ filter (\z -> fst z == stock) shis

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

-- find out the point whose snd is the largest in the list
findMaxPoint :: Ord a => [(a, a)] -> (a, a)
findMaxPoint l = head $ filter (\z -> snd z == (maximum $ map snd l)) l

-- find out the point whose snd is the smallest in the list
findMinPoint :: Ord a => [(a, a)] -> (a, a)
findMinPoint l = head $ filter (\z -> snd z == (minimum $ map snd l)) l

-- check if k > 0, use all 30 points
--lineIsIncreasing :: (Ord a, Floating a, Fractional a) => [(a, a)] -> Bool
--lineIsIncreasing pps
  --  | (calSlopeOfLine pps) > 0   = True
   -- =| (calSlopeOfLine pps) <= 0  = False
 -- =| otherwise = error "Won't happen."

-- predict x, x is the current day.
predict :: (Fractional a, Floating a) => a -> [(a, a)] -> a
predict day points = (calSlopeOfLine points) * day + (calculateB points)

calSlopeOfLine :: (Fractional a, Floating a) => [(a, a)] -> a
calSlopeOfLine pts = (correlation pts) * dy / dx
    where (dx, dy) = sdOfPoints pts

calculateB :: (Fractional a, Floating a) => [(a, a)] -> a
calculateB ptss = meany - (calSlopeOfLine ptss) * meanx
    where (meanx, meany) = meanOfPoints ptss

mean :: Fractional a => [a] -> a
mean xs = (sum xs) / fromIntegral (length xs)

meanOfPoints :: Fractional a => [(a, a)] -> (a, a)
meanOfPoints ys = (mean (fst (unzip ys)), mean (snd (unzip ys)))

staDeviation :: Floating a => [a] -> a
staDeviation x = sqrt  (sum (map (\z -> (z - mean x)**2) x)) / fromIntegral (length x)

sdOfPoints :: Floating a => [(a, a)] -> (a, a)
sdOfPoints lst = (staDeviation (fst (unzip lst)), staDeviation (snd (unzip lst)))

correlation :: Floating a => [(a, a)] -> a
correlation list = xy / sqrt (xx * yy)
        where xy = sum (map (\z -> fst z * snd z) list)
              xx = sum (map (\z -> fst z * fst z) list)
              yy = sum (map (\z -> snd z * snd z) list)
