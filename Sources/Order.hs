module Order where

import Types

-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--

makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders (cash, holdings) history =
    case history of
          [] -> []
          (s,p):sps -> case length p <= 101 of
                True -> []
                False -> case quantityHeld of
                      0 -> case lineIsIncreasing selectedMinPoints of -- the quantity of s is 0, then check if it is worth buying.
                            False -> []
                                    -- buy this stock
                            True -> [Order s buyQuantity] ++
                                     makeOrders (cash - 0.02 * cash, updateHoldings (s, buyQuantity) holdings) sps
                      _ -> case lineIsIncreasing selectedMaxPoints of -- the quantity of s is > 0, then check if it is suitable to sell.
                            True -> case sellPointIncrease of
                                  True -> [Order s (-quantityHeld)] ++
                                            makeOrders (cash + (fromInteger quantityHeld)*(head p), updateHoldings (s, -quantityHeld) holdings) sps
                                  False -> []
                            False -> case sellPointDecrease of
                                  True -> [Order s (-quantityHeld)] ++
                                            makeOrders (cash + (fromInteger quantityHeld)*(head p), updateHoldings (s, -quantityHeld) holdings) sps
                                  False -> []
                      where
                                        --shortSellAmount = floor(0.01 * cash / head p)
                      quantityHeld
                        | s `notElem` map fst holdings = 0
                        | otherwise = fromIntegral (snd $ head $ filter (\z -> fst z == s) holdings)
                      historyPricesOfS = snd $ head $ filter(\a -> fst a == s) history
                                        -- [(1,p1),(2,p2),...,(n-1,pn-1),(n,pn)]
                      listOfPoints = zip [1..4502] (reverse historyPricesOfS)
                                        -- [(n-1,pn-1),...,(n-29, pn-29)]
                      selectThirtyDays = tail $ take 101 (reverse listOfPoints)
                      selectedMaxPoints = selectMaxPoints selectThirtyDays
                      selectedMinPoints = selectMinPoints selectThirtyDays
                                        -- pn
                      currentPrice = head $ historyPricesOfS
                      buyQuantity = floor (0.02 * cash / currentPrice)
                      sellPointIncrease = predictPrice <= currentPrice
                      sellPointDecrease = 0.9 * predictPrice >= currentPrice
                      predictPrice = predict (fromIntegral $ length $ historyPricesOfS) selectThirtyDays
                      currentWealth = calculateWealth (cash, holdings) history


calculateWealth :: Portfolio -> [StockHistory] -> Cash
calculateWealth (cash, stock) his = cash + sum (map currentValue stock)
    where currentValue :: (Stock, Quantity) -> Cash
          currentValue (sname, q) = getStockPrice sname his * fromIntegral q

getStockPrice :: Stock -> [StockHistory] -> Price
getStockPrice stock shis = head $ snd $ head $ filter (\z -> fst z == stock) shis
-- predict current price smaller than actual current price
--overvalued :: [StockHistory] -> PLine -> Bool
--overvalued sh Line ck cb = predictPrice < currentPrice
  --  where
    --    predictPrice = predict sh (Line ck cb)
      --  currentPrice = head $ snd $ head $ filter(\a -> fst a == s) sh

-- isLostingMoney ::
-- isLostingMoney

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
lineIsIncreasing :: (Ord a, Floating a, Fractional a) => [(a, a)] -> Bool
lineIsIncreasing pps
    | (calSlopeOfLine pps) > 0   = True
    | (calSlopeOfLine pps) <= 0  = False
    | otherwise = error "Won't happen."

-- predict x, x is the current day.
predict :: (Fractional a, Floating a) => a -> [(a, a)] -> a
predict day points = (calSlopeOfLine points) * day + (calculateB points)

calSlopeOfLine :: (Fractional a, Floating a) => [(a, a)] -> a
calSlopeOfLine pts = (relation pts) * dy / dx
    where (dx, dy) = sdOfPoints pts

calculateB :: (Fractional a, Floating a) => [(a, a)] -> a
calculateB ptss = meany - (calSlopeOfLine ptss) * meanx
    where (meanx, meany) = meanOfPoints ptss

--simpleRelation :: [Point] -> PLine
--simpleRelation pts = Line k b
  --  where   k = (relation pts) * dy / dx
    --        b = meany - k * meanx
      --      (dx, dy) = sdOfPoints pts
        --    (meanx, meany) = meanOfPoints pts

mean :: Fractional a => [a] -> a
mean xs = (sum xs) / fromIntegral (length xs)

meanOfPoints :: Fractional a => [(a, a)] -> (a, a)
meanOfPoints ys = (mean (fst (unzip ys)), mean (snd (unzip ys)))

staDeviation :: Floating a => [a] -> a
staDeviation x = sqrt  (sum (map (\z -> (z - mean x)**2) x)) / fromIntegral (length x)

sdOfPoints :: Floating a => [(a, a)] -> (a, a)
sdOfPoints lst = (staDeviation (fst (unzip lst)), staDeviation (snd (unzip lst)))

relation :: Floating a => [(a, a)] -> a
relation list = xy / sqrt (xx * yy)
        where xy = sum (map (\z -> fst z * snd z) list)
              xx = sum (map (\z -> fst z * fst z) list)
              yy = sum (map (\z -> snd z * snd z) list)


--growthrateIsNeg :: StockHistory -> Bool
--growthrateIsNeg (s, p) = case p of
-- -> case growthrateisneg of
                 --       True -> original
                   --     False -> buy