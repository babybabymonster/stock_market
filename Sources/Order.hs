module Order where

import Types

-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--
makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders (cash, _) history = 
    case history of
        []   -> []
        (s,p):_ -> [Order s (floor (cash / 10 * head p))] 