module Order where

import Types

makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders (cash, _) history = 
    case history of
        []   -> []
        (s,p):_ -> [Order s (floor (cash / head p)) ] 