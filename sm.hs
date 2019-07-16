module SpiralMatrix (spiral) where

spiral :: Int -> Int -> Int
spiral x y
    | x==0 && y==0 = 1
    | abs(x) >= abs(y) = do
        let n = (2*( abs(x)+1 )-1)^2
        if( x<0 )
        then( n-  abs(x)-y )
        else( n-5*abs(x)+y)
    | otherwise = do
        let n = (2*(abs(y))-1)^2
        if( y<0 )
        then(n+abs(y)+x)
        else(n+5*abs(y)+x)
