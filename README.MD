A very compact function that maps a point in the (x,y) plane onto a spiral matrix:


(Just a total of 12 lines long (compared to ~68 by [fossick](https://www.vijayanant.com/posts/outward-counterclockwise-spiral-matrix-traversal-haskell/)))

spiral (x,y) returns the value this point has in the spiral matrix.
EXAMPLE:

```
Main> mapM_ print [[ spiral x y | x<-[-4..4]] | y<-[-4..4]]
[81,50,51,52,53,54,55,56,57]
[80,49,26,27,28,29,30,31,58]
[79,48,25,10,11,12,13,32,59]
[78,47,24,9 , 2, 3,14,33,60]
[77,46,23,8 , 1, 4,15,34,61]
[76,45,22,7 , 6, 5,16,35,62]
[75,44,21,18,19,20,17,36,63]
[74,43,38,39,40,41,42,37,64]
[73,66,67,68,69,70,71,72,65]
```

## Installation
Simply download the _sm.hs_ and add `import SpiralMatrix`.
