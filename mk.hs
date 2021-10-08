import Data.List

-- imperfect search for min mariokart win score

s = [15, 12] ++ reverse [1 .. 10]

data Score = Score Int Int Int Int deriving (Show)

instance Eq Score where
    (Score a b c d) == (Score e f g h) = 
        (a+b+c+d) == (e+f+g+h)

instance Ord Score where
    compare (Score a b c d) (Score e f g h) =
        compare (a+b+c+d) (e+f+g+h)


points :: Score -> Int
points (Score a b c d) = a+b+c+d

avg = 4*(sum s) `quot` 12

divergence :: [Score] -> Int
divergence l = sum [(abs ((points x)-avg)) | x <- l]

inOrder = [(scoreFromList [x,x,x,x])| x <- s]

findNth :: Int -> [Score] -> Score
findNth n l = (sort l)!!n

findWinner :: [Score] -> Score
findWinner l = maximum l 

findSecond :: [Score] -> Score
findSecond l = maximum (delete (findWinner l) l)

findLast :: [Score] -> Score
findLast l = minimum l

scoreFromList :: [Int] -> Score
scoreFromList l = (Score (l!!0) (l!!1) (l!!2) (l!!3))

scoreToList :: Score -> [Int]
scoreToList (Score a b c d) = [a,b,c,d]

swap :: Score -> Score -> [Score]
swap w l = 
    [(scoreFromList ((delete (maximum lw) lw)++[minimum ll])),
        (scoreFromList ((delete (minimum ll) ll)++[maximum lw]))]
    where lw = (scoreToList w)
          ll = (scoreToList l)

swapWL :: [Score] -> [Score]
swapWL l = (delete winner (delete loser l)) 
    ++ (swap winner loser)
    where winner = findWinner l
          loser  = findLast l

marginW :: [Score] -> Int
marginW l = (points (findWinner l)) - (points (findSecond l))

minimizeWin :: [Score] -> [Score]
minimizeWin l
    | divergence l <= divergence sw = l
    | otherwise  = minimizeWin sw
    where ogW = points (findWinner l)
          sw  = swapWL l
          swW = points (findWinner sw)
