-- Axel Froborg (ax3051fr-s) and Emil Lantz (em0377la-s)

module StringAlignment where
    
type AlignmentType = (String,String)

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments = printOptAlignments optAlignments

outputNewOptAlignments :: String -> String -> IO ()
outputNewOptAlignments = printOptAlignments newOptAlignments

printOptAlignments :: (String -> String -> [AlignmentType]) -> String -> String -> IO ()
printOptAlignments f xs ys = do
    let optimalAlignments = f xs ys
    putStrLn $ "There are " ++ show ( length optimalAlignments ) ++ " optimal alignments:\n"
    putStr $ formatTupleList optimalAlignments
    putStrLn $ "There were " ++ show ( length optimalAlignments ) ++ " optimal alignments!"
    where
        formatTuple :: AlignmentType -> String
        formatTuple (x, y) = x ++ "\n" ++ y ++ "\n\n"

        formatTupleList :: [AlignmentType] -> String
        formatTupleList [] = ""
        formatTupleList (x:xs) = formatTuple x ++ formatTupleList xs
    

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

score :: (Char, Char) -> Int
score (_, '-') = scoreSpace
score ('-', _) = scoreSpace
score (x, y)
    | x == y = scoreMatch
    | otherwise = scoreMismatch

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] ys = length ys * scoreSpace
similarityScore xs [] = length xs * scoreSpace
similarityScore (x:xs) (y:ys) = 
    maximum [
        similarityScore xs ys + score (x, y), 
        similarityScore xs (y:ys) + score (x, '-'), 
        similarityScore (x:xs) ys + score ('-', y)
    ]

newSimilarityScore :: String -> String -> Int
newSimilarityScore xs ys = simLen (length xs) (length ys)
  where
    simLen i j = simTable !! i !! j
    simTable = [[ simEntry i j | j <- [0..] ] | i <- [0..] ]
       
    simEntry :: Int -> Int -> Int
    simEntry 0 0 = 0
    simEntry 0 j = j * scoreSpace
    simEntry i 0 = i * scoreSpace
    simEntry i j = 
        maximum [
            simLen (i - 1) (j - 1) + score (x, y), 
            simLen i (j - 1) + score ('-', y), 
            simLen (i - 1) j + score (x, '-')
        ]
      where
         x = xs !! (i - 1)
         y = ys !! (j - 1)

-- attachHeads prepends h1 to xs and h2 to ys for every pair of lists (xs, ys) in aList
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a], [a])] -> [([a], [a])]
attachTails t1 t2 aList = [(xs ++ [t1], ys ++ [t2]) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == maximum (map valueFcn xs)]

spaces :: Int -> String
spaces a = replicate a '-'

optScore :: AlignmentType -> Int
optScore ([], []) = 0
optScore (x:xs, y:ys) = score (x, y) + optScore (xs, ys)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments [] ys = [(spaces (length ys), ys)]
optAlignments xs [] = [(xs, spaces (length xs))]
optAlignments (x:xs) (y:ys) = maximaBy optScore $ 
    concat [
        attachHeads x y $ optAlignments xs ys,
        attachHeads x '-' $ optAlignments xs (y:ys),
        attachHeads '-' y $ optAlignments (x:xs) ys
    ]
        

newOptAlignments :: String -> String -> [AlignmentType]
newOptAlignments xs ys = maximaBy optScore (snd $ optLen (length xs) (length ys))
  where
    optLen i j = optTable !! i !! j
    optTable = [[ optEntry i j | j <- [0..] ] | i <- [0..] ]
       
    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [([], [])])
    optEntry i 0 = (i * scoreSpace, [(take i xs, spaces i)])
    optEntry 0 j = (j * scoreSpace, [(spaces j, take j ys)])
    optEntry i j = (fst $ head findAligns, concatMap snd findAligns)
      where
        x = xs !! (i - 1)
        y = ys !! (j - 1)
        findAligns = maximaBy fst [(partScore1 , align1), (partScore2 , align2), (partScore3, align3)]
        partScore1 = partScore (i - 1) (j - 1) x y
        partScore2 = partScore i (j - 1) x '-'
        partScore3 = partScore (i - 1) j '-' y

        align1 = align (i - 1) (j - 1) x y
        align2 = align i (j - 1) '-' y
        align3 = align (i - 1) j x '-'
    
        partScore i j x y = fst (optLen i j) + score (x, y)
        align i j x y = attachTails x y $ snd $ optLen i j