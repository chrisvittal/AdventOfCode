module Advent01 where

import Data.List

data Inst = L Int | R Int
         deriving (Eq, Show)

getS :: Inst -> Int
getS (R x) = x
getS (L x) = x

data Face = Nort | Sout | East | West
          deriving (Eq,Show)


type Pos = (Face,(Int,Int))

turn :: Inst -> Pos -> Pos
turn (R _) (f,c) = 
  case f of
    Nort -> (East,c)
    East -> (Sout,c)
    Sout -> (West,c)
    West -> (Nort,c)
turn (L _) (f,c) =
  case f of
    Nort -> (West,c)
    East -> (Nort,c)
    Sout -> (East,c)
    West -> (Sout,c)

step :: (Int,Pos) -> (Int,Pos)
step (i,(f,(x,y))) 
  | i <= 0 = (0,(f,(x,y)))
  | otherwise = case f of
                  Nort -> (i-1,(Nort,(x,y+1)))
                  East -> (i-1,(East,(x+1,y)))
                  Sout -> (i-1,(Sout,(x,y-1)))
                  West -> (i-1,(West,(x-1,y)))
                  


runInst :: Pos -> Inst -> Pos
runInst (face,(x,y)) (R s) =
  case face of
    Nort -> (East, (x + s, y))
    East -> (Sout, (x, y - s))
    Sout -> (West, (x - s, y))
    West -> (Nort, (x, y + s))
runInst (face,(x,y)) (L s) =
  case face of
    Nort -> (West, (x - s, y))
    East -> (Nort, (x, y + s))
    Sout -> (East, (x + s, y))
    West -> (Sout, (x, y - s))

elemPos :: Pos -> [Pos] -> Bool
elemPos _ [] = False
elemPos p0 (p:ps) = (snd p0 == snd p) || elemPos p0 ps

findDuplicate :: [Inst] -> Pos -> [Pos] -> Pos
findDuplicate [] _ _ = error "No duplicates"
findDuplicate (a:as) p ps = 
  let p' = turn a p
      s = getS a in
        stepsCheck s p' ps where
          stepsCheck 0 q qs = findDuplicate as q qs
          stepsCheck t q qs = let (t',q') = step (t,q) in
            if q `elemPos` qs 
              then q
              else stepsCheck t' q' (q:qs)

takeSteps :: [Inst] -> Pos -> [Pos] -> [Pos]
takeSteps [] _ ps = ps
takeSteps (a:as) p ps = 
    let p' = turn a p
        s  = getS a in
          steps s p' ps where
            steps 0 q qs = takeSteps as q qs
            steps t q qs = let (t',q') = step (t,q) in
              steps t' q' (q:qs)

takeSteps' :: [Inst] -> Pos -> [Pos] -> [(Int,Int)]
takeSteps' as p = map snd . takeSteps as p

taxicab :: Pos -> Int
taxicab (_,(x,y)) = abs x + abs y

findDist :: [Inst] -> Int
findDist = taxicab . foldl runInst (Nort,(0,0))

posList :: [Inst] -> [((Int,Int),(Int,(Int,Int)))]
posList xs = zipWith go [1..] zs
  where zs = map c . scanl runInst (Nort,(0,0)) $ xs
        c (_,x) = x
        go a x = (x,(a,x))

posList' :: [Inst] -> [(Int,Int)]
posList' = fmap snd . scanl runInst (Nort,(0,0))

origin = (Nort,(0,0)) :: Pos

testData1 :: [Inst]
testData1 = [R 2, L 3]

testData2 :: [Inst]
testData2 = [R 2, R 2, R 2]

testData3 :: [Inst]
testData3 = [R 5, L 5, R 5, R 3]

testData4 :: [Inst]
testData4 = [R 8, R 4, R 4, R 8]

probData :: [Inst]
probData = [R 5, R 4, R 2, L 3, R 1, R 1, L 4, L 5, R 3, L 1, L 1, R 4, L 2, R 1, R 4, 
           R 4, L 2, L 2, R 4, L 4, R 1, R 3, L 3, L 1, L 2, R 1, R 5, L 5, L 1, L 1, 
           R 3, R 5, L 1, R 4, L 5, R 5, R 1, L 185, R 4, L 1, R 51, R 3, L 2, R 78,
           R 1, L 4, R 188, R 1, L 5, R 5, R 2, R 3, L 5, R 3, R 4, L 1, R 2, R 2, L 4,
           L 4, L 5, R 5, R 4, L 4, R 2, L 5, R 2, L 1, L 4, R 4, L 4, R 2, L 3, L 4, R 2,
           L 3, R 3, R 2, L 2, L 3, R 4, R 3, R 1, L 4, L 2, L 5, R 4, R 4, L 1, R 1, L 5,
           L 1, R 3, R 1, L 2, R 1, R 1, R 3, L 4, L 1, L 3, R 2, R 4, R 2, L 2, R 1, L 5,
           R 3, L 3, R 3, L 1, R 4, L 3, L 3, R 4, L 2, L 1, L 3, R 2, R 3, L 2, L 1, R 4,
           L 3, L 5, L 2, L 4, R 1, L 4, L 4, R 3, R 5, L 4, L 1, L 1, R 4, L 2, R 5, R 1,
           R 1, R 2, R 1, R 5, L 1, L 3, L 5, R 2]

