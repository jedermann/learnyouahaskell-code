doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 20 
                        then x 
                        else doubleMe x

boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs,  odd x]

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) =  1 + length' xs

removeNonUpperCase  st = [ c | c <-st, c `elem` ['A'..'Z']]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String 
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal  = "Normal"
    | bmi <= overweight = "Overweight"
    | otherwise   = "Overoverweight"
    where bmi     = weight / height ^ 2
          skinny  = 18.5
          normal  = 25.0
          overweight = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++"."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
      let sideArea = 2 * pi * r * h 
          topArea = pi *r ^2
      in sideArea + 2 * topArea
