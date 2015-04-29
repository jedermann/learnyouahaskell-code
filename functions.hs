doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 20 
                        then x 
                        else doubleMe x

boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs,  odd x]

length' xs = sum [1 | _ <-xs] 

removeNonUpperCase  st = [ c | c <-st, c `elem` ['A'..'Z']]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String