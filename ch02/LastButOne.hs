-- file ch02/LastButOne.hs
lastButOne :: [a] -> a
lastButOne xs = if (length xs) == 2
                then head xs
                else if (length xs) < 2
                     then error "list length < 2"
                     else lastButOne (tail xs)
