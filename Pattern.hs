data Expression = Number Int 
                | Add Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Ord, Show)

calculate :: Expression -> Int

calculate (Number x) = x
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Subtract x y) = (calculate x) - (calculate y)

newHead :: [a] -> a
newHead [] = error "empty list"
newHead (x:xs) = x

newTail :: [a] -> [a]
newTails [] = error "empty list"
newTail (x:xs) = xs
