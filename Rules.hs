type Left = String
type Right = String
type Operator = String

data Predicate = P Left Operator Right

compareAmount :: Num a => a -> (a -> a -> Bool) -> a -> Bool
compareAmount lhs op rhs = op lhs rhs

amountGreaterThan :: (Ord a, Num a) => a -> a -> Bool
amountGreaterThan l r = l > r

predicateToFunction :: (Read a, Ord a, Num a) => Predicate -> (a -> Bool)
predicateToFunction p = case p of
    (P "amount" "greaterThan" rhs) -> compareAmountWithRight (>) (read rhs)
    (P "amount" "lessThan" rhs) -> compareAmountWithRight (<) (read rhs)
    (P "amount" "equalTo" rhs) -> compareAmountWithRight (==) (read rhs)
    where
        compareAmountWithRight op = flip (`compareAmount` op)

