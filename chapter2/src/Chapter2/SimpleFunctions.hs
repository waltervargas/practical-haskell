firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"


-- custom concat list operator (+++)
-- base case: returns the second list if the first list is empty
-- else: takes the head and builds a list with the recursive call 
-- to the tail of the list1 concatenated with the lst2.
-- for [1,2] +++ [3,4] it evaluates recursively to 1:([2] +++ [3,4])
-- when the first list is empty (due recursion) it's returns lst2.
-- :lst2 with 1:(2:[3,4]) 

-- [1,2] +++ [3,4] => 
--                   1:([2] +++ [3,4]) => 
--                                        1:(2:([] +++ [3,4])) => 
--                                                               [1,2,3,4]                                        
lst1 +++ lst2 = if null lst1 
                {- check emptyness -}
                then lst2 -- base case
                else (head lst1) : (tail lst1 +++ lst2)

-- Custom reverse function (reverse2)
-- [1,2,3]
-- (reverse2 [2,3]) +++ [1]
--     (reverse2 [3]) +++ [2] 
--          reverse2 [] +++ [3] 
--              [] 
--               [3] 
--                 [3,2] 
--                  [3,2,1] 

reverse2 list = if null list 
                then []
                else reverse2 (tail list) +++ [head list]

-- maxmin returns the max and the min of a list
-- ugly code
maxmin' list = if null (tail list)
                  then (head list, head list)
                  else (if (head list) > fst (maxmin (tail list))
                        then head list
                        else fst (maxmin' (tail list))
                       , if (head list) < snd (maxmin (tail list))
                         then head list
                         else snd ((maxmin' (tail list)))
                       )

-- better maxmin version using let and where
maxmin :: Ord a => [a] -> (a, a)
maxmin list = let h = head list
              in if null (tail list)
                 then (h, h)
                 else ( if h < t_min then h else t_min,
                        if h > t_max then h else t_max)

  where t     = maxmin (tail list)
        t_max = fst t
        t_min = snd t
