module Core.Utils
  ( crop
  , modifyAt
  , insertAt
  , cropMin
  , removeAt
  )
  where


crop :: Int -> Int -> Int -> Int
crop min max val
  | val < min = min
  | val > max = max
  | otherwise = val


cropMin :: Int -> Int -> Int
cropMin min val
  | val < min = min
  | otherwise = val


modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt 0 f (x:xs) = f x : xs
modifyAt t f (x:xs) = x : modifyAt (t - 1) f xs


insertAt :: Int -> a -> [a] -> [a]
insertAt 0 i xs = i : xs
insertAt t i [] = [i]
insertAt t i (x:xs) = x : insertAt (t - 1) i xs


removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (x:xs) = xs
removeAt i (x:xs) = x : removeAt (i - 1) xs
