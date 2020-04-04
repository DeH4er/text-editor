module Core.Utils
  ( findIndex
  , crop
  , modifyAt
  , insertAt
  , cropMin
  , removeAt
  , mapIndex
  , mapCollect
  , removeDuplicates
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


inRange :: Int -> Int -> Int -> Bool
inRange min max val =
  val >= min && val <= max


mapIndex :: (Int -> a -> b) -> [a] -> [b]
mapIndex = doMap 0
  where
    doMap :: Int -> (Int -> a -> b) -> [a] -> [b]
    doMap _ _ [] = []
    doMap i f (x:xs) = f i x : doMap (i + 1) f xs


mapCollect :: c -> (c -> a -> (b, c)) -> [a] -> [b]
mapCollect _ _ [] = []
mapCollect item f (x:xs) =
  b : mapCollect newItem f xs
    where
      (b, newItem) = f item x


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl doRemove []
  where
    doRemove :: Eq a => [a] -> a -> [a]
    doRemove seen x
      | x `elem` seen = seen
      | otherwise = seen ++ [x]


findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex =
  doFind 0
    where
      doFind :: Int -> (a -> Bool) -> [a] -> Maybe Int
      doFind _ _ [] =
        Nothing

      doFind i f (x:xs) =
        if f x then
          Just i
        else
          doFind (i + 1) f xs
