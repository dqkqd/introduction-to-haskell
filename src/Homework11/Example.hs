module Main where

-- instance Applicative [] where
newtype ZipList a = ZipList {getZipList :: [a]}
  deriving (Eq, Show, Functor)

instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

(*>) :: (Applicative f) => f a -> f b -> f b
(*>) = liftA2 (const id)

mapA :: (Applicative f) => (a -> f b) -> ([a] -> f [b])
mapA g = foldr (liftA2 (:) . g) (pure [])

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

replicateA' :: (Applicative f) => Int -> f a -> f [a]
replicateA' n fa = sequenceA' (replicate n fa)

main :: IO ()
main = do
  print (mapA (\x -> Just (x + 1)) ([1, 2, 3, 4, 5] :: [Integer]))

  print
    (sequenceA' ([Just 1, Just 2, Just 3] :: [Maybe Integer]))

  print
    (replicateA' 5 (Just 5 :: Maybe Integer))
