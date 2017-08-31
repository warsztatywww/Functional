module WWW where

fff :: Int -> Int
fff x = x + 3

-- LISTY

len :: [a] -> Int
len [] = 0
len (h:t) = 1 + len t

nth :: Int -> [a] -> a
nth 0 (h:_) = h
nth n (_:t) = nth (n-1) t
nth _ [] = error "lol empty"

rev :: [a] -> [a]
rev l = let
  pom (h:t) acc = pom t (h:acc)
  pom [] acc = acc
  in pom l []

codrugi :: [a] -> [a]
codrugi l = let h1 (_:t) acc = h2 t acc
                h1 [] acc = acc
                h2 (h:t) acc = h1 t (h:acc)
                h2 [] acc = acc
            in rev $ h1 l []


rar [] _ = []
rar _ [] = []
rar (h1:t1) (h2:t2) = (h1, h2): rar t1 t2

unrar :: [(a,b)] -> ([a], [b])
unrar t = case t of
            [] -> ([], [])
            ((h1, h2):tt) ->
                let (l1, l2) = unrar tt
                in (h1:l1, h2:l2)

filtr :: (a -> Bool) -> [a] -> [a]
filtr _ [] = []
filtr f (h:t) = if f h
                then h: filtr f t
                else filtr f t

filtr' :: (a -> Bool) -> [a] -> [a]
filtr' f l = rev $ helper f l [] where
  helper f (h:t) acc =
    helper f t (if f h then h:acc else acc)
  helper _ [] acc = acc

mapa :: (a -> b) -> [a] -> [b]
mapa f l = rev $ helper f l [] where
  helper f (h:t) a = helper f t (f h:a)
  helper _ [] a = a

foldright f x l = case l of
  [] -> x
  (h:t) -> f h (foldright f x t)

foldleft f x [] = x
foldleft f x (h:t) = foldleft f (f x h) t

sort _ [] = []
sort c (h:t) = sort c (filter (c h) t)
               ++[h] ++ (sort c (filter (\x -> not $ c h x) t))

pierwsze = sito [2..] where
    sito (h:t) = h : sito (filter (\x -> x`mod`h /= 0) t)

-- FIFO

data Fifo a = Fifo ([a], [a])

addF :: a -> Fifo a -> Fifo a
addF x (Fifo (l1, l2)) = Fifo (x:l1, l2)

popF :: Fifo a -> Fifo a
popF (Fifo (l1, l2)) = case l2 of
  [] -> Fifo ([], tail . rev $ l1)
  (_:t) -> Fifo(l1, t)

-- DRZEWO

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

add :: Ord a => Tree a -> a -> Tree a
add Leaf k = Node Leaf k Leaf
add (Node l k r) x | x < k = Node (add l x) k r
                   | x > k = Node l k (add r x)
                   | otherwise = Node l k r

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Leaf t = t
merge t Leaf = t
merge t (Node l k r) = add (merge l (merge t r)) k

sub :: Ord a => Tree a -> a -> Tree a
sub Leaf _ = Leaf
sub (Node l k r) x | x < k = Node (sub l x) k r
                   | x > k = Node l k (sub r x)
                   | otherwise = merge l r

suma :: Num a => Tree a -> a
suma Leaf = 0
suma (Node l k r) = k + suma r + suma l

fast :: Tree a -> [a]
fast = go [] where
  go :: [a] -> Tree a -> [a]
  go acc Leaf = acc
  go acc (Node l k r) = go (k : go acc r) l

foldd :: (b -> a -> b -> b) -> b -> Tree a -> b
foldd _ x Leaf = x
foldd f x (Node l k r) =
  f (foldd f x l) k (foldd f x r)

fastd :: Tree a -> [a]
fastd t = foldd f k t [] where
  f fl a fr l = fl (a: fr l)
  k = id

-- Rekord
data Ntree a = NLeaf
             | NNode { left :: Ntree a
                     , val :: a
                     , right :: Ntree a
                     }
-- Monady

{-
class Monad m where
  (>>=) :: m a -> (a -> m b) -> mb
  return :: a -> m a
-}

data Nadjabłcze = NJ
data KraingaGrzybów = KG
data Papier = P

data Jabłko = Jabłko Nadjabłcze KraingaGrzybów Papier

mmj1 :: Maybe Nadjabłcze -> Maybe KraingaGrzybów -> Maybe Papier -> Maybe Jabłko
mmj1 mnj mkg mp =
  case mnj of
    Nothing -> Nothing
    Just nj -> case mkg of
      Nothing -> Nothing
      Just kg -> case mp of
        Nothing -> Nothing
        Just p -> Just (Jabłko nj kg p)

mmj2 :: Maybe Nadjabłcze -> Maybe KraingaGrzybów -> Maybe Papier -> Maybe Jabłko
mmj2 mnj mkg mp =
  mnj >>= \nj ->
  mkg >>= \kg ->
  mp >>= \p ->
  return $ Jabłko nj kg p

mmj3 :: Maybe Nadjabłcze -> Maybe KraingaGrzybów -> Maybe Papier -> Maybe Jabłko
mmj3 mnj mkg mp = do
  nj <- mnj
  kg <- mkg
  p <- mp
  return $ Jabłko nj kg p

main :: IO ()
main = do
  putStrLn "Whats ur name lol"
  name <- getLine
  let out = "lol nice name " ++ name
  putStrLn out

-- Funkcje o wspólnej dziedzinie jako funktory, monady...

{-
instance Functor ((->) d) where
  fmap :: (a -> b) -> (d -> a) -> (d -> b)
  fmap = (.)

instance Applicative ((->) d) where
  (<*>) :: (d -> a -> b) -> (d -> a) -> d -> b
  ap <*> f = \d -> ap d (f d)
  pure :: a -> (d -> a) -- czyli return w Applicative
  pure = const

instance Monad ((->) d) where
  return = pure
  (>>=) :: (d -> a) -> (a -> d -> b) -> d -> b
  f1 >>= f2 = \d -> f2 (f1 d) d
-}
