-- this was some exercises for a class
{-# Language MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts  #-}

-- Clarissa Littler
-- clarissa.littler@gmail.com

module HW1 where
import Data.Char (chr,ord)

import Control.Monad.State

data Tree a = Tip a | Fork (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Tip a) = "(Tip "++ show a ++ ")"
  show (Fork x y) = "(Fork " ++ show x ++" "++ show y ++")"

instance Eq a => Eq (Tree a) where
    (Tip a) == (Tip a') = a == a'
    (Fork tl tr) == (Fork tl' tr') = (tl == tl') && (tr == tr')
    _ == _ = False

instance Ord a => Ord (Tree a) where
    (Tip a) <= (Tip a') = a <= a'
    (Fork l r) <= (Fork l' r') = (l < l') || ((l == l') && (r <= r'))
    (Fork _ _) <= (Tip _) = False
    (Tip _) <= (Fork _ _) = True

instance Functor Tree where
    fmap f (Tip a) = Tip (f a)
    fmap f (Fork tl tr) = Fork (fmap f tl) (fmap f tr)

class Convertible a b where
  into:: a -> b
  outof:: b -> a

instance Convertible ([a],[b]) [(a,b)] where
  into (x,y) = zip x y
  outof xs = unzip xs

instance Convertible Bool (Maybe ()) where
    into True = Just ()
    into False = Nothing
    outof (Just _) = True
    outof Nothing = False

instance Convertible (a -> b -> c) ((a , b) -> c) where
    into = uncurry
    outof = curry

instance Convertible [a] (Int -> a) where
    into xs = \i -> xs !! i
    outof f = [ f i | i <- [0..]]

data TagExp = I Int | C Char |  F (TagExp -> TagExp) | D String [TagExp]

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt (x : xs) = (ord x) + (1000 * (stringToInt xs))

intToString :: Int -> String
intToString 0 = []
intToString i = let divved = i `rem` 1000
                    remmed = i `div` 1000
                in chr divved : (intToString remmed)

flattenTag :: TagExp -> [Int]
flattenTag (I i) = [0,i]
flattenTag (C c) = [1,ord c]
flattenTag (F _) = error "can't flatten a function"
flattenTag (D s ds) = [2,stringToInt s,length ds] ++ (concat $ map flattenTag ds)

unflattenTag :: [Int] -> (TagExp, [Int])
unflattenTag (0 : i : xs) = (I i,xs)
unflattenTag (1 : c : xs) = (C (chr c),xs)
unflattenTag (2 : s : l : ds) = let (ds', xs) = iterateTake l ds 
                                in (D (intToString s) ds', xs)
    where iterateTake 0 xs = ([],xs)
          iterateTake l xs = let (a,xs') = unflattenTag xs
                                 (as,xs'') = iterateTake (l - 1) xs'
                             in (a : as, xs'')

instance Show TagExp where
    show (I i) = show i
    show (C c) = show c
    show (F f) = "[function]"
    show (D s ds) = show s ++ " " ++  (unwords $ map show ds)

instance Eq TagExp where
    (I i) == (I i') = i == i'
    (C c) == (C c') = c == c'
    (D f ds) == (D f' ds') = (f == f') && (and $ zipWith (==) ds ds')
    _ == _ = False

encodeL :: [Int] -> TagExp
encodeL [] = D "[]" []
encodeL (x:xs) = D ":" [I x,encodeL xs]

decodeL:: TagExp -> [Int]
decodeL (D "[]" []) = []
decodeL (D ":" [I x,xs]) = x : decodeL xs
decodeL other = error "Not a Tagged list"

instance Convertible Int TagExp where
    into i = (I i)
    outof (I i) = i
    outof _ = error "Not an Int"

instance Convertible Char TagExp where
    into c = (C c)
    outof (C c) = c
    outof _ = error "Not a Char"

instance Convertible Bool TagExp where
    into True = D "True" []
    into False = D "False" []
    outof (D "True" []) = True
    outof (D "False" []) = False
    outof _ = error "Not a Bool"

instance Convertible a TagExp => Convertible [a] TagExp where
    into [] = D "[]" []
    into (a : as) = D "::" [into a, into as]
    outof (D "::" [t, ts]) = (outof t) : (outof ts)
    outof (D "[]" []) = []
    outof _ = error "Not a list"

instance Convertible a TagExp => Convertible (Tree a) TagExp where
    into (Tip x) = D "Tip" [into x]
    into (Fork l r) = D "Fork" [into l, into r]
    outof (D "Tip" [x]) = Tip (outof x)
    outof (D "Fork" [xl,xr]) = Fork (outof xl) (outof xr)
    outof _ = error "Not a tree"

instance (Convertible a TagExp,Convertible b TagExp) => Convertible (a->b) TagExp where
    into f = F (into . f . outof)
    outof (F f) = outof . f . into
    outof _ = error "Not a function"

instance (Convertible a TagExp,Convertible b TagExp) => Convertible (a,b) TagExp where
    into (x,y) = D "(,)" [into x, into y]
    outof (D "(,)" [x, y]) = (outof x, outof y)
    outof _ = error "Not a pair"

class Convertible a TagExp => Generic a where
  toString:: a -> String
  toString x = show $ (into x :: TagExp)
  equal :: a -> a -> Bool
  equal x y = (into x :: TagExp) == (into y)
  flatten:: a -> [Int]
  flatten = flattenTag . into
  unflatten:: [Int] -> (a,[Int])
  unflatten xs = let (t, xs') = unflattenTag xs
                 in (outof t, xs')
  inject :: (TagExp -> TagExp) -> a -> a
  inject f = outof . f . into

instance Generic Int where
instance Generic Char where
instance Generic Bool where
instance Generic a => Generic [a] where
instance Generic a => Generic (Tree a) where
instance (Generic a, Generic b) => Generic (a,b) where

incrementTag :: TagExp -> TagExp
incrementTag (I i) = I (i+1)
incrementTag (C c) = C c
incrementTag (F f) = F f
incrementTag (D c ds) = D c (map incrementTag ds)

increment :: Generic a => a -> a
increment = inject incrementTag

doubleFunTag :: TagExp -> TagExp
doubleFunTag (I i) = I i
doubleFunTag (C c) = C c
doubleFunTag (F f) = F (f . f)
doubleFunTag (D c ds) = D c (map doubleFunTag ds)

doubleFun :: Generic a => a -> a
doubleFun = inject doubleFunTag

data Forest a = Node a [Forest a]

instance Convertible a TagExp => Convertible (Forest a) TagExp where
    into (Node x fs) = D "FNode" $ into x : (map into fs)
    outof (D "FNode" (x : fs)) = Node (outof x) (map outof fs)
    outof _ = error "Not a forest"

instance Generic a => Generic (Forest a) where

weirdDoubleTag (I i) = I i
weirdDoubleTag (C c) = C c
weirdDoubleTag (F f) = F f
weirdDoubleTag (D c ds) = D c (ds ++ ds)

-- I was holding out for quadruple tree
doubleTree = inject weirdDoubleTag
-- this is just silly and you'd never want to do this, although it does mean that if you have a variable
-- set of arguments to a constructor they'll get doubled orrr they might lead to an error
