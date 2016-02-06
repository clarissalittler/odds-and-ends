{-# Language DeriveFunctor, FlexibleInstances  #-}

module HW5  where


import Control.Arrow( Arrow, ArrowChoice, ArrowLoop, Kleisli(..)
                    , arr, first, second, (***), (&&&)
                    , left, right, (+++), (|||)
                    , loop )
import Control.Category(Category, (>>>), (.), id)
import Control.Monad(liftM)
import Prelude hiding((.), id)

import Data.Char(ord,chr,digitToInt)

-----------------------------------------------------
-- Part 1. Programming with Arrows
-----------------------------------------------------

-- Arrow type signatures for reference.
{-
first :: Arrow a => a b c -> a (b, d) (c, d)
second :: Arrow a => a b c -> a (d, b) (d, c)
(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
(&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')


left :: ArrowChoice a => a b c -> a (Either b d) (Either c d)
right :: ArrowChoice a => a b c -> a (Either d b) (Either d c)
(+++) :: ArrowChoice a => a b c -> a b' c' -> a (Either b b') (Either c c')
(|||) :: ArrowChoice a => a b d -> a c d -> a (Either b c) d
-}
------------------------------------------------------

-------------------------------------------------
-- Question #1
-- Write strToInteger using arrows
-- test your code
-- use the following guide
--    "4385"::Int -->
--    ([4,3,8,5],[0,1,2,3])::([Int],[Integer]) -->
--    ([4,3,8,5],[0,1,2,3])::([Integer],[Integer])  -->
--    ([5,8,3,4],[1,10,100,1000])::([Integer],[Integer]) -->
--    [5,80,300,4000]::[Integer] -->
--    4385::Integer

toPos :: Integer -> [a] -> [Integer]
toPos _ [] = []
toPos p (x : xs) = p : (toPos (p+1) xs)

strToInteger :: Control.Arrow.Arrow a => a [Char] Integer
strToInteger = (arr (map digitToInt)) &&& (arr (toPos 0)) 
               >>> (first $ arr (map fromIntegral))
               >>> ((arr $ reverse) *** (arr $ map (\x -> 10^x))) 
               >>> (arr (uncurry $ zipWith (*))) >>> arr sum
-- this is hideous, though, but I broke it out into the phases you asked for so I'm not sure if I did anything wrong

-------------------------------------------------------
-- Question #2
-- Write the function printFileInc using arrows
-- I reads a string of digits from a file, and prints to the terminal
-- the successor of the value of that string of digits as an Integer .
-- Test your code.


incy :: Kleisli IO FilePath Integer
incy = Kleisli readFile >>> arr (takeWhile (/= '\n')) >>> strToInteger >>> (arr (+1))

printFileInc :: Kleisli IO FilePath ()
printFileInc = incy >>> Kleisli print 

-------------------------------------------------------
-- Question #3
-- Write the function incFile using arrows.
-- It reads a string of digits from a file, and prints
-- the successor of the value of that string of digits
-- to another file.
-- Test your code.

swap (a,b) = (b,a)

incFile' :: Kleisli IO (FilePath,FilePath) ()
incFile' = first incy >>> arr swap >>> second (arr show) >>> Kleisli (uncurry writeFile)

incFile:: FilePath -> FilePath -> IO ()
incFile s t = runKleisli incFile' (s,t)


--------------------------------------------------------
-- Part 2, Programming with Algebras
--------------------------------------------------------
-- Some Functors

data F1 x = Zero | One | Plus x x deriving Functor
data ListF a x = Nil | Cons a x   deriving Functor
data StreamF n x = C n x          deriving Functor
data NatF x = Z | S x             deriving Functor

-----------------------------------------
-- Algebras and their duals co-Algebras

data Algebra f c = Algebra (f c -> c)

data CoAlgebra f c = CoAlgebra {unCoAlgebra:: c -> f c }

--------------------------------------------
-- Initial and Final Algebras

data Initial f = Init (f (Initial f))

data Final f = Final{ unFinal:: (f (Final f)) }

--------------------------------------------
-- Operations on Initial (cata) and Final (ana) algebras

cata :: Functor f => (Algebra f b) -> Initial f -> b
cata (Algebra phi) (Init x) = phi(fmap (cata (Algebra phi)) x)

ana :: Functor f => (CoAlgebra f seed) -> seed -> (Final f)
ana (CoAlgebra phi) seed = Final(fmap (ana (CoAlgebra phi)) (phi seed))

-----------------------------------------------------------
-- Stating Proofs about Algebras and thir operations

data Arrow f a b = Arr (Algebra f a) (Algebra f b) (a->b)

-- For every Arrow (Arr (Algebra f) (Algebra g) h) it must be the case that
--
--   F a ---- fmap h -----> F b
--    |                      |
--    | f                    | g
--    |                      |
--    V                      V
--    a -------- h --------> b

valid :: (Eq b, Functor f) => HW5.Arrow f a b -> f a -> Bool
valid (Arr (Algebra f) (Algebra g) h) x =  h(f x) == g(fmap h x)

data CoHom f a b = CoHom (CoAlgebra f a) (CoAlgebra f b) (a->b)

-- For every arrow in the category
-- (CoHom (CoAlgebra f) (CoAlgebra g) h) it must be the case that
--
--   F a ---- fmap h -----> F b
--    ^                      ^
--    |                      |
--    | f                    | g
--    |                      |
--    a -------- h --------> b

covalid :: (Eq (f b), Functor f) => CoHom f a b -> a -> Bool
covalid (CoHom (CoAlgebra f) (CoAlgebra g) h) x =  fmap h (f x) == g(h x)

--------------------------------------------------------------------

-- Question #4

data TreeF x = TipF | ForkF x Int x   
             deriving Functor
data LangF x = Add5F x x | Sub5F x x | Mult5F x x | Int5F Int | Var5F String | Assign5F String x
             deriving Functor

-------------------------------------------
-- Question #5

-- 4

intToNatF :: Int -> Initial NatF
intToNatF 0 = Init Z
intToNatF n = Init $ S $ intToNatF (n-1)

n1 :: Initial NatF
n1 = intToNatF 4
-- [3,7,1,6]

lstToListF :: [a] -> Initial (ListF a)
lstToListF [] = Init Nil
lstToListF (x : xs) = Init $ Cons x $ lstToListF xs

l1 :: Initial (ListF Int)
l1 = lstToListF [3,7,1,6]
-- Init $ Cons 3 $ Init $ Cons 7 $ Init $ Cons 1 $ Init $ Cons 6 $ Init Nil

-- A binary search tree (maintaining the search tree invariant) that stores the values 7,2,45, and 11, as an (Initial TreeF)

-- ugly but works
insert :: Int -> Initial TreeF -> Initial TreeF
insert i (Init TipF) = Init (ForkF (Init TipF) i (Init TipF))
insert i (Init (ForkF l j r)) = if i <= j 
                                 then Init (ForkF (insert i l) j r)
                                 else Init (ForkF l j (insert i r))

instance Show (Initial TreeF) where
    show = cata (Algebra aux)
        where aux TipF = "tip"
              aux (ForkF l i r) = l ++ " " ++ (show i) ++ " " ++ r

t1 :: Initial TreeF
t1 = insert 7 $ insert 2 $ insert 45 $ insert 11 $ (Init TipF)

e1 :: Initial LangF
e1 = Init $ Add5F (Init $ Var5F "y") $ Init $ Sub5F (Init $ Int5F 4) $ Init $ Assign5F "x" (Init $ Int5F 8)


-----------------------------------------------------
-- Question #6

toInt :: (Initial NatF) -> Int
toInt = cata (Algebra aux)
    where aux Z = 0
          aux (S x) = x + 1

instance Show a => Show (Initial (ListF a)) where
  show = cata (Algebra aux)
      where aux Nil = "[]"
            aux (Cons a str) = (show a) ++ " : " ++ str

parens s = "(" ++ s ++ ")"

instance Show (Initial LangF ) where
  show = cata (Algebra aux)
      where aux (Add5F s1 s2) = parens $ s1 ++ " + " ++ s2
            aux (Sub5F s1 s2) = parens $ s1 ++ " - " ++ s2
            aux (Mult5F s1 s2) = parens $ s1 ++ " * " ++ s2
            aux (Int5F i) = show i
            aux (Var5F s) = s
            aux (Assign5F s s') = parens $ s ++ " := " ++ s'

add :: Initial NatF -> Initial NatF -> Initial NatF
add = cata (Algebra aux)
    where aux Z = id
          aux (S f) = (Init . S) . f

data Store s x = St(s -> (x,s))
type Map = [(String,Value)]
type Value = Int

eval5help op ef1 ef2 = St $ \s -> 
                         let (v1,s1) = ef1 s
                             (v2,s2) = ef2 s1
                         in (op v1 v2, s2)
eval5a :: Initial LangF -> Store Map Value
eval5a = cata (Algebra aux)
    where aux (Add5F (St ef1) (St ef2)) = eval5help (+) ef1 ef2
          aux (Sub5F (St ef1) (St ef2)) = eval5help (-) ef1 ef2
          aux (Mult5F (St ef1) (St ef2)) = eval5help (*) ef1 ef2
          aux (Int5F i) = St $ \s -> (i,s)
          aux (Var5F x) = St $ \s -> ((\(Just z) -> z) $ lookup x s, s)
          aux (Assign5F x (St ef)) = St $ \s -> 
                                         let (v,s') = ef s
                                         in (v,(x,v):s')

-------------------------------------------------------
-- Question # 7

q7aux :: Int -> LangF (Final LangF)
q7aux n = Add5F (Final (Int5F n)) (Final $ q7aux $ n + 1)

infExp :: Final LangF
infExp = Final $ q7aux 1


-------------------------------------------------------
-- Question 8

prefix:: Int -> Final LangF -> Initial LangF
prefix 0 (Final x) = Init (Int5F 0)
prefix n (Final x) = aux x
    where aux (Add5F e1 e2) = Init $ Add5F (prefix (n-1) e1) (prefix (n-1) e2)
          aux (Sub5F e1 e2) = Init $ Sub5F (prefix (n-1) e1) (prefix (n-1) e2)
          aux (Mult5F e1 e2) = Init $ Mult5F (prefix (n-1) e1) (prefix (n-1) e2)
          aux (Int5F i) = Init (Int5F i)
          aux (Var5F x) = Init (Var5F x)
          aux (Assign5F x e) = Init (Assign5F x (prefix (n-1) e))
