{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module MonadInterpWorksheet where

data Store s x = St {runStore :: s -> (x,s)}

evalStore x = fst . (runStore x)

type Map = [(String,Value)]

-- Assumption that the list is a set
-- Order doesn't matter, and no duplicates

--------------------------------------
-- Find a value in the Map

find :: Eq a => a -> [(a,b)] -> b
find nm pairs = head [ v | (n,v) <- pairs, n==nm]

-- Update a value in a Map

update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update nm value pairs = (nm,value) : [ (n,v) | (n,v) <- pairs, n /= nm ]

-----------------------------------------------
-- Here is a new language with variables and assignments

data T5 = Add5 T5 T5
        | Sub5 T5 T5
        | Mult5 T5 T5
        | Int5 Int
        | Var5 String
        | Assign5 String  T5


-- Question #1. Write an evaluator for the language T5
-- write it in functional style (that is don't use monads)
-- I started it for you.

type Value = Int

eval5a :: T5 -> Store Map Value
eval5a (Add5 x y) = helper (+) x y
eval5a (Sub5 x y) = helper (-) x y
eval5a (Mult5 x y) = helper (*) x y
eval5a (Int5 n) = St $ \s -> (n , s)
eval5a (Var5 v) = St $ \s -> (find v s,s) 
eval5a (Assign5 nm x) = St $ \s -> let St f = eval5a x
                                       (x', s1) = f s
                                   in (x', update nm x' s1)

helper op x y =  St (\s -> let St f = eval5a x
                               St g = eval5a y
                               (x', s1) = f s
                               (y', s2) = g s1
                           in (op x' y', s2))

-----------------------------------------------
-- Question #2 Now write the operators on the Store monad


-- Given a name "x", "getStore x" returns a
-- Store computation, that when run returns
-- the value associated in the Map
-- with the name given as input.

getStore :: String -> (Store Map Value)
getStore nm = St h
  where h s = (find nm s, s)

-- Given a name "x", and a new value "v"
-- "putStore x v" returns a Store computation,
-- that when it runs returns unit, but updates
-- the map so "x" is now mapped to "v"

putStore :: String -> Value -> (Store Map ())
putStore nm n = St $ \s -> ((), update nm n s)

-------------------------------------------------------
-- Question #3. Write an evaluator for the language T5 but
-- this time use use monads and the "do" syntax
-- Hint. use the operators on the Store Monad
-- I started it for you.

eval5 :: T5 -> Store Map Value
eval5 (Add5 x y) = mhelper (+) x y
eval5 (Sub5 x y) = mhelper (-) x y
eval5 (Mult5 x y) = mhelper (*) x y
eval5 (Int5 n) = return n
eval5 (Var5 s) = getStore s
eval5 (Assign5 s x) = do
  x' <- eval5 x
  putStore s x'
  return x'

mhelper op x y = do
  x' <- eval5 x
  y' <- eval5 y
  return $ op x' y'


------------------------------
instance Functor (Store s) where
    fmap f (St g) = St $ \s -> let (x,s') = g s
                               in (f x, s')

instance Applicative (Store s) where
    pure x = St (\s -> (x,s))
    (St sf) <*> (St sx) = St $ \s -> let (f,s') = sf s
                                         (x,s'') = sx s'
                                     in (f x, s'')
 
instance Monad (Store s) where
  (>>=) (St f) g = St h
    where h s1 = g' s2 where (x,s2) = f s1
                             St g' = g x

--------------------------------------------
-- Question #4


-- so I'm guessing you don't want just Store2 a = StateT Id a but rather define our own, eh?

newtype Identity a = ID a

instance Functor Identity where
    fmap f (ID x) = ID (f x)
instance Applicative Identity where
    pure = ID
    (ID f) <*> (ID x) = ID (f x)
instance Monad Identity where
    (ID x) >>= f = f x

class MonadTrans t where
    lift :: Monad m => m a -> t m a

newtype StoreT s m a = StT {runStoreT :: (s -> m (a, s))}

evalStoreT x = fmap fst . (runStoreT x)

instance Monad m => Functor (StoreT s m) where
    fmap f (StT g) = StT $ \s -> do
                       (x, s') <- g s
                       return $ (f x, s')

instance Monad m => Applicative (StoreT s m) where
    pure x = StT $ \s -> return (x,s)
    (StT mf) <*> (StT mx) = StT $ \s -> do
                              (f,s') <- mf s
                              (x,s'') <- mx s'
                              return (f x, s'')

-- mx :: s ->  m (a,s)
-- f :: a -> StoreT s m b
-- f' :: s -> m (b, s)
instance Monad m => Monad (StoreT s m) where
    (StT mx) >>= f = StT $ \s -> do
                       (x,s') <- mx s
                       let (StT f') = f x
                       f' s'

instance MonadTrans (StoreT s) where
    lift m = StT $ \s -> do
               x <- m
               return (x,s)

type Store3 m a = StoreT Map m a

type Store2 a = Store3 Identity a

getStore2 :: Monad m => String -> Store3 m Value
getStore2 nm = StT $ \s -> return (find nm s, s)

putStore2 :: Monad m => String -> Value -> Store3 m ()
putStore2 x v = StT $ \s -> return ((), update x v s)

--------------------------------------------
-- Question #5

eval5T :: T5 -> Store2 Value
eval5T (Add5 x y) = mthelper (+) x y
eval5T (Sub5 x y) = mthelper (-) x y
eval5T (Mult5 x y) = mthelper (*) x y
eval5T (Int5 n) = return n
eval5T (Assign5 nm x) = do
  x' <- eval5T x
  putStore2 nm x'
  return x'

mthelper op x y = do
  x' <- eval5T x
  y' <- eval5T y
  return $ op x' y'

-- this is really Monad m => Store3 m Value as a more general type

