-- this is based on the paper by Odersky and L\"aufer
-- http://webpages.cs.luc.edu/~laufer/papers/reflection93.pdf

data Rep = Zero | Succ | Cond | S | K | Y | Eval | Reif | Refl | Rep :@: Rep
         deriving (Eq, Show)
intp :: Rep -> Rep
apply :: Rep -> Rep

intp (Reif :@: x) = Reif :@: x
intp (x :@: y) = apply (intp x :@: intp y)
intp e = e

apply (Succ :@: x) = Succ :@: x
apply (Cond :@: Zero :@: c :@: f) = intp c
apply (Cond :@: (Succ :@: x) :@: c :@: f) = intp (f :@: x)
apply (S :@: f :@: g :@: x) = intp (f :@: x :@: (g :@: x))
apply (K :@: x :@: y) = intp x
apply (Y :@: f) = intp (f :@: (Y :@: f))
apply (Eval :@: (Reif :@: x)) = Reif :@: (intp x)
apply (Refl :@: (Reif :@: x)) = intp x
apply e = e

data Exp a = Exp Rep a
           deriving (Eq, Show)

eval :: Exp a -> Exp a
eval (Exp e v) = Exp (intp e) v

reflect :: Exp a -> a
reflect (Exp e v) = v

(&) :: Exp (a -> b) -> Exp a -> Exp b
(Exp e u) & (Exp f v) = Exp (e :@: f) (u v)

class Reify a where
    reify :: a -> Exp a

instance Reify Int where
    reify 0 = zero
    reify x = suc & (reify $ x - 1)

instance (Reify a) => Reify (Exp a) where
    reify e = reif & e

zero :: Exp Int
zero = Exp Zero 0
suc :: Exp (Int -> Int)
suc = Exp Succ (+1)
cond :: Exp (Int -> a -> (Int -> a) -> a)
cond = Exp Cond (\x c f -> if x == 0 then c else f (x - 1))
s :: Exp ((a -> b -> c) -> (a -> b) -> a -> c)
s = Exp S (\f g x -> f x (g x))
k :: Exp (a -> b -> a)
k = Exp K (\x y -> x)
i :: Exp (a -> a)
i = s & k & k
y :: Exp ((a -> a) -> a)
y = Exp Y fix
    where fix h = h (fix h)
evl :: Exp (Exp a -> Exp a)
evl = Exp Eval eval
reif :: Reify a => Exp (a -> Exp a)
reif = Exp Reif reify
refl :: Exp (Exp a -> a)
refl = Exp Refl reflect
