{-# LANGUAGE    FlexibleInstances, FlexibleContexts  #-}
module E3Lang where
-- my own version of the http://web.cecs.pdx.edu/~sheard/course/CS558/code/hw3/hw3.hs interpreter
-- trying perhaps some code reorganization to make it prettier

-- These imports are for defining parsers
import Text.Parsec hiding (State,Column)
import Text.Parsec.Expr(Operator(..),Assoc(..),buildExpressionParser)
import Text.Parsec.Prim(getInput)
import Text.Parsec.Pos(SourcePos)
import Text.Parsec.Token hiding (natural)
import Data.Functor.Identity(Identity(..))
import Data.Char(digitToInt,isUpper)
import qualified Control.Exception
import Control.Monad.Reader
import Control.Monad.State

-- These are used for tracing the stack machine
import Data.IORef(newIORef,readIORef,writeIORef,IORef)
import System.IO.Unsafe(unsafePerformIO)

-- This import is for getting command line arguments
import System.Environment(getArgs)

-- This import is for indenting pretty printing
import qualified Text.PrettyPrint.HughesPJ as PP

{- Abstract Syntax for expressions. -}

type Vname = String  -- Names of variables
type Fname = String  -- Names of functions
type Addr = Int      -- Address of a location in the Heap

data Exp 
  = Var  Vname
  | Int  Int
  | Asgn  Vname Exp
  | While Exp Exp
  | If Exp Exp Exp
  | Write Exp
  | Block [Exp]
  | At Fname [Exp]
  | Add  Exp Exp
  | Sub  Exp Exp
  | Mul  Exp Exp
  | Div  Exp Exp
  | Leq  Exp Exp
  | Pair Exp Exp
  | Fst Exp
  | Snd Exp
  | Ispair Exp
  
data Def 
  = GlobalDef Vname Exp
  | FunDef Fname [Vname] Exp

data Program = Prog [Def] Exp  

data Value 
     = IntV Int       -- An Int
     | PairV Addr     -- Or a pointer into the heap 
  deriving Show     

numeric :: (Int -> Int -> Int) -> Value -> Value -> Value
numeric fun (IntV x) (IntV y) = IntV(fun x y)
numeric fun (v@(PairV _)) _ = error ("First arg is not an Int. "++ show v)
numeric fun _ (v@(PairV _)) = error ("Second arg of is not an Int. "++ show v)

type Env a = [(Vname,a)]

type Interp a = ReaderT (Env Addr) (StateT Heap IO) a

-- States and Heaps

type Heap = [Value]

-- Access the State for the Value at a given Address
access n (State heap) = heap !! n

-- Allocate a new location in the heap. Intitialize it
-- with a value, and return its Address and a new heap.
alloc :: Value -> Heap -> (Addr,Heap)
alloc value heap = (addr,heap ++ [value])   
    where addr = length heap

allocM :: MonadState Heap m => Value -> m Addr
allocM v = do
  h <- get
  let (a,h') = alloc v h
  put h'
  return a

-- For each Name and associated value, allocate the value on the heap,
-- and pair its name with the address it was allocated to.
-- For example:    bind [a,b,c] 
--                      [IntV 3,IntV 7,IntV 1] 
--                      (State [IntV 17])  ---> 
-- returns the pair
-- ( [(a,1),(b,2),(c,3)]    -- names paired with Addr 
-- , (State [IntV 17,IntV 3,IntV 7,IntV 1]) -- The new heap
-- )


bind:: [String] -> [Value] -> Heap -> ([(Vname,Addr)],Heap)
bind names objects state = loop (zip names objects) state
  where loop [] state = ([],state)
        loop ((nm,v):more) state = ((nm,ad):xs,state3)
          where (ad,state2) = alloc v state
                (xs,state3) = loop more state2
                
-- Update the State at a given Adress
stateStore addr u heap = update addr heap
  where update 0 (x:xs) = (u:xs)
        update n (x:xs) = x : update (n-1) xs
        update n [] = error ("Address "++show addr++" too large for heap.")

lookupM :: MonadReader (Env a) m => Vname -> m a
lookupM v = reader $ \vars -> case lookup v vars of
                                Just a -> a
                                _ -> error "Unknown variable"

setStoreM :: MonadState Heap m => Addr -> Value -> m ()
setStoreM a v = modify $ \s -> setStore a v 

bindM :: MonadState Heap m => [String] -> [Value] -> m (Env Addr)
bindM ss vs = do
  h <- get
  let (env,h') = bind ss vs h
  put h'
  return env

-- A small heap as a test.
heap = h4
  where h1 = []
        (_,h2) = alloc (IntV 3) h1
        (_,h3) = alloc (IntV 6) h2
        (_,h4) = alloc (IntV 1) h3
        
when True x = x
when False x = return ()

------------------------------------------------------------------
-- An interpretor for Exp
-- We store functions and variables in two different name spaces
-- functions in an   Env (Env Addr,[Vname],Exp)
-- variables in an   Env Addr
-- The values of variables are stored in the heap.

-- Interpret a single Exp
-- An intepretation might compute a new State (heap)
-- because a new variable might be allocated, or an
-- assignment might change what is stored at an address.


interpEBinOp funs op e1 e2 = do
  v1 <- interpE funs e1
  v2 <- interpE funs e2
  return $ numeric op v1 v2

interpE :: Env (Env Addr,[Vname],Exp)  -- The function name space
        -> Exp                         -- the Exp to interpret
        -> Interp Value
interpE funs exp = run exp where
   run (Var v) = lookupM v
   run (Int n) = return $ IntV n
   run (Asgn v e ) = do 
     val <- interpE funs e
     a <- lookupM v
     setStoreM a val
     return val
   run (term@(At f args)) = 
     case lookup funs f of
        Nothing -> error ("\nUnknown function: "++f++" in function call.")
        Just (vars2,formals,body) -> do 
                             when (length args /= length formals) 
                                      (error ("\nWrong number of args to: "++f++" in: "++show term))
               -- Eval all the arguments
                             vs <- mapM (interpE funs) args 
               -- bind names to addresses, and allocate them on the heap             
                             pairs <- bindM formals vs  
               -- run the body in new environment
                             local (const $ pairs ++ vars2) $ interpE funs body

   run (While tst body)= while
     where while = do  
             v <- interpE funs tst
             test "while" v (do  
                             interpE funs body
                             while)
                             (return IntV 0)   
                       
   run (If x y z) = 
     do { v <- interpE funs x
        ; test "if" v (interpE funs vars state2 y) (interpE funs vars state2 z) }  
   
   run (Write x)  = do 
     v1 <- interpE funs x
     lift $ putStrLn $ show v1
     return v1

   run (Block es) = do
     let last [] = (IntV 0)  -- empty block returns 0
         last [x] = x
         last (x:xs) = last xs
     vs <- mapM (interpE funs) es
     return $ last vs

   run (Add  x y) = interpEBinOp (+) x y
   run (Sub  x y) = interpEBinOp (-) x y
   run (Mul  x y) = interpEBinOp (*) x y
   run (Div  x y) = interpEBinOp (/) x y
   run (Leq  x y) = interpEBinOp (\ v1 v2 -> boolToInt (v1 <= v2)) x y
   run (Pair x y) = do
          v1 <- interpE funs x
          v2 <- interpE funs y
          a1 <- allocM v1 state2  -- Allocate consequtive locations
          a2 <- allocM v2 state3  -- for v1 and v2
          return $ PairV a1           -- return the PairV of the first address

   run state (Fst x) = do 
       (VPair a) <- interpE funs x
       gets (!! a)

   run state (Snd x) = do
       (VPair a) <- interpE funs x
       gets (!! (a + 1)) 

   run state (Ispair x) =  do 
     v1 <- interpE funs x
     case v1 of
       (PairV _) -> return $ IntV $ boolToInt True
       other -> return $ IntV $ boolToInt False

-- elab:: Def -> (Env (Stack,[Vname],Exp),Env Address,State) -> IO (Env (Stack,[Vname],Exp),Env Address,State)
elab (FunDef f vs e) funs = do
  v <- ask
  return $ extend f (v,vs,e) funs
elab (GlobalDef v e) funs = do 
  value <- interpE funs e
  addr <- allocM value
  return(funs, extend v addr vars,state3)}

elaborate loud [] ts = return ts
elaborate loud (d:ds) ts = 
  do { when loud (putStr (defName d++" "))
     ; ts1 <- elab d ts
     ; elaborate loud ds ts1}
                                     
--------------------------------------------------------------------------
-- A parser that reads a file and produces a program in abstract syntax
-- Most of this section is boilerplate
-- The interesting functions are "operatorP" and "expP"

-- A Exp is either a an Atom or a listlike thing like: (tag x1 x2 ... xn)
-- Depending upon the "tag" a different number of xi are expected.
-- We parse tag to a LOWER CASE CONSTRUCTOR (see below) by using "operatorP",
-- and then apply it to a list [p1 p2 ... pn] where the pi
-- are the result of parsing the xi with "expP". When parsing if too many, 
-- or too few arguments, are parsed then "report" an error.

report:: SourcePos -> Int -> Int -> String -> a
report pos actual expected message 
  | actual < expected = error ("Near "++show pos++"\noperator '"++message++"' is given too few arguments.")
  | otherwise = error ("Near "++show pos++"\noperator '"++message++"' is given too many arguments.")  

-- There is one LOWER CASE CONSTRUCTORS for each Exp constructor (Var, Int, Asgn, etc).
-- The constructors of Exp are lifted to take a position (for error reporting)
-- and a list of arguments. Thus every "lowercase" constructor has type: 
--     SourcePos -> [ Exp ] -> Exp
-- If one is given the wrong number of arguments an error is "report"ed,
-- otherwise a well formed Exp is constructed.

ifkey pos [x,y,z] = If x y z
ifkey pos args = report pos (length args) 3 "if"

write pos [x] = Write x
write pos args = report pos (length args) 1 "write"

while pos [x,y] = While x y
while pos args = report pos (length args) 2 "while"

add pos [x,y] = Add x y
add pos args = report pos (length args) 2 "+"

sub pos [x,y] = Sub x y
sub pos args = report pos (length args) 2 "-"

mul pos [x,y] = Mul x y
mul pos args = report pos (length args) 2 "*"

divide pos [x,y] = Div x y
divide pos args = report pos (length args) 2 "/"

leq pos [x,y] = Leq x y
leq pos args = report pos (length args) 2 "<="

atSign pos (Var x:args) = At x args
atSign pos args = error ("Near "++show pos++"\nfirst argument to '@' is not a function name.")

pair pos [x,y] = Pair x y
pair pos args = report pos (length args) 2 "pair"

fstC pos [x] = Fst x
fstC pos args = report pos (length args) 1 "fst"

sndC pos [x] = Snd x
sndC pos args = report pos (length args) 1 "snd"

ispair pos [x] = Ispair x
ispair pos args = report pos (length args) 1 "ispair"

-- Asgn is special because its first argument must be a variable!!
assign pos [Var x,y] = Asgn x y
assign pos [x,y] = error ("Near "++show pos++"\nfirst argument to ':=' is not a variable.")
assign pos args = report pos (length args) 2 ":="

local vs pos [x] = yourlocal vs x  
  where yourlocal = error ("\nReplace 'yourlocal' with the constructor for 'local' you added to Exp.\n It should have type: [Exp] -> Exp -> Exp.")
local vs pos args = report pos (length args) 1 "local"

setfst pos [x,y] = error "\nYou need to define setfst"
setfst pos args = report pos (length args) 2 "setfst"

setsnd pos [x,y] = error "\nYou need to define setsnd"
setsnd pos args = report pos (length args) 2 "setsnd"
  
-- This function parses a string, but returns a function with type: 
-- SourcePos -> [Exp] -> Exp    See "lowercase" constructors above.
-- There are many cases, one for each of the legal constructors to Exp.
-- To add to the parser, write a new function "f" then add " <|> f "
-- to the definition of "operatorP"

operatorP = plus <|> times <|> minus <|> divid <|> ltheq <|> asg <|> atP <|>
            blck <|> writ <|> iff <|> whil <|>
            pairP <|> fstP <|> sndP <|> ispairP <|>
            localP <|> setfstP <|> setsndP
  where plus   = do { symbol xxx "+";  return add}
        times  = do { symbol xxx "*";  return mul}
        minus  = do { symbol xxx "-";  return sub}
        divid  = do { symbol xxx "/";  return divide}
        ltheq  = do { symbol xxx "<="; return leq}
        asg    = do { symbol xxx ":="; return assign}
        atP    = do { symbol xxx "@"; return atSign}
        blck   = do { try(symbol xxx "block"); return (\ pos es -> Block es)} 
        writ   = do { try(symbol xxx "write"); return write}
        iff    = do { try(symbol xxx "if"); return ifkey}
        whil   = do { try(symbol xxx "while"); return while}
        pairP  = do { try(symbol xxx "pair"); return pair}
        fstP   = do { try(symbol xxx "fst"); return fstC}
        sndP   = do { try(symbol xxx "snd"); return sndC}
        ispairP= do { try(symbol xxx "ispair"); return ispair}
        setfstP= do { try(symbol xxx "setfst"); return setfst}
        setsndP= do { try(symbol xxx "setsnd"); return setsnd}        
        localP = do { try(symbol xxx "local")
                    ; let pair = do { s <- identifier xxx <?> "'local variable'"
                                    ; e <- expP
                                    ; return[Var s,e]}
                    ; vs <- parenS(do { ps <- many pair; return(concat ps)}) <?> "'local var-value list'"
                    ; return (local vs)}
        
        
        
-- An expression is either an atom or a Lisp like parenthesized operation
expP = try atom <|> sexpr
  where atom = constant <|> var
        var = 
          do { s <- identifier xxx; return(Var s)}
        constant = 
           do { n <- lexemE(number 10 digit)
              ; return (Int n)}
        sexpr  = 
           do { symbol xxx "(" 
              ; pos <- getPosition
              ; constrByList <- operatorP  -- Parse a string, get a lowercase constructor
              ; args <- many expP
              ; symbol xxx ")"
              ; return(constrByList pos args)}

-- Parse a Def
defP = parenS ( global <|> local )
  where global =
           do { symbol xxx "global"
              ; v <- identifier xxx
              ; e <- expP
              ; return(GlobalDef v e)}
        local = 
           do { symbol xxx "fun"
              ; f <- identifier xxx
              ; vs <- parenS(many (identifier xxx))
              ; body <- expP
              ; return(FunDef f vs body)}

-- Parse a Prog              
progP = do { ds <- parenS (many defP)
           ; e <- expP
           ; return(Prog ds e)}
         
-- Boilerplate below here        
       
type MParser a = ParsecT
                    String                -- The input is a sequence of Char
                    ()                    -- The internal state
                    Identity              -- The underlying monad
                    a                     -- the type of the object being parsed
      
xxxStyle = LanguageDef
                { commentStart   = "{-"
                , commentEnd     = "-}"
                , commentLine    = ""
                , nestedComments = True
                , identStart     = lower <|> upper
                , identLetter    = lower <|> upper <|> digit
                , opStart        = oneOf "+*-/"
                , opLetter       = oneOf "+*-/"
                , caseSensitive  = True
                , reservedOpNames = []
                , reservedNames  = []
                }                    

xxx = makeTokenParser xxxStyle
lexemE p    = lexeme xxx p
parenS p    = between (symbol xxx "(") (symbol xxx ")") p

number ::  Int -> MParser Char -> MParser Int
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + (digitToInt d)) 0 digits
        ; seq n (return n)
        }

-- Running parsers

runMParser parser name tokens =
  runIdentity (runParserT parser () name tokens) 

parse1 file x s = runMParser (whiteSpace xxx >> x) file s 

parseWithName file x s =
  case parse1 file x s of
   Right(ans) -> ans
   Left message -> error (show message)
   
parse2 x s = parseWithName "keyboard input" x s  

parseString x s =
  case parse1 s x s of
   Right(ans) -> return ans
   Left message -> fail (show message)   

parseFile parser file =
    do possible <- Control.Exception.try (readFile file)
                   -- System.IO.Error.tryIOError (readFile file)
       case possible of
         Right contents -> 
            case parse1 file parser contents of
              Right ans -> return ans
              Left message -> error(show message)
         Left err -> error(show (err::IOError))

-----------------------------------------------
-- Turning things into String

txt = PP.text

-- This is an indenting pretty printer.
ppExp :: Exp -> PP.Doc
ppExp (Var v) = txt v
ppExp (Int n) = txt (show n)
ppExp (Asgn v e )= txt "(:= " PP.<> PP.nest 3 (PP.sep [txt v , ppExp e PP.<> txt ")"])
ppExp (At f args)= txt "(@ " PP.<> PP.nest 3 (PP.sep (txt f : many args))
  where many [] = [txt ")"]
        many [x] = [ppExp x PP.<> txt ")"]
        many (x:xs) = ppExp x : many xs
ppExp (While x y)= txt "(while " PP.<> PP.nest 3 (PP.sep [ppExp x , ppExp y PP.<> txt ")"])
ppExp (If x y z)= txt "(if " PP.<> PP.nest 3 (PP.sep [ppExp x , ppExp y,ppExp z PP.<> txt ")"])
ppExp (Write x)= txt "(write " PP.<> PP.nest 3 (PP.sep [ppExp x PP.<> txt ")"])
ppExp (Block es) = txt "(block " PP.<> PP.nest 3 (PP.sep ((map ppExp es)++ [txt ")"]))
ppExp (Add  x y)  = txt "(+ " PP.<> PP.nest 3 (PP.sep [ppExp x , ppExp y PP.<> txt ")"])
ppExp (Sub  x y)  = txt "(- " PP.<> PP.nest 3 (PP.sep [ppExp x , ppExp y PP.<> txt ")"])
ppExp (Mul  x y) = txt "(* " PP.<> PP.nest 3 (PP.sep [ppExp x , ppExp y PP.<> txt ")"])
ppExp (Div  x y)  = txt "(/ " PP.<> PP.nest 3 (PP.sep [ppExp x , ppExp y PP.<> txt ")"])
ppExp (Leq  x y) = txt "(<= " PP.<> PP.nest 3 (PP.sep [ppExp x , ppExp y PP.<> txt ")"])
ppExp (Pair x y) = txt "(pair " PP.<> PP.nest 3 (PP.sep [ppExp x , ppExp y PP.<> txt ")"])
ppExp (Fst x) = txt "(fst " PP.<> PP.nest 3 (PP.sep [ppExp x PP.<> txt ")"])
ppExp (Snd x) = txt "(snd " PP.<> PP.nest 3 (PP.sep [ppExp x PP.<> txt ")"])
ppExp (Ispair x) = txt "(ispair " PP.<> PP.nest 3 (PP.sep [ppExp x PP.<> txt ")"])
{-
ppExp (Local vs e) = txt "(local " PP.<> PP.nest 3 ( vars PP.$$ ppExp e PP.<> txt ")")
  where pairs = f vs
        f (x:y:zs) = (x,y): f zs
        f _ = []
        vars :: PP.Doc
        vars = PP.parens(PP.sep (map h pairs))
        h (x,y) = PP.sep [ppExp x,ppExp y]
-}        


ppDef (GlobalDef v e) = txt "(global " PP.<> PP.nest 3 (PP.sep [txt v, ppExp e PP.<> txt ")"])
ppDef (FunDef f args body) = 
  txt "(fun " PP.<> PP.nest 3 (PP.sep [txt f, PP.parens (PP.sep (map txt args)),ppExp body PP.<> txt ")"])
  
ppProgram (Prog ds e) = 
  ( PP.parens (PP.vcat (map ppDef ds)))
  PP.$$
  (ppExp e)
    
plistf :: (a -> String) -> String -> [a] -> String -> String -> String
plistf f open xs sep close = open ++ help xs ++ close
  where help [] = ""
        help [x] = f x
        help (x:xs) = f x ++ sep ++ help xs    

plist = plistf id

instance Show State where
  show (s@(State hp)) =  plistf f "" hp " " " ."
    where f x = showV (x,s)
    
prog_to_string instrs = showList instrs ""

instance Show Exp where
  show x = PP.render(ppExp x)
           
instance Show Def where
  show x = PP.render(ppDef x)
  showList xs str = PP.render(txt str PP.$$ PP.vcat (map ppDef xs))

instance Show Program where
  show p = PP.render(ppProgram p)

showV:: (Value,State) -> String
showV (IntV n,state) = show n
showV (PairV addr,state) = "("++showV(get addr,state)++"."++showV(get (addr+1),state)++")"
  where get n = xs !! n where (State xs) = state

showEnv:: Env Addr -> State -> [PP.Doc]
showEnv (Tab env) (state@(State xs)) = loop env
  where loop [] = [txt " "]
        loop [x] = [txt (f x),txt " "]
        loop (x:xs) = (txt (f x)): (txt " "): loop xs
        f (name,addr) = name++"@"++show addr++"="++showV(xs !! addr,state)


------------------------------------------------------- 
-- Functions for indenting pretty printing. Needed for the tracer.

class Pretty x where
  pp:: x -> PP.Doc

instance Pretty Exp where
   pp = ppExp

instance (Pretty key,Pretty value) => Pretty (Table key value) where
  pp (Tab xs) = PP.sep(map f xs)
    where f (x,y) = pp x PP.<> txt ("=") PP.<> pp y
    
instance (Pretty a, Pretty b) => Pretty (a,b) where
  pp (x,y) = PP.parens (PP.sep [pp x, txt ",", pp y])

instance Pretty [Char] where
  pp str = txt str

instance Pretty Int where
  pp x = txt(show x)
  
instance Pretty State where
  pp x = txt(show x)


------------------------------------------------------------
-- This is for tracing the interpreter

-- Boolean flag for tracing.
trace_on = unsafePerformIO (newIORef False)

-- typing "trace" at the GHCI prompt or within the READ EVAL PRINT
-- loop flips the tracing flag.
trace = do { b <- readIORef trace_on
           ; writeIORef trace_on (not b)
           ; let f True = "on"
                 f False = "off"
           ; putStrLn ("\nTracing is "++f (not b)++".")}

-- Keeps track of the indentation of the tracer
traceIndent = unsafePerformIO (newIORef 0)

type InterpType = State -> Exp -> IO (Value,State)

traceF :: Env Addr -> InterpType -> InterpType 
traceF env f state exp =
  do { n <- readIORef traceIndent
     ; let indent x = txt(concat(replicate n " |")++x)
     ; putStrLn(PP.render(indent "> " PP.<> PP.fsep (pp exp : showEnv env state)))
     ; writeIORef traceIndent (n+1)
     ; ans <- f state exp
     ; writeIORef traceIndent n
     ; putStrLn(PP.render(indent "< " PP.<> txt(showV ans)))
     ; return ans }

-- Like traceF if tracing is on, like f if not.
traceG :: Env Addr -> InterpType -> InterpType 
traceG env f vars exp = 
  do {  b <- readIORef trace_on
     ; if b then traceF env f vars exp else f vars exp }

-------------------------------------------------------------
-- The top level functions
-- Runs

-- Runs an expression of type (IO a), and catches calls to error with "bad"
guard command bad =
  do possible <- Control.Exception.try command
     case possible of
       Right ans -> return ans
       Left err -> (bad (err::(Control.Exception.ErrorCall)))

              
runFile filename = 
  do { putStrLn "\n********** PARSING ******************"
     ; (source@(Prog ds exp)) <- parseFile progP filename  -- this is a *.e3 file
     ; putStrLn ("Program:\n" ++ show source)
     ; putStrLn "\n******** LOADING DEFS **************"
     ; (funs,vars,state) <- elaborate True ds (emptyE,emptyE,State [])
     ; putStrLn "\n\n********** EXECUTING BODY **********"
     ; (v,state2) <- interpE funs vars state exp
     ; putStrLn(showV (v,state2))
     ; putStrLn "\n*********** ENTERING READ EVAL PRINT ***************"    
     ; putStrLn "type ':q' to exit, type 'trace' to flip tracer state\n"
     ; let loop state = 
              do { putStrLn "enter Exp>"
                 ; str <- getLine
                 ; case str of
                     ":q" -> (error "Quitting")
                     "trace" -> do { trace; loop state}
                     other -> case parse1 ("keyboard input\n"++str) expP str of
                                Left message -> do { putStrLn("\n"++show message); loop state}
                                Right exp -> guard
                                  (do { (v,state2) <- interpE funs vars state exp
                                      ; putStrLn(showV (v,state2))
                                      ; loop state2 })
                                  (\ s -> do { putStrLn("\n"++show s); loop state})}               
     ; loop state2 }
