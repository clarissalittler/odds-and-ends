{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Parser where

import qualified System.IO
import Data.Char(digitToInt,isUpper,isLower)
import GHC.Float(double2Float)

-- These are for defining parsers
import Text.Parsec hiding (State)
import Text.Parsec.Expr(Operator(..),Assoc(..),buildExpressionParser)
-- Replaces Text.Parsec.Token
import qualified LayoutToken as Token

-- This is for possible Monads underlying the Parsing Monad
import Control.Monad.State
import Data.Functor.Identity(Identity(..))

-- This is to catch errors when Parsing
import qualified Control.Exception

import Debug.Trace


-- import the Hughes Pretty Printing library qualified
import qualified Text.PrettyPrint.HughesPJ as PP
-- import a few widely used operations without qualification
import Text.PrettyPrint.HughesPJ(Doc,text,int,float,hang,sep,vcat,comma,(<>),(<+>),($$),($+$),render,
                                parens,punctuate)


-----------------------------------------------
-- running parsers

-- Extract a computation from the Parser Monad
runMParser parser parserState name tokens =
  runIdentity (runParserT parser parserState name tokens)

-- Skip whitespace before you begin
parse1 file x = runMParser (whiteSp >> x) initColumns file

-- Raise an Haskell error if a parsing error occurs
parseWithName file x s =
  case parse1 file x s of
   Right(ans) -> ans
   Left message -> error (show message)

-- Parse with a default name for the input
parse2 x s = parseWithName "keyboard input" x s

-- Parse and return the internal state
parse3 p s = putStrLn (show state) >> return object
  where (object,state) = parse2 (do { x <- p; st <- getState; return(x,st)}) s

-- Parse an t-object, return
-- (t,rest-of-input-not-parsed)
parse4 p s =
   parse2 (do { x <- p
              ; rest <- getInput
              ; return (x,rest)}) s

-- Parse a string in an arbitray monad
parseString x s =
  case parse1 s x s of
   Right(ans) -> return ans
   Left message -> fail (show message)

-- Parse a File in the IO monad
parseFile parser file =
    do possible <- Control.Exception.try (readFile file)
       case possible of
         Right contents ->
            case parse1 file parser contents of
              Right ans -> return ans
              Left message -> error(show message)
         Left err -> error(show (err::IOError))

--------------------------------------------
-- A parser with internal state of a list of columns
-- use (updateState,setState,getState) to access the [Column]

-- for debugging only
traceP p = do { ((c,vs),_) <- getState; ans <- p; ((d,us),_) <- getState
              ; trace ("In  "++show c++"\nOut "++show d) (return ans)}

initColumns = []

type MParser a = ParsecT
                    String                -- The input is a sequence of Char
                    [Column]              -- The internal state for Layout tabs
                    (Identity)            -- The underlying monad is simple
                    a                     -- the type of the object being parsed

-- Based on Parsec's haskellStyle (which we can not use directly since
-- Parsec gives it a too specific type).

lbStyle = Token.LanguageDef
                { Token.commentStart   = "{-"
                , Token.commentEnd     = "-}"
                , Token.commentLine    = "--"
                , Token.nestedComments = True
                , Token.identStart     = lower
                , Token.identLetter    = alphaNum <|> oneOf "_'"
                , Token.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , Token.caseSensitive  = True
                , Token.reservedOpNames =
                    ["!","?","\\",":",".", "<", "=", "+", "-", "^", "()", "_", "@"]
                , Token.reservedNames  =
                  ["if","then","else","case","of","let","in"]
                }


(haskellTP,Token.LayFun layout) = Token.makeTokenParser lbStyle "{" ";" "}"

lexemE p    = Token.lexeme haskellTP p
arrow       = lexemE(string "->")
larrow      = lexemE(string "<-")
dot         = lexemE(char '.')
parenS p    = between (symboL "(") (symboL ")") p
braceS p    = between (symboL "{") (symboL "}") p
bracketS p  = between (symboL "[") (symboL "]") p
symboL      = Token.symbol haskellTP
natural     = lexemE(number 10 digit)
whiteSp     = Token.whiteSpace haskellTP
idenT       = Token.identifier haskellTP
keyworD     = Token.reserved haskellTP
commA       = Token.comma haskellTP
resOp       = Token.reservedOp haskellTP
opeR        = Token.operator haskellTP
character c = lexemE(char c)


---------------------------------------------------------------

number ::  Integer -> MParser Char -> MParser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

signed p = do { f <- sign; n <- p ; return(f n)}
  where sign = (character '-' >> return (* (-1))) <|>
               (character '+' >> return id) <|>
               (return id)

int32:: MParser Int
int32 = do { n <- signed natural; return(fromInteger n)}

float32:: MParser Float
float32 = do { n <- Token.float haskellTP ; return(double2Float n)}

letParser declP expP =
  do { pos <- getPosition   -- This gets the SourcePos
     ; keyworD "let"
     ; ds <- layout declP (keyworD "in")
     ; exp <- expP
     ; return(ds,exp)}


------------------------------------------------------------------

type Var = (SourcePos,String)  -- Starts lowercase
type Con = (SourcePos,String)  -- Starts with Upper case
type Name = (SourcePos,String) -- Starts with either lower case or uppercase

data Lit
  = Int Int
  | Char Char
  | Unit
  | Float Float

data Pat
  = Plit Lit                  -- { 5 or 'c' }
  | Pvar Var                  -- { x }
  | Pprod [Pat]               -- { (p1,p2,p3) }
  | Paspat Var Pat            -- { x @ p }
  | Pwild                     -- { _ }
  | Pcon Con [Pat]            -- C x y (z,a)


data Exp
  = Var Name                   -- { x  or  Nil }
  | Lit Lit                   -- { 5 or 'c'  or  5.6 }
  | Prod [Exp]                -- { (e1,e2,e3) }
  | App Exp Exp               -- { f x }
  | Lam [Pat] Exp             -- { \ p1 p2 -> e }
  | Let [Dec] Exp             -- { let { x=e1;   y=e2 } in e3 }
  | Case Exp [Match Pat Exp Dec]    -- { case e of { m1; m2 }}
  | Do [Stmt Pat Exp Dec]           -- { do { p <- e1; e2 } }

-- Let, Case, Do, all use layout

type Match p e d = (SourcePos,p,Body e,[d]) -- case e of { p -> b where decs }

data Body e
  = Guarded [(e,e)]           -- f p { | e1 = e2 | e3 = e4 } where ds
  | Normal e                  -- f p = { e } where ds
                              -- Where uses layout

data Stmt p e d
  = BindSt SourcePos p e
  | LetSt SourcePos [d]
  | NoBindSt SourcePos e

data Dec
  = Fun SourcePos Var [Match [Pat] Exp Dec]   -- { f p1 p2 = b where decs }
  | Val SourcePos Pat (Body Exp) [Dec]        -- { p = b where decs }


------------------------------
-- Parsing code here

tryChoice = choice . map try

parseIden pred = do
  x <- idenT
  if (pred (head x)) 
  then do
    p <- getPosition
    return $ (p,x)
  else fail "wrong type of identifier"

parseVar = parseIden isLower
parseCon = parseIden isUpper
parseName = parseIden (const True)

parseLit = tryChoice [parseFloat, parseInt, parseChar, parseUnit]

parseInt = fmap Int int32
parseFloat = fmap Float float32
parseChar = do
  char '\''
  c <- anyChar
  char '\''
  return $ Char c
parseUnit = do
  char '('
  char ')'
  return Unit


-- PATTERNS
parsePat = pLit <|> pVar <|> pProd <|> pAs <|> pWild <|> pCon

pLit = fmap Plit parseLit
pVar = fmap Pvar parseVar 
pProd = fmap Pprod $ parenS (sepBy1 parsePat commA)

pAs = do
  v <- parseVar
  symboL "@"
  p <- parenS parsePat
  return $ Paspat v p
pWild = symboL "_" >> return Pwild
pCon = parenS $ do
         c <- parseCon
         ps <- many1 parsePat
         return $ Pcon c ps

-- STATEMENTS
parseStmt ppat pexp pdecl = bindSt ppat pexp <|> letSt pdecl <|> noBind pexp

bindSt ppat pexp = do
  pos <- getPosition
  p <- ppat
  e <- pexp
  return $ BindSt pos p e

letSt pdecl = do
  pos <- getPosition
  keyworD "let"
  ds <- layout pdecl (return ())
  return $ LetSt pos ds

noBind pexp = do
  pos <- getPosition
  e <- pexp
  return $ NoBindSt pos e

-- WHERE

parseWhere pdecl = do
  keyworD "where"
  layout pdecl (return ())

-- MATCH

parseMatch ppat pexp pdecl sep = do
  pos <- getPosition
  p <- ppat
  sep
  b <- parseBody pexp
  ds <- parseWhere pdecl
  return $ (pos,p,b,ds)

-- BODY
parseBody pexp = pGuarded pexp <|> (fmap Normal pexp)

guardedAux pexp = do
  symboL "|"
  e1 <- pexp
  symboL "="
  e2 <- pexp
  return $ (e1,e2)

pGuarded pexp = fmap Guarded $ layout (guardedAux pexp) (return ())
 
-- EXPRESSIONS

parseExp = pEName <|> pELit <|> pEProd <|> pEApp <|> pELam <|> pELet <|> pECase <|> pEDo

pEName = fmap Var parseName
pELit = fmap Lit parseLit
pEProd = fmap Prod $ parenS (sepBy1 parseExp commA)
pEApp = do -- this probably won't work because of left descent, but we'll leave it at the moment
  f <- parseExp
  a <- parseExp
  return $ App f a
pELam = do
  symboL "\\"
  ps <- many1 parsePat
  arrow
  e <- parseExp
  return $ Lam ps e
pELet = do
  (ds,e) <- letParser parseDecl parseExp
  return $ Let ds e
pECase = do
  keyworD "case"
  e <- parseExp
  keyworD "of"
  ms <- layout (parseMatch parsePat parseExp parseDecl arrow) (return ())
  return $ Case e ms
pEDo = do
  keyworD "do"
  sts <- layout (parseStmt parsePat parseExp parseDecl) (return ())
  return $ Do sts

-- DECLARATIONS
parseDecl = funDecl <|> valDecl

funEq f = do
  f' <- parseVar
  if f == f'
  then do
    many1 parsePat
  else fail "not matching function definitions"

funDecl = do
  pos <- getPosition
  f <- parseVar
  m <- parseMatch (many1 parsePat) parseExp parseDecl (symboL "=")
  ms <- many $ parseMatch (funEq f) parseExp parseDecl (symboL "=")
  return $ Fun pos f (m : ms)

valDecl = do
  pos <- getPosition
  p <- parsePat
  symboL "="
  b <- parseBody parseExp
  ds <- parseWhere parseDecl
  return $ Val pos p b ds

---------------------------------
-- Pretty printing code here


ppDec:: Dec -> Doc
ppDec = undefined

ppLit:: Lit -> Doc
ppLit (Int i) = int i
ppLit (Char c) = text $ "'c'"
ppLit Unit = text "()"
ppLit (Float f) = float f

ppExp:: Exp -> Doc
ppExp (Var n) = text (snd n)
ppExp (Lit l) = ppLit l
ppExp (Prod es) = parens $ ppCommaSep $ map ppExp es
ppExp (App f a) = ppExp f <+> ppExp a
ppExp (Lam ps e) = (text "\\ ") <> sep (map ppPat ps) <+> text "->" <+> ppExp e
ppExp (Let ds e) = hang (text "let") 4 (vcat $ map ppDec ds) $$ text "in" <+> ppExp e
ppExp (Case e ms) = hang (text "case" <+> ppExp e <+> text "of") 4 (vcat (map (ppMatch "->") ms))
ppExp (Do stms) = hang (text "do") 4 (vcat $ map (ppStmt ppPat ppExp ppDec) stms)

ppCommaSep = sep . (punctuate comma)

ppMatch = undefined

ppPat:: Pat -> Doc
ppPat (Plit l) = ppLit l
ppPat (Pvar v) = text (snd v)
ppPat (Pprod vs) = parens $ ppCommaSep $ map ppPat vs
ppPat (Paspat v p) = text (snd v) <> text "@" <> parens (ppPat p)
ppPat Pwild = text "_"
ppPat (Pcon c ps) = text (snd c) <+> sep (map ppPat ps)

ppBody:: (e -> Doc) -> Body e -> Doc
ppBody = undefined


ppStmt :: (p -> Doc) -> (e -> Doc) -> (d -> Doc) -> Stmt p e d -> Doc
ppStmt = undefined


ppProg :: [Dec] -> Doc
ppProg = undefined



--------------------------------
-- The main functions here


main :: IO()
main = undefined


test:: IO()
test = undefined
