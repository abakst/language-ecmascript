 -- | Parser for ECMAScript 3.
{-# LANGUAGE FlexibleContexts #-}
module Language.ECMAScript3.Parser
  (parse
  , parseScriptFromString
  , parseJavaScriptFromFile
  , parseScript
  , parseExpression
  , parseString
  , ParsedStatement
  , ParsedExpression
  , parseSimpleExpr'
  , parseBlockStmt
  , parseStatement
  , StatementParser
  , ExpressionParser
  , assignExpr
  -- debugging, remove the next 2 lines
  , mkDecimal
  , intLen
  , parseObjectLit  
  , Parser  
  , SourceSpan (..)
  ) where

import Language.ECMAScript3.Lexer hiding (identifier)
import qualified Language.ECMAScript3.Lexer as Lexer
import Language.ECMAScript3.Parser.State
import Language.ECMAScript3.Parser.Type
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Annotations
import Data.Default
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Control.Monad(liftM,liftM2,liftM3)
import Control.Monad.Trans (MonadIO,liftIO)
import Numeric(readDec,readOct,readHex)
import Data.Char
import Control.Monad.Identity
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Typeable
import Data.Generics hiding (Infix)

-- | Tag each entity with the span from the file from which it was parsed.

data SourceSpan      = Span { sp_begin :: !SourcePos
                            , sp_end   :: !SourcePos 
                            }
                       deriving (Eq, Ord, Show, Data, Typeable)

-- We parameterize the parse tree over source-locations.
type ParsedStatement = Statement SourceSpan
type ParsedExpression = Expression SourceSpan

-- These parsers can store some arbitrary state
type StatementParser s = Parser s ParsedStatement
type ExpressionParser s = Parser s ParsedExpression


initialParserState :: ParserState
initialParserState = []

-- | checks if the label is not yet on the stack, if it is -- throws
-- an error; otherwise it pushes it onto the stack
pushLabel :: String -> Parser s ()
pushLabel lab = do labs <- getState
                   pos <- getPosition
                   if lab `elem` labs 
                     then fail $ "Duplicate label at " ++ show pos
                     else putState (lab:labs)

popLabel :: Parser s ()
popLabel = modifyState safeTail
  where safeTail [] = []
        safeTail (_:xs) = xs

clearLabels :: ParserState -> ParserState
clearLabels _ = []

withFreshLabelStack :: Parser s a -> Parser s a
withFreshLabelStack p = do oldState <- getState
                           putState $ clearLabels oldState
                           a <- p
                           putState oldState
                           return a

identifier :: Stream s Identity Char => Parser s (Id SourceSpan)
identifier = withSpan Id Lexer.identifier 
  -- liftM2 Id getPosition Lexer.identifier

--{{{ Statements

-- Keep in mind that Token.reserved parsers (exported from the lexer) do not
-- consume any input on failure.  Note that all statements (expect for labelled
-- and expression statements) begin with a reserved-word.  If we fail to parse
-- this reserved-word, no input is consumed.  Hence, we can have the massive or
-- block that is parseExpression.  Note that if the reserved-word is parsed, it 
-- must be whatever statement the reserved-word indicates.  If we fail after the
-- reserved-word, we truly have a syntax error.  Since input has been consumed,
-- <|> will not try its alternate in parseExpression, and we will fail.

parseIfStmt:: Stream s Identity Char => StatementParser s 
parseIfStmt = do
  pos <- getPosition
  reserved "if"
  test <- parseParenExpr <?> "parenthesized test-expression in if statement"
  consequent <- parseStatement <?> "true-branch of if statement"
  pos'       <- getPosition 
  optional semi -- TODO: in spec?
  ((do reserved "else"
       alternate <- parseStatement
       pos''     <- getPosition 
       return $ IfStmt (Span pos pos'') test consequent alternate)
   <|> return (IfSingleStmt (Span pos pos') test consequent))

parseSwitchStmt :: Stream s Identity Char => StatementParser s
parseSwitchStmt =
  let parseDefault = do
        pos <- getPosition
        reserved "default"
        colon
        statements <- many parseStatement
        pos' <- getPosition
        return (CaseDefault (Span pos pos') statements)
      parseCase = do
         pos <- getPosition
         reserved "case"
         condition <- parseListExpr
         colon
         actions <- many parseStatement
         pos' <- getPosition 
         return (CaseClause (Span pos pos') condition actions)
      isCaseDefault (CaseDefault _ _) = True   
      isCaseDefault _                 = False
      checkClauses cs = case filter isCaseDefault cs of
        (_:c:_) -> fail $ "duplicate default clause in switch statement at " ++
                          show (getAnnotation c)
        _ -> return ()                  
    in do pos <- getPosition
          reserved "switch"
          test <- parseParenExpr
          clauses <- braces $ many $ parseDefault <|> parseCase
          checkClauses clauses
          pos' <- getPosition 
          return (SwitchStmt (Span pos pos') test clauses)

parseWhileStmt:: Stream s Identity Char => StatementParser s
parseWhileStmt = do
  pos <- getPosition
  reserved "while"
  test <- parseParenExpr <?> "parenthesized test-expression in while loop"
  body <- parseStatement
  pos' <- getPosition
  return (WhileStmt (Span pos pos') test body)

parseDoWhileStmt:: Stream s Identity Char => StatementParser s
parseDoWhileStmt = do
  pos <- getPosition
  reserved "do"
  body <- parseStatement
  reserved "while" <?> "while at the end of a do block"
  test <- parseParenExpr <?> "parenthesized test-expression in do loop"
  pos' <- getPosition
  optional semi
  return (DoWhileStmt (Span pos pos') body test)

parseContinueStmt:: Stream s Identity Char => StatementParser s
parseContinueStmt = do
  pos <- getPosition
  reserved "continue"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'continue.'
  id <- if sourceLine pos == sourceLine pos'
        then liftM Just identifier <|> return Nothing
        else return Nothing
  optional semi
  pos'' <- getPosition 
  return $ ContinueStmt (Span pos pos'') id

parseBreakStmt:: Stream s Identity Char => StatementParser s
parseBreakStmt = do
  pos <- getPosition
  reserved "break"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'break.'
  id <- if sourceLine pos == sourceLine pos'
        then liftM Just identifier <|> return Nothing
        else return Nothing
  optional semi           
  pos'' <- getPosition
  return $ BreakStmt (Span pos pos'') id

parseBlockStmt:: Stream s Identity Char => StatementParser s
parseBlockStmt = do
  pos <- getPosition
  statements <- braces (many parseStatement)
  pos' <- getPosition
  return (BlockStmt (Span pos pos') statements)

parseEmptyStmt:: Stream s Identity Char => StatementParser s
parseEmptyStmt = do
  pos <- getPosition
  semi
  pos' <- getPosition
  return (EmptyStmt (Span pos pos'))

parseLabelledStmt:: Stream s Identity Char => StatementParser s
parseLabelledStmt = do
  pos <- getPosition
  -- Lookahead for the colon.  If we don't see it, we are parsing an identifier
  -- for an expression statement.
  label <- try (do label <- identifier
                   colon
                   return label)
  pushLabel $ unId label
  statement <- parseStatement
  popLabel
  pos' <- getPosition
  return (LabelledStmt (Span pos pos') label statement)

parseExpressionStmt:: Stream s Identity Char => StatementParser s
parseExpressionStmt = do
  pos <- getPosition
  expr <- parseListExpr -- TODO: spec 12.4?
  optional semi
  pos' <- getPosition
  return $ ExprStmt (Span pos pos') expr


parseForInStmt:: Stream s Identity Char => StatementParser s
parseForInStmt =
  let parseInit = (reserved "var" >> liftM ForInVar identifier)
               <|> liftM ForInLVal lvalue
  in do pos <- getPosition
        -- Lookahead, so that we don't clash with parseForStmt
        (init,expr) <- try $ do reserved "for"
                                parens $ do init <- parseInit
                                            reserved "in"
                                            expr <- parseExpression
                                            return (init,expr)
        body <- parseStatement
        pos' <- getPosition
        return $ ForInStmt (Span pos pos') init expr body

parseForStmt:: Stream s Identity Char => StatementParser s
parseForStmt =
  let parseInit = (reserved "var" >> liftM VarInit (parseVarDecl `sepBy` comma))
               <|> liftM ExprInit parseListExpr
               <|> return NoInit
    in do pos <- getPosition
          reserved "for"
          reservedOp "("
          init <- parseInit
          semi
          test <- optionMaybe parseExpression
          semi
          iter <- optionMaybe parseListExpr
          reservedOp ")" <?> "closing paren"
          stmt <- parseStatement
          pos' <- getPosition
          return $ ForStmt (Span pos pos') init test iter stmt

parseTryStmt:: Stream s Identity Char => StatementParser s
parseTryStmt =
  let parseCatchClause = do pos <- getPosition
                            reserved "catch"
                            id <- parens identifier
                            stmt <- parseStatement
                            pos' <- getPosition
                            return $ CatchClause (Span pos pos') id stmt
  in do reserved "try"
        pos <- getPosition
        guarded <- parseStatement
        mCatch <- optionMaybe parseCatchClause
        mFinally <- optionMaybe $ reserved "finally" >> parseStatement
        pos' <- getPosition
        -- the spec requires at least a catch or a finally block to
        -- be present
        if isJust mCatch || isJust mFinally 
          then return $ TryStmt (Span pos pos') guarded mCatch mFinally
          else fail $ "A try statement should have at least a catch\ 
                      \ or a finally block, at " ++ show pos

parseThrowStmt:: Stream s Identity Char => StatementParser s
parseThrowStmt = do
  pos <- getPosition
  reserved "throw"
  expr <- parseExpression
  optional semi
  pos' <- getPosition
  return (ThrowStmt (Span pos pos') expr)

parseReturnStmt:: Stream s Identity Char => StatementParser s
parseReturnStmt = do
  pos <- getPosition
  reserved "return"
  expr <- optionMaybe parseListExpr
  optional semi
  pos' <- getPosition
  return (ReturnStmt (Span pos pos') expr)

parseWithStmt:: Stream s Identity Char => StatementParser s
parseWithStmt = do
  pos <- getPosition
  reserved "with"
  context <- parseParenExpr
  stmt <- parseStatement
  pos' <- getPosition
  return (WithStmt (Span pos pos') context stmt)

parseVarDecl :: Stream s Identity Char => Parser s (VarDecl SourceSpan) 
parseVarDecl = do
  pos <- getPosition
  id <- identifier
  init <- (reservedOp "=" >> liftM Just parseExpression) <|> return Nothing
  pos' <- getPosition
  return (VarDecl (Span pos pos') id init)

parseVarDeclStmt:: Stream s Identity Char => StatementParser s
parseVarDeclStmt = do 
  pos <- getPosition
  reserved "var"
  decls <- parseVarDecl `sepBy` comma
  optional semi
  pos' <- getPosition
  return (VarDeclStmt (Span pos pos') decls)

parseFunctionStmt:: Stream s Identity Char => StatementParser s
parseFunctionStmt = do
  pos <- getPosition
  name <- try (reserved "function" >> identifier) -- ambiguity with FuncExpr
  args <- parens (identifier `sepBy` comma)
  -- label sets don't cross function boundaries
  BlockStmt _ body <- withFreshLabelStack parseBlockStmt <?> 
                      "function body in { ... }"
  pos' <- getPosition
  return (FunctionStmt (Span pos pos') name args body)

parseStatement:: Stream s Identity Char => StatementParser s
parseStatement = parseIfStmt <|> parseSwitchStmt <|> parseWhileStmt 
  <|> parseDoWhileStmt <|> parseContinueStmt <|> parseBreakStmt 
  <|> parseBlockStmt <|> parseEmptyStmt <|> parseForInStmt <|> parseForStmt
  <|> parseTryStmt <|> parseThrowStmt <|> parseReturnStmt <|> parseWithStmt 
  <|> parseVarDeclStmt  <|> parseFunctionStmt
  -- labelled, expression and the error message always go last, in this order
  <|> parseLabelledStmt <|> parseExpressionStmt <?> "statement"

--}}}

--{{{ Expressions

-- References used to construct this stuff:
-- + http://developer.mozilla.org/en/docs/
--     Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
-- + http://www.mozilla.org/js/language/grammar14.html
--
-- Aren't expression tables nice?  Well, we can't quite use them, because of 
-- JavaScript's ternary (?:) operator.  We have to use two expression tables.
-- We use one expression table for the assignment operators that bind looser 
-- than ?: (assignTable).  The terms of assignTable are ternary expressions 
-- (parseTernaryExpr).  parseTernaryExpr left-factors the left-recursive
-- production for ?:, and is defined over the second expression table, 
-- exprTable, which consists of operators that bind tighter than ?:.  The terms
-- of exprTable are atomic expressions, parenthesized expressions, functions and
-- array references.

--{{{ Primary expressions

parseThisRef:: Stream s Identity Char => ExpressionParser s
parseThisRef = do
  pos <- getPosition
  reserved "this"
  pos' <- getPosition
  return (ThisRef (Span pos pos'))

parseNullLit:: Stream s Identity Char => ExpressionParser s
parseNullLit = do
  pos <- getPosition
  reserved "null"
  pos' <- getPosition
  return (NullLit (Span pos pos'))


parseBoolLit:: Stream s Identity Char => ExpressionParser s
parseBoolLit = do
    pos <- getPosition
    let parseTrueLit  = reserved "true"  >> getPosition >>= \pos' -> return (BoolLit (Span pos pos') True)
        parseFalseLit = reserved "false" >> getPosition >>= \pos' -> return (BoolLit (Span pos pos') False)
    parseTrueLit <|> parseFalseLit

parseVarRef:: Stream s Identity Char => ExpressionParser s
parseVarRef = withSpan VarRef identifier 

parseArrayLit:: Stream s Identity Char => ExpressionParser s
parseArrayLit = withSpan ArrayLit (squares (parseExpression `sepEndBy` comma))

parseFuncExpr :: Stream s Identity Char => ExpressionParser s
parseFuncExpr = do
  pos <- getPosition
  reserved "function"
  name <- optionMaybe identifier
  args <- parens (identifier `sepBy` comma)
  -- labels don't cross function boundaries
  BlockStmt _ body <- withFreshLabelStack parseBlockStmt
  pos' <- getPosition
  return $ FuncExpr (Span pos pos') name args body

--{{{ parsing strings

escapeChars =
 [('\'','\''),('\"','\"'),('\\','\\'),('b','\b'),('f','\f'),('n','\n'),
  ('r','\r'),('t','\t'),('v','\v'),('/','/'),(' ',' '),('0','\0')]

allEscapes:: String
allEscapes = map fst escapeChars

parseEscapeChar :: Stream s Identity Char => Parser s Char
parseEscapeChar = do
  c <- oneOf allEscapes
  let (Just c') = lookup c escapeChars -- will succeed due to line above
  return c' 

parseAsciiHexChar :: Stream s Identity Char => Parser s Char
parseAsciiHexChar = do
  char 'x'
  d1 <- hexDigit
  d2 <- hexDigit
  return ((chr.fst.head.readHex) (d1:d2:""))

parseUnicodeHexChar :: Stream s Identity Char => Parser s Char
parseUnicodeHexChar = do
  char 'u'
  liftM (chr.fst.head.readHex) 
        (sequence [hexDigit,hexDigit,hexDigit,hexDigit])
        
isWhitespace ch = ch `elem` " \t"


-- The endWith argument is either single-quote or double-quote, depending on how
-- we opened the string.
parseStringLit' endWith =
  (char endWith >> return "") <|>
  (do try (string "\\'")
      cs <- parseStringLit' endWith
      return $ "'" ++ cs) <|>
  (do char '\\'
      c <- parseEscapeChar <|> parseAsciiHexChar <|> parseUnicodeHexChar <|> 
           char '\r' <|> char '\n'
      cs <- parseStringLit' endWith
      if c == '\r' || c == '\n' 
        then return (c:dropWhile isWhitespace cs) 
        else return (c:cs)) <|>
   liftM2 (:) anyChar (parseStringLit' endWith)

parseStringLit:: Stream s Identity Char => ExpressionParser s
parseStringLit = do
  pos <- getPosition
  -- parseStringLit' takes as an argument the quote-character that opened the
  -- string.
  str <- lexeme $ (char '\'' >>= parseStringLit') <|> (char '\"' >>= parseStringLit')
  -- CRUCIAL: Parsec.Token parsers expect to find their token on the first
  -- character, and read whitespaces beyond their tokens.  Without 'lexeme'
  -- above, expressions like:
  --   var s = "string"   ;
  -- do not parse.
  pos' <- getPosition
  return $ StringLit (Span pos pos') str

--}}}

parseRegexpLit:: Stream s Identity Char => ExpressionParser s
parseRegexpLit = do
  let parseFlags = do
        flags <- many (oneOf "mgi")
        return $ \f -> f ('g' `elem` flags) ('i' `elem` flags) 
  let parseEscape :: Stream s Identity Char => Parser s Char
      parseEscape = char '\\' >> anyChar
  let parseChar :: Stream s Identity Char => Parser s Char
      parseChar = noneOf "/"
  let parseRe = (char '/' >> return "") <|> 
                (do char '\\'
                    ch <- anyChar -- TODO: too lenient
                    rest <- parseRe
                    return ('\\':ch:rest)) <|> 
                liftM2 (:) anyChar parseRe
  pos <- getPosition
  char '/'
  notFollowedBy $ char '/'
  pat <- parseRe --many1 parseChar
  flags <- parseFlags
  spaces -- crucial for Parsec.Token parsers
  pos' <- getPosition
  return $ flags (RegexpLit (Span pos pos') pat)
          
parseObjectLit:: Stream s Identity Char => ExpressionParser s
parseObjectLit =
  let parseProp = do
        -- Parses a string, identifier or integer as the property name.  I
        -- apologize for the abstruse style, but it really does make the code
        -- much shorter.
        name <- liftM (\(StringLit p s) -> PropString p s) parseStringLit
            <|> withSpan PropId  identifier
            <|> withSpan PropNum decimal
        colon
        val <- assignExpr
        return (name,val)
    in do pos <- getPosition
          props <- braces (parseProp `sepEndBy` comma) <?> "object literal"
          pos' <- getPosition
          return $ ObjectLit (Span pos pos') props

--{{{ Parsing numbers.  From pg. 17-18 of ECMA-262.
hexLit :: Stream s Identity Char => Parser s (Bool, Double)
hexLit = do
  try (string "0x")
  digits <- many1 (oneOf "0123456789abcdefABCDEF")
  [(hex,"")] <- return $ Numeric.readHex digits
  return (True, hex)

-- | Creates a decimal value from a whole, fractional and exponent part.
mkDecimal :: Integer -> Integer -> Integer -> Integer -> Double
mkDecimal whole frac fracLen exp = 
  ((fromInteger whole) + ((fromInteger frac) * (10 ^^ (-fracLen)))) * (10 ^^ exp)

exponentPart :: Stream s Identity Char => Parser s Integer
exponentPart = do
  oneOf "eE"
  (char '+' >> decimal) <|> (char '-' >> negate `fmap` decimal) <|> decimal

--wrap a parser's result in a Just:
jparser :: Stream s Identity Char => Parser s a -> Parser s (Maybe a)
jparser = liftM Just

decLit :: Stream s Identity Char => Parser s (Bool, Double)
decLit = 
  (do whole <- decimal
      mfrac <- option Nothing (jparser (char '.' >> decimal))
      mexp <-  option Nothing (jparser exponentPart)
      if isNothing mfrac && isNothing mexp
        then return (True, fromIntegral whole)
        else let frac = fromIntegral (fromMaybe 0 mfrac)
             in  return (False, mkDecimal (fromIntegral whole) frac 
                                          (intLen frac)
                                          (fromIntegral (fromMaybe 0 mexp))))
  <|>
  (do frac <- char '.' >> decimal
      exp <- option 0 exponentPart
      let ifrac = fromIntegral frac
      return (False, mkDecimal 0 ifrac (intLen frac) (fromIntegral exp)))

intLen i | i `div` 10 < 1 = 1
intLen i | otherwise = 1 + intLen (i `div` 10)

parseNumLit:: Stream s Identity Char => ExpressionParser s
parseNumLit = do
    pos <- getPosition
    (isint, num) <- lexeme $ hexLit <|> decLit
    notFollowedBy identifierStart <?> "whitespace"
    pos' <- getPosition
    if isint
      then return $ IntLit (Span pos pos') (round num) 
      else return $ NumLit (Span pos pos') num


------------------------------------------------------------------------------
-- Position Helper
------------------------------------------------------------------------------

withPos cstr p = do { pos <- getPosition; e <- p; return $ cstr pos e }

withSpan cstr p = do pos   <- getPosition
                     x     <- p
                     pos'  <- getPosition
                     return $ cstr (Span pos pos') x

-------------------------------------------------------------------------------
-- Compound Expression Parsers
-------------------------------------------------------------------------------

dotRef e = (reservedOp "." >> withSpan cstr identifier) <?> "property.ref"
    where cstr pos = DotRef pos e

funcApp e = parens (withSpan cstr (parseExpression `sepBy` comma)) 
         <?>"(function application)"
    where cstr pos = CallExpr pos e

bracketRef e = brackets (withSpan cstr parseExpression) <?> "[property-ref]"
    where cstr pos = BracketRef pos e

-------------------------------------------------------------------------------
-- Expression Parsers
-------------------------------------------------------------------------------

parseParenExpr:: Stream s Identity Char => ExpressionParser s
parseParenExpr = parens parseListExpr

-- everything above expect functions
parseExprForNew :: Stream s Identity Char => ExpressionParser s
parseExprForNew = parseThisRef <|> parseNullLit <|> parseBoolLit <|> parseStringLit 
  <|> parseArrayLit <|> parseParenExpr <|> parseNewExpr <|> parseNumLit 
  <|> parseRegexpLit <|> parseObjectLit <|> parseVarRef

-- all the expression parsers defined above
parseSimpleExpr' :: Stream s Identity Char => ExpressionParser s
parseSimpleExpr' = parseThisRef <|> parseNullLit <|> parseBoolLit 
  <|> parseStringLit <|> parseArrayLit <|> parseParenExpr
  <|> parseFuncExpr <|> parseNumLit <|> parseRegexpLit <|> parseObjectLit
  <|> parseVarRef

parseNewExpr :: Stream s Identity Char => ExpressionParser s
parseNewExpr =
  (do pos <- getPosition
      reserved "new"
      constructor <- parseSimpleExprForNew Nothing -- right-associativity
      arguments <- try (parens (parseExpression `sepBy` comma)) <|> return []
      pos' <- getPosition
      return (NewExpr (Span pos pos') constructor arguments)) <|>
  parseSimpleExpr'

parseSimpleExpr (Just e) = ((dotRef e <|> funcApp e <|> bracketRef e) >>=
                            parseSimpleExpr . Just)  
                        <|> return e
parseSimpleExpr Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExpr (Just e)

parseSimpleExprForNew :: Stream s Identity Char
                      =>(Maybe ParsedExpression) -> ExpressionParser s
parseSimpleExprForNew (Just e) = ((dotRef e <|> bracketRef e) >>=
                                  parseSimpleExprForNew . Just)
                              <|> return e
parseSimpleExprForNew Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExprForNew (Just e)
    
--}}}

makeInfixExpr str constr = Infix parser AssocLeft where
  parser:: Stream s Identity Char
        => Parser s (Expression SourceSpan -> Expression SourceSpan -> Expression SourceSpan)
  parser = do
    pos <- getPosition
    reservedOp str
    pos' <- getPosition
    return (InfixExpr (Span pos pos') constr)  -- points-free, returns a function


-- apparently, expression tables can't handle immediately-nested prefixes
parsePrefixedExpr :: Stream s Identity Char => ExpressionParser s
parsePrefixedExpr = do
  pos <- getPosition
  op <- optionMaybe $ (reservedOp "!" >> return PrefixLNot) <|> 
                      (reservedOp "~" >> return PrefixBNot) <|>
                      (try (lexeme $ char '-' >> notFollowedBy (char '-')) >>
                       return PrefixMinus) <|>
                      (try (lexeme $ char '+' >> notFollowedBy (char '+')) >>
                       return PrefixPlus) <|>
                      (reserved "typeof" >> return PrefixTypeof) <|>
                      (reserved "void" >> return PrefixVoid) <|>
                      (reserved "delete" >> return PrefixDelete)
  case op of
    Nothing -> unaryAssignExpr
    Just op -> do
      innerExpr <- parsePrefixedExpr
      pos'      <- getPosition
      return (PrefixExpr (Span pos pos') op innerExpr)

exprTable:: Stream s Identity Char => [[Operator s ParserState Identity ParsedExpression]]
exprTable = 
  [ [ makeInfixExpr "*" OpMul
    , makeInfixExpr "/" OpDiv
    , makeInfixExpr "%" OpMod
    ]
  , [ makeInfixExpr "+" OpAdd
    , makeInfixExpr "-" OpSub
    ]
  , [ makeInfixExpr "<<" OpLShift
    , makeInfixExpr ">>" OpSpRShift
    , makeInfixExpr ">>>" OpZfRShift
    ]
  , [ makeInfixExpr "<" OpLT
    , makeInfixExpr "<=" OpLEq
    , makeInfixExpr ">" OpGT
    , makeInfixExpr ">=" OpGEq
    , makeInfixExpr "instanceof" OpInstanceof
    , makeInfixExpr "in" OpIn
    ]
  , [ makeInfixExpr "==" OpEq
    , makeInfixExpr "!=" OpNEq
    , makeInfixExpr "===" OpStrictEq
    , makeInfixExpr "!==" OpStrictNEq
    ]
  , [ makeInfixExpr "&" OpBAnd ]
  , [ makeInfixExpr "^" OpBXor ]
  , [ makeInfixExpr "|" OpBOr ]
  , [ makeInfixExpr "&&" OpLAnd ]
  , [ makeInfixExpr "||" OpLOr ]
  ]

parseExpression' :: Stream s Identity Char => ExpressionParser s
parseExpression' = 
  buildExpressionParser exprTable parsePrefixedExpr <?> "simple expression"

asLValue :: Stream s Identity Char
         => SourcePos
         -> Expression SourceSpan
         -> Parser s (LValue SourceSpan)
asLValue p' e = case e of
  VarRef p (Id _ x) -> return (LVar p x)
  DotRef p e (Id _ x) -> return (LDot p e x)
  BracketRef p e1 e2 -> return (LBracket p e1 e2)
  otherwise -> fail $ "expected a left-value at " ++ show p'

lvalue :: Stream s Identity Char => Parser s (LValue SourceSpan)
lvalue = do
  p <- getPosition
  e <- parseSimpleExpr Nothing
  asLValue p e

unaryAssignExpr :: Stream s Identity Char => ExpressionParser s
unaryAssignExpr = do
  p <- getPosition
  let prefixInc = do
        reservedOp "++"
        liftM2 (\x p' -> UnaryAssignExpr (Span p p') PrefixInc x) lvalue getPosition
  let prefixDec = do
        reservedOp "--"
        liftM2 (\x p' -> UnaryAssignExpr (Span p p') PrefixDec x) lvalue getPosition
  let postfixInc e = do
        reservedOp "++"
        liftM2 (\x p' -> UnaryAssignExpr (Span p p') PostfixInc x) (asLValue p e) getPosition
  let postfixDec e = do
        reservedOp "--"
        liftM2 (\x p' -> UnaryAssignExpr (Span p p') PostfixDec x) (asLValue p e) getPosition
  let other = do
        e <- parseSimpleExpr Nothing
        postfixInc e <|> postfixDec e <|> return e
  prefixInc <|> prefixDec <|> other

parseTernaryExpr':: Stream s Identity Char
                 => Parser s (ParsedExpression,ParsedExpression)
parseTernaryExpr' = do
    reservedOp "?"
    l <- assignExpr
    colon
    r <- assignExpr
    return (l,r)

parseTernaryExpr:: Stream s Identity Char => ExpressionParser s
parseTernaryExpr = do
  p <- getPosition
  e <- parseExpression'
  e' <- optionMaybe parseTernaryExpr'
  case e' of
    Nothing -> return e
    Just (l,r) -> do p' <- getPosition
                     return $ CondExpr (Span p p') e l r

assignOp :: Stream s Identity Char => Parser s AssignOp
assignOp = (reservedOp "=" >> return OpAssign)
        <|>(reservedOp "+=" >> return OpAssignAdd)
        <|>(reservedOp "-=" >> return OpAssignSub)
        <|>(reservedOp "*=" >> return OpAssignMul)
        <|>(reservedOp "/=" >> return OpAssignDiv)
        <|>(reservedOp "%=" >> return OpAssignMod)
        <|>(reservedOp "<<=" >> return OpAssignLShift)
        <|>(reservedOp ">>=" >> return OpAssignSpRShift)
        <|>(reservedOp ">>>=" >> return OpAssignZfRShift)
        <|>(reservedOp "&=" >> return OpAssignBAnd)
        <|>(reservedOp "^=" >> return OpAssignBXor)
        <|>(reservedOp "|=" >> return OpAssignBOr)

assignExpr :: Stream s Identity Char => ExpressionParser s
assignExpr = do
  p <- getPosition
  lhs <- parseTernaryExpr
  let assign = do
        op <- assignOp
        lhs <- asLValue p lhs
        rhs <- assignExpr
        p' <- getPosition
        return (AssignExpr (Span p p') op lhs rhs)
  assign <|> return lhs

parseExpression:: Stream s Identity Char => ExpressionParser s
parseExpression = assignExpr

parseListExpr :: Stream s Identity Char => ExpressionParser s
-- parseListExpr = assignExpr `sepBy1` comma >>= \exprs ->
--   case exprs of
--     [expr] -> return expr
--     es     -> liftM2 ListExpr getPosition (return es)

parseListExpr 
  = do pos   <- getPosition
       exprs <- assignExpr `sepBy1` comma
       pos'  <- getPosition 
       case exprs of
         [expr] -> return   expr
         es     -> return $ ListExpr (Span pos pos') es



parseScript:: Stream s Identity Char => Parser s (JavaScript SourceSpan)
parseScript = do
  whiteSpace
  withSpan Script (parseStatement `sepBy` whiteSpace)
  
-- | Parse from a stream; same as 'Text.Parsec.parse'
parse :: Stream s Identity Char
      => Parser s a -- ^ The parser to use
      -> SourceName -- ^ Name of the source file
      -> s -- ^ the stream to parse, usually a 'String'
      -> Either ParseError a
parse p = runParser p initialParserState

-- | Read a JavaScript program from file an parse it into a list of
-- statements
parseJavaScriptFromFile :: MonadIO m => String -- ^ file name
                        -> m [Statement SourceSpan]
parseJavaScriptFromFile filename = do
  chars <- liftIO $ readFile filename
  case parse parseScript filename chars of
    Left err               -> fail (show err)
    Right (Script _ stmts) -> return stmts

-- | Parse a JavaScript program from a string
parseScriptFromString :: String -- ^ source file name
                      -> String -- ^ JavaScript source to parse
                      -> Either ParseError (JavaScript SourceSpan)
parseScriptFromString = parse parseScript

-- | Parse a JavaScript source string into a list of statements
parseString :: String -- ^ JavaScript source
            -> [Statement SourceSpan]
parseString str = case parse parseScript "" str of
  Left err -> error (show err)
  Right (Script _ stmts) -> stmts
