{
{-# OPTIONS -w #-}
module Language.Mizar.Parser( parseExp, readExp ) where

import Language.Mizar
import Language.Mizar.Lexer

}


-- The expression language used here comes straight from the happy
-- documentation with virtually no changes (open, so TokenOB/TokenCB were
-- changed to TokenLParen/TokenRParen

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { TokenEOF }
-- Without this we get a type error
%error { happyError }

%token
      let             { TokenLet }
      in              { TokenIn }
      int             { TokenInt $$ }
      var             { TokenVar $$ }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenLParen }
      ')'             { TokenRParen }

%%

Article   :  { Article [] [] }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap cont = do
  t <- alexMonadScan'
  cont t

-- We rewrite alexMonadScan' to return the position when lexing fails (the
-- default implementation just returns an error message).
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (pos, _, _, _) -> alexError (show pos)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

getPosn :: Alex (Int,Int)
getPosn = do
  (AlexPn _ l c,_,_,_) <- alexGetInput
  return (l,c)

happyError :: Token -> Alex a
happyError t = do
  (l,c) <- getPosn
  fail (show l ++ ":" ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n")

parseExp :: String -> Either String Article
parseExp s = runAlex s parse

readExp :: FilePath -> IO (Either String Article)
readExp fp = do
  cs <- readFile fp
  return (parseExp cs)
}
