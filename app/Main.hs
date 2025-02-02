{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO
import qualified RIO.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import qualified Data.HashTable.IO as H
import qualified Text.Megaparsec.Debug as D

data Expr a where
   Lit :: Double-> Expr Double
   Add :: Fractional a=>(Expr a)->(Expr a) ->Expr a
   Sub :: Fractional a=>(Expr a)->(Expr a) ->Expr a
   Mul :: Fractional a=>(Expr a)->(Expr a) ->Expr a
   Div :: Fractional a=>(Expr a)->(Expr a) ->Expr a
   Assign :: T.Text->Expr a-> Expr a
   Var :: T.Text -> Expr a
   SeqOp :: [Expr a]->Expr a

type HashTable k v = H.CuckooHashTable k v

data App = App {
                 symbolTable:: HashTable T.Text Double
               }

eval :: Expr a -> RIO App Double
eval (Lit x) = return x
eval (Add x1 x2) = do 
                     x1'<-eval x1
                     x2'<-eval x2
                     return (x1'+x2')

eval (Sub x1 x2) = do
                     x1'<-eval x1
                     x2'<-eval x2
                     return (x1'-x2')

eval (Mul x1 x2) = do 
                    x1'<-eval x1
                    x2'<-eval x2
                    return (x1'*x2')

eval (Div x1 x2) = do
                    x1'<-eval x1
                    x2'<-eval x2
                    return (x1'/x2')
eval (Assign varname x1) = do
                            x1'<-eval x1
                            App symbTab <-ask
                            liftIO $ H.insert symbTab varname x1'
                            return x1'
eval (Var varname) = do
                      App symbTab<-ask
                      v<- liftIO $ H.lookup symbTab varname
                      case v of
                        Just x    -> return x
                        Nothing   -> error "Variable not found"

eval (SeqOp listOfOp) = case listOfOp of
                             []     -> error "no operations"
                             [x]    -> eval x
                             (x:xs) -> do
                                        x'<-eval x
                                        eval (SeqOp xs)

printDouble :: Double->T.Text
printDouble x = T.pack $ show x 

type Parser  = M.Parsec Void T.Text 

sc :: Parser ()
sc = L.space space1 M.empty M.empty 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pVariable :: Parser (Expr Double)
pVariable = do
             d<- pVariable2
             return (Var d)

pVariable2 :: Parser Text
pVariable2 = do
              c<- letterChar
              s<- lexeme (many alphaNumChar)
              let d = T.cons c (T.pack s)
              return d

pAssigment :: Parser (Expr Double)
pAssigment = do
              pv<- pVariable2
              eq<- (symbol ":=")
              va<- M.choice [pExpr,pVariable,pNumber]
              return (Assign pv va)

pDouble :: Parser (Expr Double)
pDouble = Lit <$> lexeme L.float

pInt :: Parser (Expr Double)
pInt = (Lit . fromIntegral) <$> lexeme L.decimal

pNumber :: Parser (Expr Double)
pNumber = M.choice [pInt,pDouble]

parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")

pTerm :: Parser (Expr Double)
pTerm = M.choice
       [ parens pExpr
       , M.try pAssigment
       , pVariable
       , pNumber
       ]
pExpr :: Parser (Expr Double)
pExpr = makeExprParser pTerm operatorTable



operatorTable :: [[Operator Parser (Expr Double)]]
operatorTable = 
   [ [binary "*" Mul,
      binary "/" Div],
     [binary "+" Add,
      binary "-" Sub]]

binary :: Text->(Expr Double->Expr Double->Expr Double)->Operator Parser (Expr Double)
binary name f = InfixL (f <$ symbol name)


parse :: T.Text -> Expr Double
parse t = case (M.parse  pExpr  ""  t ) of
           Right x-> x
           Left y-> Lit (1/0)
mainloop :: App->IO ()
mainloop app = do
                 x<-TIO.getLine
                 if x == "exit" then
                   return ()
                  else do

                      let expr = parse x
                      v <- runRIO app $ eval expr
                      let s = printDouble v
                      TIO.putStrLn s
                      mainloop app

test = do
        x<-H.new
        let env = App x
        let a = Assign "a" (Lit 1)
        let b = Assign "b" (Lit 2)
        let c = Add (Var "a") (Var "b")
        let d = SeqOp [a,b,c]
        y<- runRIO env (eval d)
        return y

main = do
        x<-H.new
        mainloop (App x)
