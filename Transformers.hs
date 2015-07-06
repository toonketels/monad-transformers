module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer          -- Literals
         | Var Name             -- Variables
         | Plus Exp Exp         -- Addition
         | Abs Name Exp         -- Abstractions
         | App Exp Exp          -- Function application
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value   -- Maps names to values


-- Reference implementation interpreter

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Abs n e)    = FunVal env n e
eval0 env (App e1 e2)  = let val1 = eval0 env e1
                             val2 = eval0 env e2
                         in case val1 of
                             FunVal env' n body -> eval0 (Map.insert n val2 env') body

-- 12 + ((\x -> x)(4 + 2))
-- eval0 Map.empty exampleExp
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x"))(Lit 4 `Plus` Lit 2))
