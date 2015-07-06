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

eval0                 :: Env -> Exp -> Value
eval0 env (Lit i)      = IntVal i
eval0 env (Var n)      = fromJust (Map.lookup n env)
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
invalidExp = Plus (Lit 1) (Abs "x" (Var "x"))

type Eval1 a = Identity a

runEval1   :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1                 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)      = return $ IntVal i
eval1 env (Var n)      = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e)    = return $ FunVal env n e
eval1 env (App e1 e2)  = do val1 <- eval1 env e1
                            val2 <- eval1 env e2
                            case val1 of
                              FunVal env' n body -> eval1 (Map.insert n val2 env') body

-- eval1 Map.empty exampleExp               Identity (IntVal 18)
-- runEval1 $ eval1 Map.empty exampleExp    IntVal 18


-- ErrorT error monad
type Eval2 a = ErrorT String Identity a

runEval2   :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)


eval2a                 :: Env -> Exp -> Eval2 Value
eval2a env (Lit i)      = return $ IntVal i
eval2a env (Var n)      = return $ fromJust $ Map.lookup n env
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e)    = return $ FunVal env n e
eval2a env (App e1 e2)  = do val1 <- eval2a env e1
                             val2 <- eval2a env e2
                             case val1 of
                               FunVal env' n body -> eval2a (Map.insert n val2 env') body

-- eval2a Map.epmty exampleExp               ErrorT (Identity (Right (IntVal 18)))
-- runEval2 $ eval2a Map.empty exampleExp    Right (IntVal 18)

-- better errors
eval2b                 :: Env -> Exp -> Eval2 Value
eval2b env (Lit i)      = return $ IntVal i
eval2b env (Var n)      = case Map.lookup n env of
                            Just val  -> return val
                            Nothing   -> throwError ("unbound variable: " ++ n)
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                             e2' <- eval2b env e2
                             case (e1', e2') of
                                (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                _                      -> throwError "type error in addition"
eval2b env (Abs n e)    = return $ FunVal env n e
eval2b env (App e1 e2)  = do val1 <- eval2b env e1
                             val2 <- eval2b env e2
                             case val1 of
                               FunVal env' n body -> eval2b (Map.insert n val2 env') body
                               _                  -> throwError "type error in application"

-- Pas the environment as part of the monad instead of as a param to eval func
type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))

eval3             :: Exp -> Eval3 Value
eval3 (Lit i)      = return $ IntVal i
eval3 (Var n)      = do env <- ask
                        case Map.lookup n env of
                           Just val  -> return val
                           Nothing   -> throwError ("unbound variable: " ++ n)
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval3 (Abs n e)    = do env <- ask
                        return $ FunVal env n e
eval3 (App e1 e2)  = do val1 <- eval3 e1
                        val2 <- eval3 e2
                        case val1 of
                           FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                           _                  -> throwError "type error in application"

-- runEval3 Map.empty (eval3 exampleExp)


type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env state ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) state)

-- Helper to update the state with one
tick :: (Num s, MonadState s m) => m ()
tick = do state <- get
          put (state + 1)

eval4             :: Exp -> Eval4 Value
eval4 (Lit i)      = do tick
                        return $ IntVal i
eval4 (Var n)      = do tick
                        env <- ask
                        case Map.lookup n env of
                           Just val  -> return val
                           Nothing   -> throwError ("unbound variable: " ++ n)
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval4 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval4 (App e1 e2)  = do tick
                        val1 <- eval4 e1
                        val2 <- eval4 e2
                        case val1 of
                           FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
                           _                  -> throwError "type error in application"


-- runEval4 Map.empty 0 (eval4 exampleExp)

type Eval4' a = ReaderT Env (StateT Integer (ErrorT String Identity)) a

runEval4' :: Env -> Integer -> Eval4' a -> (Either String (a, Integer))
runEval4' env state ev = runIdentity (runErrorT (runStateT (runReaderT ev env) state))

eval4'             :: Exp -> Eval4' Value
eval4' (Lit i)      = do tick
                         return $ IntVal i
eval4' (Var n)      = do tick
                         env <- ask
                         case Map.lookup n env of
                            Just val  -> return val
                            Nothing   -> throwError ("unbound variable: " ++ n)
eval4' (Plus e1 e2) = do tick
                         e1' <- eval4' e1
                         e2' <- eval4' e2
                         case (e1', e2') of
                             (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                             _                      -> throwError "type error in addition"
eval4' (Abs n e)    = do tick
                         env <- ask
                         return $ FunVal env n e
eval4' (App e1 e2)  = do tick
                         val1 <- eval4' e1
                         val2 <- eval4' e2
                         case val1 of
                            FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4' body)
                            _                  -> throwError "type error in application"


-- Swapping error and state give different results (on error)
-- runEval4' Map.empty 0 (eval4' exampleExp)


type Eval5 a = ReaderT Env (ErrorT String
                           (WriterT [String] (StateT Integer Identity))) a

runEval5             :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env state ev = runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) state)


eval5             :: Exp -> Eval5 Value
eval5 (Lit i)      = do tick
                        return $ IntVal i
eval5 (Var n)      = do tick
                        tell [n]
                        env <- ask
                        case Map.lookup n env of
                           Just val  -> return val
                           Nothing   -> throwError ("unbound variable: " ++ n)
eval5 (Plus e1 e2) = do tick
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval5 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval5 (App e1 e2)  = do tick
                        val1 <- eval5 e1
                        val2 <- eval5 e2
                        case val1 of
                           FunVal env' n body -> local (const (Map.insert n val2 env')) (eval5 body)
                           _                  -> throwError "type error in application"

-- runEval5 Map.empty 0 (eval5 exampleExp)


type Eval6 a = ReaderT Env (ErrorT String
                           (WriterT [String] (StateT Integer IO))) a

-- we return something of type IO
-- runIdentity is removed because this action needs to be performed in main
runEval6             :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env state ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) state


eval6             :: Exp -> Eval6 Value
eval6 (Lit i)      = do tick
                        liftIO $ print i
                        return $ IntVal i
eval6 (Var n)      = do tick
                        tell [n]
                        env <- ask
                        case Map.lookup n env of
                           Just val  -> return val
                           Nothing   -> throwError ("unbound variable: " ++ n)
eval6 (Plus e1 e2) = do tick
                        e1' <- eval6 e1
                        e2' <- eval6 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval6 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval6 (App e1 e2)  = do tick
                        val1 <- eval6 e1
                        val2 <- eval6 e2
                        case val1 of
                           FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
                           _                  -> throwError "type error in application"

-- runEval6 Map.empty 0 (eval6 exampleExp)

main = runEval6 Map.empty 0 (eval6 exampleExp)
