module Compiler where

import Data.List
import MonadTransLearn
import Control.Monad

type Name = Char
type Label = Int

data Exp = Val Int
         | Var Name
         | App Op Exp Exp
  deriving Show

data Op = Add | Sub | Mul | Div deriving (Show, Eq)

data Program = Assign Name Exp
             | If Exp Program Program
             | While Exp Program
             | Seqn [Program]
  deriving Show

comexp :: Exp -> Code
comexp (Val int) = [PUSH int]
comexp (Var name) = [PUSHV name]
comexp (App op e1 e2) = comexp e1 ++ comexp e2 ++ [DO op]

factorial :: Int -> Program
factorial n = Seqn [
  Assign 'A' (Val 1),
  Assign 'B' (Val n),
  While (Var 'B') (Seqn [
      Assign 'A' (App Mul (Var 'A') (Var 'B')),
      Assign 'B' (App Sub (Var 'B') (Val 1))])]

type Stack = [Int]

type Code = [Inst]
data Inst = PUSH Int
          | PUSHV Name
          | POP Name
          | POPM
          | DO Op
          | JMP Label
          | JMPZ Label
          | LABEL Label
  deriving Show

type WT a = WriterT Code (State Label) a
-- ~= Int -> ((a, Code), Int)

fresh :: WT Label
fresh = WriterT $ State $ \s -> ((s, mempty), s + 1)

tell :: Code -> WT ()
tell code = WriterT $ State $ \s -> (((), code), s)

mlabel :: Program -> WT ()
mlabel (Assign name expr) = do
  tell $ comexp expr
  tell [POP name]

mlabel (If expr prg1 prg2) = do
  n <- fresh
  m <- fresh --分配行号
  tell $ comexp expr
  tell $ [JMPZ n]
  mlabel prg1
  tell $ [JMP m]
  tell $ [LABEL n]
  mlabel prg2
  tell $ [LABEL m]
  tell $ [POPM]

mlabel (While expr prog) = do
  n <- fresh
  m <- fresh
  tell $ [LABEL n]
  tell $ comexp expr
  tell $ [JMPZ m]
  mlabel prog 
  tell $ [POPM]
  tell $ [JMP n]
  tell $ [LABEL m]
  tell $ [POPM]

mlabel (Seqn []) = tell []
mlabel (Seqn (x:xs)) = mlabel x >> mlabel (Seqn xs)

compile :: Program -> Code
compile prog = snd $ fst $ (runState $ runWriterT $ mlabel prog) 0

cmpTest :: IO ()
cmpTest = do
  forM_ (compile (factorial 3)) $ \i -> do
    putStrLn $ show i







































































































































































































































































































