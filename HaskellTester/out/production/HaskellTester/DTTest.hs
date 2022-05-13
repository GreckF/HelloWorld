{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DTTest where

import Data.Kind

data Nat = Zero | Succ Nat

data List a = Nil | Cons a (List a)

n1, n2, n3 :: Nat
n1 = Succ Zero
n2 = Succ n1
n3 = Succ n2

type T1 = 'Succ 'Zero
type T2 = 'Succ T1
type T3 = 'Succ T2

data Vec :: Nat -> Type -> Type where
  VNil  :: Vec 'Zero a
  VCons :: a -> Vec n a -> Vec ('Succ n) a

instance Show a => Show (Vec n a) where
  show VNil = "[]"
  show (VCons x xs) = show x ++ " : " ++ show xs

type family (a :: Nat) :< (b :: Nat) where
  m :< 'Zero = 'False
  'Zero :< 'Succ n = 'True
  ('Succ m) :< ('Succ n) = m :< n

data LTE :: Nat -> Nat -> Type where
  LTEZero :: LTE 'Zero ('Succ m)
  LTESucc :: LTE m n -> LTE ('Succ m) ('Succ n)

data Fin :: Nat -> Type where
  FZ :: Fin ('Succ k)
  FS :: Fin k -> Fin ('Succ k)

data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

vecInd :: (a :< b ~ 'True) => SNat a ->  Vec b t -> t
vecInd SZero (VCons x _) = x
vecInd (SSucc n) (VCons _ xs) = vecInd n xs

index :: Fin n -> Vec n a -> a
index FZ (VCons x _) = x
index (FS n) (VCons _ xs) = index n xs

lteInd :: (LTE a b) -> SNat a -> Vec b t -> t
lteInd LTEZero SZero (VCons x _) = x
lteInd (LTESucc lte) (SSucc m) (VCons _ xs) = lteInd lte m xs

testVec :: Vec T3 Int
testVec = VCons 0 $ VCons 1 $ VCons 2 $ VNil

one :: Int
one = vecInd (SSucc SZero) testVec

one' :: Int
one' = index (FS FZ) testVec

one'' :: Int
one'' = lteInd (LTESucc LTEZero) (SSucc SZero) testVec




