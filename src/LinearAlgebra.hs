{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module LinearAlgebra where

class Field c where
  (|+|) :: c -> c -> c
  (|*|) :: c -> c -> c
  fieldPlusId :: c
  fieldMultId :: c

  fieldPlusInv :: c -> c
  fieldMultInv :: c -> c

class Field f => VecSpc vs f | vs -> f where
  (/+/) :: vs -> vs -> vs
  (/*/) :: f -> vs -> vs
  vecPlusId :: vs
  vecPlusInv :: vs -> vs

newtype Matrix a = Matrix { runMat :: [[a]] }










