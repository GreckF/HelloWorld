

module Test where

import Text.Read (readMaybe)

--import Control.Applicative
--import Data.Char
--import Text.Parsec

data Reader a b = Reader { runReader :: a -> b }

instance Functor (Reader a) where
  fmap f fa = Reader $ f . runReader fa


instance Monad (Reader a) where
  return b = Reader $ const b
  fb >>= f = Reader $ \s->
    runReader (f (runReader fb s)) s  


instance Applicative (Reader a) where
  pure = return
  f <*> fb = Reader $ \s->
    let f' = runReader f s
        b  = runReader fb s
    in  f' b

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where 
  return a = State $ \s'-> (a,s')
  
  sta >>= f = State $ \s'->
    let (a, newS) = runState sta s'
    in  runState (f a) newS
    
instance Functor (State s) where --todo
  fmap f ma = ma >>= \ a -> return (f a)
  
instance Applicative (State s) where --todo
  pure = return
  mf <*> ma = mf >>= \ f -> fmap f ma

  

{-
ff :: State [Char] Int
ff = State $ \str-> case (readMaybe ((:[]) (head str)) :: Maybe Int) of Just x -> (x,tail str)

newState :: (Int, [Char])
newState = runState (ff >>= \x-> State $ \s-> ((x + 1),s) ) "33333"

testmain :: IO ()
testmain = do
  a <- (getLine >>= readIO) :: IO Int
  putStr $ "TEST: " ++ show a
  putStr $ " TEST: " ++ show a

testmain' = ((getLine >>= readIO) :: IO Int) >>= \ a -> putStr (show a) >> putStr (show a)
-}
{-
  stateTrans >>= \x->
    f1(x) >>= \x->
     f2(x) >>= \x->
      f3(x)  
  $ 
  sta
-}


{-
data ListT m a = Nil | Cons (m a) (ListT m a)

instance (Show (m a)) => Show (ListT m a) where
  show Nil = "[]"
  show (Cons x xs) = show x ++ " : " ++ show xs

instance (Functor m) => Functor (ListT m) where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (fmap f x) (fmap f xs)

instance (Applicative m) => Applicative (ListT m) where

instance (Monad m) => Monad (ListT m) where
  return x = Cons (return x) Nil
  Nil >>= f = Nil
  (Cons x xs) >>= f = undefined
-}

pop :: State [a] a
pop = State $ \ s -> (head s, tail s)

push :: a -> State [a] ()
push a = State $ \ s -> ((), a:s)

top :: State [a] a
top = State $ \ s -> (head s, s)

runls = do
  pop
  pop
  pop
  a <- pop
  push a
  t <- top
  push t

apb = do
  s <- getLine
  ls <- mySeq $ (readIO :: String -> IO Int) <$> words s
  putStrLn(show $ sum ls)

mySeq :: Monad m => [m a] -> m [a]
mySeq [] = pure []
mySeq (mx:mxs) = mx >>= \ x -> left >>= \ s -> pure $ x:s
  where left = mySeq mxs

