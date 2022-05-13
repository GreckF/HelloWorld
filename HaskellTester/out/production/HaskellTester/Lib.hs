module Lib where
import Control.Lens
import Control.Monad

import Text.Megaparsec.Char
import Text.Megaparsec



genpar4 = [(a, b, c, d) | a <- [1..4], b <- [1..4], c <- [1..4], d <- [1..4], f a b c d]
  where 
    f a b c d = and $ do
      x <- [a, b, c, d] `zip` [1 ..]
      y <- drop (snd x) [a, b, c, d]
      if fst x /= y
      then return True
      else return False
      
libmain = do 
  forM_ genpar4 $ \x -> do
    putStrLn $ show x
    