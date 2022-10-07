module Main where

import Prelude
import Data.List(List(..),(:),reverse,foldl)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)


singleton :: forall a.a -> List a
singleton a = Cons a Nil

null :: forall a. List a -> Boolean
null Nil = true
null (Cons x _) = false

snoc :: forall a. List a -> a -> List a
snoc Nil a = a : Nil
snoc ( x : Nil) a = x : a : Nil
snoc ( x : xs) a = x : snoc xs a

length :: forall a. List a -> Int
length Nil  = 0
length (x : Nil)  = 1
length (x : xs)  = 1 + length xs

main :: Effect Unit
main = do
  log $ show $ singleton "asd222"
  log $ show $ null Nil
  log $ show $ null ( 1 :2:Nil)
  log $ show $ snoc ( 1 :2: Nil) 3
  log $ show $ length ( 1 :2:Nil)