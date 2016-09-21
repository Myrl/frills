{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Arrows #-}

module Control.Wire.Frills where

import Control.Monad.Free
import Control.Wire

data Element a = Square Vector Int a
  deriving (Eq)

instance Show a => Show (Element a) where
  show (Square loc size next) = "Square " ++ show loc ++ "\n" ++ show next
instance Functor Element where
  fmap f (Square loc size next) = Square loc size (f next)

type Vector = (Int, Int)

type Picture = Free Element

square :: Picture ()
square = liftF (Square (0, 0) 1 ())

translate :: (Int, Int) -> Picture a -> Picture a
translate (x, y) z@(Pure _) = z
translate (x, y) (Free (Square (x', y') size next)) = Free $ Square (x + x', y + y') size $ translate (x, y) next

translateWire :: Wire Picture (Int, Int) b
translateWire = undefined -- Wire $ \x -

testPicture = do
  square22
  translate (10, 0) rectangle21
  translate (20, 0) rectangle21

square22 = do
  square
  translate (0,1) square
  translate (1,0) square
  translate (1,1) square
  
rectangle21 = do
  square
  translate (1,0) square
