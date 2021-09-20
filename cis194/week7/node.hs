-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}


newtype Sum a = Sum a deriving (Eq, Ord, Show)

instance Num a => Semigroup (Sum a) where

  (<>) (Sum a) (Sum b) = Sum (a + b) 

instance Num a => Monoid (Sum a) where
  mempty = Sum 0