module Monoids where

import Data.Monoid;

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada Nada = Nada
    mappend (Only a) Nada = Only $ mappend a mempty
    mappend Nada (Only b) = Only $ mappend mempty b
    mappend (Only a) (Only b) = Only $ mappend a b
