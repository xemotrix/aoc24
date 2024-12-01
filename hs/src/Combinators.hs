module Combinators where

import Control.Arrow ((***))

infixr 8 .:

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

apply2Way :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
apply2Way f g h x = f (g x) (h x)

(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f1 (f0 a) -> f1 (f0 b)
(<$$>) = (<$>) . (<$>)

infixr 0 $$

($$) :: (a -> a -> b) -> a -> b
f $$ x = f x x
