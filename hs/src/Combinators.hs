module Combinators where

import Control.Arrow ((***))

infixr 8 .:

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f1 (f0 a) -> f1 (f0 b)
(<$$>) = (<$>) . (<$>)

infixr 0 $$

($$) :: (a -> a -> b) -> a -> b
f $$ x = f x x
