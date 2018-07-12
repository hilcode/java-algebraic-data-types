module Result where

import Data.Text
    (Text)

data Result a
    = Success a
    | Failure Text

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Failure t) = Failure t

instance Applicative Result where
    pure = Success
    (<*>) (Success f) (Success x) = Success (f x)
    (<*>) (Failure t) _           = Failure t
    (<*>) _ (Failure t)           = Failure t

instance Monoid a => Monoid (Result a) where
    mempty = Success mempty
    mappend (Success x) (Success y) = Success (x `mappend` y)
    mappend (Failure t) _           = Failure t
    mappend _ (Failure t)           = Failure t
