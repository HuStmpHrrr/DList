{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.DList where
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Foldable (Foldable)

import GHC.Exts
import Prelude hiding (concat, zip)

data DList a = Nil
             | Link { elem :: a
                    , prev :: DList a
                    , next :: DList a }

instance Show a => Show (DList a) where
    show l = "fromList " ++ (show $ toList l)

pattern Empty = Nil
pattern Cons a t <- Link a _ t

cons ::  a -> DList a -> DList a
cons a l = let n = Link a Nil t
               t = cons' n l
            in n
    where cons' n Nil      = Nil
          cons' n Link{..} = let n' = Link elem n t
                                 t  = cons' n' next
                              in n'


singleton :: a -> DList a
singleton a = cons a Nil

concat ::  DList a -> DList a -> DList a
concat Nil l = l
concat l Nil = l
concat (Cons a t) l2 = cons a $ concat t l2 


instance IsList (DList a) where
    type Item (DList a) = a

    toList Nil          = []
    toList (Cons a t)   = a : toList t

    fromList l = build Nil l
        where build l []    = Nil
              build l (h:t) = let n = Link h l g
                                  g = build n t
                               in n

instance Functor DList where
    fmap _ Nil        = Nil
    fmap f (Cons a t) = cons (f a) $ fmap f t

instance Applicative DList where
    pure               = singleton
    Nil <*> _          = Nil
    (Cons _ _) <*> Nil = Nil
    (Cons f ft) <*> l  = fmap f l `concat` (ft <*> l)

instance Monad DList where
    return           = singleton
    fail _           = Nil
    Nil >>= _        = Nil
    (Cons h t) >>= f = f h `concat` (t >>= f)

instance Alternative DList where
    empty = Nil
    (<|>) = concat

instance MonadPlus DList

instance Monoid (DList a) where
    mempty  = Nil
    mappend = concat

instance Foldable DList where
    foldMap _ Nil = mempty
    foldMap f (Cons h t) = f h `mappend` foldMap f t

instance Traversable DList where
    traverse f = foldr go $ pure Nil
        where go e acc = cons <$> (f e) <*> acc

zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons h1 t1) (Cons h2 t2) = cons (h1, h2) $ zip t1 t2
