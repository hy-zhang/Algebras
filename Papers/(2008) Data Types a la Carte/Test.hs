{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

-- For monomorphic functors:

data Val e = Val Int
data Add e = Add e e
data (f :+: g) e = Inl (f e) | Inr (g e)

infixr :+:

instance Functor Val where
  fmap f (Val x) = Val x
instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap h (Inl e1) = Inl (fmap h e1)
  fmap h (Inr e2) = Inr (fmap h e2)

type Algebra f x = f x -> x

class Functor f => Eval f where
  evalAlgebra :: Algebra f Int
instance Eval Val where
  evalAlgebra (Val x) = x
instance Eval Add where
  evalAlgebra (Add e1 e2) = e1 + e2
instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl e1) = evalAlgebra e1
  evalAlgebra (Inr e2) = evalAlgebra e2
  
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
instance Functor f => f :<: f where
  inj = id
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  
data Fix f = In {out :: f (Fix f)}
fold :: Functor f => Algebra f a -> Fix f -> a
fold f = f . fmap (fold f) . out

inject :: (g :<: f) => g (Fix f) -> Fix f
inject = In . inj
val :: (Val :<: f) => Int -> Fix f
val x = inject $ Val x
add :: (Add :<: f) => Fix f -> Fix f -> Fix f
add x y = inject $ Add x y
eval :: (Eval f) => Fix f -> Int
eval = fold evalAlgebra

expr :: Fix (Val :+: Add)
expr = add (val 3) (val 7)