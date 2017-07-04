{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} 

exl = fst
exr = snd
inl = Inl
inr = Inr

-- fork (unfold)
(/\) :: (a -> b) -> (a -> c) -> (a -> b |*| c)
f /\ g = \x -> (f x, g x)

-- join (fold)
(\/) :: (a -> c) -> (b -> c) -> (a |+| b -> c)
f \/ g = \x -> case x of
  Inl l -> f l
  Inr r -> g r

class BiFunctor a b
instance BiFunctor a b

type a |*| b = (a, b)
class BiFunctor a b => Product a b where
  (|*|) :: (a -> c) -> (b -> d) -> (a |*| b -> c |*| d)
  f |*| g = (f . exl) /\ (g . exr)
instance Product a b
  
data a |+| b = Inl a | Inr b
class BiFunctor a b => CoProduct a b where
  (|+|) :: (a -> c) -> (b -> d) -> (a |+| b -> c |+| d)
  f |+| g = (inl . f) \/ (inr . g)
instance CoProduct a b

-- Example: Bool = 1 + 1
  
type MyBool = () |+| ()
myTrue :: MyBool
myTrue = inl ()
myFalse :: MyBool
myFalse = inr ()

distl :: a |*| (b |+| c) -> (a |*| b) |+| (a |*| c)
distl (x, Inl y) = inl (x, y)
distl (x, Inr y) = inr (x, y)

guard :: (a -> MyBool) -> (a -> a |+| a)
guard p = (exl |+| exl) . distl . (id /\ p)

ifCond :: (a -> MyBool) -> (a -> b) -> (a -> b) -> (a -> b)
ifCond p f g = (f \/ g) . guard p

-- Monomorphic datatypes

data IntList = Nil | Cons Int IntList

-- T = DATA F
-- F X = 1 + Int * X
-- f :: A -> B  ==>  F f :: F A -> F B
-- F f (Inl ()) = Inl ()
-- F f (Inr (intI, a)) = Inr (intI, f a)

-- Question: composition of coalgebras on extensible datatypes?
