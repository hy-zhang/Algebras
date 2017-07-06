{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

data (f :+: g) e = Inl (f e) | Inr (g e)

infixr :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap h (Inl e1) = Inl (fmap h e1)
  fmap h (Inr e2) = Inr (fmap h e2)

  
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  proj :: sup a -> Maybe (sub a)
instance Functor f => f :<: f where
  inj = id
  proj = Just
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  proj (Inl e) = Just e
  proj _ = Nothing
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  proj (Inr e) = proj e
  proj _ = Nothing

data Term f = Term {unTerm :: f (Term f)}

data Val e = Const Int | Pair e e
data Op e = Mult e e | Fst e | Snd e
instance Functor Val where
  fmap f (Const x) = Const x
  fmap f (Pair e1 e2) = Pair (f e1) (f e2)
instance Functor Op where
  fmap f (Mult e1 e2) = Mult (f e1) (f e2)
  fmap f (Fst e) = Fst (f e)
  fmap f (Snd e) = Snd (f e)

inject :: (g :<: f) => g (Term f) -> Term f
inject = Term . inj

project :: (g :<: f) => Term f -> Maybe (g (Term f))
project = proj . unTerm

-- smart constructors
iConst :: (Val :<: f) => Int -> Term f
iConst x = inject $ Const x

iPair :: (Val :<: f) => Term f -> Term f -> Term f
iPair x y = inject $ Pair x y

iMult :: (Op :<: f) => Term f -> Term f -> Term f
iMult x y = inject $ Mult x y

iFst :: (Op :<: f) => Term f -> Term f
iFst x = inject $ Fst x

iSnd :: (Op :<: f) => Term f -> Term f
iSnd x = inject $ Snd x

-- algebras
type Alg f a = f a -> a

cata :: Functor f => Alg f a -> Term f -> a
cata f = f . fmap (cata f) . unTerm

class Eval f v where evalAlg :: Alg f (Term v)
instance (Eval f v, Eval g v) => Eval (f :+: g) v where
  evalAlg (Inl x) = evalAlg x
  evalAlg (Inr x) = evalAlg x
eval :: (Functor f, Eval f v) => Term f -> Term v
eval = cata evalAlg

-- interesting cases
instance (Val :<: v) => Eval Val v where
  evalAlg = inject
instance (Val :<: v) => Eval Op v where
  evalAlg (Mult x y) = iConst $ projC x * projC y
  evalAlg (Fst x) = fst $ projP x
  evalAlg (Snd x) = snd $ projP x
projC :: (Val :<: v) => Term v -> Int
projC v = case project v of Just (Const n) -> n
projP :: (Val :<: v) => Term v -> (Term v, Term v)
projP v = case project v of Just (Pair e1 e2) -> (e1, e2)

e :: Term (Val :+: Op)
e = iMult (iConst 3) (iFst (iPair (iConst 5) (iSnd (iPair (iConst 6) (iConst 7)))))

w :: Term Val
w = eval e -- Const 15

-- desugaring
data Sug e = Neg e | Swap e
instance Functor Sug where
  fmap f (Neg e) = Neg (f e)
  fmap f (Swap e) = Swap (f e)

-- smart constructors
iNeg :: (Sug :<: f) => Term f -> Term f
iNeg x = inject $ Neg x

iSwap :: (Sug :<: f) => Term f -> Term f
iSwap x = inject $ Swap x

class Desug f g where desugAlg :: Alg f (Term g) -- looks like a general "transformation"
instance (Desug f v, Desug g v) => Desug (f :+: g) v where
  desugAlg (Inl x) = desugAlg x
  desugAlg (Inr x) = desugAlg x
desug :: (Functor f, Desug f g) => Term f -> Term g
desug = cata desugAlg

-- interesting cases
instance (f :<: g) => Desug f g where
  desugAlg = inject
instance (Val :<: f, Op :<: f) => Desug Sug f where
  desugAlg (Neg x) = iConst (-1) `iMult` x
  desugAlg (Swap x) = iSnd x `iPair` iFst x

eval' :: Term (Val :+: Sug :+: Op) -> Term Val
eval' = eval . (desug :: Term (Val :+: Sug :+: Op) -> Term (Val :+: Op))

e2 :: Term (Val :+: Sug :+: Op)
e2 = iMult (iNeg (iConst 3)) (iFst (iSwap (iPair (iConst 5) (iSnd (iPair (iConst 6) (iConst 7))))))

w2 :: Term Val
w2 = eval' e2 -- Const (-21)

