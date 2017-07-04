{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE RankNTypes #-}

-- Concrete datatype

data Complex0 = MkComplex Double Double

isReal :: Complex0 -> Bool
isReal (MkComplex x y) = y == 0.0

-- Abstract datatype

data ComplexF s = CF {_new :: (Double, Double) -> s,
                      _add :: Complex -> s,
                      _rea :: Double,
                      _ima :: Double}

data Complex = forall s. C (s -> ComplexF s) s

new :: Complex -> (Double, Double) -> Complex
new (C f s) (x, y) = let CF {_new = res} = f s in C f (res (x, y))

add :: Complex -> Complex -> Complex
add (C f s) c = let CF {_add = res} = f s in C f (res c)

rea, ima :: Complex -> Double
rea (C f s) = let CF {_rea = res} = f s in res
ima (C f s) = let CF {_ima = res} = f s in res

zeroC :: Complex
zeroC = C fc (1.0, 0.0) where
  fc :: (Double, Double) -> ComplexF (Double, Double)
  fc (x, y) = CF {_new = id,
                  _add = \c -> (x + rea c, y + ima c),
                  _rea = x,
                  _ima = y}
                  
-- Co-church encoding. vs Church encoding.
-- Codatatypes are greatest fixpoints of recursive types.
-- Whereas datatypes are least fixpoints.

data Functor f => LF f = Church (forall s. (f s -> s) -> s)
data Functor f => GF f = forall s. CoChurch (s -> f s) s

data ListF x = Nil | Cons Int x
instance Functor ListF where
  fmap f Nil = Nil
  fmap f (Cons i x) = Cons i (f x)

type LFList = LF ListF
type GFList = GF ListF

constr :: Either () (Int, LFList) -> LFList
constr (Left _) = Church $ \alg -> alg Nil
constr (Right (i, l)) = Church $ \alg -> alg $ Cons i $ list $ alg
  where Church list = l
  
destr :: GFList -> Either () (Int, GFList)
destr (CoChurch f s) = case f s of
  Nil -> Left ()
  Cons i s1 -> Right (i, CoChurch f s1)

l1 :: LFList
l1 = cR (1, cR (2, cR (3, cL ())))
  where cR = constr . Right
        cL = constr . Left

pretty :: ListF String -> String
pretty Nil = ""
pretty (Cons i j) = show i ++ " " ++ j

pretty2 x = let Church f = x in f pretty

