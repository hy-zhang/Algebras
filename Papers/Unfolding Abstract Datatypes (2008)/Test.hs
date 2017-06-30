{-# LANGUAGE ExistentialQuantification #-} 

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
                  
-- Stream

data Maybe 