-- data Mu f = In (f (Mu f))

data List x = Nil | Cons Int x
data Tree x = Leaf | Bin Int x x

data Cycle f a = Var a | RIn (f (Cycle f (Maybe a)))

type CyclicList = Cycle List
type CyclicTree = Cycle Tree

-- Question: cyclic structures in this paper vs (cyclic) abstract syntax graphs?