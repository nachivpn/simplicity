## Simplicity to Closed Cartesian categories

### Introduction: Simplicity & CCC

Simplicity [1] is typed combinator based language used for blockchain applications. The primary goal of Simplicity is to enable estimation of computational resources used by a certain blockchain script. This is achieved by providing operational semantics of Simplicity using a bit machine which has been specifically designed for this purpose.

A recent work [2] presents two Haskell eDSLs: the simply typed lambda calculus and it's corresponsing closed cartesian category (CCC), and shows a translation from one to the other (in Haskell), and finally compiles the CCC eDSL into instructions of the Categorical Abstract Machine (CAM) - also implemented in Haskell. A translation to the CCC eDSL enables optimizations using category theoretic tools.

### Simplicity eDSL

Taking inspiration from [2], we model Simplicity as a Haskell eDSL and provide a translation from the Simplicty eDSL to the CCC eDSL (extended with co-products). This idea arises from noticing the close correspondence between the typing judgements of Simplicity [1] and the morphisms in the CCC eDSL[2]. For example, the `Iden` expression constructor resembles the `Id` morphism, the sum type in simplicity resembles coproduct objects in CCC, etc.

Since both Simplicity and the CCC eDSL use the same types (unit, sums and products), we simply re-use these types instead of creating new types.

The expressions in Simplicity can be modeled using the GADTs extension. Since Simplicity support only unit, sum and product types, we create a type class `Types` to constrain the types. More importantly, this allows us to do some type level computation on these types, which is required later.

```Haskell
data Simpl i o where
    Iden :: Types a => Simpl a a
    Comp :: (Types a, Types b, Types c) => Simpl a b -> Simpl b c -> Simpl a c
    Unit :: Types a => Simpl a T
    Injl :: (Types a, Types b, Types c) => Simpl a b -> Simpl a (b :+: c)
    Injr :: (Types a, Types b, Types c) => Simpl a c -> Simpl a (b :+: c)
    Case :: (Types a, Types b, Types c, Types d) => 
        Simpl (a :*: c) d -> Simpl (b :*: c) d -> Simpl ((a :+: b) :*: c) d
    Pair :: (Types a, Types b, Types c) => Simpl a b -> Simpl a c -> Simpl a (b :*: c)
    Take :: (Types a, Types b, Types c) => Simpl a c -> Simpl (a :*: b) c
    Drop :: (Types a, Types b, Types c) => Simpl b c -> Simpl (a :*: b) c
```

### Translating Simplicity to CCC eDSL

Now, the translation from Simplicity to CCC is simply a function `simpl2ccc`:
```
simpl2ccc :: Simpl i o -> Mph i o
simpl2ccc Iden          = Id
simpl2ccc (Comp f g)    = simpl2ccc g `O` simpl2ccc f 
simpl2ccc Unit          = Terminal
simpl2ccc (Injl f)      = Inj1CCC `O` (simpl2ccc f) 
simpl2ccc (Injr f)      = Inj2CCC `O` (simpl2ccc f)
simpl2ccc (Pair p q)    = Factor (simpl2ccc p) (simpl2ccc q)
simpl2ccc (Take f)      = simpl2ccc f `O` Fst
simpl2ccc (Drop f)      = simpl2ccc f `O` Snd
simpl2ccc (Case p q)    = Copair
                            (simpl2ccc p `O` prodFlip) 
                            (simpl2ccc q `O` prodFlip) 
                        `O` prodFlip
```

### The Simplicity bit machine

The Simplicity bit machine has been implemented in the module `SBM`. 

### Challenges and current work


Currently, our focus is on translating a restricted subset of CCC into the Simplicity bit machine. This part is work in progress. A brief desciption of this follows.

The original presentation of the operational semantics of Simplicity, uses type level functions such as `bitSize(A)`, where `A` is a type, and the return value is an integer. This is quite challenging to do when `A` is Haskell type! We implement such functions as a part of the `Types` class (and the `ScopedTypeVariables` extension), which allows us to mock such functions in Haskell. For example, the `bitSize` function is implemented as follows:

```
class Types a where
    bsize :: a -> Int
    ...
instance Types T where
    bsize _ = 0
    ...
instance (Types a, Types b) => Types (a :+: b) where
    bsize _ = let 
                a = bsize (undefined :: a)
                b = bsize (undefined :: b) 
            in 1 + max a b
    ...
```

These are then used by the function `c2s` in `CCC2SMB` module. The goal of this function is to translate a restricted subset of the CCC eDSL to instructions of the simplicity bit machine. Here's an incomplete (and possible incorrect) version:

```
c2s :: (Types a, Types b) => Mph a b -> [Instr]
c2s (Id :: Mph a b) = [Copy $ bsize (undefined :: a)]
c2s ((f :: Mph c b) `O` (g :: Mph a c))  =
        [NewFrame $ bsize (undefined :: b)] 
        ++ c2s g ++ [MoveFrame] ++ c2s f ++ [DropFrame]
c2s (Terminal) = [Nop]
c2s (Inj1CCC :: Mph a bc) = [Write False, Skip $ padl (undefined :: bc)]
c2s (Inj2CCC :: Mph a bc) = [Write True, Skip $ padr (undefined :: bc)]
c2s (Factor p q) = c2s p ++ c2s q
c2s (Fst) = []
c2s (Snd :: Mph ab c) = [Fwd (bsizf (undefined :: ab))]
```

### Learning outcomes

Understanding and working with these eDSLs requires us to be fluent with standard Haskell concepts such as algebraic data types, monads, etc. The simplicity bit machine is a stateful program - the perfect place to apply monads. Achieving the goals of this project requires us to work with the bleeding edge of Haskell's type system. Moreover, translating from one eDSL to another also requires us to translate the corresponding types - which requires type level computation made possible by advanced type system extensions. This includes GADTs, DataKinds and TypeFamilies.

### References
[1] https://blockstream.com/simplicity.pdf

[2] Functional Pearl: Interpreting Lambda Calculus via Category Theory (Unpublished)
