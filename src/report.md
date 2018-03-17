## Simplicity in Haskell

### Introduction: Simplicity & CCC

Simplicity [1] is typed combinator based language used for blockchain applications. The primary goal of Simplicity is to enable estimation of computational resources used by a certain blockchain script. This is achieved by providing operational semantics of Simplicity using a bit machine which has been specifically designed for this purpose.

A recent work [2] presents two Haskell eDSLs: the simply typed lambda calculus and it's corresponsing closed cartesian category (CCC), and shows a translation from one to the other (in Haskell), and finally compiles the CCC eDSL into instructions of the Categorical Abstract Machine (CAM) - also implemented in Haskell. A translation to the CCC eDSL enables optimizations using category theoretic tools.

### Simplicity eDSL

Taking inspiration from [2], we model Simplicity as a Haskell eDSL and provide a translation from the Simplicty eDSL to the CCC eDSL (extended with co-products). This idea arises from noticing the close correspondence between the typing judgements of Simplicity [1] and the morphisms in the CCC eDSL [2]. For example, the `Iden` expression constructor resembles the `Id` morphism, the sum type in simplicity resembles coproduct objects in CCC, etc.

Since both Simplicity and the CCC eDSL use the same types (unit, sums and products), we simply re-use these types instead of creating new types.

The expressions in Simplicity can be modeled using the GADTs extension. Since Simplicity supports only unit, sum and product types, we create a type class `Types` to constrain the types. More importantly, this allows us to do some type level computation on these types, which is required later.

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

### Translating Simplicity to CCC

The translation from Simplicity to CCC is simply a function `simpl2ccc`:
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
The last case is the most interesting one: the CCC eDSL has copair constructors of the form `A x (B + C)`, while simplicity has them in the form `(B + C) x A`. We achieve the conversion between them using `prodFlip` which constructs the isomorphism between symmetric products (`prodFlip` is defined as `Factor Snd Fst`).

### Simplicity Bit Machine as a monadic eDSL

The Simplicity bit machine has been implemented in the module `SBM`. The Simplicity bit machine has several instructions which manipulate the state of the machine, and one instruction `readFrame` which outputs a bit. The operational semantics in [1] uses `readFrame` to make run-time decisions on the next steps of execution.

We model `readFrame` as a function which returns a `Maybe Bit` in the `SBM` monad. This enables us to model "interactive computation". That is, the value returned to `readFrame` is bound to a variable and decisions can be made based on the value of this variable. This is neatly captured in the monadic structure:

```
mbit <- readFrame
    case mbit of
        (Just False) -> do ... 
        (Just True)  -> do ...
```

### Evaluating Simplicity expressions

Simplicity expressions are evaluated on the simplicity bit machine by translating an expression to it's corresponding instructions. The original presentation of this in [1] uses type level functions such as `bitSize(A)`, where `A` is a type, and the return value is an integer. This is quite challenging to do when `A` is Haskell type! We implement such functions as a part of the `Types` class (and using the `ScopedTypeVariables` extension), which allows us to mock such functions in Haskell. For example, the `bitSize` function is implemented as follows:

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

These are then used by the function `simpl2sbm` in `Simpl2SBM` module. The goal of this function is to translate a given simplicity expression to instructions of the simplicity bit machine.
```
simpl2sbm :: Simpl a b -> SBM (Maybe Bit)
simpl2sbm (Iden :: Simpl a b) = do
        copy (bsize (undefined :: a))
        return Nothing
...
```

Now, let's try to evaluate some simplicity expressions. The following examples are available in the `Example` module.

We start by defining three simplicity expressions:
```
-- duplicates the input
duplicate :: Simpl SBool (SBool :*: SBool)
duplicate = Pair Iden Iden

-- identity
iden :: Simpl SBool SBool
iden = Iden

-- negates the input
not' :: Simpl SBool SBool
not' = Comp (Pair Iden Unit) (Case (Injr Unit) (Injl Unit))
```

and then we define a function to setup the input `False` in the bit machine:

```
example se = do
    -- allocate bit for value
    newFrame 1
    -- write value
    write False
    -- move it to read frame (as that is place for input)
    moveFrame
    debugS "Machine state before: "
    -- allocate new frame for result value
    newFrame 1
    -- translate (and "run") simplicity expression to SBM instructions
    (simpl2sbm se)
    debugS "Machine state after: "
    -- move result to read frame
    moveFrame
    -- read!
    readFrame
```

`iden` returns `False` as expected:
```
*Example Control.Monad Control.Monad.ST> run $ example iden
    Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
    Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False],1)]}
    Just False
```

`duplicate` duplicates the input (as observed from the resulting write stack):
```
*Example Control.Monad Control.Monad.ST> run $ example duplicate
Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False,Just False],2)]}
```

`not` negates the input and gives us `True`:
```
*Example Control.Monad Control.Monad.ST> run $ example not'
    Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
    Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just True],1)]}
    Just True
```

And then double negation of `False` equals `False`:

```
*Example Control.Monad Control.Monad.ST> run $ example $ Comp not' not'
    Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
    Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False],1)]}
    Just False
```

### What we did not do

One of the original goals in the proposal was to tranlsate from the CCC eDSL to the instructions of the simplicity bit machine. This turned out to be harder than we expected. A generic translation from morphisms to instructions requires more work, and isn't straightforward adaptation of the operational semantics in [1]. This is because the morphisms are more general than simplicity expressions and the operational semantics in [1] is specifically tailored for simplicity expressions.

### References
[1] https://blockstream.com/simplicity.pdf

[2] Functional Pearl: Interpreting Lambda Calculus via Category Theory (Unpublished)
