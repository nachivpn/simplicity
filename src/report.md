## Simplicity in Haskell

### Introduction: Simplicity & CCC

Simplicity [1] is typed combinator based language used for blockchain applications. The primary goal of Simplicity is to enable estimation of computational resources used by a certain blockchain script. This is achieved by providing operational semantics of Simplicity using a bit machine which has been specifically designed for this purpose.

A recent work [2] presents two Haskell eDSLs: the simply typed lambda calculus and it's corresponsing closed cartesian category (CCC), and shows a translation from one to the other (in Haskell), and finally compiles the CCC eDSL into instructions of the Categorical Abstract Machine (CAM) - also implemented in Haskell. A translation to the CCC eDSL enables optimizations using category theoretic tools.

### Simplicity eDSL

Taking inspiration from [2], we model Simplicity as a Haskell eDSL and provide a translation from the Simplicty eDSL to a BCC eDSL (a subset of the CCC eDSL in [2] extended with co-products, but without exponential objects and the curry/eval morphisms). This idea arises from noticing the close correspondence between the typing judgements of Simplicity [1] and the morphisms in the CCC eDSL [2]. For example, the `Iden` expression constructor resembles the `Id` morphism, the sum type in simplicity resembles coproduct objects in a BCC, etc.

The BCC eDSL supports unit, sum and product types. Since both Simplicity and the BCC eDSL use the same types (unit, sums and products), we simply re-use these types instead of creating new types. Expressions in Simplicity can be modeled using the GADTs extension. Since Simplicity allows only unit, sum and product types, we create a type class `Types` which is only implemented by these types. While another option is to constrain the types using `KindConstraints` extension, our choice enables us to mock some computations on types which we will be needing later. The definition looks like this:

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

### Translating Simplicity to BCC


The translation from Simplicity to BCC eDSL is simply a function `simpl2bcc`:
```
simpl2bcc :: Simpl i o -> Mph i o
simpl2bcc Iden          = Id
simpl2bcc (Comp f g)    = simpl2bcc g `O` simpl2bcc f 
simpl2bcc Unit          = Terminal
simpl2bcc (Injl f)      = Inj1 `O` (simpl2bcc f) 
simpl2bcc (Injr f)      = Inj2 `O` (simpl2bcc f)
simpl2bcc (Pair p q)    = Factor (simpl2bcc p) (simpl2bcc q)
simpl2bcc (Take f)      = simpl2bcc f `O` Fst
simpl2bcc (Drop f)      = simpl2bcc f `O` Snd
simpl2bcc (Case p q)    = Copair
                            (simpl2bcc p `O` prodFlip) 
                            (simpl2bcc q `O` prodFlip) 
                        `O` prodFlip
```
The last case is the most interesting one: the BCC eDSL has copair constructors of the form `A x (B + C)`, while the type of the simplicity `case` expressions is of the form `(B + C) x A`. Fortunately, it is a known and proveable fact - even within our eDSL - that products are symmetric wrt to isomorphism. Hence, we construct a morphism between them using `prodFlip` - which is defined as `Factor Snd Fst` (i.e., the isomorphism between symmetric products).

### Simplicity Bit Machine as a monadic eDSL

The Simplicity bit machine has been implemented in the module `SBM`. The Simplicity bit machine has several instructions which manipulate the state of the machine, and one instruction `readFrame` which outputs a bit. The operational semantics in [1] uses `readFrame` to make run-time decisions on the next steps of execution.

We model `readFrame` as a function which returns a `Maybe Bit` in the `SBM` monad. This enables us to model "interactive computation". That is, the value returned to `readFrame` is bound to a variable and decisions can be made based on the value of this variable. This is neatly captured in the monadic structure:

```Haskell
mbit <- readFrame
    case mbit of
        (Just False) -> do ... 
        (Just True)  -> do ...
```

### Evaluating Simplicity expressions

Simplicity expressions are evaluated on the simplicity bit machine by translating an expression to it's corresponding instructions. The original presentation of this in [1] uses type level functions such as `bitSize(A)`, where `A` is a type, and the return value is an integer. This is quite challenging to do when `A` is Haskell type! We implement such functions as a part of the `Types` class (and using the `ScopedTypeVariables` extension), which allows us to mock such functions in Haskell. For example, the `bitSize` function is implemented as follows:

```Haskell
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
```Haskell
simpl2sbm :: Simpl a b -> SBM (Maybe Bit)
simpl2sbm (Iden :: Simpl a b) = do
        copy (bsize (undefined :: a))
        return Nothing
...
```

Now, let's try to evaluate some simplicity expressions. The following examples are available in the `Example` module.

We start by defining three simplicity expressions:
```Haskell
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

```Haskell
example f se = do
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
    f se
    debugS "Machine state after: "
    -- move result to read frame
    moveFrame
    -- read!
    readFrame

example1 = example simpl2sbm
```

`iden` returns `False` as expected:
```Haskell
*Example Control.Monad Control.Monad.ST> run $ example1 iden
    Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
    Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False],1)]}
    Just False
```

`duplicate` duplicates the input (as observed from the resulting write stack):
```Haskell
*Example Control.Monad Control.Monad.ST> run $ example1 duplicate
Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False,Just False],2)]}
```

`not` negates the input and gives us `True`:
```Haskell
*Example Control.Monad Control.Monad.ST> run $ example1 not'
    Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
    Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just True],1)]}
    Just True
```

And then double negation of `False` equals `False`:

```Haskell
*Example Control.Monad Control.Monad.ST> run $ example $ Comp not' not'
    Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
    Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False],1)]}
    Just False
```

### Translating from BCC to Simplicity bit machine

One of the original goals in the proposal was to translate from the BCC eDSL to the instructions of the simplicity bit machine. This is slightly tricky as morphisms in BCC are more general than Simplicity instructions. For example, `take t` in BCC is a morphism composition `(simpl2bcc t) O (Fst)` - where `simpl2bcc f` and `Fst` are in a sense simpler and more general. `(simpl2bcc t)`  and `(Fst)` are composed using `O` - and hence we are unable to take advantage of the fact that they compose to a `take` computation. We must translate `(simpl2bcc t)`, `Fst` and `O` individually - while ensuring that the semantics of `take t` is preserved.

Fortunately, trying to write down the problem showed us the solution! The full implementation can be found in the module `BCC2SBM`. The operational semantics (or translation) looks very similar to the case of `Simpl2SBM`, except the cases of `Copair`, `Fst` and `Snd`. As the module is well commented, we skip a description here. We believe that translation preserves semantics, and show a few examples of this:

We define `example2` using our `example` function from earlier:
```Haskell
example2 :: (Types a, Types b) => Simpl a b -> SBM (Maybe Bit)
example2 = example (bcc2sbm . simpl2bcc)  
```

As expected the values computed are the same as in the case of `example1`:

```Haskell
*Example> run $ example2 $ iden
Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False],1)]}
Just False

*Example> run $ example2 $ duplicate
Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False,Just False],2)]}

*Example> run $ example2 $ not'
Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just True],1)]}
Just True

*Example> run $ example2 $ Comp not' not'
Machine state before: Machine {readStack = [([Just False],0)], writeStack = []}
Machine state after: Machine {readStack = [([Just False],0)], writeStack = [([Just False],1)]}
Just False
```
This gives us an indication that the translation to SBM via BCC might be correct, but ofcourse this needs to be formally proved. We don't do this as it is outside the scope of this project.


### References
[1] https://blockstream.com/simplicity.pdf

[2] Functional Pearl: Interpreting Lambda Calculus via Category Theory (Unpublished)
