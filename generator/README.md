# generator

Parser for Cuda 10 doc intrinsics for use in Haskell code definitions. Takes an intrinsic definition in Cuda docs like:

```
__device__ ​ float __fmaf_rn ( float  x, float  y, float  z )
    Compute x × y + z as a single operation, in round-to-nearest-even mode. 
```

and generates Haskell like:

``` Haskell
-- | Compute x × y + z as a single operation, in round-to-nearest-even mode.
__fmaf_rn :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
__fmaf_rn = opify (Op "__fmaf_rn" :: Op (ThreeArg F F F) F)
```

Indicating to the Haskell type system that ```__fmaf_rn``` is a function taking three Floating arguments and returning a Float
