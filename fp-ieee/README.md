# fp-ieee: IEEE 754 operations for floating-point types

This library provides IEEE 754-compliant operations, including

* `fusedMultiplyAdd`.
* correctly-rounding versions of `fromInteger`.
* `realFloatToFrac`, which correctly handles signed zeros, infinities, and NaNs (unlike `realToFrac`).

Some operations (e.g. `fusedMultiplyAdd`) can make use of the native instruction in the architecture via C FFI, or GHC 9.8's FMA primitives.

For non-native targets, "Pure Haskell" mode is supported via a package flag.

Most operations require only `RealFloat` constraint, but `RealFloatNaN` is needed by some operations that access the sign and payload of NaNs.
