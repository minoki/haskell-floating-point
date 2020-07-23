# fp-ieee: IEEE 754 operations for floating-point types

This library provides IEEE 754-compliant operations, including

* `fusedMultiplyAdd`.
* correctly-rounding versions of `fromInteger`.
* `realFloatToFrac`, which always handles NaNs and infinites correctly (unlike `realToFrac`).

Some operations (e.g. `fusedMultiplyAdd`) can make use of the native instruction in the architecture.

For non-native targets, "Pure Haskell" mode is supported.

Most operations require only `RealFloat` constraint.
