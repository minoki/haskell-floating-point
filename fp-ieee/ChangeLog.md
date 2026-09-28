# Changelog for fp-ieee

## Version 0.1.0.7 (2026-09-28)

* Fix bugs with `Float128`.
* Fix `nextTowardZeroHalf`.
* Fix correctness issue with the generic FMA.
* Fix `remainder` when the result is zero.
* Fix `doubleToHalf` on the F16C configuration.
* Make use of RISC-V instructions. Some features need `rva22u64` or `rva23u64` package flags.
* Support GHC 10.0.

## Version 0.1.0.6 (2025-12-29)

* Support GHC 9.14.

## Version 0.1.0.5 (2024-12-15)

* Support GHC 9.10/9.12.

## Version 0.1.0.4 (2024-02-18)

* Documentation chanegs.

## Version 0.1.0.3 (2023-11-18)

* Allow ghc-bignum 1.3.
* Fix `IntegerInternals.roundingMode#`.
* Fix assertion in `augmentedMultiplication`.
* Use FMA primitives on GHC 9.8.

## Version 0.1.0.2 (2021-11-30)

* Allow ghc-bignum 1.2.
* Documentation changes.

## Version 0.1.0.1 (2021-01-02)

Fix some packaging issues.

## Version 0.1.0 (2020-12-27)

Initial release.
