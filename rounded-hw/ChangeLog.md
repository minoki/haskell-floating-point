# Changelog for rounded-hw

## 0.4.0.2 (2025-12-29)

* Support GHC 9.14.

## 0.4.0.1 (2024-12-15)

* Support GHC 9.10/9.12 and Cabal 3.14.0.

## 0.4.0 (2023-11-18)

* Disable `x87-long-double` flag by default.
* Support GHC 9.4 to 9.8 (inclusive).
* Fix `roundedMul` of `ViaRational`.

## 0.3.0 (2022-01-08)

* Support Clang on AArch64.
* Show the rounding attribute in `instance Show (Rounded r a)`.

## 0.2.0 (2020-12-27)

* Some functionality was moved to fp-ieee.
* Fix roundedFusedMultiplyAdd of ViaRational.
* Fix showFFloatRn.

## 0.1.0.0 (2020-06-23)

* Initial release.
