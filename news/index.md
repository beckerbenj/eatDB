# Changelog

## eatDB 0.5.0

CRAN release: 2021-10-05

- [`dbPull()`](https://beckerbenj.github.io/eatDB/reference/dbPull.md)
  now uses
  [`dbSingleDF()`](https://beckerbenj.github.io/eatDB/reference/dbSingleDF.md)
  internally, if variables from only a single data table are selected
  (slightly increases memory usage for these cases but guarantees
  correct output length and speeds up performance)
- Switched to Github Action for CI

## eatDB 0.4.1

CRAN release: 2020-07-17

- Fixed bug causing `createDB` to fail when using `"//"` path
  specifications.

## eatDB 0.4.0

CRAN release: 2020-04-07

- Initial release on `CRAN`.
