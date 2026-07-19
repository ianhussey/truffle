# truffle 0.1.6

* Prepared the package for CRAN: moved runtime dependencies from `Depends` to
  `Imports`, namespaced all external function calls, and added `URL`,
  `BugReports`, `Language`, and testthat configuration to `DESCRIPTION`.
* Added a `testthat` (edition 3) test suite covering the `truffle_`, `dirt_`,
  and `snuffle_` functions, plus a spelling check and `inst/WORDLIST`.
* Added a GitHub Actions `R-CMD-check` workflow across macOS, Windows, and Linux
  (R-devel, release, oldrel-1).
* Converted the vignette from Quarto to a knitr/rmarkdown vignette so it builds
  on CRAN.
* Fixed the `LICENSE` file to the CRAN two-line format and added `LICENSE.md`.
* Removed an unfinished, duplicated three-group definition of `truffle_likert()`
  that shadowed the working two-group/cross-sectional version.

# truffle 0.1.0

* Initial version: `truffle_*` functions to generate item-level Likert data with
  known effects, `dirt_*` functions to add realistic data-processing challenges,
  and helpers to check and score the generated data.
