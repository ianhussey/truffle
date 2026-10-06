# truffle 0.1.7

* `round_p_value()` now rounds half up (e.g., `.0445` → `.045`), with a small
  tolerance so floating-point representation error does not change the result
  (`0.1235` → `.124`). Previously `formatC()` rounded the stored binary value.
* `round_p_value()` gains an `alpha` argument (default `.05`). When rounding
  would move a value across `alpha` (e.g., `.0499` → `.050`), extra decimal
  places are shown instead (`.0499`). Use `alpha = NULL` to disable.
* `round_p_value()` now reports values that round to 1 as `> .999` (as
  `papaja::apa_p()` does) rather than `1.000`, and errors on values outside
  [0, 1] instead of silently capping them.

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
