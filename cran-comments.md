## Submission

This is a new submission.

## Test environments

- local macOS (aarch64), R 4.5.2 (release)
- GitHub Actions: macOS, Windows, and Ubuntu (R-devel, release, oldrel-1)
- win-builder (x86_64-w64-mingw32), R-devel, via `devtools::check_win_devel()`

## R CMD check results

0 errors | 0 warnings | 1 note.

The one NOTE is the standard "New submission" flag.

## Notes for the reviewer

* The package generates synthetic teaching data with `set.seed()` and writes no
  files outside `tempdir()`. It has no compiled code.
* The package uses domain terms and author names that a spell-checker does not
  recognise but are spelled correctly and intentional (for example Likert,
  Cronbach, disattenuation, tidyselect). These are recorded in `inst/WORDLIST`
  so `spelling::spell_check_package()` passes with no spelling NOTE.
* All examples that call the data-generation functions (which depend on
  `lavaan::simulateData()`) are wrapped in `\dontrun{}` to keep check times
  short; the same code paths are exercised by the test suite.
