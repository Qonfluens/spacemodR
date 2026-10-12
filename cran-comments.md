## Submission

This is a new submission.

## Test environments

* local: Ubuntu 24.04, R 4.3.3, `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 3 notes

* checking CRAN incoming feasibility: new submission.

* checking installed package size: the `libs` sub-directory is large because
  the package ships three compiled 'Stan' models (built with 'rstantools').

* checking for GNU extensions in Makefiles: GNU make is a SystemRequirements.
  It is required by the 'rstantools' build infrastructure.

## Notes for the reviewers

* Functions that download data (`get_ocsge()`, `get_ocsge_data_ign()`,
  `get_ocsge_data_fgb()`, `get_eunis_data()`, `get_departements_for_roi()`)
  need an Internet connection: their examples are not run and their tests are
  skipped on CRAN.
* `get_ocsge_data_ign()` keeps the downloaded files in
  `tools::R_user_dir("spacemodR", "cache")` by default; the directory can be
  changed with the `cache_dir` argument.
* `dispersal(method = "omniscape")` calls the optional 'Julia' package
  'Omniscape'; its example is not run.
