## This is an update to CDCPLACES

# CDCPLACES 1.2.0

This is a major update that adds support for the 2025 PLACES release, fixes
numerous bugs, improves query performance, and substantially rewrites the test
suite.

### Key changes

* Added support for 2025 release year data (now the default).
* Added support for `'place'` geography (cities, towns, and CDPs) across all
  release years 2020–2025, including `geometry` and `age_adjust` support.
* Renamed `geography = "census"` to `geography = "tract"` (with informative
  deprecation error).
* Fixed `geometry = TRUE` to use 2020 Census shapefiles for 2024+ releases
  (PLACES switched from 2010 to 2020 Census geographies).
* Fixed multiple bugs: operator precedence in `check_multiples()`, missing
  `return()` calls, `readline()` hanging in non-interactive contexts, county
  filter case-sensitivity, and connection-error handling in `check_api()`.
* Rewrote query helpers to use SQL `IN` operators for shorter, more efficient
  API URLs; added automatic batching for large ZCTA queries.
* Replaced manual Google Sheets–based endpoint lookup (`data-raw/DATASET.R`)
  with programmatic discovery via the Socrata catalog API.
* Added 26 offline unit tests (`test-helpers.R`) that run on CRAN; rewrote API
  integration tests with `skip_on_cran()` and deeper assertions.

### R CMD check results

0 errors | 0 warnings | 0 notes

### Test environments

* Local: macOS Sonoma 14.5, R 4.5.1 (aarch64-apple-darwin20)
