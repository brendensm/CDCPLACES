# CDCPLACES 1.2.0

## Breaking changes

* The `geography` argument value `'census'` has been renamed to `'tract'` for clarity. Using `'census'` now produces an informative error directing users to update their code.
* The default `release` year is now `"2025"`.
* Minimum R version bumped to 4.1.0 (required for the base pipe `|>`).

## New features

* Added support for 2025 release year data.
* Added support for `'place'` geography (cities, towns, and CDPs) across all release years 2020–2025, including `geometry` and `age_adjust` support.
* API endpoint URLs are now stored in an internal data frame (`api_urls`) rather than being hardcoded, making it easier to add future release years.
* `data-raw/DATASET.R` now uses the Socrata Discovery API to programmatically discover all PLACES endpoints, replacing the manual Google Sheets approach.

## Performance improvements

* Rewrote `formatted_zctas()`, `measure_text()`, and `format_query()` to use SQL `IN` operators instead of chained `LIKE`/`OR` clauses, producing shorter and more efficient API queries.
* Increased ZCTA and other query row limits from 50,000 to 5,000,000 to prevent silent truncation of large result sets.
* ZCTA queries that would exceed Socrata's URL length limit (~7,000 characters) are now automatically batched into multiple smaller requests, preventing failures when querying states with many ZCTAs (e.g., Texas with ~2,000 ZCTAs).

## Bug fixes

* Fixed operator precedence bug in `check_multiples()` and `check_multiples_cc()` where `nrow(final_sum > 0)` was evaluated instead of `nrow(final_sum) > 0`.
* Fixed `check_api()` and `test_check_api()` crashing with `$ operator is invalid for atomic vectors` when a connection-level error (DNS failure, timeout) occurred instead of an HTTP response.
* Fixed missing `return()` calls in `check_multiples()` and `check_multiples_cc()` that caused values to silently be lost on several code paths.
* Fixed geography validation order so the informative `'census'` deprecation message is shown instead of a generic error.
* Fixed base URL validation so unsupported geography or release year values produce a clear error instead of a malformed API request.
* Guarded `readline()` calls in `check_multiples()` and `check_multiples_cc()` with `interactive()` checks so the package no longer hangs in non-interactive contexts (Shiny, knitr, CI). Defaults to including all overlapping counties with a message.
* Replaced hardcoded column indices (`places_out[8:11]`) in the ZCTA path with named column references, preventing incorrect conversions if the API schema changes.
* Fixed typo in `cat` argument message ("overrideen" → "overridden").
* Fixed `geometry = TRUE` using 2010 Census shapefiles for 2024+ release data. PLACES switched to 2020 Census geographies starting with the 2024 release, so tract and ZCTA GEOIDs no longer matched. Geometry vintage is now selected based on the `release` year.
* Fixed county filter using `toupper()` which caused a case-sensitive mismatch with the API's title-case `locationname` values. Added `to_title_case()` helper to normalize county names regardless of user input casing.

## Testing

* Added 23 offline unit tests for internal helpers and input validators that run on CRAN (`test-helpers.R`).
* Rewrote API integration tests: replaced fragile outer `if`/`for` blocks with `skip_if_api_unavailable()` inside each `test_that()` for proper skip reporting.
* Condensed API tests from 60 year-by-combination tests to 15 targeted tests with deeper assertions (column names, filter correctness, numeric types, row counts).
* Added test coverage for `county`, `age_adjust`, `cat`, `get_dictionary()`, ZCTA geography, and graceful no-internet failure.
* Removed `Sys.sleep(10)` calls and duplicate test blocks.

## Other changes

* Added `\dontrun{}` wrappers to examples.
* Removed dead code: unused `stop_quietly()`, `firstup()`, and commented-out `httr` references.
* Cleaned up stale entries in `globalVariables()`.
* Fixed documentation: `@param measure` now correctly says "multiple measures" instead of "multiple states"; `parse_request()` docstring updated from `httr2` to `curl`.

# CDCPLACES 1.1.10

* Increment version for CRAN resubmission.
* Added `skip_on_cran()` to all test cases.
* Added `\dontrun{}` to examples.
* Removed problematic CDC URL from documentation.
* Updated minimum R version to 4.1.0 in DESCRIPTION.

# CDCPLACES 1.1.9

* Edits the logic of check_api to stop the main function and return a null value if the api is unable to be reached.
* Fixes a typo in test-get_places.R.

# CDCPLACES 1.1.8

* Add the newest release year (2024), updating the API's base URL.
* Added two new arguments: `cat` to query a specific category of measures and `age_adjust` to return only the age adjusted rates if set to `TRUE`.

# CDCPLACES 1.1.7

* Removed other unnecessary dependencies to improve performance.
* Fixes a bug from 1.1.6 related to overlapping counties when querying census tracts/counties.

# CDCPLACES 1.1.6

* Added functionality to query ZCTA-level data. This is available by setting the 'geography' argument to 'zcta'.
* Added a mechanism to check for overlapping county names. This is useful for querying all levels of data.
* Changed the dependency used to parse JSON data, improving the performance of querying data.

# CDCPLACES 1.1.5

* Added a `NEWS.md` file to track changes to the package.
* Added argument 'geometry' to return sf data frame with shapefile for requested geography.
* Added argument 'county' to filter query results by county name.
* Argument 'geo' has been changed to 'geography' for clarity.


# CDCPLACES 1.1.4

* Updated version accepted to CRAN January 24, 2024.
* Replaced 'get_vars' with 'get_dictionary'. This new arguement returns a data frame of all variable availability across all years of PLACES data.

# CDCPLACES 1.1.1

* Initial version of CDCPLACES published to CRAN December 23, 2023.
* Included base functions 'get_vars' and 'get_places'.
