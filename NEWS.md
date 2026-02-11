# CDCPLACES 1.1.11

## Breaking changes

* The `geography` argument value `'census'` has been renamed to `'tract'` for clarity. Using `'census'` now produces an informative error directing users to update their code.
* The default `release` year is now `"2025"`.
* Minimum R version bumped to 4.1.0 (required for the base pipe `|>`).

## New features

* Added support for 2025 release year data.
* API endpoint URLs are now stored in an internal data frame (`api_urls`) rather than being hardcoded, making it easier to add future release years.

## Performance improvements

* Rewrote `formatted_zctas()`, `measure_text()`, and `format_query()` to use SQL `IN` operators instead of chained `LIKE`/`OR` clauses, producing shorter and more efficient API queries.
* Increased ZCTA and other query row limits from 50,000 to 5,000,000 to prevent silent truncation of large result sets.

## Bug fixes

* Fixed operator precedence bug in `check_multiples()` and `check_multiples_cc()` where `nrow(final_sum > 0)` was evaluated instead of `nrow(final_sum) > 0`.
* Fixed `check_api()` and `test_check_api()` crashing with `$ operator is invalid for atomic vectors` when a connection-level error (DNS failure, timeout) occurred instead of an HTTP response.
* Fixed missing `return()` calls in `check_multiples()` and `check_multiples_cc()` that caused values to silently be lost on several code paths.
* Fixed geography validation order so the informative `'census'` deprecation message is shown instead of a generic error.
* Guarded `readline()` calls in `check_multiples()` and `check_multiples_cc()` with `interactive()` checks so the package no longer hangs in non-interactive contexts (Shiny, knitr, CI). Defaults to including all overlapping counties with a message.
* Replaced hardcoded column indices (`places_out[8:11]`) in the ZCTA path with named column references, preventing incorrect conversions if the API schema changes.
* Fixed typo in `cat` argument message ("overrideen" → "overridden").

## Other changes

* Added `\dontrun{}` wrappers to examples and `skip_on_cran()` to all tests.
* Removed duplicate test blocks that were doubling test runtime.
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
