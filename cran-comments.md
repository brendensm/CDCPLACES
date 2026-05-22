## This is an update to CDCPLACES

# CDCPLACES 1.2.1

This is a minor update that improves measure validation and adds support for
the 2024 ZCTA release endpoint.

### Key changes

* Measure validation now uses the per-release availability matrix from the
  PLACES data dictionary, producing clearer errors when a measure is requested
  for a release year in which it is not available.
* Health-related social needs (SOCLNEED) measures are now permitted for the
  2025 release in addition to 2024.
* Added a dedicated error for Disability (DISABILT) measures when requested
  for a release year prior to 2023.
* Measure validation is now also applied to the ZCTA code path.
* Updated internal data to include the 2024 ZCTA release API endpoint.

### R CMD check results

0 errors | 0 warnings | 0 notes

### Test environments

* Local: macOS Sonoma 14.5, R 4.5.1 (aarch64-apple-darwin20)
