## This is an update to CDCPLACES

# CDCPLACES 1.2.2

This is a minor update that removes a package dependency.

### Key changes

* Removed `zctaCrosswalk` from Imports, eliminating a runtime dependency. The
  ZCTA-to-county crosswalk and state-abbreviation data it provided are now
  bundled internally, with no change to user-facing behavior.

### R CMD check results

0 errors | 0 warnings | 0 notes

### Test environments

* Local: macOS Sonoma 14.5, R 4.5.1 (aarch64-apple-darwin20)
