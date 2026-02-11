# Obtain data from the CDC PLACES APIs.

Use this function to access CDC PLACES API data. Measures are sourced
from the Behavioral Risk Factor Surveillance System and the American
Community Survey ACS.

## Usage

``` r
get_places(
  geography = "county",
  state = NULL,
  measure = NULL,
  county = NULL,
  release = "2025",
  geometry = FALSE,
  cat = NULL,
  age_adjust = NULL
)
```

## Arguments

- geography:

  The level of desired geography. Currently supports 'county', 'place',
  'tract', and 'zcta'.

- state:

  Specify the state of the desired data using the two letter
  abbreviation. Supports multiple states if desired.

- measure:

  Specify the measures of the data pull. Supports multiple measures if
  desired. For a full list of available measures, see the function
  'get_dictionary'.

- county:

  Specify the county of the desired data using the full name of the
  county, with a capital letter. Not supported for 'place' or 'zcta'
  geography (use 'state' to filter instead).

- release:

  Specify the year of release for the PLACES data set. Currently
  supports years 2020-2025.

- geometry:

  if FALSE (the default), return a regular data frame of PLACES data. If
  TRUE, uses the tigris package to return an sf data frame with simple
  feature geometry in the 'geometry' column.

- cat:

  Specify the category of measures to return. Overrides the argument
  'measure'. Category ID must be used here. Options include 'DISABILT',
  'HLTHOUT', 'HLTHSTAT', 'PREVENT', 'RISKBEH', and 'SOCLNEED' (for
  release 2024). To see all the available categories and their
  corresponding variables, run get_dictionary.

- age_adjust:

  For queries on the county or place level. If TRUE, returns only the
  age-adjusted values.

## Value

A data frame that contains observations for each measure and geographic
level.

## Examples

``` r
if (FALSE) { # \dontrun{
get_places(geography = "county", state = "MI", measure = "SLEEP", release = "2023")
get_places(geography = "county", state = c("MI", "OH"),
measure = c("SLEEP", "ACCESS2"), release = "2023")
} # }
```
