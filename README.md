<!-- badges: start -->
  [![R-CMD-check](https://github.com/brendensm/CDCPLACES/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brendensm/CDCPLACES/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# Functions to Access the 'CDC PLACES' API

This package allows users to seamlessly query the Centers for Disease Control and Prevention's (CDC) Population Level Analysis and Community Estimates (PLACES) API.

From the [CDC's website:](https://www.cdc.gov/places/index.html) 
>PLACES is a collaboration between CDC, the Robert Wood Johnson Foundation, and the CDC Foundation. PLACES provides health data for small areas across the country. This allows local health departments and jurisdictions, regardless of population size and rurality, to better understand the burden and geographic distribution of health measures in their areas and assist them in planning public health interventions.
>
>PLACES provides model-based, population-level analysis and community estimates of health measures to all counties, places (incorporated and census designated places), census tracts, and ZIP Code Tabulation Areas (ZCTAs) across the United States.

For more information on this data set's methodology and measure definitions refer to the [CDC PLACES website.](https://www.cdc.gov/places/about/index.html) 

# Installation

To install the latest development version run:

``` r
devtools::install_github("brendensm/CDCPLACES")
```

# Main Functions

-   `get_places` &mdash; a function to query the PLACES API. Arguments allows the user to specify geography (census/county), state, measure, and release (2020-2023).
-   `get_dictionary` &mdash; a function to pull a full list of the measures available in the PLACES data set.

# Examples

To view a complete walk through of the package, view the vignette found [here.](https://brendenmsmith.com/posts/introducing%20the%20places%20package/) You can also view the pkgdown site [here.](https://brendensm.github.io/CDCPLACES/)
