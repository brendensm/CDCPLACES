<!-- badges: start -->
  [![R-CMD-check](https://github.com/brendensm/CDCPLACES/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brendensm/CDCPLACES/actions/workflows/R-CMD-check.yaml)
  [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/CDCPLACES)](https://cran.r-project.org/package=CDCPLACES)

  <!-- badges: end -->

# Access the 'CDC PLACES' API

This package allows users to seamlessly query the Centers for Disease Control and Prevention's (CDC) Population Level Analysis and Community Estimates (PLACES) API.

From the [CDC Foundation's website:](https://www.cdcfoundation.org/programs/places-project) 
>PLACES is a collaboration between CDC, the Robert Wood Johnson Foundation, and the CDC Foundation. PLACES provides health data for small areas across the country. This allows local health departments and jurisdictions, regardless of population size and rurality, to better understand the burden and geographic distribution of health measures in their areas and assist them in planning public health interventions.
>
>PLACES provides model-based, population-level analysis and community estimates of health measures to all counties, places (incorporated and census designated places), census tracts, and ZIP Code Tabulation Areas (ZCTAs) across the United States.

# Installation

To install the latest development version run:

``` r
devtools::install_github("brendensm/CDCPLACES")
```

To install from CRAN:
```r
install.packages("CDCPLACES")
```

# Main Functions

-   `get_places` &mdash; a function to query the PLACES API. Arguments allows the user to specify geography (census/county), state, measure, release (2020-2023), and geometry (to include shapefiles).
-   `get_dictionary` &mdash; a function to pull a full list of the measures available in the PLACES data set.

# Examples

To view a complete walk through of the package, view the vignette found [here.](https://brendenmsmith.com/posts/introducing%20the%20places%20package/) You can also view the pkgdown site [here.](https://brendensm.github.io/CDCPLACES/)
