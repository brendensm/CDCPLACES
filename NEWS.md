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
