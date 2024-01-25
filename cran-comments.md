## This is a resubmission of patch 1.1.4

This patch:

* added functions to check for an internet connection and API status before testing.

* added an if statement to check if the machine has an internet connection. If the machine is offline, an informative message if given and the function exits without error.

* added the helper function 'check_api' to test whether the api is available. This function gives the user a helpful message if the response returns with an error code. Then, the function exits without error.

* changed the format of the internal data sets used in the package.

## R CMD check results

0 errors | 0 warnings | 0 notes



