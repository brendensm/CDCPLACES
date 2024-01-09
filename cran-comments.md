## Patch 1.1.4

This patch:

* Edits get_dictionary to return the data frame of the data dictionary directly from API and removes the built in View() command. 

* Fixes the invalid URL in the README.md

## Patch 1.1.3

This is a patch to address feedback received on Dec 22, 2023. 

Maintainer was given until Jan 10, 2024 to ensure the package follows the the CRAN policy: 'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error).' This patch:

* Added check functions to ensure a supported 'state' or 'measure' is queried with a helpful error message if a typo is entered.

* Added an if statement to check if the API is online and an informative stop() message.

* Added a message to confirm the request was performed with a status code.

* The function get_measures has been replaced with get_dictionary.

## R CMD check results

0 errors | 0 warnings | 0 notes



