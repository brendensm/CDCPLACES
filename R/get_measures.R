#'Get Measures
#'
#'Use this function to source a list of available measures in the CDC PLACES data set.
#'
#'@param release The year of release for the requested data set (2020-2023)
#'
#'@examples
#'
#'\dontrun{
#'get_measures(2022)
#'}
#'
#'@importFrom utils View
#'@export get_measures
#'@returns A viewer that opens a tibble of the available measures for a specified release year.

get_measures <- function(release){
  if (release == "2023"){
    View(measures23)
  }else if (release == "2022"){
    View(measures22)
  }else if (release == "2021"){
    View(measures21)
  }else if (release == "2020"){
    View(measures20)
  }else{
    stop("Please enter a valid year (2020-2023).")
  }
}
