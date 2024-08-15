#' View the 'CDC PLACES' data dictionary
#'
#' This function provides the user with a data frame that shows all of the available measures in the PLACES data set and for which release years the measures are included.
#'
#'@importFrom yyjsonr read_json_str
#'@importFrom curl has_internet
#'
#'@examples
#'# First save the data
#'dictionary <- get_dictionary()
#'# Then view the data frame
#'# View(dictionary)
#'
#'@export get_dictionary
#'@returns a dataframe with the current PLACES data dictionary.

get_dictionary <- function(){

  if(!curl::has_internet()){
    message("Request could not be completed. No internet connection.")
    return(invisible(NULL))
  }

  base <- "https://data.cdc.gov/resource/m35w-spkz.json"

  check_api(base)

  resp <- curl::curl_fetch_memory(base)

  resp$content |>
    rawToChar() |>
    yyjsonr::read_json_str()
}
