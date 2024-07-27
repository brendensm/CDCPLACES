#' View the 'CDC PLACES' data dictionary
#'
#' This function provides the user with a data frame that shows all of the available measures in the PLACES data set and for which release years the measures are included.
#'#'
#'@importFrom httr2 request req_perform resp_body_string
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

  check_api("https://data.cdc.gov/resource/m35w-spkz.json")

  resp <- httr2::request("https://data.cdc.gov/resource/m35w-spkz.json") |>
    httr2::req_perform()

  resp |>
    httr2::resp_body_string() |>
    yyjsonr::read_json_str()
}
