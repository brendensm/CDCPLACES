#' View the 'CDC PLACES' data dictionary
#'
#' This function provides the user with a data frame that shows all of the available measures in the PLACES data set and for which release years the measures are included.
#'#'
#'@importFrom httr2 request req_perform resp_body_string
#'@importFrom jsonlite fromJSON
#'
#'@examples \dontrun{
#'dictionary <- get_dictionary()
#'View(dictionary)
#'}
#'
#'@export get_dictionary
#'@returns a dataframe with the current PLACES data dictionary.

get_dictionary <- function(){
  resp <- httr2::request("https://data.cdc.gov/resource/m35w-spkz.json") |>
    httr2::req_perform()

  if(resp$status_code == 500){
    stop("Request failed with status code:", resp$status_code, "Server Error.",
         "Please try again at another time. For more information, see Socrata's response codes at 'https://dev.socrata.com/docs/response-codes.html'.")
  }else{
    message(paste("API request performed with status code:", resp$status_code))
  }

  resp |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
}
