#'Obtain data from the CDC PLACES APIs.
#'
#'Use this function to access CDC PLACES API data. Measures are sourced from the Behavioral Risk Factor Surveillance System and the American Community Survey ACS.
#'
#'@param geo The level of desired geography. Currently supports 'county' and 'census'.
#'@param state Specify the state of the desired data using the two letter abbreviation. Supports multiple states if desired.
#'@param measure Specify the measures of the data pull. Supports multiple states if desired. For a full list of available measures, see the function 'get_dictionary'.
#'@param release Specify the year of release for the PLACES data set. Currently supports years 2020-2023.
#'
#'@examples
#'get_places(geo = "county", state = "MI", measure = "SLEEP", release = "2023")
#'get_places(geo = "county", state = c("MI", "OH"),
#'measure = c("SLEEP", "ACCESS2"), release = "2023")
#'
#'@importFrom httr2 request req_perform resp_body_string
#'@importFrom jsonlite fromJSON
#'@importFrom tidyr unnest
#'@importFrom dplyr filter rename mutate
#'@importFrom httr http_error timeout GET message_for_status
#'@importFrom curl has_internet
#'
#'@export get_places
#'@returns A tibble that contains observations for each measure (adjusted and unadjusted prevalence) and geographic level.

get_places <- function(geo = "county", state = NULL, measure = NULL, release = "2023"){

  if(release == "2023"){
    if(geo == "county"){

      base <-  "https://data.cdc.gov/resource/swc5-untb.json"

    } else if(geo == "census"){

      base <- "https://data.cdc.gov/resource/cwsq-ngmh.json"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2022"){
    if(geo == "county"){

      base <- "https://data.cdc.gov/resource/duw2-7jbt.json"

    } else if(geo == "census"){

      base <- "https://data.cdc.gov/resource/nw2y-v4gm.json"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2021"){
    if(geo == "county"){

      base <- "https://data.cdc.gov/resource/pqpp-u99h.json"

    } else if(geo == "census"){

      base <- "https://data.cdc.gov/resource/373s-ayzu.json"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2020"){
    if(geo == "county"){

      base <- "https://data.cdc.gov/resource/dv4u-3x3q.json"

    } else if(geo == "census"){

      base <- "https://data.cdc.gov/resource/4ai3-zynv.json"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else{
    stop("Release year is not available. Please enter a year 2020-2023.")
  }

  if(!curl::has_internet()){
    message("Request could not be completed. No internet connection.")
    return(invisible(NULL))
  }

  if(is.null(state) & is.null(measure)){

    message("Pulling data for all geographies. This may take some time...")

    check_api(base)

    places1 <- httr2::request(base) |>
      httr2::req_perform()

    places_out <-  places1 |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON() |>
      tidyr::unnest(cols = geolocation) |>
      dplyr::filter(stateabbr != "US")

  }else if(is.null(measure)){

    lapply(state, check_states)

    check_api(base)

    places_out <- data.frame()

    for(i in state){

      places1 <- httr2::request(paste0(base, "?$limit=5000000", "&stateabbr=", i)) |>
        httr2::req_perform()

      places_out_add <- places1 |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation)

      places_out <- rbind(places_out, places_out_add, row.names = NULL)


    }

  }else if(is.null(state)){

    lapply(measure, check_measures, ryear=release)

    check_api(base)

    places_out <- data.frame()

    for(i in measure){

      places1 <- httr2::request(paste0(base, "?$limit=5000000", "&measureid=", i)) |>
        httr2::req_perform()

      places_out_add <- places1 |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation) |>
        dplyr::filter(stateabbr != "US")

      places_out <- rbind(places_out, places_out_add, row.names = NULL)

    }

  }else if (length(measure) > 1 & length(state) > 1){ # multiple states, multiple measures

    lapply(state, check_states)

    lapply(measure, check_measures, ryear=release)

    check_api(base)

    places_out <- data.frame()

    if(length(measure) > length(state)){

      for(i in measure){

        p1 <- paste0(base, "?$limit=5000000", "&stateabbr=", state, "&measureid=", i)

        for(b in seq(state)){

          places1 <- p1[b] |>
            httr2::request() |>
            httr2::req_perform()

          places_out_add <- places1 |>
            httr2::resp_body_string() |>
            jsonlite::fromJSON() |>
            tidyr::unnest(cols = geolocation)

          places_out <- rbind(places_out, places_out_add, row.names = NULL)

        }

      }

    }else{

      for(i in state){

        p1 <- paste0(base, "?$limit=5000000", "&stateabbr=", i, "&measureid=", measure)

        for(b in seq(measure)){

          places1 <- p1[b] |>
            httr2::request() |>
            httr2::req_perform()

          places_out_add <- places1 |>
            httr2::resp_body_string() |>
            jsonlite::fromJSON() |>
            tidyr::unnest(cols = geolocation)

          places_out <- rbind(places_out, places_out_add, row.names = NULL)

        }

      }

    }


  }else if(length(state) >= 1 & length(measure) < 2){

    lapply(state, check_states)

    lapply(measure, check_measures, ryear=release)

    check_api(base)

    places_out <- data.frame()

    for(i in state){

      base_url <- paste0(base, "?$limit=5000000", "&measureid=", measure, "&stateabbr=", i)

      places1 <- httr2::request(base_url) |>
        httr2::req_perform()

      places_out_add <- places1 |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation)

      places_out <- rbind(places_out, places_out_add, row.names = NULL)

    }

  }else if (length(measure >= 1 & length(state) < 2)){

    lapply(state, check_states)

    lapply(measure, check_measures, ryear=release)

    check_api(base)

    places_out <- data.frame()

    for(i in measure){


      places1 <- httr2::request(paste0(base, "?$limit=5000000", "&stateabbr=", state, "&measureid=", i)) |>
        httr2::req_perform()

      places_out_add <- places1 |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation)

      places_out <- rbind(places_out, places_out_add, row.names = NULL)

    }

  }

  places_out$coordinates <- lapply(places_out$coordinates, function(x) as.data.frame(t(x)))

  places_out <- places_out |>
    tidyr::unnest(coordinates) |>
    dplyr::rename(lon = V1, lat = V2) |>
    dplyr::mutate(data_value = as.numeric(data_value),
                  low_confidence_limit = as.numeric(low_confidence_limit),
                  high_confidence_limit = as.numeric(high_confidence_limit))

  return(places_out)

}



#'check if measures can be queried, or if entered properly
#'@param x The measure to be compared to the list
#'@param ryear The release year of the query
#'@noRd

check_measures <- function(x, ryear){
  if(ryear == "2023"){
    if(!(x %in% measures23$measureid)){
      stop(paste("Please enter a valid measure for release year", paste0(ryear, "."), "For a full list of valid measures, use the function 'get_measures'."))
    }
  }else if(ryear == "2022"){
    if(!(x %in% measures22$measureid)){
      stop(paste("Please enter a valid measure for release year", paste0(ryear, "."), "For a full list of valid measures, use the function 'get_measures'."))
    }
  }else if(ryear == "2021"){
    if(!(x %in% measures21$measureid)){
      stop(paste("Please enter a valid measure for release year", paste0(ryear, "."), "For a full list of valid measures, use the function 'get_measures'."))
    }
  }else if(ryear == "2020"){
    if(!(x %in% measures20$measureid)){
      stop(paste("Please enter a valid measure for release year", paste0(ryear, "."), "For a full list of valid measures, use the function 'get_measures'."))
    }
  }
}

#'check if states can be queried or if entered correctly
#'@param x The state to be compared to the US state list
#'@noRd

check_states <- function(x){
  us_states <- c("CA", "AK", "AL", "AZ", "AR", "GA", "DC", "CO", "DE", "CT", "IN", "IL", "ID", "HI", "KS", "IA", "KY", "MD", "LA", "MA", "ME", "MI", "MS",
                 "MN", "MO", "MT", "NE", "NV", "NJ", "NM", "NC", "NY", "NH", "OH", "OK", "ND", "OR", "SD", "SC", "PA", "RI", "TN", "TX", "VA", "UT", "VT",
                 "WA", "WI", "WV", "WY")

  if(!(x %in% us_states)){
    stop("Please enter a valid US State name.")
  }

}


#'check if api returns error, if so: fail gracefully.
#'@param x The base url used in API query
#'@noRd

check_api <- function(x){

  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(10), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  resp <- try_GET(x)

  if(httr::http_error(resp)){
    httr::message_for_status(resp)
    message("\nFor full response code details visit: https://dev.socrata.com/docs/response-codes.html.")
    stop_quietly()
    #return(invisible(NULL))
  }
}


#' internal test check to see if API is online
#'@param x The base url used in API query
#'@noRd
test_check_api <- function(x){

  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(10), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  resp <- try_GET(x)

  if(httr::http_error(resp)){
    httr::message_for_status(resp)
    return(invisible(1))
  }else{
    return(invisible(0))
  }

}





testfunc <- function(base){

  check_api(base)

  places1 <- httr2::request(base) |>
    httr2::req_perform()

  places_out <-  places1 |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    tidyr::unnest(cols = geolocation) |>
    dplyr::filter(stateabbr != "US")

}

