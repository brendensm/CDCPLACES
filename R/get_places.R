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
#'
#'@export get_places
#'@returns A tibble that contains observations for each measure (adjusted and unadjusted prevalence) and geographic level.

get_places <- function(geo = "county", state = NULL, measure = NULL, release = "2023"){


  if(release == "2023"){
    if(geo == "county"){

      base <-  "https://data.cdc.gov/resource/swc5-untb.json?$limit=500000"

    } else if(geo == "census"){

      base <- "https://data.cdc.gov/resource/cwsq-ngmh.json?$limit=5000000"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2022"){
    if(geo == "county"){

      base <- "https://data.cdc.gov/resource/duw2-7jbt.json?$limit=500000"

    } else if(geo == "census"){

      base <- "https://data.cdc.gov/resource/nw2y-v4gm.json?$limit=5000000"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2021"){
    if(geo == "county"){

      base <- "https://data.cdc.gov/resource/pqpp-u99h.json?$limit=5000000"

    } else if(geo == "census"){

      base <- "https://data.cdc.gov/resource/373s-ayzu.json?$limit=5000000"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2020"){
    if(geo == "county"){

      base <- "https://data.cdc.gov/resource/dv4u-3x3q.json?$limit=5000000"

    } else if(geo == "census"){

      base <- "https://data.cdc.gov/resource/4ai3-zynv.json?$limit=5000000"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else{
    stop("Release year is not available. Please enter a year 2020-2023.")
  }

  if(is.null(state) & is.null(measure)){

    message("Pulling data for all geographies. This may take some time...")

    places1 <- httr2::request(base) |>
      httr2::req_perform()

    if(places1$status_code == 500){
      stop("Request failed with status code:", places1$status_code, "Server Error.",
           "Please try again at another time. For more information, see Socrata's response codes at 'https://dev.socrata.com/docs/response-codes.html'.")
    }else{
      message(paste("Request performed with status code:", places1$status_code))
    }

    places_out <-  places1 |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON() |>
      tidyr::unnest(cols = geolocation) |>
      dplyr::filter(stateabbr != "US")

  }else if(is.null(measure)){

    lapply(state, check_states)

    places_out <- data.frame()

    for(i in state){

      places1 <- httr2::request(paste0(base, "&stateabbr=", i)) |>
        httr2::req_perform()

      if(places1$status_code == 500){
        stop("Request failed with status code:", places1$status_code, "Server Error.",
             "Please try again at another time. For more information, see Socrata's response codes at 'https://dev.socrata.com/docs/response-codes.html'.")
      }else{
        message(paste("Request performed with status code:", places1$status_code))
      }

      places_out_add <- places1 |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation)

      places_out <- rbind(places_out, places_out_add, row.names = NULL)


    }

  }else if(is.null(state)){

    lapply(measure, check_measures, ryear=release)

    places_out <- data.frame()

    for(i in measure){

      places1 <- httr2::request(paste0(base, "&measureid=", i)) |>
        httr2::req_perform()

      if(places1$status_code == 500){
        stop("Request failed with status code:", places1$status_code, "Server Error.",
             "Please try again at another time. For more information, see Socrata's response codes at 'https://dev.socrata.com/docs/response-codes.html'.")
      }else{
        message(paste("Request performed with status code:", places1$status_code))
      }

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

    places_out <- data.frame()

    if(length(measure) > length(state)){

      for(i in measure){

        p1 <- paste0(base, "&stateabbr=", state, "&measureid=", i)

        for(b in seq(state)){

          places1 <- p1[b] |>
            httr2::request() |>
            httr2::req_perform()

          if(places1$status_code == 500){
            stop("Request failed with status code:", places1$status_code, "Server Error.",
                 "Please try again at another time. For more information, see Socrata's response codes at 'https://dev.socrata.com/docs/response-codes.html'.")
          }else{
            message(paste("Request performed with status code:", places1$status_code))
          }

          places_out_add <- places1 |>
            httr2::resp_body_string() |>
            jsonlite::fromJSON() |>
            tidyr::unnest(cols = geolocation)

          places_out <- rbind(places_out, places_out_add, row.names = NULL)

        }

      }

    }else{

      for(i in state){

        p1 <- paste0(base, "&stateabbr=", i, "&measureid=", measure)

        for(b in seq(measure)){

          places1 <- p1[b] |>
            httr2::request() |>
            httr2::req_perform()

          if(places1$status_code == 500){
            stop("Request failed with status code:", places1$status_code, "Server Error.",
                 "Please try again at another time. For more information, see Socrata's response codes at 'https://dev.socrata.com/docs/response-codes.html'.")
          }else{
            message(paste("Request performed with status code:", places1$status_code))
          }

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

    places_out <- data.frame()

    for(i in state){

      base_url <- paste0(base, "&measureid=", measure, "&stateabbr=", i)

      places1 <- httr2::request(base_url) |>
        httr2::req_perform()

      if(places1$status_code == 500){
        stop("Request failed with status code:", places1$status_code, "Server Error.",
             "Please try again at another time. For more information, see Socrata's response codes at 'https://dev.socrata.com/docs/response-codes.html'.")
      }else{
        message(paste("Request performed with status code:", places1$status_code))
      }

      places_out_add <- places1 |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation)

      places_out <- rbind(places_out, places_out_add, row.names = NULL)

    }

  }else if (length(measure >= 1 & length(state) < 2)){

    lapply(state, check_states)

    lapply(measure, check_measures, ryear=release)

    places_out <- data.frame()

    for(i in measure){


      places1 <- httr2::request(paste0(base, "&stateabbr=", state, "&measureid=", i)) |>
        httr2::req_perform()

      if(places1$status_code == 500){
        stop("Request failed with status code:", places1$status_code, "Server Error.",
             "Please try again at another time. For more information, see Socrata's response codes at 'https://dev.socrata.com/docs/response-codes.html'.")
      }else{
        message(paste("Request performed with status code:", places1$status_code))
      }

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

