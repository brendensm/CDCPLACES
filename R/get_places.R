#'Obtain data from the CDC PLACES data base.
#'
#'Use this function to access CDC PLACES API data. Measures are sourced from the Behavioral Risk Factor Surveillance System and the American Community Survey ACS.
#'
#'@param geo The level of desired geography. Currently supports 'county' and 'census'.
#'@param state Specify the state of the desired data. Supports multiple states if desired.
#'@param measure Specify the measures of the data pull. Supports multiple states if desired. For a full list of available measures, see the function 'get_measures'.
#'@param release Specify the year of release for the PLACES data set. Currently supports years 2020-2023.
#'
#'@examples
#'get_places(geo = "county", state = "MI", measure = "SLEEP", release = "2023")
#'get_places(geo = "county", state = c("MI", "OH", "WI", "IN", "IL"),
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

    places1 <- httr2::request(base)

    places_out <-  httr2::req_perform(places1) |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON() |>
      tidyr::unnest(cols = geolocation) |>
      dplyr::filter(stateabbr != "US")

  }else if(is.null(measure)){

    places_out <- data.frame()

    for(i in state){

      places1 <- httr2::request(paste0(base, "&stateabbr=", i))

      places_out_add <- httr2::req_perform(places1) |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation)

      places_out <- rbind(places_out, places_out_add, row.names = NULL)

    }

  }else if(is.null(state)){

    places_out <- data.frame()

    for(i in measure){

      places1 <- httr2::request(paste0(base, "&measureid=", i))

      places_out_add <- httr2::req_perform(places1) |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation) |>
        dplyr::filter(stateabbr != "US")

      places_out <- rbind(places_out, places_out_add, row.names = NULL)

    }

  }else if (length(measure) > 1 & length(state) > 1){ # multiple states, multiple measures

    places_out <- data.frame()

    if(length(measure) > length(state)){

      for(i in measure){

        p1 <- paste0(base, "&stateabbr=", state, "&measureid=", i)

        for(b in seq(state)){
          places_out_add <- p1[b] |>
            httr2::request() |>
            httr2::req_perform() |>
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
          places_out_add <- p1[b] |>
            httr2::request() |>
            httr2::req_perform() |>
            httr2::resp_body_string() |>
            jsonlite::fromJSON() |>
            tidyr::unnest(cols = geolocation)

          places_out <- rbind(places_out, places_out_add, row.names = NULL)
        }

      }

    }


  }else if(length(state) >= 1 & length(measure) < 2){

    places_out <- data.frame()

    for(i in state){

      base_url <- paste0(base, "&measureid=", measure, "&stateabbr=", i)

      places1 <- httr2::request(base_url)

      places_out_add <- httr2::req_perform(places1) |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON() |>
        tidyr::unnest(cols = geolocation)

      places_out <- rbind(places_out, places_out_add, row.names = NULL)

    }

  }else if (length(measure >= 1 & length(state) < 2)){

    places_out <- data.frame()

    for(i in measure){


      places1 <- httr2::request(paste0(base, "&stateabbr=", state, "&measureid=", i))

      places_out_add <- httr2::req_perform(places1) |>
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
