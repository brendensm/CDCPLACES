#'Obtain data from the CDC PLACES APIs.
#'
#'Use this function to access CDC PLACES API data. Measures are sourced from the Behavioral Risk Factor Surveillance System and the American Community Survey ACS.
#'
#'@param geography The level of desired geography. Currently supports 'county', 'census', and 'zcta'.
#'@param state Specify the state of the desired data using the two letter abbreviation. Supports multiple states if desired.
#'@param measure Specify the measures of the data pull. Supports multiple states if desired. For a full list of available measures, see the function 'get_dictionary'.
#'@param county Specify the county of the desired data using the full name of the county, with a capital letter.
#'@param release Specify the year of release for the PLACES data set. Currently supports years 2020-2023.
#'@param geometry if FALSE (the default), return a regular data frame of PLACES data. If TRUE, uses the tigris package to return an sf data frame with simple feature geometry in the 'geometry' column.
#'
#'@examples
#'get_places(geography = "county", state = "MI", measure = "SLEEP", release = "2023")
#'get_places(geography = "county", state = c("MI", "OH"),
#'measure = c("SLEEP", "ACCESS2"), release = "2023")
#'
#'@importFrom httr2 request req_perform resp_body_string
#'@importFrom tidyr unnest_wider
#'@importFrom dplyr filter rename mutate left_join select
#'@importFrom httr http_error timeout GET message_for_status
#'@importFrom curl has_internet
#'@importFrom tigris counties tracts
#'@importFrom sf st_as_sf
#'@importFrom yyjsonr read_json_str
#'@importFrom zctaCrosswalk zcta_crosswalk
#'
#'@export get_places
#'@returns A tibble that contains observations for each measure (age-adjusted and unadjusted prevalence for counties) and geographic level.

get_places <- function(geography = "county", state = NULL, measure = NULL, county = NULL,
                       release = "2023", geometry = FALSE){

  # Assigning base url

  if(release == "2023"){
    if(geography == "county"){

      base <-  "https://data.cdc.gov/resource/swc5-untb.json"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/cwsq-ngmh.json"

    }else if(geography == "zcta"){

      base <- "https://data.cdc.gov/resource/qnzd-25i4.json?$query=SELECT%20year%2C%20locationname%2C%20datasource%2C%20category%2C%20measure%2C%20data_value_unit%2C%20data_value_type%2C%20data_value%2C%20data_value_footnote_symbol%2C%20data_value_footnote%2C%20low_confidence_limit%2C%20high_confidence_limit%2C%20totalpopulation%2C%20geolocation%2C%20locationid%2C%20categoryid%2C%20measureid%2C%20datavaluetypeid%2C%20short_question_text%20"



    }else{
      stop("Geographic level not supported. Please enter 'census', 'county', or 'zcta'.")
    }

  }else if(release == "2022"){
    if(geography == "county"){

      base <- "https://data.cdc.gov/resource/duw2-7jbt.json"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/nw2y-v4gm.json"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2021"){
    if(geography == "county"){

      base <- "https://data.cdc.gov/resource/pqpp-u99h.json"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/373s-ayzu.json"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2020"){
    if(geography == "county"){

      base <- "https://data.cdc.gov/resource/dv4u-3x3q.json"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/4ai3-zynv.json"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else{
    stop("Release year is not available. Please enter a year 2020-2023.")
  }

  # Check for internet

  if(!curl::has_internet()){
    message("Request could not be completed. No internet connection.")
    return(invisible(NULL))
  }

  # add tests for measures and api?

  # Data pull
  # if county is null

  if(geography == "zcta"){

    check_api(base)
    crosswalk <- zctaCrosswalk::zcta_crosswalk

    if(is.null(county)){

      if(length(state) == 1){
        zlist <- unique(crosswalk[crosswalk$state_usps == state,]$zcta)
      }else{
        #stop("Only one state can currently be queried.")

        zlist <- unique(crosswalk[crosswalk$state_usps %in% state,]$zcta)

      }

    }else{

      if(length(state) == 1){
        zlist <- unique(crosswalk[crosswalk$state_usps == state &
                                                        crosswalk$county_name %in% paste(tolower(county), "county"),]$zcta)
      }else{

        #stop("Only one state can currently be queried at a time.")

        zlist <- check_multiples(state, county)


      }
      }



    if(is.null(state) & is.null(measure)){

      stop("You must select a state to query ZCTA data.")

    }else if (is.null(state)){
      stop("You must select at least one state to query ZCTA data.")
    }else if(is.null(measure)){

      places1 <- paste0(base, formatted_zctas(zlist), "%20LIMIT%2050000") |>
      httr2::request() |>
        httr2::req_perform()

      places_out <-  parse_request(places1)

    }else{

      places1 <- paste0(base, formatted_zctas(zlist), measure_text(measure), "%20LIMIT%2050000") |>
        httr2::request() |>
        httr2::req_perform()

      places_out <- parse_request(places1)

     # return(places_out)
    }

    places_out[8:11] <- lapply(places_out[8:11], as.numeric)

    if(isTRUE(geometry)){

      geo <- data.frame()

        for(i in state){

          geo_add <- tigris::zctas(state = i, year = 2010) |>
            dplyr::select(ZCTA5CE10, geometry)

          geo <- rbind(geo, geo_add)

        }

        # geo <- tigris::zctas(state = state, year = 2010) |>
        #   dplyr::select(ZCTA5CE10, geometry)

        places_out <- dplyr::left_join(places_out, geo, by = c("locationid" = "ZCTA5CE10")) |>
          sf::st_as_sf()

    }



    return(places_out)


    #############################################################


  }

  if(is.null(county)){

    if(is.null(state) & is.null(measure)){

      message("Pulling data for all geographies. This may take some time...")

      check_api(base)

      places1 <- httr2::request(base) |>
        httr2::req_perform()

      places_out <-  parse_request(places1) |>
        dplyr::filter(stateabbr != "US")

    }else if(is.null(measure)){

      lapply(state, check_states)

      check_api(base)

      places_out <- data.frame()

      for(i in state){

        places1 <- httr2::request(paste0(base, "?$limit=5000000", "&stateabbr=", i)) |>
          httr2::req_perform()

        places_out_add <- parse_request(places1)

        places_out <- rbind(places_out, places_out_add, row.names = NULL)


      }

    }else if(is.null(state)){

      lapply(measure, check_measures, ryear=release)

      check_api(base)

      places_out <- data.frame()

      for(i in measure){

        places1 <- httr2::request(paste0(base, "?$limit=5000000", "&measureid=", i)) |>
          httr2::req_perform()

        places_out_add <- parse_request(places1)

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

            places_out_add <- parse_request(places1)

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

            places_out_add <- parse_request(places1)

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

        places_out_add <- parse_request(places1)

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

        places_out_add <- parse_request(places1)

        places_out <- rbind(places_out, places_out_add, row.names = NULL)

      }

    }

  }else{ # if county is provided

    lapply(county, check_counties)

    if(is.null(state) & is.null(measure)){

      stop("If querying counties, you must supply the argument 'state'.")

    }else if(is.null(measure)){ # all measures

      lapply(state, check_states)

      check_api(base)

      places_out <- data.frame()

      for(i in state){

        places1 <- httr2::request(paste0(base, "?$limit=5000000", "&stateabbr=", i)) |>
          httr2::req_perform()

        places_out_add <- parse_request(places1)

        places_out <- rbind(places_out, places_out_add, row.names = NULL)


      }

    }else if(is.null(state)){

      stop("If querying counties, you must supply the argument 'state'.")

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

            places_out_add <- parse_request(places1)

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

            places_out_add <- parse_request(places1)

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

        places_out_add <- parse_request(places1)

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

        places_out_add <- parse_request(places1)

        places_out <- rbind(places_out, places_out_add, row.names = NULL)

      }

    }
  }


  if(!is.null(county)){

     if(geography == "county"){


      #output the filtered data with a new function

      places_out <- places_out |>
        filter(locationname %in% county)

    }else if(geography == "census"){

      places_out <- places_out |>
        filter(countyname %in% county)

    }

  places_out <- check_multiples_cc(state, county, places_out, geography)


  }


  places_out <- places_out |>
    dplyr::mutate(data_value = as.numeric(data_value),
                  low_confidence_limit = as.numeric(low_confidence_limit),
                  high_confidence_limit = as.numeric(high_confidence_limit))

if(isTRUE(geometry)){



  if(geography == "county"){

    if(release == "2020"){

      # add locationid for county 2020

      fips <- tigris::fips_codes |>
        dplyr::mutate(locationid = paste0(state_code, county_code),
               locationname_p = paste0(county, ", ", state)) |>
        dplyr::select(locationname_p, locationid)

      places_out <- places_out |>
        dplyr::mutate(locationname_p = paste0(locationname, " County, ", stateabbr))

      places_out <- dplyr::left_join(places_out, fips, by = "locationname_p")

      geo <- tigris::counties(state = state, year = 2020, cb = TRUE) |>
        dplyr::select(GEOID, geometry)

      places_out <- dplyr::left_join(places_out, geo, by = c("locationid" = "GEOID")) |>
        sf::st_as_sf()


    }else{

      geo <- tigris::counties(state = state, year = 2020, cb = TRUE) |>
        dplyr::select(GEOID, geometry)

      places_out <- dplyr::left_join(places_out, geo, by = c("locationid" = "GEOID")) |>
        sf::st_as_sf()

    }



  }else if(geography == "census"){

    if(is.null(state)){
      stop("You must provide state names in order to add shapefiles to this query.", call. = FALSE)
    }else if(length(state) > 1){

      geo <- data.frame()

      for (i in state){

        geo_add <- tigris::tracts(state = i, year = 2010) |>
          dplyr::select(GEOID10, geometry)

        geo <- rbind(geo, geo_add)

      }

      places_out <- dplyr::left_join(places_out, geo, by = c("locationid" = "GEOID10")) |>
        sf::st_as_sf()

    }else{

      geo <- tigris::tracts(state = state, year = 2010) |>
        dplyr::select(GEOID10, geometry)

      places_out <- dplyr::left_join(places_out, geo, by = c("locationid" = "GEOID10")) |>
        sf::st_as_sf()


    }


  }


}

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
    stop("\nPlease enter a valid US State name.")
  }

}


#'check if counties can be queried or if entered correctly
#'@param x The counties to be compared to the US counties list
#'@noRd
check_counties <- function(x){
  us_counties <- unique(usa::counties$name)

  if(!(x %in% us_counties)){
    stop("\nPlease enter a valid US County name.")
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




#'pastes together the required url to query the API from a state/county's ZCTAs.
#'@param my_vector vector of zip codes to add to the query
#'@noRd
formatted_zctas <- function(my_vector) {

  firstprefix <- "WHERE%20((upper(%60locationname%60)%20LIKE%20'%25"
  firstsuffix <- "%25')%20"

  # Define the prefix and suffix
  bodyprefix <- "OR%20(upper(%60locationname%60)%20LIKE%20'%25"
  bodysuffix <- "%25')%20"

  lastsuffix <- "%25'))"

  first_vec <- my_vector[1]

  last_vec <- my_vector[length(my_vector)]

  body <- my_vector[2:(length(my_vector)-1)]


  part_one <- paste0(firstprefix, first_vec, firstsuffix, collapse = "")

  part_two <- paste0(bodyprefix, body, bodysuffix, collapse = "")

  part_three <- paste0(bodyprefix, last_vec, lastsuffix, collapse = "")

  # Concatenate the elements of the vector with the prefix and suffix
  formatted_strings <- paste0(part_one, part_two, part_three, collapse = "")

  return(formatted_strings)
}

#'pastes together the required measures to query the API for a ZCTA query.
#'@param my_vector vector of zip codes to add to the query
#'@noRd
measure_text <- function(measure){

  if(length(measure) == 1){
    paste0("%20AND%20((%60measureid%60%20%3D%20'", measure, "'))", collapse = "")

  }else if(length(measure) < 3){
    first_measure <- measure[1]
    last_measure <- measure[length(measure)]

    paste0("%20AND%20((%60measureid%60%20%3D%20'", first_measure, "')%20",
           "OR%20(upper(%60measureid%60)%20LIKE%20'%25", last_measure, "%25'))")

  }else if(length(measure) >= 3){

    first_measure <- measure[1]
    last_measure <- measure[length(measure)]

    middle <- measure[2:(length(measure) - 1)]

    one <- paste0("%20AND%20((%60measureid%60%20%3D%20'", first_measure, "')%20")
    two <- paste0("OR%20(upper(%60measureid%60)%20LIKE%20'%25", middle, "%25')", collapse = "")
    three <- paste0("OR%20(upper(%60measureid%60)%20LIKE%20'%25", last_measure, "%25'))")

    return(paste0(one, two, three, collapse = ""))

  }

}



#'parses the json of a the httr2 request
#'@param x httr2 request object
#'@noRd
parse_request <- function(x){

  # out <- x |>
  #   httr2::resp_body_string() |>
  #   RcppSimdJson::fparse()

  x |>
  httr2::resp_body_string() |>
  yyjsonr::read_json_str()  |>
  tidyr::unnest_wider(col = c(geolocation)) |>
  tidyr::unnest_wider(col = coordinates, names_sep = "_") |>
  dplyr::rename(lon = coordinates_1,
                  lat = coordinates_2)


}



#'checks if returned zcta data has overlapping county names
#'@param state names of states given in get_places call
#'@param county names of counties given in get_places call
#'@noRd
check_multiples <- function(state, county){

  crosswalk <- zctaCrosswalk::zcta_crosswalk

  trial <- crosswalk[crosswalk$state_usps %in% state  &
                       crosswalk$county_name %in% paste(tolower(county), "county"),]

  initial_sum <- trial |>
    dplyr::count(county_name, state_usps)
  final_sum <- initial_sum |>
    dplyr::count(county_name)

  if(nrow(final_sum > 0)){

    if(max(final_sum$n) > 1){
      message("You have overlapping county names.")
      print(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, -3])

      message("Do you want to include overlaps?")
      response1 <- readline("Response (y/n):  ")

      if(response1 == "y"){
        message("OK, we will include all counties.")

        trial$zcta

        # return() the full zlist

      }else{

        message("Which should we exclude? Please respond with the state abbreviation(s) to filter out. If you have multiple states, please separate with a space (ex. 'NY TX').")

        response2 <- readline("Response:  ")

        if(nchar(response2) > 2){

          sep_response <- strsplit(response2, split = " ")[[1]]

          return(

            filter(trial, !(county_name %in% unique(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, -3]$county_name) & state_usps %in% sep_response))$zcta
          )

        }else{
          filter(trial, !(county_name %in% unique(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, -3]$county_name) & state_usps %in% response2))$zcta

        }


      }

    }

  }

}


#'checks if returned county/census data contains overlapping county names
#'@param state names of states given in get_places call
#'@param county names of counties given in get_places call
#'@param places the queried places data
#'@param geography the geographical level given in get_places call
#'@noRd
check_multiples_cc <- function(state, county, places, geography){

  crosswalk <- zctaCrosswalk::zcta_crosswalk

  trial <- crosswalk[crosswalk$state_usps %in% state  &
                       crosswalk$county_name %in% paste(tolower(county), "county"),]

  initial_sum <- trial |>
    dplyr::count(county_name, state_usps, county_fips)
  final_sum <- initial_sum |>
    dplyr::count(county_name)

  if(nrow(final_sum > 0)){

    if(max(final_sum$n) > 1){
      message("You have overlapping county names.")
      print(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, -3])

      message("Do you want to include overlaps?")
      response1 <- readline("Response (y/n):  ")

      if(response1 == "y"){
        message("OK, we will include all counties.")

        places

      }else{

        message("Which should we exclude? Please respond with the state abbreviation(s) to filter out. If you have multiple states, please separate with a space (ex. 'NY TX').")

        response2 <- readline("Response:  ")

        if(nchar(response2) > 2){

          sep_response <- strsplit(response2, split = " ")[[1]]


            if(geography == "county"){
              filter(places, !(stateabbr %in% sep_response & locationid %in% unique(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, ])$county_fips))


            }else if (geography == "census"){
              filter(places, !(stateabbr %in% sep_response & countyfips %in% unique(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, ])$county_fips))

            }

        }else{


          if(geography == "county"){
            filter(places, !(stateabbr %in% response2 & locationid %in% unique(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, ])$county_fips))


          }else if (geography == "census"){
            filter(places, !(stateabbr %in% response2 & countyfips %in% unique(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, ])$county_fips))

          }



        }


      }

    }

  }

}

