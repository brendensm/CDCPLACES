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
#'@param cat Specify the category of measures to return. Overrides the argument 'measure'. Category ID must be used here. Options include 'DISABILT', 'HLTHOUT', 'HLTHSTAT', 'PREVENT', 'RISKBEH', and 'SOCLNEED' (for release 2024). To see all the available categories and their corresponding variables, run get_dictionary.
#'@param age_adjust For queries on the county level only. If TRUE, returns only the age-adjusted values.
#'
#'@examples
#'get_places(geography = "county", state = "MI", measure = "SLEEP", release = "2023")
#'get_places(geography = "county", state = c("MI", "OH"),
#'measure = c("SLEEP", "ACCESS2"), release = "2023")
#'
#'@importFrom curl has_internet curl_fetch_memory
#'@importFrom tigris counties tracts
#'@importFrom sf st_as_sf
#'@importFrom yyjsonr read_json_str
#'@importFrom zctaCrosswalk zcta_crosswalk
#'
#'@export get_places
#'@returns A data frame that contains observations for each measure and geographic level.

get_places <- function(geography = "county", state = NULL, measure = NULL, county = NULL,
                       release = "2024", geometry = FALSE, cat = NULL, age_adjust = NULL){

  if(!is.null(cat)){
    if(!is.null(measure)){
      message("A category was provided. Any items included in the 'measure' argument will be overrideen.")
    }
    measure = unique(measures[measures$categoryid == cat,]$measureid)
  }

  # Assigning base url
  if(release == "2024"){
    if(geography == "county"){

      base <-  "https://data.cdc.gov/resource/swc5-untb.json?$query=SELECT%20*%20"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/cwsq-ngmh.json?$query=SELECT%20*%20"

    }else if(geography == "zcta"){

      base <- "https://data.cdc.gov/resource/qnzd-25i4.json?$query=SELECT%20*%20"

    }else{
      stop("Geographic level not supported. Please enter 'census', 'county', or 'zcta'.")
    }
  }else if(release == "2023"){
    if(geography == "county"){

      base <- "https://data.cdc.gov/resource/h3ej-a9ec.json?$query=SELECT%20*%20"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/em5e-5hvn.json?$query=SELECT%20*%20"
    }else if(geography == "zcta"){

      base <- "https://data.cdc.gov/resource/9umn-c3jf.json?$query=SELECT%20*%20"

    }else{
      stop("Geographic level not supported. Please enter 'census', 'county', or 'zcta'.")
    }

  }else if(release == "2022"){
    if(geography == "county"){

      base <- "https://data.cdc.gov/resource/duw2-7jbt.json?$query=SELECT%20*%20"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/nw2y-v4gm.json?$query=SELECT%20*%20"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2021"){
    if(geography == "county"){

      base <- "https://data.cdc.gov/resource/pqpp-u99h.json?$query=SELECT%20*%20"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/373s-ayzu.json?$query=SELECT%20*%20"

    }else{
      stop("Geographic level not supported. Please enter 'census' or 'county'.")
    }

  }else if(release == "2020"){
    if(geography == "county"){

      base <- "https://data.cdc.gov/resource/dv4u-3x3q.json?$query=SELECT%20*%20"

    } else if(geography == "census"){

      base <- "https://data.cdc.gov/resource/4ai3-zynv.json?$query=SELECT%20*%20"

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

  # Data pull
  # if county is null

  if(geography == "zcta"){

    check_api(base)
    crosswalk <- zctaCrosswalk::zcta_crosswalk

    if(is.null(county)){

      if(length(state) == 1){
        zlist <- unique(crosswalk[crosswalk$state_usps == state,]$zcta)
      }else{

        zlist <- unique(crosswalk[crosswalk$state_usps %in% state,]$zcta)

      }

    }else{

      if(length(state) == 1){
        zlist <- unique(crosswalk[crosswalk$state_usps == state &
                                                        crosswalk$county_name %in% paste(tolower(county), "county"),]$zcta)
      }else{


        zlist <- check_multiples(state, county)

      }
      }



    if(is.null(state) & is.null(measure)){

      stop("You must select a state to query ZCTA data.")

    }else if (is.null(state)){
      stop("You must select at least one state to query ZCTA data.")
    }else if(is.null(measure)){

      places1 <- paste0(base, formatted_zctas(zlist), "%20LIMIT%2050000") |>
        curl::curl_fetch_memory()

      places_out <-  parse_request(places1$content)

    }else{
     # print(paste0(base, formatted_zctas(zlist), measure_text(measure), "%20LIMIT%2050000"))
      places1 <- paste0(base, formatted_zctas(zlist), measure_text(measure), "%20LIMIT%2050000") |>
        curl::curl_fetch_memory()

      places_out <- parse_request(places1$content)

    }

    places_out[8:11] <- lapply(places_out[8:11], as.numeric)

    if(isTRUE(geometry)){

      geo <- data.frame()

        for(i in state){

          geo_add <- tigris::zctas(state = i, year = 2010)

          geo_add <- geo_add[,c("ZCTA5CE10", "geometry")]

          geo <- rbind(geo, geo_add)

        }

        places_out <- merge(places_out, geo, by.x = "locationid", by.y = "ZCTA5CE10") |>
          sf::st_as_sf()

    }

    return(places_out)

  }

  if(is.null(county)){

    if(is.null(state) & is.null(measure)){

      message("Pulling data for all geographies. This may take some time...")

      check_api(base)

      places1 <- curl::curl_fetch_memory(base)

      places_out <-  parse_request(places1$content)

    }else if(is.null(measure)){ # state, no measures or county

      lapply(state, check_states)

      check_api(base)

      places1 <- paste0(
        base,
        format_query(state, "state", "WHERE", geography),
        "%20LIMIT%205000000"
      ) |>
        curl::curl_fetch_memory()

      places_out <- places1$content |>
        parse_request()

    }else if(is.null(state)){ # measure - no state, no county

      lapply(measure, check_measures, ryear=release)

      check_api(base)

      places1 <- paste0(
        base,
        format_query(measure, "measure", "WHERE", geography),
        "%20LIMIT%205000000"
      ) |>
        curl::curl_fetch_memory()

      places_out <- places1$content |>
        parse_request()



    }else{ #if (length(measure) > 1 & length(state) > 1){ # multiple states, multiple measures, # no county

      lapply(state, check_states)

      lapply(measure, check_measures, ryear=release)

      check_api(base)

      places1 <- paste0(
        base,
        format_query(measure, "measure", "WHERE", geography),
        format_query(state, "state", "AND", geography),
        "%20LIMIT%205000000"
      ) |>
        curl::curl_fetch_memory()

      places_out <- places1$content |>
        parse_request()
    }

  }else{ # if county is provided

    lapply(county, check_counties)

    if(is.null(state) & is.null(measure)){ # all measures and states of a specified county

      stop("If querying counties, you must supply the argument 'state'.")

    }else if(is.null(measure)){ # all measures, specific county and states

      lapply(state, check_states)

      check_api(base)

      places1 <- paste0(
        base,
        format_query(state, "state", "WHERE", geography),
        "%20",
        format_query(toupper(county), "county", "AND", geography),
        "%20",
        "%20LIMIT%205000000"
      ) |>
        curl::curl_fetch_memory()

      places_out <- places1$content |>
        parse_request()


    }else if(is.null(state)){ #specific measures and county, no state specified

      lapply(measure, check_measures, ryear=release)

      check_api(base)

      places1 <- paste0(
        base,
        format_query(measure, "measure", "WHERE", geography),
        "%20",
        format_query(toupper(county), "county", "AND", geography),
        "%20LIMIT%205000000"
      ) |>
        curl::curl_fetch_memory()

      places_out <- places1$content |>
        parse_request()

    }else{

      lapply(state, check_states)

      lapply(measure, check_measures, ryear=release)

      check_api(base)

      places1 <- paste0(
        base,
        format_query(state, "state", "WHERE", geography),
        "%20",
        format_query(toupper(county), "county", "AND", geography),
        "%20",
        format_query(measure, "measure", "AND", geography),
       "%20LIMIT%205000000"
      ) |>
        curl::curl_fetch_memory()

      places_out <- places1$content |>
        parse_request()

    }

    if(length(state) > 1){
      places_out <- check_multiples_cc(state, county, places_out, geography)
    }

  }

 places_out[,c("data_value", "low_confidence_limit", "high_confidence_limit")] <-
  lapply(places_out[,c("data_value", "low_confidence_limit", "high_confidence_limit")], as.numeric)

if(isTRUE(geometry)){



  if(geography == "county"){

    if(release == "2020"){

      # add locationid for county 2020

      fips <- tigris::fips_codes
      fips$locationid <- paste0(fips$state_code, fips$county_code)
      fips$locationname_p <- paste0(fips$county, ", ", fips$state)

      fips <- fips[,c("locationname_p", "locationid")]

      places_out$locationname_p <- paste0(places_out$locationname, " County, ", places_out$stateabbr)

      places_out <- merge(places_out, fips, by = "locationname_p")

      geo <- tigris::counties(state = state, year = 2020, cb = TRUE)

      geo <- geo[,c("GEOID", "geometry")]

      places_out <- merge(places_out, geo, by.x = "locationid", by.y = "GEOID") |>
        sf::st_as_sf()


    }else{

      geo <- tigris::counties(state = state, year = 2020, cb = TRUE) #|>

      geo <- geo[,c("GEOID", "geometry")]

      places_out <- merge(places_out, geo, by.x = "locationid", by.y = "GEOID") |>
        sf::st_as_sf()

    }



  }else if(geography == "census"){

    if(is.null(state)){
      stop("You must provide state names in order to add shapefiles to this query.", call. = FALSE)
    }else if(length(state) > 1){

      geo <- data.frame()

      for (i in state){

        geo_add <- tigris::tracts(state = i, year = 2010) #|>

        geo_add <- geo_add[,c("GEOID10", "geometry")]

        geo <- rbind(geo, geo_add)

      }

      places_out <- merge(places_out, geo, by.x = "locationid", by.y = "GEOID10") |>
        sf::st_as_sf()

    }else{

      geo <- tigris::tracts(state = state, year = 2010) #|>

      geo <- geo[,c("GEOID10", "geometry")]

      places_out <- merge(places_out, geo, by.x = "locationid", by.y = "GEOID10") |>
        sf::st_as_sf()


      }


    }


  }

  if(geography == "county"){

    if(isTRUE(age_adjust)){
      places_out <- places_out[places_out$datavaluetypeid == "AgeAdjPrv",]
    }

  }

  return(places_out)

}



#'check if measures can be queried, or if entered properly
#'@param x The measure to be compared to the list
#'@param ryear The release year of the query
#'@noRd

check_measures <- function(x, ryear){

    if(ryear != "2024"){
      if(x %in% measures[measures$categoryid == "SOCLNEED",]$measureid){
        stop("Health-related social needs variables are currently only available for 2024 release data.")
      }
    }

    if(!(x %in% measures$measureid)){
      stop(paste("Please enter a valid measure for release year. For a full list of valid measures, use the function 'get_dictionary'."))
    }

}

#'check if states can be queried or if entered correctly
#'@param x The state to be compared to the US state list
#'@noRd
check_states <- function(x){

  us_states <- zctaCrosswalk::state_names$usps[1:51]

  if(!(x %in% us_states)){
    stop("\nPlease enter a valid US State name.")
  }

}


#'check if counties can be queried or if entered correctly
#'@param x The counties to be compared to the US counties list
#'@noRd
check_counties <- function(x){
  #us_counties <- unique(usa::counties$name)

  us_counties <- unique(zctaCrosswalk::zcta_crosswalk$county_name)

  if(!(paste(tolower(x), "county") %in% us_counties)){
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
      #httr::GET(url = x, httr::timeout(10), ...),
      curl::curl_fetch_memory(url = x),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  resp <- try_GET(x)

  if(resp$status_code != 200){
    #httr::message_for_status(resp)
    message("Status code:", resp$status_code)
    message("For full response code details visit: https://dev.socrata.com/docs/response-codes.html.")
    stop_quietly()
    #return(invisible(NULL))
  }

  # if(httr::http_error(resp)){
  #   httr::message_for_status(resp)
  #   message("\nFor full response code details visit: https://dev.socrata.com/docs/response-codes.html.")
  #   stop_quietly()
  #   #return(invisible(NULL))
  # }
}


#' internal test check to see if API is online
#'@param x The base url used in API query
#'@noRd
test_check_api <- function(x){

  try_GET <- function(x, ...) {
    tryCatch(
     # httr::GET(url = x, httr::timeout(10), ...),
      curl::curl_fetch_memory(url = x),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  resp <- try_GET(x)

  if(resp$status_code != 200){
    #httr::message_for_status(resp)
    message("Status code:", resp$status_code)
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

#'pastes together the required variable names to query the API.
#'@param x variable to query
#'@param var the var type: measure, state, or county.
#'@param operator AND or WHERE
#'@param type the geography type of the query
#'@noRd
format_query <-  function(x, var, operator, type){

  if(var == "measure"){
    var <- "measureid"
  }else if(var == "state"){
    var <-  "stateabbr"
  }else if(var == "county"){
    if(type == "county"){
      var <- "locationname"
    }else if(type == "census"){
      var <- "countyname"
    }
  }

  if(length(x) == 1){
    paste0( operator, "%20(upper(%60", var, "%60)%20LIKE%20'%25", x, "%25')", collapse = "")

  }else if(length(x) < 3){
    first <- x[1]
    last <- x[length(x)]

    paste0( operator, "%20((upper(%60", var, "%60)%20LIKE%20'%25", first, "%25')",
           "%20OR%20(upper(%60", var, "%60)%20LIKE%20'%25", last, "%25'))")

  }else if(length(x) >= 3){

    first <- x[1]
    last <- x[length(x)]

    middle <- x[2:(length(x) - 1)]

    one <- paste0( operator, "%20((upper(%60", var, "%60)%20LIKE%20'%25", first, "%25')%20")
    two <- paste0("OR%20(upper(%60", var, "%60)%20LIKE%20'%25", middle, "%25')", collapse = "")
    three <- paste0("%20OR%20(upper(%60", var, "%60)%20LIKE%20'%25", last, "%25'))")

    return(paste0(one, two, three, collapse = ""))

  }

}

#'parses the json of a the httr2 request
#'@param x httr2 request object
#'@noRd
parse_request <- function(x){

  x |>
  rawToChar() |>
  yyjsonr::read_json_str()

}



#'checks if returned zcta data has overlapping county names
#'@param state names of states given in get_places call
#'@param county names of counties given in get_places call
#'@importFrom stats aggregate
#'@noRd
check_multiples <- function(state, county){

  crosswalk <- zctaCrosswalk::zcta_crosswalk

  trial <- crosswalk[crosswalk$state_usps %in% state  &
                       crosswalk$county_name %in% paste(tolower(county), "county"),]


  initial_sum <- aggregate(. ~ county_name + state_usps + county_fips, data = trial, FUN = length)
  names(initial_sum)[4] <- "n"
  initial_sum <- initial_sum[,1:4]

  final_sum <- aggregate(. ~ county_name, data = initial_sum, FUN = length)
  final_sum <- final_sum[, c("county_name", "n")]

  if(nrow(final_sum > 0)){

    if(max(final_sum$n) > 1){
      message("You have overlapping county names.")
      print(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, -3])

      message("Do you want to include overlaps?")
      response1 <- readline("Response (y/n):  ")

      if(response1 == "y"){
        message("OK, we will include all counties.")

        trial$zcta


      }else{

        message("Which should we exclude? Please respond with the state abbreviation(s) to filter out. If you have multiple states, please separate with a space (ex. 'NY TX').")

        response2 <- readline("Response:  ")

        if(nchar(response2) > 2){

          sep_response <- strsplit(response2, split = " ")[[1]]

          fil <- trial[
            !(
              trial$county_name %in% unique(initial_sum$county_name[initial_sum$county_name %in% final_sum$county_name[final_sum$n > 1]]) &
                trial$state_usps %in% sep_response
            ),
          ]

          return(
            fil$zcta
          )

        }else{

          fil <- trial[
            !(
              trial$county_name %in% unique(initial_sum$county_name[initial_sum$county_name %in% final_sum$county_name[final_sum$n > 1]]) &
                trial$state_usps %in% response2
            ),
          ]

          fil$zcta

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

  initial_sum <- aggregate(. ~ county_name + state_usps + county_fips, data = trial, FUN = length)
  names(initial_sum)[4] <- "n"
  initial_sum <- initial_sum[,1:4]

  final_sum <- aggregate(. ~ county_name, data = initial_sum, FUN = length)
  final_sum <- final_sum[, c("county_name", "n")]

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
              places[
                !(places$stateabbr %in% sep_response &
                    places$locationid %in% unique(initial_sum$county_fips[initial_sum$county_name %in% final_sum$county_name[final_sum$n > 1]])),
              ]

            }else if (geography == "census"){
              places[
                !(places$stateabbr %in% sep_response &
                    places$countyfips %in% unique(initial_sum$county_fips[initial_sum$county_name %in% final_sum$county_name[final_sum$n > 1]])),
              ]
            }

        }else{


          if(geography == "county"){

            places[
              !(places$stateabbr %in% response2 &
                places$locationid %in% unique(initial_sum$county_fips[initial_sum$county_name %in% final_sum$county_name[final_sum$n > 1]])),
            ]

          }else if (geography == "census"){


            places_filtered <- places[
              !(places$stateabbr %in% response2 &
                  places$countyfips %in% unique(initial_sum$county_fips[initial_sum$county_name %in% final_sum$county_name[final_sum$n > 1]])),
            ]


          }



        }


      }

    }

  }

}

#'Upper case the first letter of a string
#'@param x string to capitalize
#'@noRd
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
