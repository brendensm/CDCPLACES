#'Obtain data from the CDC PLACES APIs.
#'
#'Use this function to access CDC PLACES API data. Measures are sourced from the Behavioral Risk Factor Surveillance System and the American Community Survey ACS.
#'
#'@param geography The level of desired geography. Currently supports 'county', 'place', 'tract', and 'zcta'.
#'@param state Specify the state of the desired data using the two letter abbreviation. Supports multiple states if desired.
#'@param measure Specify the measures of the data pull. Supports multiple measures if desired. For a full list of available measures, see the function 'get_dictionary'.
#'@param county Specify the county of the desired data using the full name of the county, with a capital letter. Not supported for 'place' or 'zcta' geography (use 'state' to filter instead).
#'@param release Specify the year of release for the PLACES data set. Currently supports years 2020-2025.
#'@param geometry if FALSE (the default), return a regular data frame of PLACES data. If TRUE, uses the tigris package to return an sf data frame with simple feature geometry in the 'geometry' column.
#'@param cat Specify the category of measures to return. Overrides the argument 'measure'. Category ID must be used here. Options include 'DISABILT', 'HLTHOUT', 'HLTHSTAT', 'PREVENT', 'RISKBEH', and 'SOCLNEED' (for release 2024). To see all the available categories and their corresponding variables, run get_dictionary.
#'@param age_adjust For queries on the county or place level. If TRUE, returns only the age-adjusted values.
#'
#'@examples
#'\dontrun{
#'get_places(geography = "county", state = "MI", measure = "SLEEP", release = "2023")
#'get_places(geography = "county", state = c("MI", "OH"),
#'measure = c("SLEEP", "ACCESS2"), release = "2023")
#'}
#'@importFrom curl has_internet curl_fetch_memory
#'@importFrom tigris counties places tracts zctas
#'@importFrom sf st_as_sf
#'@importFrom yyjsonr read_json_str
#'
#'@export get_places
#'@returns A data frame that contains observations for each measure and geographic level.

get_places <- function(geography = "county", state = NULL, measure = NULL, county = NULL,
                       release = "2025", geometry = FALSE, cat = NULL, age_adjust = NULL){

  if(geography == "census"){
    stop("As of version 1.1.11, 'census' geography has been changed to 'tract' for clarity. Please adjust your code accordingly.")
  }

  if(!is.null(county) && geography == "place"){
    stop("The 'county' parameter is not supported for place geography. Use 'state' to filter place data instead.")
  }

  if(!is.null(cat)){
    if(!is.null(measure)){
      message("A category was provided. Any items included in the 'measure' argument will be overridden.")
    }
    measure = unique(measures[measures$categoryid == cat,]$measureid)
  }

  # Assigning base url
  base_url <- api_urls[api_urls$release %in% release &
                     api_urls$geography %in% geography,]$url


  if(length(base_url) == 0){
    stop("Geographic level or release year not supported.")
  }

  if(is.na(base_url)){
    stop("ZCTA data is not available for the 2024 release. ",
         "The CDC did not publish a long-format ZCTA dataset for this release year. ",
         "Please use release '2023' or '2025' instead.")
  }

  base <- paste0(base_url, "?$query=SELECT%20*%20")

  # Check for internet

  if(!curl::has_internet()){
    message("Request could not be completed. No internet connection.")
    return(invisible(NULL))
  }

  # Data pull
  # if county is null

  if(geography == "zcta"){

    if(is.null(check_api(base))){
      return(NULL)
    }

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

      places_out <- fetch_zcta_batched(base, zlist)

    }else{

      places_out <- fetch_zcta_batched(base, zlist, measure)

    }

    numeric_cols <- c("data_value", "low_confidence_limit",
                       "high_confidence_limit", "data_value_footnote_symbol")
    numeric_cols <- intersect(numeric_cols, names(places_out))
    places_out[numeric_cols] <- lapply(places_out[numeric_cols], as.numeric)

    if(isTRUE(geometry)){

      if(release %in% c("2024", "2025")){

        geo <- tigris::zctas(year = 2020)
        geo <- geo[geo$ZCTA5CE20 %in% places_out$locationid, c("ZCTA5CE20", "geometry")]

        places_out <- merge(places_out, geo, by.x = "locationid", by.y = "ZCTA5CE20") |>
          sf::st_as_sf()

      }else{

        geo <- data.frame()

        for(i in state){
          geo_add <- tigris::zctas(state = i, year = 2010)
          geo_add <- geo_add[,c("ZCTA5CE10", "geometry")]
          geo <- rbind(geo, geo_add)
        }

        places_out <- merge(places_out, geo, by.x = "locationid", by.y = "ZCTA5CE10") |>
          sf::st_as_sf()

      }

    }

    return(places_out)

  }

  if(is.null(county)){

    if(is.null(state) & is.null(measure)){

      message("Pulling data for all geographies. This may take some time...")

      if(is.null(check_api(base))){       return(NULL)     }

      places1 <- curl::curl_fetch_memory(base)

      places_out <-  parse_request(places1$content)

    }else if(is.null(measure)){ # state, no measures or county

      lapply(state, check_states)

      if(is.null(check_api(base))){       return(NULL)     }

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

      if(is.null(check_api(base))){       return(NULL)     }

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

      if(is.null(check_api(base))){       return(NULL)     }

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

      if(is.null(check_api(base))){       return(NULL)     }

      places1 <- paste0(
        base,
        format_query(state, "state", "WHERE", geography),
        "%20",
        format_query(to_title_case(county), "county", "AND", geography),
        "%20",
        "%20LIMIT%205000000"
      ) |>
        curl::curl_fetch_memory()

      places_out <- places1$content |>
        parse_request()


    }else if(is.null(state)){ #specific measures and county, no state specified

      lapply(measure, check_measures, ryear=release)

      if(is.null(check_api(base))){       return(NULL)     }

      places1 <- paste0(
        base,
        format_query(measure, "measure", "WHERE", geography),
        "%20",
        format_query(to_title_case(county), "county", "AND", geography),
        "%20LIMIT%205000000"
      ) |>
        curl::curl_fetch_memory()

      places_out <- places1$content |>
        parse_request()

    }else{

      lapply(state, check_states)

      lapply(measure, check_measures, ryear=release)

      if(is.null(check_api(base))){       return(NULL)     }

      places1 <- paste0(
        base,
        format_query(state, "state", "WHERE", geography),
        "%20",
        format_query(to_title_case(county), "county", "AND", geography),
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



  }else if(geography == "tract"){

    if(is.null(state)){
      stop("You must provide state names in order to add shapefiles to this query.", call. = FALSE)
    }

    tract_year <- if(release %in% c("2024", "2025")) 2020 else 2010
    tract_geoid <- if(tract_year == 2020) "GEOID" else "GEOID10"

    geo <- data.frame()

    for (i in state){
      geo_add <- tigris::tracts(state = i, year = tract_year)
      geo_add <- geo_add[, c(tract_geoid, "geometry")]
      geo <- rbind(geo, geo_add)
    }

    places_out <- merge(places_out, geo, by.x = "locationid", by.y = tract_geoid) |>
      sf::st_as_sf()

  }else if(geography == "place"){

    if(is.null(state)){
      stop("You must provide state names in order to add shapefiles to this query.", call. = FALSE)
    }

    geo <- data.frame()

    for (i in state){
      geo_add <- tigris::places(state = i, year = 2020)
      geo_add <- geo_add[, c("GEOID", "geometry")]
      geo <- rbind(geo, geo_add)
    }

    places_out <- merge(places_out, geo, by.x = "locationid", by.y = "GEOID") |>
      sf::st_as_sf()

  }


  }

  if(geography %in% c("county", "place")){

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

  try_GET <- function(x, ...) {
    tryCatch(
      curl::curl_fetch_memory(url = x),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  resp <- try_GET(x)

  if(is.character(resp)){
    message("API request failed: ", resp)
    return(invisible(NULL))
  }

  if(resp$status_code != 200){
    message("Status code: ", resp$status_code)
    message("For full response code details visit: https://dev.socrata.com/docs/response-codes.html.")
    return(invisible(NULL))
  }

  return(invisible(1))

}


#' internal test check to see if API is online
#'@param x The base url used in API query
#'@noRd
test_check_api <- function(x){

  try_GET <- function(x, ...) {
    tryCatch(
      curl::curl_fetch_memory(url = x),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  resp <- try_GET(x)

  if(is.character(resp)){
    message("API request failed: ", resp)
    return(invisible(1))
  }

  if(resp$status_code != 200){
    message("Status code: ", resp$status_code)
    return(invisible(1))
  }

  return(invisible(0))

}




#'pastes together the required url to query the API from a state/county's ZCTAs.
#'@param my_vector vector of zip codes to add to the query
#'@noRd
#'
formatted_zctas <- function(my_vector) {
  if(length(my_vector) == 1) {
    return(paste0("WHERE%20locationname%20%3D%20'", my_vector, "'"))
  }

  # Use IN operator instead of multiple LIKE statements
  zcta_list <- paste(my_vector, collapse = "','")
  paste0("WHERE%20locationname%20IN%20('", zcta_list, "')")
}


#'pastes together the required measures to query the API for a ZCTA query.
#'@param measure vector of measures to add to the query
#'@noRd
measure_text <- function(measure){
  if(length(measure) == 1) {
    paste0("%20AND%20measureid%20%3D%20'", measure, "'", collapse = "")
  } else {
    # Use IN operator instead of multiple LIKE statements
    measure_list <- paste(measure, collapse = "','")
    paste0("%20AND%20measureid%20IN%20('", measure_list, "')")
  }
}

#'pastes together the required variable names to query the API.
#'@param x variable to query
#'@param var the var type: measure, state, or county.
#'@param operator AND or WHERE
#'@param type the geography type of the query
#'@noRd
format_query <- function(x, var, operator, type){

  # Map var to actual column name
  if(var == "measure"){
    var <- "measureid"
  } else if(var == "state"){
    var <- "stateabbr"
  } else if(var == "county"){
    if(type %in% c("county", "place")){
      var <- "locationname"
    } else if(type == "tract"){
      var <- "countyname"
    }
  }

  # Build query using IN operator for cleaner, shorter URLs
  if(length(x) == 1){
    paste0(operator, "%20", var, "%20%3D%20'", x, "'")
  } else {
    values <- paste(x, collapse = "','")
    paste0(operator, "%20", var, "%20IN%20('", values, "')")
  }
}

#'Convert a string to title case to match API county name format
#'@param x character vector to convert
#'@noRd
to_title_case <- function(x) {
  vapply(x, function(s) {
    words <- strsplit(tolower(s), " ")[[1]]
    paste(vapply(words, function(w) {
      paste0(toupper(substring(w, 1, 1)), substring(w, 2))
    }, character(1)), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

#'Fetch ZCTA data, batching requests if the URL would exceed the max length
#'@param base the base API URL with query prefix
#'@param zlist vector of ZCTAs to query
#'@param measure optional measure filter
#'@param max_url_length maximum URL length before batching (Socrata limit ~7000)
#'@noRd
fetch_zcta_batched <- function(base, zlist, measure = NULL, max_url_length = 7000){

  suffix <- if(is.null(measure)){
    "%20LIMIT%205000000"
  }else{
    paste0(measure_text(measure), "%20LIMIT%205000000")
  }

  full_url <- paste0(base, formatted_zctas(zlist), suffix)

  if(nchar(full_url) <= max_url_length){
    resp <- curl::curl_fetch_memory(full_url)
    return(parse_request(resp$content))
  }

  # Estimate batch size that keeps URL under the limit
  overhead <- nchar(paste0(base, "WHERE%20locationname%20IN%20('')", suffix))
  chars_per_zcta <- 8  # 5 digits + "','"
  batch_size <- max(1, floor((max_url_length - overhead) / chars_per_zcta))

  batches <- split(zlist, ceiling(seq_along(zlist) / batch_size))

  results <- lapply(batches, function(batch){
    url <- paste0(base, formatted_zctas(batch), suffix)
    resp <- curl::curl_fetch_memory(url)
    parse_request(resp$content)
  })

  do.call(rbind, results)

}

#'parses the json content from an API response
#'@param x raw content from curl response
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

  if(nrow(final_sum) > 0){

    if(max(final_sum$n) > 1){

      if(!interactive()){
        message("Overlapping county names detected. Including all counties in non-interactive mode.")
        return(trial$zcta)
      }

      message("You have overlapping county names.")
      print(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, -3])

      message("Do you want to include overlaps?")
      response1 <- readline("Response (y/n):  ")

      if(response1 == "y"){
        message("OK, we will include all counties.")

        return(trial$zcta)

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

          return(fil$zcta)

        }else{

          fil <- trial[
            !(
              trial$county_name %in% unique(initial_sum$county_name[initial_sum$county_name %in% final_sum$county_name[final_sum$n > 1]]) &
                trial$state_usps %in% response2
            ),
          ]

          return(fil$zcta)

        }


      }

    }

  }

  return(trial$zcta)

}


#'checks if returned county/census tract data contains overlapping county names
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

  if(nrow(final_sum) > 0){

    if(max(final_sum$n) > 1){

      if(!interactive()){
        message("Overlapping county names detected. Including all counties in non-interactive mode.")
        return(places)
      }

      message("You have overlapping county names.")
      print(initial_sum[initial_sum$county_name %in% final_sum[final_sum$n>1,]$county_name, -3])

      message("Do you want to include overlaps?")
      response1 <- readline("Response (y/n):  ")

      if(response1 == "y"){
        message("OK, we will include all counties.")

        return(places)

      }else{

        message("Which should we exclude? Please respond with the state abbreviation(s) to filter out. If you have multiple states, please separate with a space (ex. 'NY TX').")

        response2 <- readline("Response:  ")

        exclude_states <- if(nchar(response2) > 2){
          strsplit(response2, split = " ")[[1]]
        } else {
          response2
        }

        overlap_fips <- unique(initial_sum$county_fips[initial_sum$county_name %in% final_sum$county_name[final_sum$n > 1]])

        if(geography == "county"){
          return(places[
            !(places$stateabbr %in% exclude_states &
                places$locationid %in% overlap_fips),
          ])
        }else if (geography == "tract"){
          return(places[
            !(places$stateabbr %in% exclude_states &
                places$countyfips %in% overlap_fips),
          ])
        }

      }

    }

  }

  return(places)

}
