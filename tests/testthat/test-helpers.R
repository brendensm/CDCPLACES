# Offline unit tests for internal helper functions
# These run on CRAN — no network access required

# --- format_query() -----------------------------------------------------------

test_that("format_query produces correct SoQL for a single measure", {
  result <- format_query("SLEEP", "measure", "WHERE", "county")
  expect_equal(result, "WHERE%20measureid%20%3D%20'SLEEP'")
})

test_that("format_query produces correct SoQL for multiple measures", {

  result <- format_query(c("SLEEP", "ACCESS2"), "measure", "WHERE", "county")
  expect_equal(result, "WHERE%20measureid%20IN%20('SLEEP','ACCESS2')")
})

test_that("format_query produces correct SoQL for a single state", {
  result <- format_query("MI", "state", "WHERE", "county")
  expect_equal(result, "WHERE%20stateabbr%20%3D%20'MI'")
})

test_that("format_query produces correct SoQL for multiple states with AND", {
  result <- format_query(c("MI", "OH"), "state", "AND", "county")
  expect_equal(result, "AND%20stateabbr%20IN%20('MI','OH')")
})

test_that("format_query maps county var to locationname for county geography", {
  result <- format_query("WAYNE", "county", "AND", "county")
  expect_equal(result, "AND%20locationname%20%3D%20'WAYNE'")
})

test_that("format_query maps county var to countyname for tract geography", {
  result <- format_query("WAYNE", "county", "AND", "tract")
  expect_equal(result, "AND%20countyname%20%3D%20'WAYNE'")
})

# --- formatted_zctas() --------------------------------------------------------

test_that("formatted_zctas produces WHERE = for a single ZCTA", {
  result <- formatted_zctas("48104")
  expect_equal(result, "WHERE%20locationname%20%3D%20'48104'")
})

test_that("formatted_zctas produces WHERE IN for multiple ZCTAs", {
  result <- formatted_zctas(c("48104", "48105", "48108"))
  expect_equal(result, "WHERE%20locationname%20IN%20('48104','48105','48108')")
})

# --- measure_text() -----------------------------------------------------------

test_that("measure_text produces AND = for a single measure", {
  result <- measure_text("SLEEP")
  expect_equal(result, "%20AND%20measureid%20%3D%20'SLEEP'")
})

test_that("measure_text produces AND IN for multiple measures", {
  result <- measure_text(c("SLEEP", "ACCESS2"))
  expect_equal(result, "%20AND%20measureid%20IN%20('SLEEP','ACCESS2')")
})

# --- parse_request() ----------------------------------------------------------

test_that("parse_request correctly parses raw JSON into a data frame", {
  json_raw <- charToRaw('[{"col_a":"1","col_b":"hello"}]')
  result <- parse_request(json_raw)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$col_a, "1")
  expect_equal(result$col_b, "hello")
})

test_that("parse_request handles multiple rows", {
  json_raw <- charToRaw('[{"x":"a"},{"x":"b"},{"x":"c"}]')
  result <- parse_request(json_raw)
  expect_equal(nrow(result), 3)
  expect_equal(result$x, c("a", "b", "c"))
})

# --- check_states() -----------------------------------------------------------

test_that("check_states accepts valid US state abbreviations", {
  expect_error(check_states("MI"), NA)
  expect_error(check_states("CA"), NA)
  expect_error(check_states("DC"), NA)
})

test_that("check_states rejects invalid state abbreviations", {
  expect_error(check_states("XX"), "valid US State")
  expect_error(check_states("Michigan"), "valid US State")
  expect_error(check_states(""), "valid US State")
})

# --- check_measures() ---------------------------------------------------------

test_that("check_measures accepts valid measure IDs", {
  expect_error(check_measures("SLEEP", ryear = "2025"), NA)
  expect_error(check_measures("ACCESS2", ryear = "2023"), NA)
})

test_that("check_measures rejects invalid measure IDs", {
  expect_error(check_measures("NOTREAL", ryear = "2025"), "valid measure")
})

test_that("check_measures rejects SOCLNEED measures for non-2024 releases", {
  expect_error(
    check_measures("ISOLATION", ryear = "2023"),
    "social needs"
  )
})

test_that("check_measures allows SOCLNEED measures for 2024 release", {
  expect_error(check_measures("ISOLATION", ryear = "2024"), NA)
})

# --- to_title_case() ----------------------------------------------------------

test_that("to_title_case converts single word", {
  expect_equal(to_title_case("wayne"), "Wayne")
  expect_equal(to_title_case("WAYNE"), "Wayne")
  expect_equal(to_title_case("Wayne"), "Wayne")
})

test_that("to_title_case converts multi-word names", {
  expect_equal(to_title_case("los angeles"), "Los Angeles")
  expect_equal(to_title_case("LOS ANGELES"), "Los Angeles")
  expect_equal(to_title_case("ST. LOUIS"), "St. Louis")
})

test_that("to_title_case handles vector input", {
  result <- to_title_case(c("wayne", "LOS ANGELES"))
  expect_equal(result, c("Wayne", "Los Angeles"))
})

# --- check_counties() ---------------------------------------------------------

test_that("check_counties accepts valid county names", {
  expect_error(check_counties("Wayne"), NA)
  expect_error(check_counties("Los Angeles"), NA)
})

test_that("check_counties rejects invalid county names", {
  expect_error(check_counties("Fake County Name XYZ"), "valid US County")
})

# --- get_places() input validation (offline) ----------------------------------

test_that("get_places errors with informative message for census geography", {
  expect_error(
    get_places(geography = "census"),
    "tract"
  )
})

test_that("get_places errors for unsupported geography", {
  expect_error(get_places(geography = "zipcode"), "not supported")
})

test_that("get_places errors for unsupported release year", {
  expect_error(get_places(release = "1999"), "not supported")
})

test_that("get_places errors for ZCTA with 2024 release", {
  expect_error(
    get_places(geography = "zcta", state = "MI", release = "2024"),
    "not available for the 2024 release"
  )
})

test_that("get_places errors when county is used with place geography", {
  expect_error(
    get_places(geography = "place", state = "MI", county = "Wayne"),
    "not supported for place geography"
  )
})

# --- format_query() with place geography ------------------------------------

test_that("format_query maps county var to locationname for place geography", {
  result <- format_query("Springfield", "county", "AND", "place")
  expect_equal(result, "AND%20locationname%20%3D%20'Springfield'")
})
