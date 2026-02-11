# Live API integration tests — skipped on CRAN
#
# These tests make real HTTP requests to the CDC PLACES Socrata APIs.
# They are skipped on CRAN and when the network/API is unavailable.

skip_if_api_unavailable <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  resp <- tryCatch(
    curl::curl_fetch_memory("https://data.cdc.gov/resource/swc5-untb.json?$query=SELECT%20*%20%20LIMIT%201"),
    error = function(e) NULL
  )

  if (is.null(resp) || resp$status_code != 200) {
    testthat::skip("CDC PLACES API is not available")
  }
}

# --- County tests -------------------------------------------------------------

test_that("county: single state, single measure", {
  skip_if_api_unavailable()

  result <- get_places(state = "MI", measure = "SLEEP", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(c("data_value", "measureid", "stateabbr", "locationid") %in% names(result)))
  expect_true(all(result$stateabbr == "MI"))
  expect_true(all(result$measureid == "SLEEP"))
  expect_true(is.numeric(result$data_value))
})

test_that("county: multiple states, multiple measures", {
  skip_if_api_unavailable()

  result <- get_places(
    state = c("MI", "OH"),
    measure = c("SLEEP", "ACCESS2"),
    release = "2025"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(result$stateabbr %in% c("MI", "OH")))
  expect_true(all(result$measureid %in% c("SLEEP", "ACCESS2")))
})

test_that("county: state only, no measure returns all measures", {
  skip_if_api_unavailable()

  result <- get_places(state = "MI", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(length(unique(result$measureid)), 1)
})

test_that("county: measure only, no state returns all states", {
  skip_if_api_unavailable()

  result <- get_places(measure = "SLEEP", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(length(unique(result$stateabbr)), 1)
})

test_that("county: older release year (2020) still works", {
  skip_if_api_unavailable()

  result <- get_places(state = "MI", measure = "SLEEP", release = "2020")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
})

# --- County with county filter ------------------------------------------------

test_that("county: county filter returns data", {
  skip_if_api_unavailable()

  result <- get_places(state = "MI", measure = "SLEEP", county = "Wayne", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
})

# --- age_adjust ---------------------------------------------------------------

test_that("county: age_adjust = TRUE returns only age-adjusted values", {
  skip_if_api_unavailable()

  result <- get_places(state = "MI", measure = "SLEEP", release = "2025", age_adjust = TRUE)

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(result$datavaluetypeid == "AgeAdjPrv"))
})

# --- cat argument -------------------------------------------------------------

test_that("county: cat argument returns measures from that category", {
  skip_if_api_unavailable()

  result <- get_places(state = "MI", cat = "PREVENT", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  prevent_ids <- unique(measures[measures$categoryid == "PREVENT", ]$measureid)
  expect_true(all(result$measureid %in% prevent_ids))
})

test_that("county: cat overrides measure with a message", {
  skip_if_api_unavailable()

  expect_message(
    result <- get_places(state = "MI", measure = "SLEEP", cat = "PREVENT", release = "2025"),
    "overridden"
  )

  expect_s3_class(result, "data.frame")
  # Should contain PREVENT measures, not just SLEEP
  expect_gt(length(unique(result$measureid)), 1)
})

# --- Tract tests --------------------------------------------------------------

test_that("tract: single state, single measure", {
  skip_if_api_unavailable()

  result <- get_places(geography = "tract", state = "MI", measure = "SLEEP", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(result$stateabbr == "MI"))
  expect_true(all(result$measureid == "SLEEP"))
})

test_that("tract: multiple states, multiple measures", {
  skip_if_api_unavailable()

  result <- get_places(
    geography = "tract",
    state = c("WI", "OH"),
    measure = c("STROKE", "SLEEP"),
    release = "2025"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(result$stateabbr %in% c("WI", "OH")))
  expect_true(all(result$measureid %in% c("STROKE", "SLEEP")))
})

# --- ZCTA tests ---------------------------------------------------------------

test_that("zcta: single state, single measure", {
  skip_if_api_unavailable()

  result <- get_places(geography = "zcta", state = "MI", measure = "SLEEP", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(is.numeric(result$data_value))
})

test_that("zcta: single state, no measure returns all measures", {
  skip_if_api_unavailable()

  result <- get_places(geography = "zcta", state = "DC", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(length(unique(result$measureid)), 1)
})

test_that("zcta: errors when state is missing", {
  expect_error(
    get_places(geography = "zcta", measure = "SLEEP", release = "2025"),
    "state"
  )
})

# --- Place tests --------------------------------------------------------------

test_that("place: single state, single measure", {
  skip_if_api_unavailable()

  result <- get_places(geography = "place", state = "MI", measure = "SLEEP", release = "2025")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(c("data_value", "measureid", "stateabbr", "locationid") %in% names(result)))
  expect_true(all(result$stateabbr == "MI"))
  expect_true(all(result$measureid == "SLEEP"))
  expect_true(is.numeric(result$data_value))
})

test_that("place: age_adjust = TRUE returns only age-adjusted values", {
  skip_if_api_unavailable()

  result <- get_places(geography = "place", state = "DC", measure = "SLEEP", release = "2025", age_adjust = TRUE)

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(result$datavaluetypeid == "AgeAdjPrv"))
})

# --- get_dictionary() ---------------------------------------------------------

test_that("get_dictionary returns a data frame of measures", {
  skip_if_api_unavailable()

  result <- get_dictionary()

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
})

# --- Graceful failure ---------------------------------------------------------

test_that("get_places returns NULL gracefully when there is no internet", {
  skip_if_api_unavailable()

  # Mock has_internet to return FALSE
  local_mocked_bindings(has_internet = function() FALSE, .package = "curl")

  expect_message(
    result <- get_places(state = "MI", measure = "SLEEP"),
    "No internet"
  )
  expect_null(result)
})
