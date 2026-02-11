
# First test for internet

if(isTRUE(curl::has_internet())){

  # check if APIs are online

  if(sum(
    unlist(lapply(api_urls$url[!is.na(api_urls$url)],test_check_api))
  ) < 1){

    # Initial API Access by Year
    years <- c("2020", "2021", "2022", "2023", "2024", "2025")

    # county tests for each year (one state, one measure)

    for (i in years){
      test_that(paste0("function accesses county api ", i, "(one state, one measure)"), {
        testthat::skip_on_cran()
        expect_s3_class(get_places(state = "MI", measure = "SLEEP", release = i), "data.frame")
        Sys.sleep(10)
      })
    }

    # county test for each year (multple states, one measure)
    for (i in years){
      test_that(paste0("function accesses county api", i, "(multiple states, one measure)"), {
        testthat::skip_on_cran()
        expect_s3_class(get_places(state = c("OH", "WI"), measure = "ACCESS2", release = i), "data.frame")
        Sys.sleep(10)
      })
    }


    # county test for each year (multiple measures, one state)
    for (i in years){
      test_that(paste0("function access county api", i, "(multiple measures, one state)"),{
        testthat::skip_on_cran()
        expect_s3_class(get_places(state = c("WI"), measure = c("ACCESS2", "SLEEP"), release = i), "data.frame")
        Sys.sleep(10)
      })
    }


    # county tests for each year (multiple measures and states)

    for (i in years){
      test_that(paste0("function access county api", i, "(multiple measures, multiple states), more states"),{
        testthat::skip_on_cran()
        expect_s3_class(get_places(state = c("WI", "OH", "MI", "CA"), measure = c("ACCESS2", "SLEEP"), release = i), "data.frame")
        Sys.sleep(10)
      })
    }

    for (i in years){
      test_that(paste0("function access county api", i, "(multiple measures, multiple states), more measures"),{
        testthat::skip_on_cran()
        expect_s3_class(get_places(geography = "county", state = c("WI", "OH"), measure = c("STROKE", "SLEEP", "DIABETES", "BINGE"), release = i), "data.frame")
        Sys.sleep(10)
      })
    }

    ######################### Census level tests #################################

    # census test for each year (one state, one measure)

    for (i in years){
      test_that(paste0("function accesses census tract api ", i, "(one state, one measure)"), {
        testthat::skip_on_cran()
        expect_s3_class(get_places(geography = "tract", state = "MI", measure = "SLEEP", release = i), "data.frame")
        Sys.sleep(10)
      })
    }

    # census test for each year (multple states, one measure)
    for (i in years){
      test_that(paste0("function accesses census tract api", i, "(multiple states, one measure)"), {
        testthat::skip_on_cran()
        expect_s3_class(get_places(geography = "tract", state = c("OH", "WI"), measure = "ACCESS2"), "data.frame")
        Sys.sleep(10)
      })
    }


    # census test for each year (multiple measures, one state)
    for (i in years){
      test_that(paste0("function access census tract api", i, "(multiple measures, one state)"),{
        testthat::skip_on_cran()
        expect_s3_class(get_places(geography = "tract", state = c("WI"), measure = c("STROKE", "SLEEP"), release = i), "data.frame")
        Sys.sleep(10)
      })
    }


    # census tests for each year (multiple measures and states)

    for (i in years){
      test_that(paste0("function access census tract api", i, "(multiple measures, multiple states), more states"),{
        testthat::skip_on_cran()
        expect_s3_class(get_places(geography = "tract", state = c("WI", "OH", "MI", "CA"), measure = c("STROKE", "SLEEP"), release = i), "data.frame")
        Sys.sleep(10)
      })
    }

    for (i in years){
      test_that(paste0("function access census tract api", i, "(multiple measures, multiple states), more measures"),{
        testthat::skip_on_cran()
        expect_s3_class(get_places(geography = "tract", state = c("WI", "OH"), measure = c("DIABETES", "SLEEP", "STROKE", "BINGE"), release = i), "data.frame")
        Sys.sleep(10)
      })
    }

  }else{
    test_that("query returns null with no internet",{
      testthat::skip_on_cran()
      testthat::expect_null(get_places(state = "MI", measure = "SLEEP"))
    })

  }


}










