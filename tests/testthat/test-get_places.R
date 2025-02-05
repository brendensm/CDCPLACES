
# First test for internet

if(isTRUE(curl::has_internet())){

  # check if APIs are online

  base23_cy_val <- test_check_api("https://data.cdc.gov/resource/swc5-untb.json")
  base23_cs_val <- test_check_api("https://data.cdc.gov/resource/cwsq-ngmh.json")
  base22_cy_val <- test_check_api("https://data.cdc.gov/resource/duw2-7jbt.json")
  base22_cs_val <- test_check_api("https://data.cdc.gov/resource/nw2y-v4gm.json")
  base21_cy_val <- test_check_api("https://data.cdc.gov/resource/pqpp-u99h.json")
  base21_cs_val <- test_check_api("https://data.cdc.gov/resource/373s-ayzu.json")
  base20_cy_val <- test_check_api("https://data.cdc.gov/resource/dv4u-3x3q.json")
  base20_cs_val <- test_check_api("https://data.cdc.gov/resource/4ai3-zynv.json")

  if(sum(
    base23_cy_val,
    base23_cs_val,
    base22_cy_val,
    base22_cs_val,
    base21_cy_val,
    base21_cs_val,
    base20_cy_val,
    base20_cs_val
  ) < 1){

    # Initial API Access by Year
    years <- c("2020", "2021", "2022", "2023")

    # county tests for each year (one state, one measure)

    for (i in years){
      test_that(paste0("function accesses county api ", i, "(one state, one measure)"), {
        expect_s3_class(get_places(state = "MI", measure = "SLEEP", release = i), "data.frame")
      })
    }

    # county test for each year (multple states, one measure)
    for (i in years){
      test_that(paste0("function accesses county api", i, "(multiple states, one measure)"), {
        expect_s3_class(get_places(state = c("OH", "WI"), measure = "ACCESS2", release = i), "data.frame")
      })
    }


    # county test for each year (multiple measures, one state)
    for (i in years){
      test_that(paste0("function access county api", i, "(multiple measures, one state)"),{
        expect_s3_class(get_places(state = c("WI"), measure = c("ACCESS2", "SLEEP"), release = i), "data.frame")
      })
    }


    # county tests for each year (multiple measures and states)

    for (i in years){
      test_that(paste0("function access county api", i, "(multiple measures, multiple states), more states"),{
        expect_s3_class(get_places(state = c("WI", "OH", "MI", "CA"), measure = c("ACCESS2", "SLEEP"), release = i), "data.frame")
      })
    }



    # county tests for each year (one state, one measure)

    for (i in years){
      test_that(paste0("function accesses county api ", i, "(one state, one measure)"), {
        expect_s3_class(get_places(state = "MI", measure = "SLEEP", release = i), "data.frame")
      })
    }

    # county test for each year (multple states, one measure)
    for (i in years){
      test_that(paste0("function accesses county api", i, "(multiple states, one measure)"), {
        expect_s3_class(get_places(state = c("OH", "WI"), measure = "ACCESS2"), "data.frame")
      })
    }


    # county test for each year (multiple measures, one state)
    for (i in years){
      test_that(paste0("function access county api", i, "(multiple measures, one state)"),{
        expect_s3_class(get_places(state = c("WI"), measure = c("ACCESS2", "SLEEP"), release = i), "data.frame")
      })
    }


    # county tests for each year (multiple measures and states)

    for (i in years){
      test_that(paste0("function access county api", i, "(multiple measures, multiple states), more states"),{
        expect_s3_class(get_places(state = c("WI", "OH", "MI", "CA"), measure = c("ACCESS2", "SLEEP"), release = i), "data.frame")
      })
    }

    for (i in years){
      test_that(paste0("function access county api", i, "(multiple measures, multiple states), more measures"),{
        expect_s3_class(get_places(geography = "county", state = c("WI", "OH"), measure = c("STROKE", "SLEEP", "DIABETES", "BINGE"), release = i), "data.frame")
      })
    }

    ######################### Census level tests #################################

    # census test for each year (one state, one measure)

    for (i in years){
      test_that(paste0("function accesses census api ", i, "(one state, one measure)"), {
        expect_s3_class(get_places(geography = "census", state = "MI", measure = "SLEEP", release = i), "data.frame")
      })
    }

    # census test for each year (multple states, one measure)
    for (i in years){
      test_that(paste0("function accesses census api", i, "(multiple states, one measure)"), {
        expect_s3_class(get_places(geography = "census", state = c("OH", "WI"), measure = "ACCESS2"), "data.frame")
      })
    }


    # census test for each year (multiple measures, one state)
    for (i in years){
      test_that(paste0("function access census api", i, "(multiple measures, one state)"),{
        expect_s3_class(get_places(geography = "census", state = c("WI"), measure = c("STROKE", "SLEEP"), release = i), "data.frame")
      })
    }


    # census tests for each year (multiple measures and states)

    for (i in years){
      test_that(paste0("function access census api", i, "(multiple measures, multiple states), more states"),{
        expect_s3_class(get_places(geography = "census", state = c("WI", "OH", "MI", "CA"), measure = c("STROKE", "SLEEP"), release = i), "data.frame")
      })
    }

    for (i in years){
      test_that(paste0("function access census api", i, "(multiple measures, multiple states), more measures"),{
        expect_s3_class(get_places(geography = "census", state = c("WI", "OH"), measure = c("DIABETES", "SLEEP", "STROKE", "BINGE"), release = i), "data.frame")
      })
    }

  }else{
    test_that("query returns null with no internet",{
      testthat::expect_null(get_places(state = "MI", measure = "SLEEP"))
    })

  }


}










