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
    expect_s3_class(get_places(geo = "county", state = c("WI", "OH"), measure = c("STROKE", "SLEEP", "DIABETES", "BINGE"), release = i), "data.frame")
  })
}

######################### Census level tests #################################

# census test for each year (one state, one measure)

for (i in years){
  test_that(paste0("function accesses census api ", i, "(one state, one measure)"), {
    expect_s3_class(get_places(geo = "census", state = "MI", measure = "SLEEP", release = i), "data.frame")
  })
}

# census test for each year (multple states, one measure)
for (i in years){
  test_that(paste0("function accesses census api", i, "(multiple states, one measure)"), {
    expect_s3_class(get_places(geo = "census", state = c("OH", "WI"), measure = "ACCESS2"), "data.frame")
  })
}


# census test for each year (multiple measures, one state)
for (i in years){
  test_that(paste0("function access census api", i, "(multiple measures, one state)"),{
    expect_s3_class(get_places(geo = "census", state = c("WI"), measure = c("STROKE", "SLEEP"), release = i), "data.frame")
  })
}


# census tests for each year (multiple measures and states)

for (i in years){
  test_that(paste0("function access census api", i, "(multiple measures, multiple states), more states"),{
    expect_s3_class(get_places(geo = "census", state = c("WI", "OH", "MI", "CA"), measure = c("STROKE", "SLEEP"), release = i), "data.frame")
  })
}

for (i in years){
  test_that(paste0("function access census api", i, "(multiple measures, multiple states), more measures"),{
    expect_s3_class(get_places(geo = "census", state = c("WI", "OH"), measure = c("DIABETES", "SLEEP", "STROKE", "BINGE"), release = i), "data.frame")
  })
}





