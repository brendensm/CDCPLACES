library(dplyr)

measures23 <- get_places(state = "MI") %>%
  select(year, measureid, short_question_text, measure, categoryid) %>%
  unique() %>%
  mutate(release = "2023")

measures22 <- get_places(state = "MI", release = "2022") %>%
  select(year, measureid, short_question_text, measure, categoryid) %>%
  unique() %>%
  mutate(release = "2022")

measures21 <- get_places(state = "MI", release = "2021") %>%
  select(year, measureid, short_question_text, measure, categoryid) %>%
  unique() %>%
  mutate(release = "2021")

measures20 <- get_places(state = "MI", release = "2020") %>%
  select(year, measureid, short_question_text, measure, categoryid) %>%
  unique() %>%
  mutate(release = "2020")

usethis::use_data(measures23, compress = "xz")
usethis::use_data(measures22, compress = "xz")
usethis::use_data(measures21, compress = "xz")
usethis::use_data(measures20, compress = "xz")
