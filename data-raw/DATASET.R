library(dplyr)

measures <- get_places(state = "MI") %>%
  select(measureid, short_question_text, measure, categoryid) %>%
  unique()


usethis::use_data(measures, compress = "xz")
