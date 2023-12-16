#'Get Vars
#'
#'Use this function to source a list of available measures in the CDC PLACES data set.
#'

get_measures <- function(){
  measures <- read.csv("data/measures.csv")[,-1]
  View(measures)
}
