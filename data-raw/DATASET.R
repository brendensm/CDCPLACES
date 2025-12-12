library(dplyr)
library(googlesheets4)

measures <- get_dictionary()

api_urls <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1kYtpuH9DeKQJ6TxYhbBsUR2Kr9p_o3T1dPw1-nfpVB0/edit?gid=0#gid=0")

usethis::use_data(measures, api_urls, internal = TRUE, compress = "xz", overwrite = TRUE)

