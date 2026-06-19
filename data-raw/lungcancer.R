## code to prepare `lungcancer` dataset goes here
library(readr)
lungcancer <- read_csv("data-raw/lungcancer.csv")
usethis::use_data(lungcancer, overwrite = TRUE)
