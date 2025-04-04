library(readr)
coauthors<-read_csv("./development/fukasawa_et_al_2025_biodiv_data_J.csv")
usethis::use_data(coauthors,overwrite=TRUE)
