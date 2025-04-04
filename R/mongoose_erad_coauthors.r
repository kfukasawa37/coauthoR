setwd("L:\\amami_mongoose")
#library(coauthoR)
library(readxl)
library(dplyr)
library(tidyr)
coauthors<-readxl::read_xlsx("coauthors_mongoose.xlsx")
coauthors<-coauthors%>%dplyr::select(-namejp,-affiliation_shortjp,-affiliationjp1,-affiliationjp2,-addressjp1,-addressjp2)%>%
            
email<-coauthors%>%dplyr::select(rank,email1,email2)
coauthors%>%tidyr::pivot_longer(rank,name,address)
