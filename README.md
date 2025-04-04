
# coauthoR

<!-- badges: start -->
<!-- badges: end -->

The goal of coauthoR is to create formatted author list in a title page of a manuscript automatically.

## Installation

You can install the development version of coauthoR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kfukasawa37/coauthoR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(coauthoR)
library(magrittr)
library(dplyr)
data(coauthors)
name<-coauthors%>%
    dplyr::select(authorID,name,rank)%>%
    dplyr::distinct(authorID,name,rank,.keep_all=TRUE)
affiliation<-coauthors%>%
    dplyr::select(authorID,affiliation,address)%>%
    dplyr::distinct(authorID,affiliation,.keep_all=TRUE)
email<-coauthors%>%
    dplyr::select(authorID,email)%>%
    dplyr::distinct(authorID,email,.keep_all=TRUE)
#Write a formatted author list as txt file 
format_coauthors(name,affiliation,email,corresp=1)
```

