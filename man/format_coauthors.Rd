% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coauthor.r
\name{format_coauthors}
\alias{format_coauthors}
\title{A function to convert tables of coauthors' names, affiliations/addresses, and emails to a document file with a formatted coauthor list.}
\usage{
format_coauthors(
  name,
  affiliation,
  email,
  corresp = NULL,
  filename = NULL,
  output_type = "TXT",
  ...
)
}
\arguments{
\item{name}{tibble or data.frame. The columns of tables should include fields of unique author IDs (column \code{authorID}), names (column \code{names}) and rank of authors (column \code{rank}). Duplicated rows should be removed a priori.}

\item{affiliation}{tibble or data.frame. The columns of tables should include fields of unique author IDs (column \code{authorID}), affiliations (column \code{affiliation}) and addresses (column \code{address}). Duplicated rows should be removed a priori. If a person has multiple affiliations, they can be include to the table as rows sharing the same \code{authorID}. If the field of \code{affiliation} includes the address, the column of \code{address} should be \code{NA}s.}

\item{email}{tibble or data.frame. The columns of tables should include fields of unique author IDs (column \code{authorID}) and email addresses (column \code{email}). Duplicated rows should be removed a priori. If a person has multiple email addresses, they can be include to the table as rows sharing the same \code{authorID}.}

\item{corresp}{\code{authorID}(s) of corresponding author. By default, corresponding author is not specified.}

\item{filename}{Filename of output file. By default, "output_\%y\%m\%d\%H\%M\%S.rtf".}

\item{output_type}{The report output type passed to  \code{reporter::create_report()}. Default is "RTF". Valid values are "TXT", "RTF", "PDF", "HTML", and "DOCX".}

\item{...}{Additional arguments passed to \code{reporter::create_report()}}
}
\value{
A list with tibbles of author names, affiliations and emails.
}
\description{
This function produces a nested tibble which can be passed to function \code{str_coauthors()}
}
\examples{
library(coauthoR)
library(magrittr)
library(dplyr)
data(coauthors)
name<-coauthors\%>\%
     dplyr::select(authorID,name,rank)\%>\%
     dplyr::distinct(authorID,name,rank,.keep_all=TRUE)
affiliation<-coauthors\%>\%
     dplyr::select(authorID,affiliation,address)\%>\%
     dplyr::distinct(authorID,affiliation,.keep_all=TRUE)
email<-coauthors\%>\%
     dplyr::select(authorID,email)\%>\%
     dplyr::distinct(authorID,email,.keep_all=TRUE)
format_coauthors(name,affiliation,email,corresp=1)
}
