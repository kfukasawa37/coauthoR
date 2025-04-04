#' @title A function to convert tables of coauthors' names, affiliations/addresses, and emails to a document file with a formatted coauthor list. 
#' @description
#' This function produces a nested tibble which can be passed to function \code{str_coauthors()}
#' @param name tibble or data.frame. The columns of tables should include fields of unique author IDs (column \code{authorID}), names (column \code{names}) and rank of authors (column \code{rank}). Duplicated rows should be removed a priori.
#' @param affiliation tibble or data.frame. The columns of tables should include fields of unique author IDs (column \code{authorID}), affiliations (column \code{affiliation}) and addresses (column \code{address}). Duplicated rows should be removed a priori. If a person has multiple affiliations, they can be include to the table as rows sharing the same \code{authorID}. If the field of \code{affiliation} includes the address, the column of \code{address} should be \code{NA}s.
#' @param email tibble or data.frame. The columns of tables should include fields of unique author IDs (column \code{authorID}) and email addresses (column \code{email}). Duplicated rows should be removed a priori. If a person has multiple email addresses, they can be include to the table as rows sharing the same \code{authorID}.
#' @param corresp \code{authorID}(s) of corresponding author. By default, corresponding author is not specified.
#' @param filename Filename of output file. By default, "output_%y%m%d%H%M%S.rtf". 
#' @param output_type The report output type passed to  \code{reporter::create_report()}. Default is "RTF". Valid values are "TXT", "RTF", "PDF", "HTML", and "DOCX".
#' @param ... Additional arguments passed to \code{reporter::create_report()}
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' @importFrom magrittr "%>%"
#' @importFrom purrr map
#' @importFrom dplyr select filter left_join mutate group_by distinct arrange
#' @importFrom reporter create_text create_report add_content write_report
#' @importFrom common supsc
#' @return A list with tibbles of author names, affiliations and emails.
#' @export 
#' @examples
#' library(coauthoR)
#' library(magrittr)
#' library(dplyr)
#' data(coauthors)
#' name<-coauthors%>%
#'      dplyr::select(authorID,name,rank)%>%
#'      dplyr::distinct(authorID,name,rank,.keep_all=TRUE)
#' affiliation<-coauthors%>%
#'      dplyr::select(authorID,affiliation,address)%>%
#'      dplyr::distinct(authorID,affiliation,.keep_all=TRUE)
#' email<-coauthors%>%
#'      dplyr::select(authorID,email)%>%
#'      dplyr::distinct(authorID,email,.keep_all=TRUE)
#' format_coauthors(name,affiliation,email,corresp=1)

format_coauthors<-function(name, affiliation, email,corresp=NULL,filename=NULL,output_type="TXT",...){
  if(is.null(filename)){
    filename<-paste0("output_",format(Sys.time(),"%y%m%d%H%M%S"),".txt")
  }
  
  name1<-name%>%
      dplyr::filter(!(is.na(authorID)|is.na(name)|is.na(rank)))
  
    
  aff1<-affiliation%>%
      dplyr::left_join(name1%>%dplyr::select(authorID,name,rank),by="authorID")%>%
      dplyr::arrange(rank)%>%
      dplyr::mutate(affadd=sub('^NA, ', '', sub(', NA$', '', paste(affiliation,address,sep=', '))))%>%
      dplyr::filter(!is.na(affadd))%>%
      dplyr::select(-affiliation,-address)
  affadd_u<-unique(aff1$affadd)
  affaddnum<-tibble::tibble(affadd=affadd_u,affnum=1:length(affadd_u))
  aff1<-aff1%>%dplyr::left_join(affaddnum,by="affadd")%>%
      dplyr::arrange(rank,affnum)%>%
      dplyr::select(-rank)

  aff2<-aff1%>%
      dplyr::group_by(authorID,name)%>%
      tidyr::nest(.key="affiliation")%>%
      dplyr::mutate(num=purrr::map(affiliation,~unlist(paste(unlist(dplyr::select(.,affnum)),collapse=" "))))
  
  email1<-email%>%dplyr::left_join(name1%>%dplyr::select(authorID,name,rank),by="authorID")%>%
    dplyr::arrange(rank)%>%
    dplyr::filter(!is.na(email))%>%
    dplyr::filter(!is.na(name))
  
  email2<-email1%>%
    dplyr::group_by(authorID,name,rank)%>%
    tidyr::nest(.key="email")%>%
    dplyr::mutate(emailstack=purrr::map(email,~paste(unlist(.),collapse=", ")))
  
  namestar<-paste0(aff2$name,sapply(aff2$num,common::supsc))
  which_corresp<-is.element(aff2$authorID,corresp)
  namestar[which_corresp]<-paste0(namestar[which_corresp],"*")
  cont1<-paste(namestar,collapse=", ")
  text1<-reporter::create_text(paste0(cont1,"\n\n"),width=nchar(cont1))
  
  cont2<-paste(paste(common::supsc(affaddnum$affnum),affaddnum$affadd,sep="\t"),collapse="\n")
  text2<-reporter::create_text(paste0(cont2,"\n\n"),width=nchar(cont2))

  cont3<-paste(paste0(email2$name,":\t",unlist(email2$email)),collapse="\n")
  text3<-reporter::create_text(paste0(cont3,"\n\n"),width=nchar(cont3))

  text4<-reporter::create_text("*\tCorresponding author\n")
  
  tmp <- file.path(getwd(), filename)
  rpt<-reporter::create_report(tmp,output_type = output_type,...)%>%
        reporter::add_content(text1,page_break = FALSE, blank_row = "none")%>%
        reporter::add_content(text2,page_break = FALSE, blank_row = "none")%>%
        reporter::add_content(text3,page_break = FALSE, blank_row = "none")
  if(!is.null(corresp)){
    rpt<-rpt%>%reporter::add_content(text4,page_break = FALSE, blank_row = "none")
  }
  
  out<-list(aff2,affaddnum,email2)
  reporter::write_report(rpt)
  
    
}

#' @name coauthors
#' @title Example data of author list
#' @docType data
#' @author Keita Fukasawa \email{k.fukasawa37@gmail.com}
#' @references Fukasawa K, Morosawa T, Nakashima Y, Takagi S, Yokoyama T, Ando M, Iijima H, Saito MU, Kumada N, Tochigi K, Yoshioka A, Funatsu S, Koike S, Uno H, Enomoto T, McShea W, Kays R (2025) Snapshot Japan 2023: the first camera trap dataset under a globally standardised protocol in Japan. Biodiversity Data Journal 13: e141168. https://doi.org/10.3897/BDJ.13.e141168
#' @keywords data
NULL