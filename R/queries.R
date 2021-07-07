#Wrapper around WikidataQueryServiceR::query_wikidata
#' @title Send one or more SPARQL queries to WDQS
#' @description Makes a POST request to Wikidata Query Service SPARQL endpoint.
#' @param sparql_query SPARQL query (can be a vector of queries)
#' @param format "simple" uses CSV and returns pure character data frame, while
#'   "smart" fetches JSON-formatted data and returns a data frame with datetime
#'   columns converted to `POSIXct`
#' @param \\dots Additional parameters to supply to [httr::POST]
#' @return A `tibble`. Note: QID values will be returned as QIDs, rather than URLs.
#' @section Query limits:
#' There is a hard query deadline configured which is set to 60 seconds. There
#' are also following limits:
#' - One client (user agent + IP) is allowed 60 seconds of processing time each
#'   60 seconds
#' - One client is allowed 30 error queries per minute
#' See [query limits section](https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual#Query_limits)
#' in the WDQS user manual for more information.
#' @examples
#' # R's versions and release dates:
#' sparql_query <- 'SELECT DISTINCT
#'   ?softwareVersion ?publicationDate
#'   WHERE {
#'     BIND(wd:Q206904 AS ?R)
#'     ?R p:P348 [
#'       ps:P348 ?softwareVersion;
#'       pq:P577 ?publicationDate
#'     ] .
#' }'
#' query_wikidata(sparql_query)
#'
#' \dontrun{
#' # "smart" format converts all datetime columns to POSIXct
#' query_wikidata(sparql_query, format = "smart")
#' }
#' @export
query_wikidata <- function(sparql_query,format="simple",...) {
  output <- WikidataQueryServiceR::query_wikidata(sparql_query=sparql_query,format=format,...)
  output <- suppressWarnings(mapply(url_to_id,data.frame(output), SIMPLIFY = FALSE))
  output <- tibble(data.frame(output))
  if(nrow(output)==0){output <- tibble(value=NA)}
  output
}

#' @title QID from identifier
#' @description convert unique identifiers to QIDs (for items in wikidata). 
#' @param property the identifier property to search (for caveats, see \code{as_pid})
#' @param value the identifier value to match
#' @return tibble of QIDs corresponding to identifiers submitted
#' @examples
#' qid_from_identifier('ISBN-13','978-0-262-53817-6')
#' @export
qid_from_identifier <- function(property = 'DOI',
                                value    = c('10.15347/WJM/2019.001','10.15347/WJM/2020.002')){
  
  property <- as_pid(property)
  
  qid_from_property1 <- function(value,property){paste('SELECT ?value WHERE {?value wdt:',
                                                       property,
                                                       ' "',
                                                       value,
                                                       '"}',
                                                       sep='')}
  sparql_query <- lapply(value,property,FUN=qid_from_property1)
  output.qr    <- if(length(value)>1){
    unlist(pbapply::pblapply(sparql_query,function(x) as.character(query_wikidata(x)[[1]])))
  }else{
      lapply(sparql_query,FUN=query_wikidata)
    }
  output       <- tibble(value,qid=data.frame(output.qr))
  return(output)
}

#' @title identifier from identifier
#' @description convert unique identifiers to other unique identifiers 
#' @param property the identifier property to search (for caveats, see \code{as_pid})
#' @param return the identifier property to convert to
#' @param value the identifier value to match
#' @return tibble of identifiers corresponding to identifiers submitted
#' @examples
#' identifier_from_identifier('ORCID iD','IMDb ID',c('0000-0002-7865-7235','0000-0003-1079-5604'))
#' @export
identifier_from_identifier <- function(property = 'ORCID iD',
                                       return   = 'IMDb ID',
                                       value    = "0000-0002-7865-7235"){
  
  property <- as_pid(property)
  return   <- as_pid(return)
  
  qid_from_property1 <- function(value,return,property){paste('SELECT ?return WHERE { ?value wdt:',
                                                       property,
                                                       ' "',
                                                       value,
                                                       '". ?value wdt:',
                                                       return,
                                                       ' ?return.}',
                                                       sep='')}
  sparql_query <- lapply(value,return,property,FUN=qid_from_property1)
  output.qr    <- if(length(value)>1){
    unlist(pbapply::pblapply(sparql_query,function(x) as.character(query_wikidata(x)[[1]])))
  }else{
    lapply(sparql_query,FUN=query_wikidata)
  }
  output       <- tibble(value,return=data.frame(output.qr))
  return(output)
}
