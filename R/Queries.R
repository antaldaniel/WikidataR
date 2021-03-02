#Wrapper around WikidataQueryServiceR::query_wikidata
#' @title Send one or more SPARQL queries to WDQS
#' @description Makes a POST request to Wikidata Query Service SPARQL endpoint.
#' @param sparql_query SPARQL query (can be a vector of queries)
#' @param format "simple" uses CSV and returns pure character data frame, while
#'   "smart" fetches JSON-formatted data and returns a data frame with datetime
#'   columns converted to `POSIXct`
#' @param ... Additional parameters to supply to [httr::POST]
#' @return A `tibble`. Note: QID values will be returned as QIDs, rather than URLs.
#' @examples
#' # R's versions and release dates:
#' sparql_query <- 'SELECT DISTINCT
#'   ?softwareVersion ?publicationDate
#' WHERE {
#'   BIND(wd:Q206904 AS ?R)
#'   ?R p:P348 [
#'     ps:P348 ?softwareVersion;
#'     pq:P577 ?publicationDate
#'   ] .
#' }'
#' query_wikidata(sparql_query)
#'
#' \dontrun{
#' # "smart" format converts all datetime columns to POSIXct
#' query_wikidata(sparql_query, format = "smart")
#' }
#' @export
query_wikidata <- function(...) {
  output <- WikidataQueryServiceR::query_wikidata(...)
  output <- suppressWarnings(mapply(url_to_id,data.frame(output)))
  output <- tibble(data.frame(output))
  output
}

#' @title QID from identifier
#' @description convert unique identifiers to QIDs (for items in wikidata). 
#' @param property the identifier property to search (for caveats, see \code{as_pid})
#' @param value the identifier value to match
#' @return tibble of QIDs corresponding to DOIs submitted
#' @example
#' qid_from_identifier('ISBN-13','978-0-262-53817-6')
#' @export
qid_from_identifier <- function(property = 'DOI',
                                value    = c('10.15347/WJM/2019.001','10.15347/WJM/2020.002')){
  
  property <- as_pid(property)
  
  qid_from_property1 <- function(x,y){paste('SELECT ?value WHERE {?value wdt:',y,' "',
                                            x,
                                            '"}',
                                            sep='')}
  sparql_query <- lapply(value,property,FUN=qid_from_property1)
  article.qr   <- if(length(value)>1){pbapply::pblapply(sparql_query,FUN=query_wikidata)}else{lapply(sparql_query,FUN=query_wikidata)}
  article.qid  <- tibble(value,qid=unlist(article.qr))
  return(article.qid)
}
