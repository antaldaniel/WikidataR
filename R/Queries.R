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
#' @seealso [get_example]
#' @export
query_wikidata <- function(...) {
  output <- WikidataQueryServiceR::query_wikidata(...)
  output <- mapply(url_to_id,data.frame(articles.qr))
  output <- tibble(data.frame(output))
  output
}
