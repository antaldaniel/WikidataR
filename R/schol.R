#' @title QID freom DOI
#' @description simple converter from DOIs to QIDs (for items in wikidata)
#' @param DOI digital object identifiers submitted as strings
#' @return tibble of QIDs corresponding to DOIs submitted
#' @export
qid_from_DOI <- function(DOI = '10.15347/WJM/2019.001'){
  qid_from_DOI_nest1 <- function(x){paste('SELECT ?DOI WHERE {?DOI wdt:P356 "',
                                          x,
                                          '"}',
                                          sep='')}
  sparql_query <- lapply(DOI,FUN=qid_from_DOI_nest1)
  article.qr   <- lapply(sparql_query,FUN=query_wikidata)
  article.qid   <- tibble(DOI,qid=unlist(article.qr))
  return(article.qid)
}

qid_from_name <- function(name  = 'Thomas Shafee',
                          limit = 100){
  qid_from_name_nest1 <- function(x){lapply(x,"[[","id")}
  item.qs  <- lapply(name,find_item, limit=limit)
  item.qid <- lapply(item.qs,qid_from_name_nest1)
  names(item.qid) <- name
  item.qid <- unlist(item.qid)
  return(item.qid)
}

qid_from_ORCID <- function(ORCID = '0000-0002-2298-7593'){
  qid_from_ORCID_nest1 <- function(x){paste('SELECT ?ORCID WHERE {?ORCID wdt:P496 "',
                                            x,
                                            '"}',
                                            sep='')}
  sparql_query <- lapply(ORCID,FUN=qid_from_ORCID_nest1)
  article.qr   <- lapply(sparql_query,FUN=query_wikidata)
  author.qid   <- tibble(ORCID,qid=unlist(article.qr))
  return(author.qid)
}
