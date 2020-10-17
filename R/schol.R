
qid_from_DOI <- function(DOI = '10.15347/WJM/2019.001'){
  qid_from_DOI_nest1 <- function(x){paste('SELECT ?DOI WHERE {?DOI wdt:P356 "',
                                          x,
                                          '"}',
                                          sep='')}
  qid_from_DOI_nest2 <- function(x){tail(stringr::str_split(x,pattern = "/")[[1]],n=1)}
  sparql_query <- lapply(DOI,qid_from_DOI_nest1)
  article.qr   <- lapply(query_wikidata(sparql_query),as_tibble)
  names(article.qr)<-DOI
  article.qid  <- unlist(lapply(article.qr,qid_from_DOI_nest2))
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
  qid_from_ORCID_nest2 <- function(x){tail(stringr::str_split(x,pattern = "/")[[1]],n=1)}
  sparql_query <- lapply(ORCID,qid_from_ORCID_nest1)
  author.qr   <- lapply(query_wikidata(sparql_query),as_tibble)
  names(author.qr)<-ORCID
  author.qid  <- unlist(lapply(author.qr,qid_from_ORCID_nest2))
  return(author.qid)
}
