#Generic queryin' function for direct Wikidata calls. Wraps around WikipediR::page_content.
wd_query <- function(title, ...){
  result <- WikipediR::page_content(domain = "wikidata.org", page_name = title, as_wikitext = TRUE,
                                    httr::user_agent("WikidataR - https://github.com/Ironholds/WikidataR"),
                                    ...)
  output <- jsonlite::fromJSON(result$parse$wikitext[[1]])
  return(output)
}

#Query for a random item in "namespace" (ns). Essentially a wrapper around WikipediR::random_page.
wd_rand_query <- function(ns, limit, ...){
  result <- WikipediR::random_page(domain = "wikidata.org", as_wikitext = TRUE, namespaces = ns,
                                   httr::user_agent("WikidataR - https://github.com/Ironholds/WikidataR"),
                                   limit = limit, ...)
  output <- lapply(result, function(x){jsonlite::fromJSON(x$wikitext[[1]])})
  class(output) <- "wikidata"
  return(output)
  
}

#Generic input checker. Needs additional stuff for property-based querying
#because namespaces are weird, yo.
check_input <- function(input, substitution){
  in_fit <- grepl("^\\d+$",input)
  if(any(in_fit)){
    input[in_fit] <- paste0(substitution, input[in_fit])
  }
  return(input)
}

#Generic, direct access to Wikidata's search functionality.
searcher <- function(search_term, language, limit, type, ...){
  result <- WikipediR::query(url = "https://www.wikidata.org/w/api.php", out_class = "list", clean_response = FALSE,
                             query_param = list(
                               action   = "wbsearchentities", 
                               type     = type,
                               language = language,
                               limit    = limit,
                               search   = search_term
                             ),
                             ...)
  result <- result$search
  return(result)
}

sparql_query <- function(params, ...){
  result <- httr::GET("https://query.wikidata.org/bigdata/namespace/wdq/sparql",
                      query = list(query = params),
                      httr::user_agent("WikidataR - https://github.com/Ironholds/WikidataR"),
                      ...)
  httr::stop_for_status(result)
  return(httr::content(result, as = "parsed", type = "application/json"))
}

#'@title Extract Claims from Returned Item Data
#'@description extract claim information from data returned using
#'\code{\link{get_item}}.
#'
#'@param items a list of one or more Wikidata items returned with
#'\code{\link{get_item}}.
#'
#'@param claims a vector of claims (in the form "P321", "P12") to look for
#'and extract.
#'
#'@return a list containing one sub-list for each entry in \code{items},
#'and (below that) the found data for each claim. In the event a claim
#'cannot be found for an item, an \code{NA} will be returned
#'instead.
#'
#'@examples
#'# Get item data
#'adams_data <- get_item("42")
#'
#'# Get claim data
#'claims <- extract_claims(adams_data, "P31")
#'
#'@export
extract_claims <- function(items, claims){
  output <- lapply(items, function(x, claims){
    return(lapply(claims, function(claim, obj){
      which_match <- which(names(obj$claims) == claim)
      if(!length(which_match)){
        return(NA)
      }
      return(obj$claims[[which_match[1]]])
    }, obj = x))
  }, claims = claims)
  
  return(output)
}

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

list_properties <- function (item,
                             names=FALSE){
  properties.p <- lapply(lapply(item,"[[","claims"),names)
  if(names){
    if(length(item)==1){
      names(properties.p) <- unlist(lapply(lapply(lapply(get_property(properties.p),"[[","labels"),"[[","en"),"[[","value"))
    }
  }
  return(properties.p)
}

# Some simple is, as, and conversion functions
is.qid  <- function(x){grepl("^[Qq][0-9]+$",x)}
is.pid  <- function(x){grepl("^[Pp][0-9]+$",x)}
is.date <- function(x){grepl("[0-9]{1,4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}",x)}
is.quot <- function(x){grepl("^\".+\"$",x)}
as_qid <- function(x){if(!all(is.qid(x))){WikidataR::find_item(x)[[1]]$id}else{x}}
as_pid <- function(x){if(!all(is.pid(x))){WikidataR::find_property(x)[[1]]$id}else{x}}


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

list_properties <- function (item,
                             names=FALSE){
  properties.p <- lapply(lapply(item,"[[","claims"),names)
  if(names){
    if(length(item)==1){
      names(properties.p) <- unlist(lapply(lapply(lapply(get_property(properties.p),"[[","labels"),"[[","en"),"[[","value"))
    }
  }
  return(properties.p)
}

get_names_from_properties <- function(properties){
  get_names_from_properties_nest1 <- function(x){
    out <- lapply(lapply(lapply(lapply(x,"[[","mainsnak"),"[[","datavalue"),"[[","value"),"[[","id")
    names(out) <- lapply(lapply(lapply(x,"[[","mainsnak"),"[[","property"),"[[",1)
    return(out)
  }
  get_names_from_properties_nest2 <- function(x){
    out <- lapply(x,get_item)
    return(out)
  }
  get_names_from_properties_nest3.1 <- function(x){
    out <- lapply(lapply(lapply(x,"[[","labels"),"[[","en"),"[[","value")
    names(out) <- lapply(x,"[[","id")
    return(out)
  }
  get_names_from_properties_nest3 <- function(x){
    out <- lapply(x,get_names_from_properties_nest3.1)
    return(out)
  }
  
  property_values.qid <- lapply(properties,get_names_from_properties_nest1)
  property_values.q   <- lapply(property_values.qid,get_names_from_properties_nest2)
  property_names      <- lapply(property_values.q, get_names_from_properties_nest3)
  property_names      <- lapply(lapply(property_names,unlist),enframe,name = "QID") 
  return(property_names)
}


initials <- function(x,type="FLast"){
  if (type=="FLast"){
    gsub("^([A-Za-z]).* ([A-Za-z]*)", "\\1 \\2", x)
  }else{
    gsub("(.)\\S* *", "\\1", x)
  }
}

unspecial <- function(x){
  out <- x
  for(i in 1:ncol(x)){
    out[[i]] <- iconv(x[[i]],to = 'ASCII//TRANSLIT')
    if(Hmisc::all.is.numeric(x[[i]])){
      out[[i]] <- as.numeric(out[[i]])
    }else{
      out[[i]] <- as.factor(out[[i]])
    } 
  }
  return(as_tibble(out))
}
