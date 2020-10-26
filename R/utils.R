# -------- Gets --------

#Generic queryin' function for direct Wikidata calls. Wraps around WikipediR::page_content. - Ironholds
#'@title Download a Wikidata item
#'@description Utility wrapper for wikidata API to download item. Used by \code{get_item} and \code{get_property}
#'
#'@param title the wikidata item or property as a string
#'
#'@return a download of the full wikidata object (item or property) formatted as a nested json list
#'
#'@export
wd_query <- function(title, ...){
  result <- WikipediR::page_content(domain = "wikidata.org", page_name = title, as_wikitext = TRUE,
                                    httr::user_agent("WikidataR - https://github.com/TS404/WikidataR"),
                                    ...)
  output <- jsonlite::fromJSON(result$parse$wikitext[[1]])
  return(output)
}

# Query for a random item in "namespace" (ns). Essentially a wrapper around WikipediR::random_page. - Ironholds
#'@title Download random Wikidata items
#'@description Utility wrapper for wikidata API to download random items. Used by \code{random_item}
#'
#'@param ns string indicating namespace, most commonly "Main" for QID items, "Property" for PID properties
#'
#'@param limit how many random objesct to return
#'
#'@return a download of the full wikidata objects (items or properties) formatted as nested json lists
#'
#'@export
wd_rand_query <- function(ns, limit, ...){
  result <- WikipediR::random_page(domain = "wikidata.org", as_wikitext = TRUE, namespaces = ns,
                                   httr::user_agent("WikidataR - https://github.com/TS404/WikidataR"),
                                   limit = limit, ...)
  output <- lapply(result, function(x){jsonlite::fromJSON(x$wikitext[[1]])})
  class(output) <- "wikidata"
  return(output)
  
}

#Generic, direct access to Wikidata's search functionality.
#'@title Convert an input to a item QID
#'@description Convert an input string to the most likely item QID
#'
#'@param x a vector of strings representaing wikidata items
#'
#'@return if the inputted string is a valid QID, return the string.
#'If the inputted string matches an item label, return its QID.
#'If the inputted string matches multiple labels of multiple items, return the QID of the first hit.
#'
#'@examples
#'# if input string is a valid QID
#'as_qid("Q42")
#'# if input string matches multiple item labels
#'as_qid("Douglas Adams")
#'# if input string matches a single unique label
#'as_qid("Douglas Adams and the question of arterial blood pressure in mammals")
#'
#'@export
search <- function(search_term, language, limit, type, ...){
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

# sparql_query <- function(params, ...){
#   result <- httr::GET("https://query.wikidata.org/bigdata/namespace/wdq/sparql",
#                       query = list(query = params),
#                       httr::user_agent("WikidataR - https://github.com/Ironholds/WikidataR"),
#                       ...)
#   httr::stop_for_status(result)
#   return(httr::content(result, as = "parsed", type = "application/json"))
# }

#' @title Get an example SPARQL query from Wikidata
#' @description Gets the specified example(s) from
#'   [SPARQL query service examples page](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples)
#'   using [Wikidata's MediaWiki API](https://www.wikidata.org/w/api.php).
#' @details If you are planning on extracting multiple examples, please provide
#'   all the names as a single vector for efficiency.
#' @param example_name the names of the examples as they appear on
#'   [this page](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples)
#' @return The SPARQL query as a character vector.
#' @examples
#' \dontrun{
#' sparql_query <- extract_example(c("Cats", "Horses"))
#' query_wikidata(sparql_query)
#' # returns a named list with two data frames
#' # one called "Cats" and one called "Horses"
#'
#' sparql_query <- extract_example("Largest cities with female mayor")
#' cat(sparql_query)
#' query_wikidata(sparql_query)
#' }
#' @seealso [query_wikidata]
#' @export
get_example <- function(example_name) {
  content <- WikipediR::page_content(
    domain = "www.wikidata.org",
    page_name = "Wikidata:SPARQL query service/queries/examples",
    as_wikitext = TRUE
  )
  wiki <- strsplit(content$parse$wikitext$`*`, "\n")[[1]]
  wiki <- wiki[wiki != ""]
  return(vapply(example_name, function(example_name) {
    heading_line <- which(grepl(paste0("^===\\s?", example_name, "\\s?===$"), wiki, fixed = FALSE))
    start_line <- which(grepl("{{SPARQL", wiki[(heading_line + 1):length(wiki)], fixed = TRUE))[1]
    end_line <- which(grepl("}}", wiki[(heading_line + start_line + 1):length(wiki)], fixed = TRUE))[1]
    query <- paste0(wiki[(heading_line + start_line):(heading_line + start_line + end_line - 1)], collapse = "\n")
    return(sub("^\\s*\\{\\{SPARQL2?\\n?\\|query\\=", "", query))
  }, ""))
}

# -------- Format checkers --------
# Simple tests of strings for whether they adhere to common wikidata formats
is.qid    <- function(x){grepl("^[Qq][0-9]+$",x)}
is.pid    <- function(x){grepl("^[Pp][0-9]+$",x)}
is.sid    <- function(x){grepl("^[Ss][0-9]+$",x)}
is.date   <- function(x){grepl("[0-9]{1,4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}",x)}
is.quot   <- function(x){grepl("^\".+\"$",x)}
is.coord  <- function(x){grepl("@-?([1-8]?\\d(\\.\\d+)?|90(\\.0+)?)/-?(180(\\.0+)?|((1[0-7]\\d)|([1-9]?\\d))(\\.\\d+)?)$",x)}
is.wdURL  <- function(x){grepl("http://www.wikidata.org/entity/[PpQq][0-9]+$",x)}is.empty <- function(x){x==""}
is.create <- function(x){grepl("^CREATE",x)}
is.last   <- function(x){grepl("^LAST$",x)}
is.special<- function(x){if(grepl("^[LAD]",x)){
  substr(x,2,100) %in% as.matrix(lang.abbrev)
}else if(grepl("^S",x)){
  substr(x,2,100) %in% as.matrix(abbrev.wiki)
}else{
  FALSE
}}


#'@title Extract an identifier from a wikidata URL
#'@description Convert a URL ending in an identifier (returned by SPARQL queries) to just the plan identifier (QID or PID).
#'
#'@param x a strings representing a wikidata URL
#'
#'@return if the URL ends in a QID or PID, return that PID or QID, else return the original string
#'
#'@examples
#'url_to_id("http://www.wikidata.org/entity/42")
#'
#'@export
url_to_id <- function (x){
  if(is.wdURL(x)){x <- sapply(sapply(x,pattern = "/",stringr::str_split),tail,1)}
  output <- x
  output
}

#Generic input checker. Needs additional stuff for property-based querying
#because namespaces are weird, yo. - Ironholds
#'@title Generic input checker
#'@description Utility function to handle namespaces. Used by \code{get_item} and \code{get_property}
#'
#'@param input string to check
#'
#'@param substitution string for what's been looked for
#'
#'@return boolian indicating whether the checked string contains a match for the substitution string 
#'
#'@export
check_input <- function(input, substitution){
  in_fit <- grepl("^\\d+$",input)
  if(any(in_fit)){
    input[in_fit] <- paste0(substitution, input[in_fit])
  }
  return(input)
}
# -------- Converters --------
# Simple functions to convert plain text descriptions into their most likely QID/PIDs

#'@title Convert an input to a item QID
#'@description Convert an input string to the most likely item QID
#'
#'@param x a vector of strings representaing wikidata items
#'
#'@return if the inputted string is a valid QID, return the string.
#'If the inputted string matches an item label, return its QID.
#'If the inputted string matches multiple labels of multiple items, return the QID of the first hit.
#'
#'@examples
#'# if input string is a valid QID
#'as_qid("Q42")
#'# if input string matches multiple item labels
#'as_qid("Douglas Adams")
#'# if input string matches a single unique label
#'as_qid("Douglas Adams and the question of arterial blood pressure in mammals")
#'
#'@export
as_qid <- function(x){
  as_qid_nest1 <- function(x){
    if(is.qid(x)){
      x
    }else{
      temp     <- WikidataR::find_item(x)[[1]]
      x        <- temp$id
      names(x) <- temp$label
      x
    }
  }
  output <- unlist(lapply(x,as_qid_nest1))
  return(output)
}

#'@title Convert an input to a property PID
#'@description Convert an input string to the most likely property PID
#'
#'@param x a vector of strings representaing wikidata properties
#'
#'@return if the inputted string is a valid PID, return the string.
#'If the inputted string matches a property label, return its PID.
#'If the inputted string matches multiple labels of multiple properties, return the PID of the first hit.
#'
#'@examples
#'# if input string is a valid PID
#'as_pid("P50")
#'# if input string matches multiple item labels
#'as_pid("author")
#'# if input string matches a single unique label
#'as_pid("Scopus author ID")
#'
#'@export
as_pid <- function(x){
  as_pid_nest1 <- function(x){
    if(is.pid(x)){
      x
    }else{
      temp     <- WikidataR::find_property(x)[[1]]
      x        <- temp$id
      names(x) <- temp$label
      x
    }
  }
  output <- unlist(lapply(x,as_pid_nest1))
  return(output)
}


#'@title Convert an input to a source property SID
#'@description Convert an input string to the most likely source SID (equivalent to PID)
#'
#'@param x a vector of strings representaing wikidata source properties
#'
#'@return if the inputted string is a valid SID, return the string.
#'If the inputted string matches a property label, return its SID
#'If the inputted string matches multiple labels of multiple properties, return the SID of the first hit.
#'
#'@examples
#'# if input string is a valid SID
#'as_pid("S854")
#'# if input string matches multiple item labels
#'as_pid("URL")
#'# if input string matches a single unique label
#'as_pid("Reference URL")
#'
#'@export
as_sid <- function(x){if(all(is.sid(x))){x}
  else if(all(is.pid(x))){gsub("P","S",x,ignore.case = 1)}
  else{gsub("P","S",WikidataR::find_property(x)[[1]]$id)}
}

#'@title Extract an identifier from a wikidata URL
#'@description Convert a URL ending in an identifier (returned by SPARQL queries) to just the plan identifier (QID or PID).
#'
#'@param x a vector of strings representing wikidata URLs
#'
#'@return QID or PID
#'
#'@examples
#'url_to_id("http://www.wikidata.org/entity/42")
#'
#'@export
url_to_id <- function (x){
  sapply(sapply(x,pattern = "/",stringr::str_split),tail,1)
}

# -------- Wikidata object manipulation --------
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
extract_claims <- function (items,
                            claims){
  claims <- sapply(claims,as_pid)
  output <- lapply(items, function(x, claims) {
    return(lapply(claims, function(claim, obj) {
      which_match <- which(names(obj$claims) == claim)
      if (!length(which_match)) {
        return(NA)
      }
      return(obj$claims[[which_match[1]]])
    }, obj = x))
  }, claims = claims)
  return(output)
}

#'@title List properties of a Wikidata item
#'@description for a downloaded wikidata item, list the properties of all statements
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

#Note: This one isn't very well named. not really the property names, more the predicate names

#'@title Get names of properties
#'@description For a clam or set of claims, return the names of the properties  
#'
#'@param properties a claims list from \code{extract_claims}
#'
#'@return tibble of labels for each property for a set of claims
#'
#'@export
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

# -------- Misc. string manipulation --------
#'@title Format short form person names
#'@description Converting names into first initial and surname, or just initials
#'
#'@param x a vector of people's names as strings
#'
#'@param format a vector of strings of either "FLast" or "FL" to indicate the output format
#'
#'@return the inputted name strings with first names shortened based on the
#'selected format.
#'
#'@export
initials <- function(x,format="FLast"){
  if (format=="FLast"){
    gsub("^([A-Za-z]).* ([A-Za-z]*)", "\\1 \\2", x)
  }else{
    gsub("(.)\\S* *", "\\1", x)
  }
}

#'@title Remove special characters
#'@description Special characters can otherwise mess up wikidata read-writes
#'
#'@param x a vector of strings to check for special characters
#'
#'@return the inputted strings with special characters replaced with
#'closest match plan characters.
#'
#'@export
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

#'@title Extract a paragraph of text
#'@description Return the nth paragraph of a section of text
#'Useful for extracting information from wikipedia or other wikimarkup text
#'
#'@param text the input text as a string
#'
#'@param para number indicating whichparagraph(s) to return (default=1)
#'
#'@param templ an optional string specifying a mediawikitemplate within
#'which to restrict the search restrict search 
#'
#'@return the nth paragraph of the input text.
#'
#'@export
extract_para <- function(text,
                         para=1,
                         templ=NULL){
  extract_para_nest1 <- function(x,y){
    out <- lapply(x,gsub,pattern=".*= *| *\\|",replacement="")
    names(out) <- y
    return(out)
  }
  templ <- gsub(" ","_",templ)
  tosearch <- gsub("( |\\\\n|\\\\t)+"," ",text)
  if(!is.null(templ)){
    templates <- regmatches(tosearch, gregexpr("\\{(?:[^{}]+|(?R))*+\\}",
                                               tosearch, perl=TRUE, ignore.case=TRUE))[[1]]
    name_lens <- regexpr(" *\\|| *\\}",templates) - 1 
    templates <- paste0(gsub(" ","_",substr(templates,1,regexpr(" *\\|| *\\}",templates)-1)),
                        substr(templates,regexpr("*\\||*\\}",templates),nchar(templates)))
    
    tosearch <- unlist(str_extract_all(templates,
                                       paste0("(?i)\\{\\{ *?",templ,".*?\\}\\}")))
    names(tosearch) <- paste0(templ,"_",1:length(tosearch))
  }
  
  match_paras <- lapply(tosearch,
                        str_extract_all,
                        paste0("\\| *?",para," *?=.*?\\|"))
  
  match_exact <- lapply(match_paras,extract_para_nest1,para)
  
  return(match_exact)
}

#'@title "CREATE" rows 
#'@description Add in empty lines for QuickStatements CREATE rows that mint new QIDs.
#'This is a slightly messy quirk of the QuickStatements format that mints new QIDs via a line
#'containing only "CREATE", so this function is a way to approximate that bevaviour in a tibble
#'@param items a vector of items (which may or may not contain the keyword "CREATE")
#'@param vector a vector of properties or values which may be expanded based on the items vector
#'@return if the vector is NULL, return NULL. Otherwise, if the "CREATE" keyword appears in the
#'items vector, insert blank strings at those positions in the vector.
#'
#'@export
createrows <- function(items,vector){
  if(all(any(items=="CREATE"),!is.null(vector))){
    #expand vector to full length if just intending to repeat a single value 
    if(length(vector)==1){
      vector <- rep(vector,sum(items!="CREATE"))
    }
    
    newQID <- which(items=="CREATE")
    val <- c(vector, rep("",length(newQID)) )
    id  <- c(seq_along(vector), newQID-seq_along(newQID)+0.5)
    out <- val[order(id)]
    return(out)
  }else{
    return(vector)
  }
}

#'@title "CREATE" rows from tidy format
#'@description Add in QuickStatements CREATE rows that mint new QIDs from tidy input data.
#'New items are created by any item starting that starts with the text "CREATE" followed
#'by any unique ID.
#'
#'@param QS.tib a tibble of items, values and properties (optionally qualifiers and sources).
#'
#'@return a tibble, with items that start with "CREATE" followed by any unique text causing the
#'addition of a "Create" line above, being replaced with "LAST" in the Quickstatemnts format
#'to create new QIDs.
#'
#'@export
createrows.tidy <- function(QS.tib) {
  #insert 'CREATE' blankrows above first instance of 'CREATExyz'
  newQID <- which(!duplicated(QS.tib[,1])&sapply(QS.tib[,1],is.create))
  val <- rbind(QS.tib, array("",dim=c(length(newQID),ncol(QS.tib)),dimnames = list(newQID,names(QS.tib))) )
  id  <- c(seq_along(t(QS.tib)[1,]), newQID-0.5)
  out <- val[order(id),]
  
  #replace 'CREATEXYZ' with 'LAST'
  out[sapply(out[,1],is.create),1] <- "LAST"
  
  #replace new empty rows with 'CREATE' row
  out[apply(is.empty(out),all,MARGIN=1),1] <- "CREATE"
  return(out)
}