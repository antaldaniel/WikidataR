#'@title Write statements to Wikidata
#'@description Upload data to wikidata, including creating items,
#'adding statements to existing items (via the quickstatements format and API).
#'
#'@param items a vector of strings indicating the items to which to add statements (as QIDs or labels).
#'Note: if labels are provided, and multiple items match, the first matching item will be used
#'(see \code{as_qid} function), so use with caution.
#'New QIDs can be created by using the "CREATE_xyz", where "_xyz" is any unique string.
#'Using the same id will add additional statemnts to those new items 
#'@param properties a vector of strings indicating the properties to add as statements (as PIDs or labels).
#'Note: if labels are provided, and multiple items match, the first matching item will be used
#'(see \code{as_pid} function), so use with caution.
#'Four special properties can also be used: labels, aliases, descriptions and sitelinks. See [this link](https://www.wikidata.org/wiki/Help:QuickStatements#Adding_labels,_aliases,_descriptions_and_sitelinks) for the syntax.
#'@param values a vector of strings indicating the values to add as statements (as QIDs or strings).
#'Note: if strings are provided, they will be treated as plain text.
#'@param qual.properties a vector of strings indicating the properties to add as qualifiers to statements (as PIDs or labels).
#'Note: if labels are provided, and multiple items match, the first matching item will be used
#'(see \code{as_pid} function), so use with caution.
#'@param qual.values a vector of strings indicating the values to add as statements (as QIDs or strings).
#'Note: if strings are provided, they will be treated as plain text.
#'@param src.properties a vector of strings indicating the properties to add as reference sources to statements (as SIDs or labels).
#'Note: if labels are provided, and multiple items match, the first matching item will be used
#'(see \code{as_sid} function), so use with caution.
#'@param src.values a vector of strings indicating the values to add reference sources to statements (as QIDs or strings).
#'Note: if strings are provided, they will be treated as plain text.
#'@param remove a vector of boolians for each statemnt indicating whether it should
#'be removed from the item rather than added (default = FALSE)
#'@param format output format as a string. Options include:
#' \describe{
#'   \item{tibble}{easiest format to further manuipulation in R}
#'   \item{csv}{can be copy-pasted to [the QuickStatements website](https://quickstatements.toolforge.org/) (or manipulated in a spreadsheet programs)}
#'   \item{api}{a url that can be copy-pasted into a web browser, or automatically submitted (see \code{api.submit} parameter)}
#' }
#'@param api.username a string indicating your wikimedia username 
#'@param api.token a string indicating your api token (the unique identifier that you can find listed at [your user page](https://quickstatements.toolforge.org/#/user))
#'@param api.format a string indicateing which version of the quickstatement format used to submit the api (default = "v1")
#'@param api.batchname a string create a named batch (listed at [your batch history page](https://quickstatements.toolforge.org/#/batches)) and tag in the edit summaries
#'@param api.submit boolian indicating whether to submit instruction directly to wikidata (else returns the URL that can be copy-pasted into a web browser)
#'
#'@return data formatted to upload to wikidata (via quickstatemsnts),
#'optionally also directly uploded to wikidata (see \code{format} parameter. 
#'
#'@examples
#'Add a statement to the "Wikidata sandbox" item (Q4115189)
#'to say that it is an "instance of" (P31) of Q1 (the universe).
#'The instruction will submit directly to wikidata via the API (if you include your wikimedai username and token)
#'
#'as_quickstatement(items        = "Wikidata Sandbox",
#'                  properties   = "instance of",
#'                  values       = "Q1",
#'                  format       = "api",
#'                  api.username = "myusername", 
#'                  api.token    = , #REDACTED# Find yours from [your user page](https://tools.wmflabs.org/quickstatements/#/user)
#'                  )
#'
#'@export

as_quickstatement <- function(items,
                              properties      = NULL,
                              values          = NULL,
                              qual.properties = NULL,
                              qual.values     = NULL,
                              src.properties  = NULL,
                              src.values      = NULL,
                              remove          = FALSE,
                              format          = "tibble",
                              api.username    = "Evolution_and_evolvability",
                              api.token       = NULL,
                              api.format      = "v1",
                              api.batchname   = NULL,
                              api.submit      = TRUE
){
  
  items           <- sapply(items,function(x){if(is.create(x)|is.last(x)){x}else{as_qid(x)}})
  items[remove]   <- paste0("-",items[remove])
  properties      <- sapply(properties,function(x){if(is.special(x)){x}else{as_pid(x)}})
  
  # strings need quotation marks, and in APIs those are indicated as $22 
  if (format=="api"){
    values <- sapply(values,function(x){if(is.qid(x)|is.date(x)|is.quot(x)){x}else{paste0('$22',x,'$22')}})
    if(!is.null(qual.values)){qual.values <- sapply(qual.values,function(x){if(is.qid(x)|is.date(x)|is.quot(x)){x}else{paste0('$22',x,'$22')}})}
    if(!is.null(src.values)) {src.values  <- sapply(src.values,function(x){if(is.qid(x)|is.date(x)|is.quot(x)){x}else{paste0('$22',x,'$22')}})}
  }
  
  if (format=="tibble"|format=="csv"){
    values <- sapply(values,function(x){if(is.qid(x)|is.date(x)|is.quot(x)){x}else{paste0('"',x,'"')}})
    if(!is.null(qual.values)){qual.values <- sapply(qual.values,function(x){if(is.qid(x)|is.date(x)|is.quot(x)){x}else{paste0('"',x,'"')}})}
    if(!is.null(src.values)) {src.values  <- sapply(src.values,function(x){if(is.qid(x)|is.date(x)|is.quot(x)){x}else{paste0('"',x,'"')}})}
  }
  
  # if new QIDs are being created via the "CREATE" keyword, need to insert blank lines across the other parameters to align correctly into rows
  properties      <- createrows(items,properties)
  values          <- createrows(items,values)
  qual.properties <- createrows(items,qual.properties)
  qual.values     <- createrows(items,qual.values)
  src.properties  <- createrows(items,src.properties)
  src.values      <- createrows(items,src.values)
  
  # build the basic tibble of the items and what properties and values to add as statements
  QS <- list(items=items,
             properties=properties,
             values=values)
  QS.rowmax <- max(sapply(QS,length))
  QS.check  <- sapply(QS,length)==1|sapply(QS,length)==QS.rowmax
  
  # if wrong number of values or properties, stop with error message
  if(!all(QS.check)){stop(paste0("\n Not all quickstatement columns equal length: ",
                                sum(items!="CREATE")," items (",
                                sum(items=="CREATE")," new QIDs to CREATE) were provided, but ",
                                lapply(QS[lapply(QS,length)!=sum(items!="CREATE") & lapply(QS,length)!=1],length),
                                " ",
                                names(QS[lapply(QS,length)!=sum(items!="CREATE") & lapply(QS,length)!=1]),
                                "."))}
  QS.tib <- tibble(Item =  QS[[1]],
                   Prop =  QS[[2]],
                   Value = QS[[3]])
  
  # optionally, append columns for qualifier properties and qualifier values for those statements
  if(!is.null(qual.properties)|!is.null(qual.values)){
    qual.properties <- sapply(qual.properties,function(x){if(!is.null(x)){as_pid(x)}else{x}})
    qual.values     <- sapply(qual.values,function(x){if(!(is.qid(x)|is.date(x)|is.quot(x)|is.na(x)|is.empty(x))){paste0('"',x,'"')}else{x}})
    
    QSq <- list(qual.properties,
                qual.values)
    QSq.rowmax <- max(sapply(c(QS,QSq),length))
    QSq.check  <- sapply(c(QS,QSq),length)==1|
      sapply(c(QS,QSq),length)==QSq.rowmax
    if(!all(QSq.check)){stop("Incorrect number of qualifiers provided. If no qualifers needed for a statement, use NA or \"\".")}
    
    QS.tib <- add_column(QS.tib,
                         Qual.Prop  = QSq[[1]],
                         Qual.Value = QSq[[2]])
  }
  
  # optionally, append columns for source properties and source values for those statements
  if(!is.null(src.properties)|!is.null(src.values)){
    src.properties <- sapply(src.properties,function(x){if(!is.null(x)){as_sid(x)}else{x}})
    src.values     <- sapply(src.values,function(x){if(!(is.qid(x)|is.date(x)|is.quot(x)|is.na(x)|is.empty(x))){paste0('"',x,'"')}else{x}})
    
    QSs <- list(src.properties,
                src.values)
    QSs.rowmax <- max(sapply(c(QS,QSs),length))
    QSs.check  <- sapply(c(QS,QSs),length)==1|
      sapply(c(QS,QSs),length)==QSs.rowmax
    if(!all(QSs.check)){stop("incorrect number of sources provided")}
    
    QS.tib <- add_column(QS.tib,
                         Src.Prop  = QSs[[1]],
                         Src.Value = QSs[[2]])
  }
  
  # NAs to empty strings
  QS.tib[is.na(QS.tib)] <- ""
  
  # if new QIDs are being created via tidy "CREATExyd" keywords, need to insert CREATE lines above and replace "CREATExyz" with "LAST"
  QS.tib <- createrows.tidy(QS.tib)
  
  # output
  if (format=="csv"){
    write.table(QS.tib,quote = FALSE,row.names = FALSE,sep = ",")
  }
  # format up the output
  if (format=="tibble"){
    return(QS.tib)
  }
  if (format=="api"){
    if (is.null(api.token)){stop("API token needed. Find yours at https://quickstatements.toolforge.org/#/user")}
    api.temp1 <- format_tsv(QS.tib)
    api.temp2 <- gsub("\t","%09",api.temp1) # Replace TAB with "%09"
    api.temp3 <- gsub("\n","%0A",api.temp2) # Replace end-of-line with "%0A"
    api.temp4 <- gsub(" ", "%20",api.temp3) # Replace space with "%20"
    api.temp5 <- gsub("\"","%22",api.temp4) # Replace double quote with "%22"
    api.data  <- gsub("/", "%2F",api.temp5) # Replace slash with "%2F"
    
    API.url <- paste0("https://tools.wmflabs.org/quickstatements/api.php",
                      "?action=",   "import",
                      "&submit=",   "1",
                      "&format=",   api.format,
                      "&batchname=",api.batchname,
                      "&username=", api.username,
                      "&token=",    api.token,
                      "&data=",     api.data)
    if(api.submit){
      browseURL(API.url)
    }else{
      return(API.url)
    }
  }
}
