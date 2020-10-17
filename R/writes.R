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
  
  items           <- sapply(items,function(x){if(x!="LAST"){as_qid(x)}else{x}})
  items[remove]   <- paste0("-",items[remove])
  properties      <- sapply(properties,as_pid)
  
  #strings need quotation marks, and in APIs those are indicated as $22 
  if (format=="api"){
    values          <- sapply(values,function(x){if(!(is.qid(x)|is.date(x)|is.quot(x))){paste0('$22',x,'$22')}else{x}})
  }
  if (format=="tibble"|"csv"){
    values          <- sapply(values,function(x){if(!(is.qid(x)|is.date(x)|is.quot(x))){paste0('"',x,'"')}else{x}})
  }
  
  # build the basic tibble of the items and what properties and values to add as statements
  QS <- list(items,
             properties,
             values)
  QS.rowmax <- max(sapply(QS,length))
  QS.check  <- sapply(QS,length)==1|
    sapply(QS,length)==QS.rowmax
  if(!all(QS.check)){stop("not all quickstatement columns equal length")}
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
    if(!all(QSq.check)){stop("incorrect number of qualifiers provided")}
    
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
  
  # format up the output
  if (format=="tibble"){
    return(QS.tib)
  }
  if (format=="csv"){
    write.table(QS.tib,quote = FALSE,row.names = FALSE,sep = ",")
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
