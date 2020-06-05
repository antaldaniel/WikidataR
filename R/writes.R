# Write a Quickstatements table (and optionally submit via API)
as_quickstatement <- function(items,
                              properties,
                              values,
                              qual.properties = NULL,
                              qual.values     = NULL,
                              format          = "api", # api, tibble or print
                              api.username    = "",
                              api.token       = "", # from https://tools.wmflabs.org/quickstatements/#/user
                              api.format      = "v1",
                              api.batchname   = "ts_API_test2",
                              api.submit      = TRUE
                              ){
  
  items           <- sapply(items,function(x){if(x!="LAST"){as_qid(x)}else{x}})
  properties      <- sapply(properties,as_pid)
  
  if (format=="api"){
    values          <- sapply(values,function(x){if(!(is.qid(x)|is.date(x)|is.quot(x))){paste0('$22',x,'$22')}else{x}})
  }
  if (format=="print"){
  values          <- sapply(values,function(x){if(!(is.qid(x)|is.date(x)|is.quot(x))){paste0('"',x,'"')}else{x}})
  }
  
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
  
  # qualifiers properties and statements
  if(!is.null(qual.properties)|!is.null(qual.values)){
    qual.properties <- sapply(qual.properties,function(x){if(!is.null(x)){as_pid(x)}else{x}})
    qual.values     <- sapply(qual.values,function(x){if(!(is.qid(x)|is.date(x)|is.quot(x))){paste0('"',x,'"')}else{x}})
    
    QSq <- list(qual.properties,
                qual.values)
    QSq.rowmax <- max(sapply(c(QS,QSq),length))
    QSq.check  <- sapply(c(QS,QSq),length)==1|
      sapply(c(QS,QSq),length)==QSq.rowmax
    if(!all(QSq.check)){stop("incorrect number of qualifiers provided")}
    
    QS.tib <- add_column(QS.tib,
                         qual.Prop  = QSq[[1]],
                         qual.Value = QSq[[2]])
  }
  if (format=="print"){
    write.table(QS.tib,quote = FALSE,row.names = FALSE,sep = "|")
  }
  if (format=="tibble"){
    return(QS.tib)
  }
  if (format=="api"){
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
