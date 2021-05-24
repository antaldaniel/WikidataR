#Run these on startup or something? The parameters will change not frequently.
#Useful for checking whether quickstatements inputs will be valid to warn early.

.onLoad <- function(){

  message('Updating key variables from wikidata (estimated time <1 min)')

  #Valid reference source properties
  message(' ... Checking valid reference source properties')
  sparql_query <- 'SELECT ?Wikidata_property_to_indicate_a_source ?Wikidata_property_to_indicate_a_sourceLabel WHERE {
                      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                      ?Wikidata_property_to_indicate_a_source wdt:P31 wd:Q18608359.
                   }'
  SID.valid <- query_wikidata(sparql_query)

  #The required data type for each property
  message(' ... Checking required data type for each property')
  sparql_query <- 'SELECT ?property ?propertyLabel ?wbtype WHERE {
                      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                      ?property rdf:type               wikibase:Property.
                      ?property wikibase:propertyType  ?wbtype.
                   }'
  PID.datatype <- query_wikidata(sparql_query)
  PID.datatype$wbtype <- gsub("ontology#","",PID.datatype$wbtype)

  #The expected regex match for each property
  message(' ... Checking expected regex match for each property')
  sparql_query <- 'SELECT ?Wikidata_property ?Wikidata_propertyLabel ?fmt WHERE {
                      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                      ?Wikidata_property wdt:P31/wdt:P279* wd:Q18616576.
                      ?Wikidata_property wdt:P1793 ?fmt
                   }'
  PID.constraint <- unique(query_wikidata(sparql_query))

  #Language abbreviations
  message(' ... Checking language abbreviations')
  sparql_query <- 'SELECT ?abbrev WHERE {
                      ?language wdt:P305 ?abbrev.
                   }'
  lang.abbrev <- query_wikidata(sparql_query)

  #Language abbreviations for current wikis
  message(' ... Checking language abbreviations for current wikis')
  sparql_query <- 'SELECT ?abbrev WHERE {
                      ?Wikipedia_language_edition wdt:P31 wd:Q10876391.
                      ?Wikipedia_language_edition wdt:P424 ?abbrev.
                   }'
  lang.abbrev.wiki <- query_wikidata(sparql_query)

  #Wikimedia abbreviations for current wikis
  message(' ... Checking Wikimedia abbreviations for current wikis')
  sparql_query <- 'SELECT ?abbrev WHERE {
                      ?Wiki_edition wdt:P1800 ?abbrev.
                   }'
  abbrev.wiki <- query_wikidata(sparql_query)

  # #example
  # grep(as.matrix(PID.constraint[PID.constraint$Wikidata_property=="P968","fmt"]),
  #      "mailto:t.shafee@gmail.com",
  #      perl=TRUE)
  assign(x = "WD.globalvar",
         envir = .GlobalEnv,
         value = list(SID.valid=SID.valid,
                      PID.datatype=PID.datatype,
                      PID.constraint=PID.constraint,
                      lang.abbrev=lang.abbrev,
                      lang.abbrev.wiki=lang.abbrev.wiki,
                      abbrev.wiki=abbrev.wiki)
  )

  message('Update complete (data saved as WD.globalvar)')
}