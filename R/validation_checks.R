#Run these on startup or something? The parameters will change not frequently.
#Useful for checking whether quickstatements inputs will be valid to warn early.

#Valid reference source properties 
sparql_query <- 'SELECT ?Wikidata_property_to_indicate_a_source ?Wikidata_property_to_indicate_a_sourceLabel WHERE {
                    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                    ?Wikidata_property_to_indicate_a_source wdt:P31 wd:Q18608359.
                 }'

SID.valid <- query_wikidata(sparql_query)

#The expected regex match for each property
sparql_query <- 'SELECT ?Wikidata_property ?Wikidata_propertyLabel ?fmt WHERE {
                    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                    ?Wikidata_property wdt:P31/wdt:P279* wd:Q18616576.
                    ?Wikidata_property wdt:P1793 ?fmt
                 }'
PID.constraint <- unique(query_wikidata(sparql_query))

#The required data type for each property
sparql_query <- 'SELECT ?property ?propertyLabel ?wbtype WHERE {
                    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                    ?property rdf:type               wikibase:Property.
                    ?property wikibase:propertyType  ?wbtype.
                 }'

PID.datatype <- query_wikidata(sparql_query)

#example
grep(as.matrix(PID.constraint[PID.constraint$Wikidata_property=="P968","fmt"]),
     "mailto:t.shafee@gmail.com",
     perl=TRUE)
