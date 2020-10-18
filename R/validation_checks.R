#Run these on startup or something? The parameters will change not infrequently (e.g. regex.
#Valid reference source properties 
sparql_query <- 'SELECT ?Wikidata_property_to_indicate_a_source ?Wikidata_property_to_indicate_a_sourceLabel WHERE {
                    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                    ?Wikidata_property_to_indicate_a_source wdt:P31 wd:Q18608359.
                 }'

SIDs.valid <- query_wikidata(sparql_query)[,1]

#The expected regex match for each property
sparql_query <- 'SELECT ?Wikidata_property ?Wikidata_propertyLabel ?fmt WHERE {
                    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                    ?Wikidata_property wdt:P31/wdt:P279* wd:Q18616576.
                    ?Wikidata_property wdt:P1793 ?fmt
                 }'
PID.fmt.constraints <- query_wikidata(sparql_query)