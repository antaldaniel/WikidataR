
#Valid reference source properties 
sparql_query <- 'SELECT ?Wikidata_property_to_indicate_a_source ?Wikidata_property_to_indicate_a_sourceLabel WHERE {
                    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
                    ?Wikidata_property_to_indicate_a_source wdt:P31 wd:Q18608359.
                 }'

SIDs.valid <- query_wikidata(sparql_query)
