WikidataR
=========

An combined R package for reading, writing and handling Wikidata semantic data (via APIs).

__Authors:__ Oliver Keyes, Serena Signorelli & Christian Graul, Mikhail Popov, Thomas Shafee<br/> 
__License:__ [MIT](http://opensource.org/licenses/MIT)<br/>
__Status:__ Stable

[![Travis-CI Build Status](https://travis-ci.org/Ironholds/WikidataR.svg?branch=master)](https://travis-ci.org/Ironholds/WikidataR)![downloads](http://cranlogs.r-pkg.org/badges/grand-total/WikidataR)

Description
======
WikidataR includes functions to:
- read from wikidata (single items, properties, or properties)
- query wikidata (retreiving all items that match a set of criterial via [Wikidata SPARQL query service](https://query.wikidata.org))
- write to Wikidata (adding new items or statements via [QuickStatements](https://tools.wmflabs.org/quickstatements)) 
- Handle and manipulate Wikidata objects (as lists and tibbles)

It combines and builds on the untilities of Christian Graul's
[rwikidata](https://github.com/chgrl/rwikidata), Oliver Keyes' [WikidataR](https://github.com/Ironholds/WikidataR) and Mikhail Popov's [WikidataQueryServiceR](https://github.com/bearloga/WikidataQueryServiceR). For details on how to best use it, see the [explanatory
vignette](https://CRAN.R-project.org/package=WikidataR/vignettes/Introduction.html).

Installation
======

For the 2017 CRAN versions:

    install.packages("WikidataR","WikidataQueryServiceR")
    
For this development version:

    library(devtools)
    devtools::install_github("TS404/WikidataR")

Examples
======
### Get Wikidata QIDs and full items (example: journal articles)
In this example, we search for three articles using their DOIs ([P356](https://www.wikidata.org/wiki/Property:P356)), find their QIDs,  download their full wikidata entries, and then extract the "main topics" (note PID didn't have to be used).

``` r
    article.qid      <- qid_from_DOI(c('10.15347/WJM/2017.007','10.15347/WJM/2019.001','10.15347/WJM/2019.007'))
    article.q        <- get_item(article.qid)
    article.topics.p <- extract_claims(article.q, "main topic")
    get_names_from_properties(article.topics.p)
```
Which returns a tibble for each of the journal articles, listing the main topics of each and their QIDs.

### Query Wikidata (example: movie genres)

In this example, we search Wikidata for any items that are an "instance of" ([P31](https://www.wikidata.org/wiki/Property:P31)) "film" ([Q11424](https://www.wikidata.org/wiki/Q11424)) that has the label "The Cabin in the Woods" ([Q45394](https://www.wikidata.org/wiki/Q45394)), and ask for the item's genres ([P136](https://www.wikidata.org/wiki/Property:P136)).

``` r
query_wikidata('SELECT DISTINCT
  ?genre ?genreLabel
WHERE {
  ?film wdt:P31 wd:Q11424.
  ?film rdfs:label "The Cabin in the Woods"@en.
  ?film wdt:P136 ?genre.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}')
```
Which reurns 5 rows:

| genre                                     | genreLabel           |
|:------------------------------------------|:---------------------|
| <http://www.wikidata.org/entity/Q471839>  | science fiction film |
| <http://www.wikidata.org/entity/Q1342372> | monster film         |
| <http://www.wikidata.org/entity/Q224700>  | comedy horror        |
| <http://www.wikidata.org/entity/Q200092>  | horror film          |
| <http://www.wikidata.org/entity/Q859369>  | comedy-drama         |

For more example SPARQL queries, see [this page](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples) on [Wikidata](https://www.wikidata.org/wiki/Wikidata:Main_Page).

`query_wikidata()` can accept multiple queries, returning a (potentially named) list of data frames. If the vector of SPARQL queries is named, the results will inherit those names.

####Links for learning SPARQL

-   [A beginner-friendly course for SPARQL](https://www.wikidata.org/wiki/Wikidata:A_beginner-friendly_course_for_SPARQL)
-   Building a SPARQL query: [Museums on Instagram](https://www.wikidata.org/wiki/Help:SPARQL/Building_a_query/Museums_on_Instagram)
-   [SPARQL Query Examples](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples) for WDQS
-   [Using SPARQL to access Linked Open Data](http://programminghistorian.org/lessons/graph-databases-and-SPARQL) by Matthew Lincoln
-   Interesting or illustrative [SPARQL queries](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries) for Wikidata
-   Wikidata [2016 SPARQL Workshop](https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/2016_SPARQL_Workshop)
-   [Wikidata SPARQL Query video tutorial](https://www.youtube.com/watch?v=1jHoUkj_mKw) by Navino Evans
-   *[Learning SPARQL](http://www.learningsparql.com/)* by Bob DuCharme
-   [WDQS User Manual](https://www.mediawiki.org/wiki/Wikidata_query_service/User_Manual)

### Write to Wikidata (example: journal articles)


``` r
sparql_query <- 'SELECT ?Article ?ArticleLabel ?JLabel ?T ?peer_review_URL WHERE {
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
  ?Article wdt:P1433 wd:Q24657325.
  OPTIONAL { ?Article wdt:P1433 ?J. }
  OPTIONAL { ?Article wdt:P1476 ?T. }
  OPTIONAL { ?Article wdt:P7347 ?peer_review_URL. }}
LIMIT 10000'
articles.qr <- as_tibble(query_wikidata(sparql_query))
articles.qr <- articles.qr[articles.qr$peer_review_URL=="",] #omit those with review URLs listed
review.URLs <- paste0('https://en.wikiversity.org/wiki/Talk:',
                      articles.qr$JLabel,
                      "/",
                      articles.qr$T
                     )
review.URLs <- gsub(" ","_",review.URLs)

as_quickstatement(items      = sapply(sapply(articles.qr$Article,pattern = "/",stringr::str_split),tail,1),
                  properties = "Peer review URL",
                  values     = review.URLs,
                  format     = "tibble",
                  )
                  
as_quickstatement(items      = sapply(sapply(articles.qr$Article,pattern = "/",stringr::str_split),tail,1),
                  properties = "Peer review URL",
                  values     = review.URLs,
                  format     = "api",
                  token=,#REDCATED# Find from from https://tools.wmflabs.org/quickstatements/#/user
                  )
```

Dependencies
======
* R. Doy.
* [httr](https://cran.r-project.org/package=httr) and its dependencies.
* [WikipediR](https://cran.r-project.org/package=WikipediR)
