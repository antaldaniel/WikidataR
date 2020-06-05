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
- Handle and manipulate Wikidata objects as lists and tibbles
It combines and builds on the untilities of Christian Graul's
[rwikidata](https://github.com/chgrl/rwikidata), Oliver Keyes' [WikidataR](https://github.com/Ironholds/WikidataR) and Mikhail Popov's [WikidataQueryServiceR](https://github.com/bearloga/WikidataQueryServiceR). For details on how to best use it, see the [explanatory
vignette](https://CRAN.R-project.org/package=WikidataR/vignettes/Introduction.html).

Installation
======

For the most 2017 CRAN version:

    install.packages("WikidataR")
    
For this development version:

    library(devtools)
    devtools::install_github("TS404/WikidataR")




### Wikidata Query Example: fetching genres of a particular movie
You submit SPARQL queries using the `query_wikidata()` function.

In this example, we find an "instance of" ([P31](https://www.wikidata.org/wiki/Property:P31)) "film" ([Q11424](https://www.wikidata.org/wiki/Q11424)) that has the label "The Cabin in the Woods" ([Q45394](https://www.wikidata.org/wiki/Q45394)), get its genres ([P136](https://www.wikidata.org/wiki/Property:P136)), and then use [WDQS label service](https://www.mediawiki.org/wiki/Wikidata_query_service/User_Manual#Label_service) to return the genre labels.

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

    ## 5 rows were returned by WDQS

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


Dependencies
======
* R. Doy.
* [httr](https://cran.r-project.org/package=httr) and its dependencies.
* [WikipediR](https://cran.r-project.org/package=WikipediR)
