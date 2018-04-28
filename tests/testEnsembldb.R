
library(EnsDb.Hsapiens.v75)

library(httr)
library(jsonlite)
library(xml2)
library(configr)


## Making a "short cut"
edb <- EnsDb.Hsapiens.v75
## print some informations for this package
edb


columns_name <- listColumns(edb)
columns_name <- columns_name[-which(columns_name == "name")]
columns_name <- columns_name[-which(columns_name == "value")]

tx <- transcripts(edb, filter = list(GenenameFilter(c("ETV6", "RUNX1", "AA"))),
                  columns = listColumns(edb, c("tx")))

mcols(tx)
a <- width(tx)
a
idx = which(a == max(a))

info <- tx[idx]
info_name <- names(info)


# gene and protein feature
requestURL <- sprintf("https://www.ebi.ac.uk/proteins/api/coordinates/Ensembl:%s?offset=0&size=100", info_name)
r <- GET(requestURL, accept("application/json"))

stop_for_status(r)

json <- toJSON(content(r))
protein.info <- fromJSON(json)

write.config(httr::content(r), "tests/protein.json", write.type = "json")

# protein feature
# "accession": ["O43521"]
requestURL <- sprintf("https://www.ebi.ac.uk/proteins/api/features/%s", protein.info$accession[[1]])
r <- GET(requestURL, accept("application/json"))

stop_for_status(r)

json <- toJSON(content(r))
feature.info <- fromJSON(json)

write.config(httr::content(r), "tests/feature.json", write.type = "json")







