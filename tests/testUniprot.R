
library(httr)
library(jsonlite)
library(xml2)
library(configr)


# GET /coordinates/{dbtype}:{dbid}
requestURL <- "https://www.ebi.ac.uk/proteins/api/coordinates/Ensembl:ENST00000393256?offset=0&size=100"
r <- GET(requestURL, accept("application/json"))

stop_for_status(r)

json <- toJSON(content(r))
a <- fromJSON(json)

write.config(httr::content(r), "tests/test.json", write.type = "json")

head((as_list(xml)))

json.file <- read.config("tests/test.json")
write.config(json.file, "tests/test.json", write.type = "json")







