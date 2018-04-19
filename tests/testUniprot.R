
library(httr)
library(jsonlite)
library(xml2)
library(configr)

requestURL <- "https://www.ebi.ac.uk/proteins/api/variation?offset=0&size=100&accession=Q01196"
r <- GET(requestURL, accept("application/json"))

stop_for_status(r)

json <- toJSON(content(r))
a <- fromJSON(json)

write.config(httr::content(r), "tests/test.json", write.type = "json")

head((as_list(xml)))

json.file <- read.config("tests/test.json")
write.config(json.file, "tests/test.json", write.type = "json")

