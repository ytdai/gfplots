
library(EnsDb.Hsapiens.v75)
library(openxlsx)

library(httr)
library(jsonlite)
library(xml2)
library(configr)


db <- EnsDb.Hsapiens.v75
x <- read.xlsx("../mutation.xlsx")



db.type = "ensembl"
version = "GRCh37"
output.directory = "tests/"
output.name = paste0("gfplots_", as.character(floor(as.numeric(Sys.time()))))
plot.type = "mutation"

# visualization color theme
plot.config = NULL
gene.config.db = NULL
protein.config.db = NULL







