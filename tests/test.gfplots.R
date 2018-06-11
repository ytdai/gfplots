
library(EnsDb.Hsapiens.v75)
library(openxlsx)

library(httr)
library(jsonlite)
library(xml2)
library(configr)
library(roxygen2)

library(org.Hs.eg.db)

roxygenise()

db <- EnsDb.Hsapiens.v75
x <- read.xlsx("../mutation.xlsx")


data = "../mutation.txt"
update.data = FALSE

type = "mutation"


obj <- readMutation(data = data)



