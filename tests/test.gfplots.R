
library(EnsDb.Hsapiens.v75)
library(openxlsx)

library(httr)
library(jsonlite)
library(xml2)
library(configr)
library(roxygen2)

library(org.Hs.eg.db)

roxygenise()

edb <- EnsDb.Hsapiens.v75

data = "../mutation.txt"
update.data = FALSE
value = 600
metadata = "width"
initial = F

coding.only = T

type = "mutation"


obj <- readMutation(data = data)
obj <- updatePlotParam(obj)



