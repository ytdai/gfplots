
library(EnsDb.Hsapiens.v75)
library(openxlsx)

library(httr)
library(jsonlite)
library(xml2)
library(configr)
library(roxygen2)

library(org.Hs.eg.db)
library(easySVG)

roxygenise()

edb <- EnsDb.Hsapiens.v75

data = "../mutation.txt"
update.data = FALSE
value = 600
metadata = "width"
initial = F

coding.only = T

type = "mutation"

names(obj)

for (i in list.files("R/")) {
  source(paste0("R/", i))
}

obj <- readMutation(data = data)
obj <- updatePlotParam(obj)



