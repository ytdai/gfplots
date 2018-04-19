
library(org.Hs.eg.db)
library(magrittr)

ls("package:org.Hs.eg.db")

keys <- head( keys(org.Hs.eg.db, keytype="SYMBOL") )
keys

keys <- "RUNX1"

keytypes(org.Hs.eg.db)

select(org.Hs.eg.db, keys=keys, columns = c("UNIPROT"),
       keytype="SYMBOL")

# Ensembl
x <- org.Hs.egENSEMBL
mapped_genes <- mappedkeys(x)
xx <- as.list(x[mapped_genes])
if(length(xx) > 0) {
  # Get the Ensembl gene IDs for the first five genes
  xx[1:5]
  # Get the first one
  xx[[1]]
}
head(xx)

# Ensembl Protein
x <- org.Hs.egENSEMBLPROT
# Get the entrez gene IDs that are mapped to an Ensembl ID
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])
if(length(xx) > 0) {
  # Get the Ensembl gene IDs for the first five proteins
  xx[1:5]
  # Get the first one
  xx[[1]]
}
head(xx)

mapped_genes <- 9606
xx <- as.list(x[mapped_genes])


edb <- EnsDb.Hsapiens.v75

supportedFilters(edb)

Tx <- transcripts(edb, filter = list(GenenameFilter("RUNX1")))
Tx

start(Tx)
Tx$tx_biotype

edb %>% filter(~ symbol == "RUNX1" & tx_biotype != "protein_coding") %>% transcripts


listTables(edb)

## Define the filter
grf <- GRangesFilter(GRanges("11", ranges = IRanges(114129278, 114129328),
                             strand = "+"), type = "any")

## Query genes:
gn <- genes(edb, filter = grf)
gn

txs <- transcripts(edb, filter = GenenameFilter(gn$gene_name))
txs

library(ggbio)

## Create a plot for all transcripts of the gene SKA2
autoplot(edb, ~ genename == "SKA2")









