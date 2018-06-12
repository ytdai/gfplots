#
# fetchTranscript
# Fecthing transcript data in ensembl database
# using genelist
#
fetchTranscript <- function(obj,
                            edb = edb) {

  gene.list <- obj$gene.list

  tx <- transcripts(edb, filter = list(GenenameFilter(gene.list)))

  gene.list.tx <- unique(tx$gene_name)

  if (sum(!gene.list %in% gene.list.tx) > 0) {
    a <- gene.list[!gene.list %in% gene.list.tx]
    message("[WARNING] gene << ", paste(a, collapse = " "), " >> is not a correct symbol")
    message("[INFO] update gene list in object")
    obj$gene.list <- gene.list[gene.list %in% gene.list.tx]
    obj$mutation.data <- obj$mutation.data[obj$mutation.data$Symbol %in% obj$gene.list, ]
  }

  obj$transcript <- tx
  obj$transcript.coding <- names(obj$transcript)[grep("coding", tx$tx_biotype)]

  obj$exons <- exonsBy(edb, filter = list(TxNameFilter(obj$transcript$tx_id)))
  obj$cds <- cdsBy(edb, filter = list(TxNameFilter(obj$transcript.coding)))

  obj$transcript.zoom <- data.frame(
    symbol = obj$transcript$gene_name,
    transcript = obj$transcript$tx_id,
    type = obj$transcript$tx_biotype,
    start = obj$transcript@ranges@start,
    length = obj$transcript@ranges@width,
    zoom = obj$plot.width * 0.8 / obj$transcript@ranges@width
  )

  return(obj)
}

#
# fetchProtein
# Fecthing protein data in ensembl database
# using transcripts
#
fetchProtein <- function(obj,
                         edb = edb) {

  transcript.coding <- obj$transcript.coding

  protein <- proteins(edb, filter = ~ tx_id == transcript.coding,
                      columns = c("protein_id", "tx_id",
                                  listColumns(edb, c("protein", "protein_domain"))))

  domain <- as.character(unique(protein$protein_domain_id[which(protein$protein_domain_source == "pfam")]))
  domain = domain[!is.na(domain)]

  # match domain color
  if (length(domain) <= length(obj$color.theme)) {
    domain <- data.frame(
      domain = domain,
      color = obj$color.theme[1:length(domain)]
    )
  } else {
    domain <- data.frame(
      domain = domain,
      color = c(obj$color.theme,
                colorRampPalette(c("green", "blue", "red")[1:(length(domain) - length(obj$color.theme))]))
    )
  }

  obj$protein <- protein
  obj$protein.domain <- domain
  obj$protein.zoom <- data.frame(
    transcript = obj$protein$tx_id,
    length = nchar(obj$protein$protein_sequence),
    zoom = obj$plot.width * 0.8 / nchar(obj$protein$protein_sequence)
  )

  return(obj)
}
