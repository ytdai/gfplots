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
  }

  obj$transcript <- tx
  obj$transcript.coding <- names(obj$transcript)[grep("coding", tx$tx_biotype)]

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
                       listColumns(edb, "protein_domain")))

  domain <- as.character(unique(protein$protein_domain_id[which(protein$protein_domain_source == "pfam")]))
  domain = domain[!is.na(domain)]

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

  return(obj)
}
