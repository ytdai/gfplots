#'
#' gfMutation
#'
#' @description plot gene mutation and protein mutation
gfMutation <- function(x=x,
                       db=db,
                       # visualization color theme
                       plot.config = plot.config,
                       gene.db.config = gene.db.config,
                       protein.db.config = protein.db.config,
                       ...) {


  ##########################
  # check gene list
  ##########################

  # unique gene list
  gene_list <- data.frame(table(x$Symbol))
  gene_list <- gene_list[order(gene_list$Freq, decreasing = T), ]

  # check right symbol name
  tx <- transcripts(edb, filter = list(GenenameFilter(gene_list$Var1)),
                    columns = listColumns(edb, c("tx")))
  gene_db_name <- unique(mcols(tx)$gene_name)
  gene_list_out <- as.character(gene_list$Var1[! gene_list$Var1 %in% gene_db_name])

  # if there is outline gene symbol name, checking the database
  # if the out gene name is not included in the databse, these gene will not be plotted
  if (length(gene_list_out) > 0) {
    message("[WARNING] ", paste(gene_list_out, collapse = ", "), " is not an offical gene symbol")
    message("[INFO] checking the customized gene database ...")
    if (is.null(gene.db.config)) {
      message("[WARNING] gene.db.config is not provided. ")
      message("[WARNING] " , paste(gene_list_out, collapse = ", "), " will not be plotted. ")
    }
    if (is.null(protein.db.config)) {
      message("[WARNING] protein.db.config is not provided. ")
      message("[WARNING] domain of " , paste(gene_list_out, collapse = ", "), " will not be plotted. ")
    }


  }




}

