#'
#' gfplots packages
#'
#' @title main function of gfplots packages
#' @description \code{gfplots} provides an
#'
#' @details
#'
#'
#' @param x data.frame. The basic information that need to be plotted
#'     using gfplots. If \param{plot.type} is "mutation", then the
#'     data.frame show only have
#' @param db object. If the annotation database you use is "Ensembl-GRCh37",
#'     package EnsDb.Hsapiens.v75 needs to be loaded first. If the annotation
#'     database is "Ensembl-GRCh38", then package EnsDb.Hsapiens.v85 needs to be
#'     loaded first.
#' @param output.directory string. The absolute path of your
#'     output dirctory which contains your report.
#' @param plot.type string. "mutation" or "fusion". Different plot.type
#'     corresponds to different plot style.
#'
#' @return
#'
#' @author Yuting Dai
#' @export
#'
#' @rdname gfplots
#' @examples
#'
gfplots <- function(x,
                    db,

                    # database information
                    output.directory = ".",
                    output.name = paste0("gfplots_", as.character(floor(as.numeric(Sys.time())))),
                    plot.type = "mutation",

                    # visualization color theme
                    plot.config = NULL,
                    gene.db.config = gene.db.config,
                    protein.db.config = protein.db.config,
                    ...) {

  #####################
  # checking paramters
  #####################

  # checking x
  if (!is.data.frame(x) || ncol(x) != 9) {
    stop("[ERROR] x must be a data frame with 9 columns")
  }
  # checking db
  if (!isS4(db)) {
    stop("[ERROR] db must be a S4 object")
  }

  colname_mutation <- c("Symbol", "Sample", "Group", "Chromosome", "VariantPos",
                        "Ref", "Alt", "Tag", "Class")
  colname_fusion <- c("Symbol", "Sample", "Group", "Chromosome_1", "FusionPos_1",
                      "Chromosome_2", "FusionPos_2", "Tag", "Class")
  if (plot.type == "mutation" && sum(colnames(x) != colname_mutation) > 0 ) {
    stop("[ERROR] when plot.type is mutation, the column name must be c(\"Symbol\", \"Sample\", \"Group\", \"Chromosome\", \"VariantPos\",
                        \"Ref\", \"Alt\", \"Tag\", \"Class\") ")
  }
  if (plot.type == "fusion" && sum(colnames(x) != colname_fusion) > 0 ) {
    stop("[ERROR] when plot.type is mutation, the column name must be c(\"Symbol\", \"Sample\", \"Group\", \"Chromosome_1\", \"FusionPos_1\",
                        \"Chromosome_2\", \"FusionPos_2\", \"Tag\", \"Class\") ")
  }

  #######################
  # generate plot figure
  #######################

  if (plot.type == "mutation") {
    gfMutation(x=x,
               db=db,
               # visualization color theme
               plot.config = plot.config,
               gene.db.config = gene.db.config,
               protein.db.config = protein.db.config,
               ...)
  }
  if (plot.type == "fusion") {
    gfFusion(x=x,
             db=db,
             # visualization color theme
             plot.config = plot.config,
             gene.db.config = gene.db.config,
             protein.db.config = protein.db.config,
             ...)
  }




}






















