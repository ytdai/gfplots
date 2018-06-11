#'
#' updatePlotParam
#'
#' @param obj a list object. An object which contain all parameters
#'  and metadata of gfplots
#' @param metadata a character. Target to modify metadata
#' @param value value of the metadata
#' @return an object
#'
#' @author Yuting Dai
#' @export
#'
updatePlotParam <- function(obj,
                            metadata = NULL,
                            value = NULL,
                            initial = F) {

  # initialization of obj
  if (initial) {
    obj$plot.width = 1000
    obj$plot.height = 800
    obj$color.theme = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
                        "#E6AB02", "#A6761D", "#A6CEE3", "#1F78B4", "#B2DF8A",
                        "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                        "#CAB2D6", "#6A3D9A")
    obj$color.theme <- "default"
    obj$color.theme.default <- list(
      max.show.num = 0,
      # parameters for protein
      protein.body = "#DDDDDD",
      protein.stroke = "#000000",
      protein.body.height = 20,
      protein.domain.height = 20,
      protein.mut.height = 30,
      # parameters for gene
      gene.body = "#0066FF",
      gene.body.height = 5,
      gene.exon.height = 10,
      gene.cds.height = 15,
      gene.mut.height = 20
    )
  }

  # add metadata
  if (!is.null(metadata) & !is.null(value)) {
    if (!is.character(metadata)) {
      stop("[ERROR] metadata must be a character")
    }
    meta <- list(value)
    names(meta) <- metadata

    if (metadata %in% names(obj)) {
      obj[[which(names(obj) == metadata)]] = value
    } else {
      obj <- append(obj, meta)
    }
  }

  # check basic parameters
  if (!"plot.width" %in% names(obj)) {
    obj$plot.width = 1000
  }
  if (!"plot.height" %in% names(obj)) {
    obj$plot.height = 800
  }
  if (!"color.theme" %in% names(obj)) {
    obj$color.theme = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
                        "#E6AB02", "#A6761D", "#A6CEE3", "#1F78B4", "#B2DF8A",
                        "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                        "#CAB2D6", "#6A3D9A", "#339900", "#CC0099", "#FFCC00",
                        "#000066", "#3399FF", "#3366FF", "#3333CC")
  }
  if (!"class.str" %in% names(obj)) {
    obj$class.str <- data.frame(
      class = c("MISSENSE","EXON","FRAMESHIFT","NONSENSE","SILENT",
                "PROTEINDEL","PROTEININS","SPLICE_REGION","SPLICE","INTRON",
                "UTR_3","UTR_5","NONSTANDARD","NONCODING","SNV",
                "MNV","SEQUENCE_INSERTION","SEQUENCE_DELETION"),
      color = c("#3D88CA", "#BCBC35", "#D93F42", "#FD7F28", "#339E34",
                "#7F7F7F", "#8B564C", "#9369BB", "#663FFB", "#819881",
                "#988198", "#819881", "#000000", "#000000", "#5984FC",
                "#6479B6", "#EB5D68", "#EFB035"))
  }
  if (!"color.theme" %in% names(obj)) {
    obj$color.theme <- "default"
  }
  if (!"color.theme.default" %in% names(obj)) {
    obj$color.theme.default <- list(
      max.show.num = 0,
      # parameters for protein
      protein.body = "#DDDDDD",
      protein.stroke = "#000000",
      protein.body.height = 20,
      protein.domain.height = 20,
      protein.mut.height = 30,
      # parameters for gene
      gene.body = "#0066FF",
      gene.body.height = 5,
      gene.exon.height = 10,
      gene.cds.height = 15,
      gene.mut.height = 20
    )
  }

  return(obj)

}
