#'
#' readMutation
#'
#' @title function to read mutation from different file
#' @description \code{readMutation} is used to read mutation table from
#' data frame or tab-delimited files.
#'
#' @param data a data.frame or character. If data.frame, this must contain all
#' basic information of mutation file. And if character, this must contain the
#' file path of mutation file.
#' @param obj a list object.
#' @param update.data logical. If TRUE, the mutation.data in object will be
#' updated. And if FALSE, it will generate a new list object which contains
#' the data.
#' @param verbose logical
#' @return An object
#'
#' @author Yuting Dai
#' @export
#'
#' @rdname readMutation
#' @examples
#'
readMutation <- function(data,
                         obj,
                         update.data = FALSE,
                         verbose = TRUE) {

  if (missing(data)) {
    stop("[ERROR] data is a required")
  }
  if (missing(obj) & isTRUE(update.data)) {
    stop("[ERROR] if update.data is TRUE, the obj parameter is required")
  }
  # read mutation data
  if (is.character(data)) {
    if (file.exists(data)) {
      mutation.data <- read.table(data, header = T, sep = "\t", stringsAsFactors = F)
      if (update.data) {
        obj$mutation.rawdata <- mutation.data
      } else {
        obj <- list()
        obj$mutation.rawdata <- mutation.data
      }
    } else {
      stop(paste0("[ERROR] ", data, " is not available" ))
    }
  } else if (is.data.frame(data)) {
    if (update.data) {
      obj$mutation.rawdata <- mutation.data
    } else {
      obj <- list()
      obj$mutation.rawdata <- mutation.data
    }
  } else {
    stop("[ERROR] data must be a character or data frame")
  }

  # check mutation data header
  colnames.str <- c("Symbol", "Sample", "Group", "Chromosome",
                    "VariantPos", "Ref", "Alt", "Tag", "Class",
                    "Transcript")
  if ( sum(!colnames(obj$mutation.rawdata) %in% colnames.str) > 0 ) {
    stop("[ERROR] colnames in mutation data is not correct")
  }

  # update mutation data
  if (is.null(obj$class.str)) {
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
  class.list <- sort(as.character(unique(obj$mutation.rawdata$Class)))
  class.list <- toupper(class.list)
  class.list <- class.list[class.list %in% obj$class.str$class]

  if (length(class.list) == 0) {
    stop("[ERROR] Class in mutation data is not correct")
  } else {
    message( "[INFO] class: << ", paste(class.list, collapse = " "), " >> will be plotted" )
    obj$mutation.data <- obj$mutation.rawdata[toupper(obj$mutation.rawdata$Class) %in% class.list , ]
  }

  # update gene list
  gene.list <- sort(as.character(unique(obj$mutation.data$Symbol)))
  gene.list <- gene.list[!is.na(gene.list)]
  gene.list <- toupper(gene.list)
  obj$gene.list <- gene.list

  # return object
  return(obj)

}
