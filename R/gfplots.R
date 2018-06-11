#'
#' gfplots packages
#'
#' @title main function of gfplots packages
#' @description \code{gfplots} is an useful package to visualize gene fusions and
#' mutations. The defaluse annotation database is ENSEMBL
#'
#' @details
#'
#'
#' @param obj an list object. Contains all plot parameter
#' @param type a character. Plot type, "mutation" or "fusion"
#' @param edb an S4 object. EnsDb.Hsapiens.v75 or EnsDb.Hsapiens.v86
#'
#' @author Yuting Dai
#' @export
#'
#' @rdname gfplots
#' @examples
#'
gfplots <- function(obj,
                    type = c("mutation", "fusion"),
                    edb,
                    output.dir = ".",
                    verbose = FALSE) {

  #####################
  # checking paramters
  #####################
  if (verbose) message("[INFO] checking parameters")
  if (missing(edb)) {
    stop("[ERROR] ensembl database is required")
  }
  if (!isS4(edb)) {
    stop("[ERROR] edb must be an S4 vector, please load EnsDb.Hsapiens.v75 or EnsDb.Hsapiens.v86")
  }

  if (type == "mutation") {
    if (!"mutation.data" %in% names(obj)) {
      stop("[ERROR] mutation data is not in the object, try readMutation")
    }
  } else if (type == "fusion") {
    if (!"fusion.data" %in% names(obj)) {
      stop("[ERROR] fusion data is not in the object, try readFusion")
    }
  } else {
    stop("[ERROR] invalid type")
  }

  #####################
  # plot
  #####################

}






















