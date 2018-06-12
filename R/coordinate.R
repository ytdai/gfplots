#
# coordinate
# transform coordiante
#
gene2protein <- function(obj) {

  pos <- lapply(1:dim(obj$mutation.data)[1], function(x) {
    trans.sub <- obj$mutation.data$Transcript[x]
    pos.sub <- obj$mutation.data$VariantPos[x]

    range.sub <- obj$cds[[which(names(obj$cds) == trans.sub)]]
    if (length(grep("-", unique(as.character(range.sub@strand)), fixed = T)) > 0) {
      if (pos.sub >= range.sub@ranges@start[1]) {
        tmp.pos <- 0
      } else if (pos.sub <= (range.sub@ranges@start[length(range.sub$tx_id)]+range.sub@ranges@width[length(range.sub$tx_id)]) ) {
        tmp.pos <- sum(range.sub@ranges@width) / 3
      } else {
        tmp <- range.sub[which(range.sub@ranges@start >= pos.sub), ]
        tmp.pos <- floor((sum(tmp@ranges@width) + tmp@ranges@start[length(tmp$tx_id)] - pos.sub) / 3)
      }
    } else {
      if (pos.sub <= range.sub@ranges@start[1]) {
        tmp.pos <- 0
      } else if (pos.sub >= (range.sub@ranges@start[length(range.sub$tx_id)]+range.sub@ranges@width[length(range.sub$tx_id)]) ) {
        tmp.pos <- sum(range.sub@ranges@width) / 3
      } else {
        tmp <- range.sub[which(range.sub@ranges@start <= pos.sub), ]
        tmp.pos <- floor((sum(tmp@ranges@width) + pos.sub - tmp@ranges@start[length(tmp$tx_id)]) / 3)
      }
    }
  })
  return(unlist(pos))
}
