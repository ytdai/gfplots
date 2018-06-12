#
# renderMutation
# Generating SVG elements of mutation in
# gene and protein
#
renderMutation <- function(obj, reverse = T) {
  # merge transcript
  trans <- as.character(unique(obj$mutation.data$Transcript))
  if ( sum(!trans %in% obj$transcript.coding) > 0  ) {
    message("[WARNING] invalid transcript, use defalut transcripts instead")

    sub <- obj$transcript.zoom[grep("coding", obj$transcript.zoom$type), ]
    sub <- sub[order(sub$length, decreasing = T), ]

    idx <- !obj$mutation.data$Transcript %in% obj$transcript.coding
    sub.symbol <- obj$mutation.data$Symbol[idx]
    obj$mutation.data$Transcript[idx] <- as.character(sub$transcript[match(sub.symbol, sub$symbol)])
  }
  pos.pro <- gene2protein(obj)
  obj$mutation.data$convert <- pos.pro

  # Transcript mutation profile
  mark.transcript <- unique(obj$mutation.data$Transcript)
  mutation.gene.svg <- lapply(1:length(mark.transcript), function(x) {
    sub.name <- mark.transcript[x]
    sub.info <- obj$mutation.data[which(obj$mutation.data$Transcript == sub.name), ]
    sub.info <- sub.info[order(sub.info$convert), ]

    pos.tmp <- table(sub.info$VariantPos)
    sub.mut.type <- lapply(1:length(pos.tmp), function(xx) {
      tmp <- sub.info[which(sub.info$VariantPos == names(pos.tmp)[xx]), ]
      tmp <- length(table(tmp$Tag))
    })
    pos.tmp <- data.frame(
      pos = names(pos.tmp),
      freq = as.numeric(pos.tmp),
      type = unlist(sub.mut.type)
    )

    # dynamic position
  })


}
