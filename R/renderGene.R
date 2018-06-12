#
# renderGene
# Generating SVG elements using gene
#
renderGene <- function(obj, reverse = T) {
  # render gene body
  body.svg <- lapply(1:length(obj$transcript@ranges@width), function(x) {
    rect.svg(x = 0,
             y =  (obj$color.theme.default$gene.cds.height - obj$color.theme.default$gene.body.height) / 2,
             width = obj$plot.width * 0.8,
             height = obj$color.theme.default$gene.body.height,
             fill = obj$color.theme.default$gene.body.color)
  })
  body.svg <- unlist(body.svg)
  names(body.svg) <- obj$transcript$tx_id
  obj$gene.body.svg <- body.svg

  # render exon
  exon.name <- names(exons)
  exon.svg <- lapply(1:length(exon.name), function(x) {
    exon.sub <- obj$exons[[which(names(obj$exons) == exon.name[x])]]
    exon.start.sub <- obj$transcript.zoom$start[which(obj$transcript.zoom$transcript == exon.name[x])]
    exon.zoom.sub <- obj$transcript.zoom$zoom[which(obj$transcript.zoom$transcript == exon.name[x])]
    rect.sub <- data.frame(
      start = (exon.sub@ranges@start - exon.start.sub) * exon.zoom.sub,
      width = exon.sub@ranges@width * exon.zoom.sub
    )
    if (reverse) {
      if (length(grep("-", unique(as.character(exon.sub@strand)), fixed = T)) > 0) {
        rect.sub$start <- obj$plot.width * 0.8 - rect.sub$start - rect.sub$width
      }
    }
    exon.sub.svg <- lapply(1:length(rect.sub$start), function(x) {
      rect.svg(x = format(rect.sub$start[x], scientific = FALSE),
               y =  (obj$color.theme.default$gene.cds.height - obj$color.theme.default$gene.exon.height) / 2,
               width = format(rect.sub$width[x], scientific = FALSE),
               height = obj$color.theme.default$gene.exon.height,
               fill = obj$color.theme.default$gene.body.color)
    })
    exon.sub.svg <- unlist(exon.sub.svg)
    return(exon.sub.svg)
  })
  names(exon.svg) <- exon.name
  obj$exon.svg <- exon.svg

  # render cds
  cds.name <- names(cds)
  cds.svg <- lapply(1:length(cds.name), function(x) {
    cds.sub <- obj$cds[[which(names(obj$cds) == cds.name[x])]]
    cds.start.sub <- obj$transcript.zoom$start[which(obj$transcript.zoom$transcript == cds.name[x])]
    cds.zoom.sub <- obj$transcript.zoom$zoom[which(obj$transcript.zoom$transcript == cds.name[x])]
    rect.sub <- data.frame(
      start = (cds.sub@ranges@start - cds.start.sub) * cds.zoom.sub,
      width = cds.sub@ranges@width * cds.zoom.sub
    )
    if (reverse) {
      if (length(grep("-", unique(as.character(cds.sub@strand)), fixed = T)) > 0) {
        rect.sub$start <- obj$plot.width * 0.8 - rect.sub$start - rect.sub$width
      }
    }
    cds.sub.svg <- lapply(1:length(rect.sub$start), function(x) {
      rect.svg(x = format(rect.sub$start[x], scientific = FALSE, nsmall = 4),
               y = 0,
               width = format(rect.sub$width[x], scientific = FALSE),
               height = obj$color.theme.default$gene.cds.height,
               fill = obj$color.theme.default$gene.body.color)
    })
    cds.sub.svg <- unlist(cds.sub.svg)
    return(cds.sub.svg)
  })
  names(cds.svg) <- cds.name
  obj$cds.svg <- cds.svg

  return(obj)
}

