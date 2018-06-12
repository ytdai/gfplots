#
# renderGene
# Generating SVG elements using protein
#
renderProtein <- function(obj) {
  # render protein body
  flag <- max(obj$color.theme.default$protein.body.height, obj$color.theme.default$protein.domain.height)
  body.svg <- lapply(1:length(obj$transcript.coding), function(x) {
    rect.svg(x = 0,
             y =  (flag - obj$color.theme.default$protein.body.height) / 2,
             width = obj$plot.width * 0.8,
             height = obj$color.theme.default$protein.body.height,
             fill = obj$color.theme.default$protein.body.color)
  })
  body.svg <- unlist(body.svg)
  names(body.svg) <- obj$transcript.coding
  obj$protein.body.svg <- body.svg

  # render protein domain
  domain.svg <- lapply(1:length(obj$transcript.coding), function(x) {
    domain.sub <- obj$protein[which( (obj$protein$tx_id == obj$transcript.coding[x])
                                     & ( obj$protein$protein_domain_source == "pfam")) , ]
    domain.zoom.sub <- obj$protein.zoom$zoom[which(obj$protein.zoom$transcript == obj$transcript.coding[x])][1]
    rect.sub <- data.frame(
      name = domain.sub$protein_domain_id,
      start = domain.sub$prot_dom_start * domain.zoom.sub,
      width = (domain.sub$prot_dom_end - domain.sub$prot_dom_start) * domain.zoom.sub
    )
    if (dim(domain.sub)[1] > 0) {
      domain.sub.svg <- lapply(1:length(rect.sub$start), function(x) {
        rect.svg(x = format(rect.sub$start[x], scientific = FALSE),
                 y =  (flag - obj$color.theme.default$protein.domain.height) / 2,
                 width = format(rect.sub$width[x], scientific = FALSE),
                 height = obj$color.theme.default$protein.domain.height,
                 fill = obj$protein.domain$color[match(rect.sub$name[x], obj$protein.domain$domain)])
      })
      domain.sub.svg <- unlist(domain.sub.svg)
      return(domain.sub.svg)
    } else {
      return(NA)
    }
  })
  names(domain.svg) <- obj$transcript.coding
  obj$domain.svg <- domain.svg

  return(obj)
}
