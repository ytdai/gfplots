% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gfplots.R
\name{gfplots}
\alias{gfplots}
\title{main function of gfplots packages}
\usage{
gfplots(obj, type = c("mutation", "fusion"), edb, output.dir = "./gfplots",
  verbose = FALSE, reverse = T, coding.only = T)
}
\arguments{
\item{obj}{an list object. Contains all plot parameter}

\item{type}{a character. Plot type, "mutation" or "fusion"}

\item{edb}{an S4 object. EnsDb.Hsapiens.v75 or EnsDb.Hsapiens.v86}

\item{verbose}{logical.}

\item{reverse}{logical. If reverse is TRUE, genes in the negative
chain will be reversed}
}
\description{
\code{gfplots} is an useful package to visualize gene fusions and
mutations. The defaluse annotation database is ENSEMBL
}
\details{
gfplots packages
}
\examples{

}
\author{
Yuting Dai
}
