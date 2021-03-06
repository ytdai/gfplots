% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readMutation.R
\name{readMutation}
\alias{readMutation}
\title{function to read mutation from different file}
\usage{
readMutation(data, obj, update.data = FALSE, verbose = TRUE)
}
\arguments{
\item{data}{a data.frame or character. If data.frame, this must contain all
basic information of mutation file. And if character, this must contain the
file path of mutation file.}

\item{obj}{a list object.}

\item{update.data}{logical. If TRUE, the mutation.data in object will be
updated. And if FALSE, it will generate a new list object which contains
the data.}

\item{verbose}{logical}
}
\value{
An object
}
\description{
\code{readMutation} is used to read mutation table from
data frame or tab-delimited files.
}
\details{
readMutation
}
\examples{

}
\author{
Yuting Dai
}
