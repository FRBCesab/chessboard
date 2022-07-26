% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_neighbors.R
\name{geom_neighbors}
\alias{geom_neighbors}
\title{Highlight neighbors on a chessboard}
\usage{
geom_neighbors(neighbors)
}
\arguments{
\item{neighbors}{a \code{data.frame} with the following at least three columns:
\code{node}, \code{transect}, and \code{quadrats}. See \code{\link[=pawn]{pawn()}}, \code{\link[=fool]{fool()}}, etc. for
further information.}
}
\value{
A \code{geom_point} that must be added to a \code{ggplot2} object.
}
\description{
Highlights neighbors (cells) on a chessboard plotted with \code{\link[=gg_chessboard]{gg_chessboard()}}.
}
\examples{
library("chessboard")

# Two-dimensional sampling ----
sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)

nodes <- create_nodes_labels(data     = sites_infos, 
                             transect = "transect", 
                             quadrat  = "quadrat")

neighbors <- pawn(nodes, focus = "2-3")

gg_chessboard(nodes) +
  geom_piece(nodes, "2-3") +
  geom_neighbors(neighbors)
}
