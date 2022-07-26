% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nodes_by_edges_matrix.R
\name{nodes_by_edges_matrix}
\alias{nodes_by_edges_matrix}
\title{Create a nodes-by-edges matrix}
\usage{
nodes_by_edges_matrix(edges)
}
\arguments{
\item{edges}{a \code{data.frame} with the following three columns: \code{from} (the
first node of the edge), \code{to} (the second node of the edge), and \code{edge}
(presence/absence of the edge).}
}
\value{
A list of two elements:
\itemize{
\item \code{se.mat}: the nodes-by-edges matrix of dimensions \verb{n x k}, where \code{n} is
the number of nodes and \code{k} the number of edges (including the edge
between the fictitious origin and the first site);
\item \code{edges}: a \code{data.frame} of edges list.
}
}
\description{
Creates a nodes-by-edges matrix that will be used by \code{\link[adespatial:aem]{adespatial::aem()}}.
This function creates the same output as \code{\link[adespatial:aem.build.binary]{adespatial::aem.build.binary()}}
but works in a different way: it's only based on nodes labels (not on
coordinates). Also, this function adds labels to nodes and edges.
}
\examples{
library("chessboard")

# Two-dimensional sampling ----
sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
sites_infos

nodes <- create_nodes_labels(data     = sites_infos, 
                             transect = "transect", 
                             quadrat  = "quadrat")

edges <- create_edges_list(nodes, method = "pawn", directed = TRUE)

# Create nodes-by-edges matrix ----
nodes_by_edges_matrix(edges)
}
