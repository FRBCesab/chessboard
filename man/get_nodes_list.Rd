% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nodes_list.R
\name{get_nodes_list}
\alias{get_nodes_list}
\title{Get the list of nodes}
\usage{
get_nodes_list(nodes)
}
\arguments{
\item{nodes}{a \code{data.frame} with (at least) the following three columns:
\code{node}, \code{transect}, and \code{quadrats}. Must be the output of the function
\code{\link[=create_nodes_labels]{create_nodes_labels()}}.}
}
\value{
A vector of nodes labels.
}
\description{
Retrieves the nodes list by selecting and ordering the column \code{node} of the
output of the function \code{\link[=create_nodes_labels]{create_nodes_labels()}}.
}
\examples{
library("chessboard")

# Two-dimensional sampling (only) ----
sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)

nodes <- create_nodes_labels(data     = sites_infos, 
                             transect = "transect", 
                             quadrat  = "quadrat")
get_nodes_list(nodes)
}
