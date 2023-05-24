#' Create a nodes-by-edges matrix
#' 
#' @description
#' Creates a nodes-by-edges matrix that will be used by [adespatial::aem()].
#' This function creates the same output as [adespatial::aem.build.binary()]
#' but works in a different way: it's only based on nodes labels (not on 
#' coordinates). Also, this function adds labels to nodes and edges.
#' 
#' @param edges a `data.frame` with the following two columns: `from` (the 
#'   first node of the edge) and `to` (the second node of the edge).
#'
#' @return A list of two elements:
#' - `se.mat`: the nodes-by-edges matrix of dimensions `n x k`, where `n` is 
#'   the number of nodes and `k` the number of edges (including the edge 
#'   between the fictitious origin and the first site);
#' - `edges`: a `data.frame` of edges list.
#' 
#' @export
#'
#' @examples
#' library("chessboard")
#' 
#' # Two-dimensional sampling ----
#' sites_infos <- expand.grid("transect" = 1:3, "quadrat" = 1:5)
#' sites_infos
#' 
#' nodes <- create_nodes_labels(data     = sites_infos, 
#'                              transect = "transect", 
#'                              quadrat  = "quadrat")
#' 
#' edges <- create_edges_list(nodes, method = "pawn", directed = TRUE)
#' 
#' # Create nodes-by-edges matrix ----
#' nodes_by_edges_matrix(edges)

nodes_by_edges_matrix <- function(edges) {
  
  ## Check edges argument ----
  
  check_edges_object(edges)
  
  
  ## Extract transect and quadrat labels ----
  
  tr_labels <- get_edges_transects_labels(edges)
  qu_labels <- get_edges_quadrats_labels(edges)
  qu_labels <- qu_labels[ , -c(1:2)]
  
  edges <- data.frame(tr_labels, qu_labels)
  
  
  ## Main direction ----
  
  edges_main  <- edges[which(!(edges$"quadrats_from" == edges$"quadrats_to")), ]
  
  if (nrow(edges_main) == 0) {
    stop("The network does not seem to have a main direction (no edges along ", 
         "transects have been detected)", 
         call. = FALSE)
  }
  
  
  origins_main <- edges_main[which(!(edges_main$"from" %in% edges_main$"to")), 
                             "from", drop = TRUE]
  
  if (length(origins_main) < 1) {
    stop("This function is not designed to deal with undirected network", 
         call. = FALSE)
  }
  
  origins_main <- create_origin_edges(origins_main)
  edges_main   <- rbind(origins_main, edges_main[ , 1:2])
  
  
  ## Orthogonal directions ----
  
  edges_ortho <- edges[which((edges$"quadrats_from" == edges$"quadrats_to")), ]
  
  if (nrow(edges_ortho) > 0) {
    
    ## From left to right ----
    
    edges_ortho_r <- edges_ortho[which((edges_ortho$"transects_from" < 
                                          edges_ortho$"transects_to")), ]
    
    origins_ortho_r <- edges_ortho_r[which(!(edges_ortho_r$"from" %in% 
                                               edges_ortho_r$"to")), 
                                     "from", drop = TRUE]
    
    origins_ortho_r <- create_origin_edges(origins_ortho_r)
    edges_ortho_r   <- rbind(origins_ortho_r, edges_ortho_r[ , 1:2])
    
    
    ## From right to left ----
    
    edges_ortho_l <- edges_ortho[which((edges_ortho$"transects_from" > 
                                          edges_ortho$"transects_to")), ]
    
    origins_ortho_l <- edges_ortho_l[which(!(edges_ortho_l$"from" %in% 
                                               edges_ortho_l$"to")), 
                                     "from", drop = TRUE]
    
    origins_ortho_l <- create_origin_edges(origins_ortho_l)
    edges_ortho_l   <- rbind(origins_ortho_l, edges_ortho_l[ , 1:2])
    
  } else {
    
    edges_ortho_r <- data.frame("from" = character(0), "to" = character(0))
    edges_ortho_l <- data.frame("from" = character(0), "to" = character(0))
  }
  
  
  nodes_edges <- rbind(
    create_nodes_by_edges_list(edges_main, direction = "main"),
    create_nodes_by_edges_list(edges_ortho_r, direction = "ortho_r"),
    create_nodes_by_edges_list(edges_ortho_l, direction = "ortho_l"))
  
  nodes_edges <- create_nodes_by_edges_labels(nodes_edges)
  
  
  ## Create nodes by edges matrix ----
  
  mat <- tidyr::pivot_wider(nodes_edges, names_from = "edge_id", 
                            values_from = "link", values_fn = ~.x)
  
  
  ## Convert to matrix ----
  
  row_names <- mat[ , 1, drop = TRUE]
  mat <- data.matrix(mat[ , -1])
  rownames(mat) <- row_names
  
  
  ## Replace NA ----
  
  mat <- ifelse(is.na(mat), 0, mat) 
  
  
  ## Prepare final edges list ----
  
  row_names <- nodes_edges[!duplicated(nodes_edges$"edge_id"), "edge_id"]
  edges <- rbind(edges_main, edges_ortho_r, edges_ortho_l)
  rownames(edges) <- row_names
  
  list("se.mat" = mat, "edges" = edges)
}
