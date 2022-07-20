#' Create a nodes-by-edges matrix
#' 
#' @description
#' Creates a nodes-by-edges matrix that will be used by [adespatial::aem()].
#' This function creates the same output as [adespatial::aem.build.binary()]
#' but works in a different way: it's only based on nodes labels (not on 
#' coordinates). Also, this function adds labels to nodes and edges.
#' 
#' @param edges a `data.frame` with the following three columns: `from` (the 
#'   first node of the edge), `to` (the second node of the edge), and `edge` 
#'   (presence/absence of the edge).
#'
#' @return A list of two elements:
#' - `se.mat`: the nodes-by-edges matrix of dimensions `n x k`, where `n` is 
#'   the number of nodes and `k` the number of edges (including the edge 
#'   between the fictitious origin and the first site);
#' - `edges`: a `data.frame` of edges list.
#' 
#' @export
#' 
#' @importFrom rlang .data
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
  
  
  ## Detect undirected network ----
  
  # nodes <- get_sorted_nodes(edges)
  # nodes <- data.frame("node" = nodes, "id" = seq_len(length(nodes)))
  
  # udn <- merge(edges, nodes, by.x = "from", by.y = "node", all = FALSE)
  # udn <- merge(udn, nodes, by.x = "to", by.y = "node", all = FALSE)
  
  # if (any(udn$"id.x" >= udn$"id.y")) {
  #   stop("This function is not designed to deal with directed network. ",
  #        "Please remove symetrical edges.", call. = FALSE)
  # }
  
  
  ## Detect origins ----
  
  origins <- which(!(edges$"from" %in% edges$"to"))
  
  if (length(origins) < 1) {
    stop("Unable to find origin nodes", call. = FALSE)
  }
  
  origin_edges <- data.frame()
  
  for (origin in rev(origins)) {
    origin_edges <- rbind(data.frame("from" = "0", 
                                     "to"   = edges[origin, "from"]), 
                          origin_edges) 
  }
  
  edges <- rbind(origin_edges, edges)
  
  
  ## Rename edges ----
  
  edges$"edge_id" <- seq_len(nrow(edges))
  edges$"edge_id" <- format(edges$"edge_id")
  edges$"edge_id" <- paste0("E-", edges$"edge_id")
  edges$"edge_id" <- gsub("\\s", "0", edges$"edge_id")
  
  edges <- edges[ , c("edge_id", "from", "to")]
  
  
  ## Core code ----
  
  nodes_edges <- data.frame()
  
  for (i in 1:nrow(edges)) {
    
    edge <- edges[i, "to"]
    to_search <- edge
    go_on <- TRUE
    
    while (go_on) {
      
      pos <- which(edges$"from" %in% to_search)
      
      if (length(pos) > 0) {
        
        to_search <- edges[pos, "to"]
        edge <- c(edge, to_search)
        
      } else {
        
        go_on <- FALSE
      }
    }
    
    nodes_edge <- data.frame("edge" = edges[i, "edge_id"], 
                             "node" = sort(unique(edge)), 
                             "link" = 1)
    nodes_edges <- rbind(nodes_edges, nodes_edge)
  }
  
  
  ## Create nodes by edges matrix ----
  
  mat <- tidyr::pivot_wider(nodes_edges, names_from = .data$edge, 
                            values_from = .data$link, values_fn = ~.x)
  
  
  ## Convert to matrix ----
  
  row_names <- mat[ , 1, drop = TRUE]
  mat <- data.matrix(mat[ , -1])
  rownames(mat) <- row_names
  
  
  ## Replace NA ----
  
  mat <- ifelse(is.na(mat), 0, mat) 
  
  
  ## Prepare final edges list ----
  
  row_names <- edges[ , 1, drop = TRUE]
  edges <- edges[ , -1]
  rownames(edges) <- row_names
  
  list("se.mat" = mat, "edges" = edges)
}
