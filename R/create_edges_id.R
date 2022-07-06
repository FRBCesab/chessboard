#' Create labels for edges
#' 
#' Creates labels for edges (non edges are ignored). If two edges are identical
#' (e.g. `"S01-S02"` and `"S02-S01"`), there will have the same label 
#' (`"E-01"`).
#' 
#' @param edges a `data.frame` with three columns:
#'   - `from`: label of one of the two nodes of the edge
#'   - `to`: label of the other node of the edge
#'   - `edge`: 0 (no edge) or 1 (edge)
#' 
#' @note For internal purpose only.
#' 
#' @noRd

create_edges_id <- function(edges) {
  
  if (missing(edges)) {
    stop("Argument 'edges' is required", call. = FALSE)
  }
  
  if (!is.data.frame(edges)) {
    stop("Argument 'edges' must be a data.frame", call. = FALSE)
  }
  
  if (!("from" %in% colnames(edges))) {
    stop("The column 'from' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (!("to" %in% colnames(edges))) {
    stop("The column 'to' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (!("edge" %in% colnames(edges))) {
    stop("The column 'edge' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  
  ## Subset edges and non-edges ----
  
  edges <- as.data.frame(edges, stringsAsFactors = FALSE)
  
  no_edges <- edges[edges$"edge" == 0, ]
  edges    <- edges[edges$"edge" == 1, ]
  
  
  ## Extract nodes ----
  
  nodes <- sort(unique(as.character(c(edges$"from", edges$"to"))))
  nodes <- data.frame("node"      = nodes, 
                      "nodes_int" = seq_len(length(nodes)))
  
  
  ## Add information on nodes in edges list ----
  
  edges <- merge(edges, nodes, by.x = "from", by.y = "node")
  colnames(edges)[ncol(edges)] <- "from_int"
  
  edges <- merge(edges, nodes, by.x = "to", by.y = "node")
  colnames(edges)[ncol(edges)] <- "to_int"
  
  edges <- edges[with(edges, order(from, to)), c(3, 2, 1, 4, 5)]
  
  
  ## Split edges according to direction ----
  
  edges_down <- edges[which(edges$"from_int" <= edges$"to_int"), ]
  edges_up   <- edges[which(edges$"from_int" >  edges$"to_int"), ]
  
  
  ## Sort edges list ----
  
  edges_down <- edges_down[with(edges_down, order(from, to)), ]
  
  
  ## Create edge ids for one direction ----
  
  edges_down$"edge_id" <- seq_len(nrow(edges_down))
  edges_down$"edge_id" <- format(edges_down$"edge_id")
  edges_down$"edge_id" <- paste0("E-", edges_down$"edge_id")
  edges_down$"edge_id" <- gsub("\\s", "0", edges_down$"edge_id")
  
  
  ## Add ID to edges for other direction ----
  
  edges_down$"key" <- paste0(edges_down$"from", edges_down$"to")
  edges_up$"key"   <- paste0(edges_up$"to", edges_up$"from")
  
  edges_up <- merge(edges_up, edges_down[ , c("edge_id", "key")], by = "key", 
                    all = FALSE)
  
  
  ## Merge subset ----
  
  edges <- rbind(edges_down[ , c("from", "to", "edge", "edge_id")],
                 edges_up[ , c("from", "to", "edge", "edge_id")])
  
  if (nrow(no_edges) > 0) {
    
    no_edges$"edge_id" <- ""
  
    edges <- rbind(edges[ , c("from", "to", "edge", "edge_id")],
                   no_edges[ , c("from", "to", "edge", "edge_id")])
  }
  
  
  ## Output ----
  
  edges <- edges[with(edges, order(from, to)), c(4:3, 1:2)]
  
  rownames(edges) <- NULL
  
  edges
}
