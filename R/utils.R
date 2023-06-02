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
  
  
  ## Add information on nodes in edge list ----
  
  edges <- merge(edges, nodes, by.x = "from", by.y = "node")
  colnames(edges)[ncol(edges)] <- "from_int"
  
  edges <- merge(edges, nodes, by.x = "to", by.y = "node")
  colnames(edges)[ncol(edges)] <- "to_int"
  
  edges <- edges[with(edges, order(from, to)), c(3, 2, 1, 4, 5)]
  
  
  ## Split edges according to direction ----
  
  edges_down <- edges[which(edges$"from_int" <= edges$"to_int"), ]
  edges_up   <- edges[which(edges$"from_int" >  edges$"to_int"), ]
  
  
  ## Sort edge list ----
  
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



#' Sort edges in natural ordering
#'
#' @inheritParams edges_to_sf
#' 
#' @noRd

sort_edges <- function(edges) {
  
  ## Check argument ----
  
  check_edges_object(edges)
  
  
  ## Get transects and quadrats labels ----
  
  edges <- get_edges_transects_labels(edges)
  edges <- get_edges_quadrats_labels(edges)
  
  edges <- edges[with(edges, order(transects_from, quadrats_from, 
                                   transects_to, quadrats_to)), c("from", "to")]
  rownames(edges) <- NULL
  
  edges
}



#' Sort node labels in natural ordering
#'
#' @inheritParams edges_to_sf
#' 
#' @noRd

get_sorted_nodes <- function(edges) {
  
  ## Check argument ----
  
  check_edges_object(edges)
  
  
  ## Split labels ----
  
  nodes <- unique(c(edges$"from", edges$"to"))
  
  nodes <- data.frame(
    "node" = nodes,
    "by_1" = as.numeric(unlist(lapply(strsplit(nodes, "-"), 
                                      function(x) x[1]))),
    "by_2" = as.numeric(unlist(lapply(strsplit(nodes, "-"), 
                                           function(x) x[2]))))
  
  nodes[with(nodes, order(by_1, by_2)), "node", drop = TRUE]
}



#' Extract transects labels from an edge list
#'
#' @inheritParams edges_to_sf
#' 
#' @noRd

get_edges_transects_labels <- function(edges) {
  
  check_edges_object(edges)
  
  transects_from <- as.numeric(unlist(lapply(strsplit(edges$"from", "-"), 
                                            function(x) x[1])))
  
  transects_to   <- as.numeric(unlist(lapply(strsplit(edges$"to", "-"), 
                                            function(x) x[1])))
  
  data.frame(edges, transects_from, transects_to)
}



#' Extract quadrats labels from an edge list
#'
#' @inheritParams edges_to_sf
#' 
#' @noRd

get_edges_quadrats_labels <- function(edges) {
  
  check_edges_object(edges)
  
  quadrats_from <- as.numeric(unlist(lapply(strsplit(edges$"from", "-"), 
                                            function(x) x[2])))
  
  quadrats_to   <- as.numeric(unlist(lapply(strsplit(edges$"to", "-"), 
                                            function(x) x[2])))
  
  data.frame(edges, quadrats_from, quadrats_to)
}



#' Create origins edges
#'
#' @param origins a `character` vector. Nodes directly connected to fictitious 
#'   origin nodes.
#' 
#' @noRd

create_origin_edges <- function(origins) {
  
  if (length(origins) == 0) {
    
    return(data.frame("from" = character(0), 
                      "to"   = character(0)))
  }
  
  origins <- rev(sort(unique(origins)))
  
  origins_edges <- data.frame()
  
  for (origin in origins) {
    origins_edges <- rbind(data.frame("from" = "0", "to" = origin), 
                           origins_edges) 
  }
  
  origins_edges
}



#' Create edge list (for nodes by edges matrix)
#' 
#' @param direction a `character` of length 1. One of `'main'`, `'ortho_r'`, 
#'   and `'ortho_l'`.
#' 
#' @inheritParams edges_to_sf
#' 
#' @noRd

create_nodes_by_edges_list <- function(edges, direction) {
  
  if (nrow(edges) == 0) {
    
    return(data.frame("direction" = character(0),
                      "edge"      = integer(0), 
                      "node"      = character(0), 
                      "link"      = numeric(0)))
  }
  
  nodes_edges <- data.frame()
  
  for (i in seq_len(nrow(edges))) {
    
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
    
    nodes_edge <- data.frame("direction" = direction,
                             "edge"      = i, 
                             "node"      = sort(unique(edge)), 
                             "link"      = 1)
    nodes_edges <- rbind(nodes_edges, nodes_edge)
  }
  
  nodes_edges
}



#' Rename edges names (multi-directionality)
#' 
#' @inheritParams edges_to_sf
#' 
#' @noRd

create_nodes_by_edges_labels <- function(edges) {
  
  edges$"edge"  <- format(edges$"edge")
  edges$"edge"  <- gsub("\\s", "0", edges$"edge")
   
  edges$"label" <- paste(edges$"direction", edges$"edge", sep = "__")
  
  edges_id <- sort(unique(edges$"label"))
  
  edges_table <- data.frame("label"   = edges_id, 
                            "edge_id" = seq_len(length(edges_id)))
  
  edges_table$"edge_id" <- format(edges_table$"edge_id")
  edges_table$"edge_id" <- gsub("\\s", "0", edges_table$"edge_id")
  
  edges_table$"edge_id" <- paste0("E-", edges_table$"edge_id")
  
  edges <- merge(edges, edges_table, by = "label", all = TRUE)
  
  edges[ , c("edge_id", "node", "link")]
}



#' Convert numeric to factor and then to numeric
#' 
#' @inheritParams check_nodes_object
#' 
#' @noRd

convert_nodes_to_factor <- function(nodes) {
  
  check_nodes_object(nodes)
  
  n_transects <- length(unique(nodes$"transect"))
  n_quadrats  <- length(unique(nodes$"quadrat"))
  
  nodes$"transect" <- factor(nodes$"transect", labels = 1:n_transects)
  nodes$"transect" <- as.numeric(as.character(nodes$"transect"))
  
  nodes$"quadrat"  <- factor(nodes$"quadrat", labels = 1:n_quadrats)
  nodes$"quadrat"  <- as.numeric(as.character(nodes$"quadrat"))
  
  nodes
}
