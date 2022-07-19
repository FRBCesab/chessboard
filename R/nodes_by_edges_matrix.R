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
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "chessboard")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Retrieve nodes (from nodes vector) ----
#' adour_nodes <- nodes_list(adour_sites$"site")
#' 
#' # Find edges with 1 degree of neighborhood ----
#' adour_edges <- edges_list(adour_nodes, directed = TRUE)
#' 
#' # Create nodes-by-edges matrix ----
#' adour_matrix <- nodes_by_edges_matrix(adour_edges)
#' adour_matrix

nodes_by_edges_matrix <- function(edges) {
  
  ## Check edges argument ----
  
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
  
  if (!("edge_id" %in% colnames(edges))) {
    stop("The column 'edge_id' is absent from the edges data.frame", 
         call. = FALSE)
  }
  
  if (nrow(edges) == 0) {
    stop("Argument 'edges' must have at least one row", call. = FALSE)
  }
  
  
  ## Detect undirected network ----
  
  nodes <- nodes_list(c(edges$"from", edges$"to"))
  nodes <- data.frame("node" = nodes, "id" = seq_len(length(nodes)))
  
  udn <- merge(edges, nodes, by.x = "from", by.y = "node", all = FALSE)
  udn <- merge(udn, nodes, by.x = "to", by.y = "node", all = FALSE)
  
  if (any(udn$"id.x" >= udn$"id.y")) {
    stop("This function is not designed to deal with directed network. ",
         "Please remove symetrical edges.", call. = FALSE)
  }
  
  
  ## Prepare edges ----
  
  edges <- edges[edges$"edge" == 1, ]
  edges <- edges[ , c("edge_id", "from", "to")]
  edges <- rbind(data.frame("edge_id" = "O", 
                            "from"    = "0", 
                            "to"      = edges[1, "from"]), 
                 edges)
  
  
  ## Rename edges ----
  
  edges$"edge_id" <- seq_len(nrow(edges))
  edges$"edge_id" <- format(edges$"edge_id")
  edges$"edge_id" <- paste0("E-", edges$"edge_id")
  edges$"edge_id" <- gsub("\\s", "0", edges$"edge_id")
  
  
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
