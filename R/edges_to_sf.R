#' Convert edges to LINESTRING spatial object
#' 
#' @description
#' Convert edges to an `sf` spatial object of type `LINESTRING` with one row
#' per edge.
#' 
#' @param edges a `data.frame` with four columns:
#'   - `edge_id`: label of the edge
#'   - `edge`: 0 (no edge) or 1 (edge)
#'   - `from`: label of one of the two nodes of the edge
#'   - `to`: label of the other node of the edge
#' 
#' @param sites an `sf` object of type `POINT`. A spatial object with 
#'   coordinates of sites. Note that the first column must be the sites labels.
#'
#' @return An `sf` spatial object of type `LINESTRING` where the number of rows
#'   correspond to the number of edges.
#' 
#' @export
#'
#' @examples
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_sites_coords.csv", 
#'                             package = "bridge")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Retrieve nodes (from nodes vector) ----
#' adour_nodes <- nodes_list(adour_sites$"site")
#' 
#' # Find edges with 1 degree of neighborhood ----
#' adour_edges <- edges_list(adour_nodes)
#' 
#' # Convert sites to spatial POINT ----
#' adour_sites_sf <- sf::st_as_sf(adour_sites, coords = 2:3, crs = "epsg:2154")
#' 
#' # Convert edges to spatial LINESTRING ----
#' edges_sf <- edges_to_sf(adour_edges, adour_sites_sf)
#' head(edges_sf)
#' 
#' # Visualization ----
#' plot(sf::st_geometry(adour_sites_sf), pch = 19)
#' plot(sf::st_geometry(edges_sf), add = TRUE)
#' 
#' # Find edges with 2 degrees of neighborhood ----
#' adour_edges <- edges_list(adour_nodes, degree = 2)
#' 
#' # Convert sites to spatial POINT ----
#' adour_sites_sf <- sf::st_as_sf(adour_sites, coords = 2:3, crs = "epsg:2154")
#' 
#' # Convert edges to spatial LINESTRING ----
#' edges_sf <- edges_to_sf(adour_edges, adour_sites_sf)
#' head(edges_sf)
#' 
#' # Visualization ----
#' plot(sf::st_geometry(adour_sites_sf), pch = 19)
#' plot(sf::st_geometry(edges_sf), add = TRUE)

edges_to_sf <- function(edges, sites) {
  
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
  
  
  ## Check sites argument ----
  
  if (missing(sites)) {
    stop("Argument 'sites' (spatial layer of sites) is required", 
         call. = FALSE)
  }
  
  if (!inherits(sites, "sf")) {
    stop("The object 'sites' must be an 'sf' object", 
         call. = FALSE)
  }
  
  if (nrow(sites) < 2) {
    stop("Argument 'sites' should have at least two rows (sites)", 
         call. = FALSE)
  }
  
  if (ncol(sites) < 2) {
    stop("Argument 'sites' should have at least two columns: site label and ", 
         "geometry", call. = FALSE)
  }
  
  geom <- sf::st_geometry_type(sites) %>% as.character() %>% unique()
  
  if (length(geom) > 1) {
    stop("Argument 'sites' (spatial layer of sites) cannot contain different ", 
         "geometries", call. = FALSE)
  }
  
  if (!("POINT" %in% geom)) {
    stop("Sites geometry must be of type POINT", call. = FALSE)
  }
  
  if (any(duplicated(sites[ , 1, drop = TRUE]))) {
    stop("The argument 'sites' cannot contain duplicates", call. = FALSE)
  }
  
  
  ## Check for missing sites ----
  
  nodes <- nodes_list(edges)
  
  if (any(!(nodes %in% sites[ , 1, drop = TRUE]))) {
    stop("Some nodes (sites) are missing from the 'sites' object ", 
         "(no spatial coordinates)", call. = FALSE)
  }
  
  
  ## Extract non missing edges ----
  
  edges <- edges[edges$"edge" == 1, ]
  
  
  ## Extract sites coordinates ----
  
  coords <- sf::st_coordinates(sites)
  
  
  ## Create LINESTRING ----
  
  edges_sf <- do.call(rbind.data.frame, lapply(seq_len(nrow(edges)), 
                                               function(i) {
    
    node_1 <- coords[which(sites[ , 1, drop = TRUE] == edges[i, "from"]), ]
    node_2 <- coords[which(sites[ , 1, drop = TRUE] == edges[i, "to"]), ]
    
    
    edges_sf <- sf::st_linestring(matrix(c(node_1, node_2), ncol = 2, 
                                         byrow = TRUE)) %>% 
      list() %>% 
      sf::st_as_sfc() %>% 
      sf::st_as_sf(crs = sf::st_crs(sites)) %>% 
      dplyr::mutate(edge_id = edges[i, "edge_id"],
                    from    = edges[i, "from"],
                    to      = edges[i, "to"])
  }))
  
  sf::st_geometry(edges_sf) <- "geometry"
  
  edges_sf[ , c(2:4, 1)]
}
