#' Convert edges list to spatial object
#' 
#' @description
#' Convert an edges list to an `sf` spatial object of type `LINESTRING` with 
#' one row per edge.
#' 
#' @inheritParams connectivity_matrix
#' 
#' @param sites an `sf` object of type `POINT`. A spatial object with 
#'   coordinates of sites (nodes). Note that the **first column** must be the 
#'   nodes labels.
#'
#' @return An `sf` spatial object of type `LINESTRING` where the number of rows
#'   correspond to the number of edges.
#' 
#' @export
#'
#' @examples
#' # Import Adour sites ----
#' path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
#'                             package = "bridge")
#' adour_sites  <- read.csv(path_to_file)
#' 
#' # Select first location ----
#' adour_sites <- adour_sites[adour_sites$"location" == 1, ]
#' 
#' # Create nodes labels ----
#' adour_nodes <- create_nodes_labels(data     = adour_sites, 
#'                                    location = "location", 
#'                                    transect = "transect", 
#'                                    quadrat = "quadrat")
#' 
#' # Find edges with 1 degree of neighborhood (pawn method) ----
#' adour_edges <- create_edges_list(adour_nodes, method = "pawn", 
#'                                  directed = TRUE)
#' 
#' # Convert sites to spatial POINT ----
#' adour_sites_sf <- sf::st_as_sf(adour_nodes, coords = 5:6, crs = "epsg:2154")
#' 
#' # Convert edges to spatial LINESTRING ----
#' edges_sf <- edges_to_sf(adour_edges, adour_sites_sf)
#' head(edges_sf)
#' 
#' # Visualization ----
#' plot(sf::st_geometry(adour_sites_sf), pch = 19)
#' plot(sf::st_geometry(edges_sf), add = TRUE)
#' 
#' 
#' # Find edges with 1 degree of neighborhood (pawn and bishop methods) ----
#' adour_edges_1 <- create_edges_list(adour_nodes, method = "pawn", 
#'                                    directed = TRUE)
#' adour_edges_2 <- create_edges_list(adour_nodes, method = "bishop", 
#'                                    directed = TRUE)
#' 
#' # Append edges ----
#' adour_edges <- append_edges_lists(adour_edges_1, adour_edges_2)
#' 
#' # Convert sites to spatial POINT ----
#' adour_sites_sf <- sf::st_as_sf(adour_nodes, coords = 5:6, crs = "epsg:2154")
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
  
  check_edges_object(edges)
  
  
  ## Check sites argument ----
  
  check_sites_object(sites)
  
  
  ## Check for missing sites ----
  
  nodes <- get_sorted_nodes(edges)
  
  if (any(!(nodes %in% sites[ , 1, drop = TRUE]))) {
    stop("Some nodes (sites) are missing from the 'sites' object ", 
         "(no spatial coordinates)", call. = FALSE)
  }
  
  
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
      dplyr::mutate(from    = edges[i, "from"],
                    to      = edges[i, "to"])
  }))
  
  sf::st_geometry(edges_sf) <- "geometry"
  
  edges_sf[ , c(2:3, 1)]
}
