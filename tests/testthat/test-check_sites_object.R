# Data for tests ----

path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")
adour_sites  <- read.csv(path_to_file)
adour_sites <- adour_sites[adour_sites$"location" == 1, ]
adour_nodes <- create_node_labels(data     = adour_sites, 
                                   location = "location", 
                                   transect = "transect", 
                                   quadrat  = "quadrat")
adour_edges <- create_edge_list(adour_nodes, method = "pawn", 
                                 directed = TRUE)
adour_sites_sf <- sf::st_as_sf(adour_nodes, coords = 5:6, crs = "epsg:2154")
edges_sf <- edges_to_sf(adour_edges, adour_sites_sf)

sites_sf_mltpt <- sf::st_cast(adour_sites_sf, "MULTIPOINT")
sites_sf_lnstr <- sf::st_cast(adour_sites_sf, "LINESTRING")
sites_sf_lnstr <- rbind(adour_sites_sf, sites_sf_lnstr)

adour_sites_sf_dup <- rbind(adour_sites_sf, adour_sites_sf[1, ])


test_that("check_sites_object() - Tests for wrong inputs", {
  
  expect_error(check_sites_object(),
               paste0("Argument 'sites' (spatial layer of sites) is required"),
               fixed = TRUE)
  
  expect_error(check_sites_object(adour_sites),
               paste0("The object 'sites' must be an 'sf' object"),
               fixed = TRUE)
  
  expect_error(check_sites_object(adour_sites_sf[1, ]),
               paste0("Argument 'sites' should have at least two rows (sites)"),
               fixed = TRUE)
  
  expect_error(check_sites_object(adour_sites_sf[ , 0]),
               paste0("Argument 'sites' should have at least two columns: ", 
                      "'node' and 'geometry'"),
               fixed = TRUE)

  expect_error(check_sites_object(sites_sf_mltpt),
               paste0("Sites geometry must be of type POINT"),
               fixed = TRUE)
  
  expect_error(check_sites_object(sites_sf_lnstr),
               paste0("Argument 'sites' (spatial layer of sites) cannot ", 
                      "contain different geometries"),
               fixed = TRUE)
  
  expect_error(check_sites_object(adour_sites_sf_dup),
               paste0("The argument 'sites' cannot contain duplicates"),
               fixed = TRUE)
  
  expect_error(check_sites_object(adour_sites_sf[ , -1]),
               paste0("The first column of 'sites' must be named 'node' ", 
                      "(node labels)"),
               fixed = TRUE)
})


test_that("check_sites_object() - Tests for success", {
  
  expect_silent({ check <- check_sites_object(adour_sites_sf) })
  
  expect_null(check)
})
