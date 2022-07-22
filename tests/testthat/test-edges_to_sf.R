# Data for tests ----

path_to_file <- system.file("extdata", "adour_survey_sampling.csv", 
                            package = "chessboard")
adour_sites  <- read.csv(path_to_file)
adour_sites <- adour_sites[adour_sites$"location" == 1, ]
adour_nodes <- create_nodes_labels(data     = adour_sites, 
                                   location = "location", 
                                   transect = "transect", 
                                   quadrat  = "quadrat")
adour_edges <- create_edges_list(adour_nodes, method = "pawn", 
                                 directed = TRUE)
adour_sites_sf <- sf::st_as_sf(adour_nodes, coords = 5:6, crs = "epsg:2154")


test_that("edges_to_sf() - Tests for errors", {
  
  expect_error(edges_to_sf(),
               paste0("Argument 'edges' is required (output of the function ", 
                      "create_edges_list())"),
               fixed = TRUE)
  
  expect_error(edges_to_sf(adour_edges),
               paste0("Argument 'sites' (spatial layer of sites) is required"),
               fixed = TRUE)
  
  expect_error(edges_to_sf(adour_edges, adour_sites_sf[-2, ]),
               paste0("Some nodes (sites) are missing from the 'sites' object ",
                      "(no spatial coordinates)"),
               fixed = TRUE)
})

test_that("edges_to_sf() - Tests for success", {
  
  expect_silent({ linestrings <- edges_to_sf(adour_edges, adour_sites_sf) })
  
  expect_equal(class(linestrings), c("sf", "data.frame"))
  expect_equal(nrow(linestrings), nrow(adour_edges))
})
