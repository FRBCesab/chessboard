# Data for tests ----

nodes <- c("S01", "S02", "S11", "S21")
edges <- edges_list(nodes)

edges_edgeid <- edges[ , -1, drop = FALSE]
edges_edge   <- edges[ , -2, drop = FALSE]
edges_from   <- edges[ , -3, drop = FALSE]
edges_to     <- edges[ , -4, drop = FALSE]
edges_empty  <- edges[0, ]

sites <- data.frame("site" = nodes, 
                    "x"    = 1:4,
                    "y"    = 101:104)

sites_sf <- sf::st_as_sf(sites, coords = 2:3)
sites_sf_mltpt <- sf::st_cast(sites_sf, "MULTIPOINT")
sites_sf_lnstr <- sf::st_cast(sites_sf, "LINESTRING")
sites_sf_lnstr <- rbind(sites_sf, sites_sf_lnstr)

sites_sp <- sites
sp::coordinates(sites_sp) <- 2:3


# Test for errors ----

test_that("edges_to_sf() - Tests for wrong inputs", {
  
  ## Test edges argument ----
  
  expect_error(edges_to_sf(), 
               "Argument 'edges' is required", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(nodes), 
               "Argument 'edges' must be a data.frame", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges_edgeid), 
               "The column 'edge_id' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges_edge), 
               "The column 'edge' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges_from), 
               "The column 'from' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges_to), 
               "The column 'to' is absent from the edges data.frame", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges_empty), 
               "Argument 'edges' must have at least one row", 
               fixed = TRUE)
  
  
  ## Test sites argument ----
  
  expect_error(edges_to_sf(edges), 
               "Argument 'sites' (spatial layer of sites) is required", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges, sites), 
               "The object 'sites' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges, sites_sp), 
               "The object 'sites' must be an 'sf' object", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges, sites_sf[1, ]), 
               "Argument 'sites' should have at least two rows (sites)", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges, sites_sf[ , -1]), 
               paste0("Argument 'sites' should have at least two columns: ", 
                      "site label and geometry"), 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges, sites_sf_lnstr), 
               paste0("Argument 'sites' (spatial layer of sites) cannot ", 
                      "contain different geometries"), 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges, sites_sf_mltpt), 
               "Sites geometry must be of type POINT", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges, rbind(sites_sf, sites_sf)), 
               "The argument 'sites' cannot contain duplicates", 
               fixed = TRUE)
  
  expect_error(edges_to_sf(edges, sites_sf[-1, ]), 
               paste0("Some nodes (sites) are missing from the 'sites' object ", 
                      "(no spatial coordinates)"), 
               fixed = TRUE)
})


# Test for success ----

test_that("edges_to_sf() - Tests for good outputs", {
  
  expect_silent({
    edges_sf <- edges_to_sf(edges, sites_sf)
  })
  
  expect_equal(class(edges_sf), c("sf", "data.frame"))
  expect_equal(sum(sf::st_is(edges_sf, "LINESTRING")), nrow(edges_sf))
  expect_equal(ncol(edges_sf), 4L)
  expect_equal(nrow(edges_sf), 6L)
  expect_equal(edges_sf$"edge_id"[1], "E-1")
  expect_equal(edges_sf$"from"[1], "S01")
  expect_equal(edges_sf$"to"[1], "S02")
})