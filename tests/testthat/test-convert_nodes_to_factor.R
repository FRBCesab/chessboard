# Data for tests ----

sites <- expand.grid("transect" = 1:3, 
                     "quadrat"  = 1:5)

nodes <- create_nodes_labels(data     = sites, 
                             transect = "transect", 
                             quadrat  = "quadrat")

sites_w_gaps <- expand.grid("transect" = c(1, 3, 9), 
                            "quadrat"  = c(1, 3, 5, 6, 10))

nodes_w_gaps <- create_nodes_labels(data     = sites_w_gaps, 
                                    transect = "transect", 
                                    quadrat  = "quadrat")


# Tests for success ----

test_that("convert_nodes_to_factor() - Tests for success", {
  
  expect_silent({ check <- convert_nodes_to_factor(nodes) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), nrow(nodes))
  expect_equal(colnames(check), c("node", "location", "transect", "quadrat"))
  
  expect_equal(max(diff(check$"transect")), 1L)
  expect_equal(max(diff(check$"quadrat")), 1L)
  
  expect_silent({ check <- convert_nodes_to_factor(nodes_w_gaps) })
  
  expect_equal(class(check), "data.frame")
  expect_equal(ncol(check), 4L)
  expect_equal(nrow(check), nrow(nodes_w_gaps))
  expect_equal(colnames(check), c("node", "location", "transect", "quadrat"))
  
  expect_equal(max(diff(check$"transect")), 1L)
  expect_equal(max(diff(check$"quadrat")), 1L)
})
