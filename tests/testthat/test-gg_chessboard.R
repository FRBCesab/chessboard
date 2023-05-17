# Data for tests ----

sites <- expand.grid("transect" = 1:3, 
                     "quadrat"  = 1:5)

nodes <- create_nodes_labels(data     = sites, 
                             transect = "transect", 
                             quadrat  = "quadrat")


# Tests for success ----

test_that("gg_chessboard() - Tests for success", {
  
  expect_silent({ check <- gg_chessboard(nodes) })
  
  expect_true("ggplot" %in% class(check))
  expect_true("gg" %in% class(check))
})
