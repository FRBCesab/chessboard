# Data for tests ----

sites <- expand.grid("transect" = 1:3, 
                     "quadrat"  = 1:5)

nodes <- create_node_labels(data     = sites, 
                             transect = "transect", 
                             quadrat  = "quadrat")


# Tests for success ----

test_that("get_node_list() - Tests for success", {
  
  expect_silent({ check <- get_node_list(nodes) })
  
  expect_equal(class(check), "character")
  expect_equal(length(check), nrow(nodes))
  
  expect_equal(check[1], "1-1")
  expect_equal(check[2], "1-2")
  expect_equal(check[3], "1-3")
  
  expect_equal(check[13], "3-3")
  expect_equal(check[14], "3-4")
  expect_equal(check[15], "3-5")
})
