# Data for tests ----



# Test for errors ----

test_that("create_nodes_labels() - Tests for wrong inputs", {
  
  expect_error(create_nodes_labels())
})
