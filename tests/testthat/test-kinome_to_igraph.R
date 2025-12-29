test_that("function returns an object of class 'igraph'", {
  expect_equal(class(kinome_to_igraph(kinome_data, "hs")), "igraph")
})
