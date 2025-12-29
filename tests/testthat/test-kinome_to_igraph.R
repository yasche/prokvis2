test_that("function returns an object of class 'igraph'", {
  expect_equal(class(kinome_df_to_igraph(extract_kinome_df(kinome_data, "hs"))), "igraph")
})
