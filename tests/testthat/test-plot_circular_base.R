test_that("plot_circular_base() returns the correct object class", {
  pcb_expect <- "treedata"
  attr(pcb_expect, "package") <- "tidytree"
  expect_equal(class(plot_circular_base(extract_kinome_df(kinome_data, "hs"))), pcb_expect)
})
