test_that("multiplication works", {
  pcb_expect <- "treedata"
  attr(pcb_expect, "package") <- "tidytree"
  expect_equal(class(plot_circular_base(extract_kinome_df(kinome_data, "hs"))), pcb_expect)
})
