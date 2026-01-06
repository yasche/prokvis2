test_that(".Random.seed stays unchanged", {
  seed_before <- .Random.seed
  plot_network_base(extract_kinome_df(kinome_data, "hs"), 1)
  seed_after <- .Random.seed

  expect_equal(seed_after, seed_before)
})


test_that("produces same result if set_seed is the same", {
  p1 <- plot_network_base(extract_kinome_df(kinome_data, "hs"), 1)
  p3 <- plot_network_base(extract_kinome_df(kinome_data, "hs"), 2)
  p2 <- plot_network_base(extract_kinome_df(kinome_data, "hs"), 1)
  p4 <- plot_network_base(extract_kinome_df(kinome_data, "hs"), 2)

  expect_equal(p1, p2)
  expect_equal(p3, p4)
})

test_that("produces different result if set_seed is not the same", {
  p1 <- plot_network_base(extract_kinome_df(kinome_data, "hs"), 1)
  p2 <- plot_network_base(extract_kinome_df(kinome_data, "hs"), 2)

  expect_all_true(!all.equal(p1, p2) == TRUE)
  # control
  expect_all_true(all.equal(p1, p1) == TRUE)
})

