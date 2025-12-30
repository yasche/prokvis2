test_that("correct number of posx & posy for species = 'hs'", {
  kinase_groups <- extract_kinome_df(kinome_data, species_selection = "hs") %>%
    extract_kinase_groups()

  expected_length <- length(kinase_groups) * 2

  expect_equal(length(kinase_groups_to_custom_xy(kinase_groups)) , expected_length)
})

test_that("Vectors are interlaced", {
  kinase_groups <- extract_kinome_df(kinome_data, species_selection = "hs") %>%
    extract_kinase_groups()

  expected_nums <- as.character(rep(1:length(kinase_groups), each = 2))
  actual_nums <- stringr::str_remove_all(kinase_groups_to_custom_xy(kinase_groups), "custom_group_pos_[a-z]")

  expected_xy <- rep(c("x", "y"), length(kinase_groups))
  actual_xy <- stringr::str_remove_all(stringr::str_remove_all(kinase_groups_to_custom_xy(kinase_groups), "custom_group_pos_"), "[0-9]")

  expect_equal(actual_nums , expected_nums)
  expect_equal(actual_xy , expected_xy)
})

