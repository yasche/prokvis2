test_that("Correct length for kinase edges & species = 'hs'", {
  ne_names <- extract_kinome_df(kinome_data, "hs") %>%
    dplyr::select("Manning_Name") %>%
    dplyr::pull() %>%
    unique()

  expect_equal(nrow(ne_df_helper(ne_names)), 551)
})

test_that("Correct length for groups & species = 'hs'", {
  ne_names <- extract_kinome_df(kinome_data, "hs") %>%
    dplyr::select("Kinase_Group") %>%
    dplyr::pull() %>%
    unique()

  expect_equal(nrow(ne_df_helper(ne_names)), 12)
})

test_that("Correct length for family & species = 'hs'", {
  ne_names <- extract_kinome_df(kinome_data, "hs") %>%
    dplyr::select("Kinase_Family") %>%
    dplyr::pull() %>%
    unique()

  expect_equal(nrow(ne_df_helper(ne_names)), length(ne_names))
})

test_that("Correct length for family & species = 'hs'", {
  ne_names <- extract_kinome_df(kinome_data, "hs") %>%
    dplyr::select("Kinase_Subfamily") %>%
    dplyr::pull() %>%
    unique()

  expect_equal(nrow(ne_df_helper(ne_names)), length(ne_names))
})
