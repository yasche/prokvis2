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


test_that("ne_rhot_to_df_helper gives empty data frames if first arg is NULL for all node & edge types", {
  kdf <- ne_rhot_to_df_helper(NULL, "Manning_Name", extract_kinome_df(kinome_data, "hs"), NULL) %>% select(-Name) %>% as.matrix() %>% is.na()
  kgroupdf <- ne_rhot_to_df_helper(NULL, "Kinase_Group", extract_kinome_df(kinome_data, "hs"), "Group") %>% select(-Name) %>% as.matrix() %>% is.na()
  kfamilydf <- ne_rhot_to_df_helper(NULL, "Kinase_Family", extract_kinome_df(kinome_data, "hs"), "Family") %>% select(-Name) %>% as.matrix() %>% is.na()
  ksubfamilydf <- ne_rhot_to_df_helper(NULL, "Kinase_Subfamily", extract_kinome_df(kinome_data, "hs"), "Subfamily") %>% select(-Name) %>% as.matrix() %>% is.na()

  expect_true(all(kdf == TRUE))
  expect_true(all(kgroupdf == TRUE))
  expect_true(all(kfamilydf == TRUE))
  expect_true(all(ksubfamilydf == TRUE))
})

test_that("ne_rhot_to_df_helper gives data frames with correct names if first arg is NULL", {
  kdf <- ne_rhot_to_df_helper(NULL, "Manning_Name", extract_kinome_df(kinome_data, "hs"), NULL) %>% pull(Name)
  kgroupdf <- ne_rhot_to_df_helper(NULL, "Kinase_Group", extract_kinome_df(kinome_data, "hs"), "Group") %>% pull(Name)
  kfamilydf <- ne_rhot_to_df_helper(NULL, "Kinase_Family", extract_kinome_df(kinome_data, "hs"), "Family") %>% pull(Name)
  ksubfamilydf <- ne_rhot_to_df_helper(NULL, "Kinase_Subfamily", extract_kinome_df(kinome_data, "hs"), "Subfamily") %>% pull(Name)

  kinome_df <- extract_kinome_df(kinome_data, species_selection = "hs")

  expected_k <- pull(kinome_df, "Manning_Name")
  expected_group <- paste0("Group_", pull(kinome_df, "Kinase_Group")) %>% unique()
  expected_family <- paste0("Family_", pull(kinome_df, "Kinase_Family")) %>% unique()
  expected_subfamily <- paste0("Subfamily_", pull(kinome_df, "Kinase_Subfamily")) %>% unique()

  expect_true(is_empty(setdiff(kdf, expected_k)))
  expect_true(is_empty(setdiff(expected_k, kdf)))

  expect_true(is_empty(setdiff(kgroupdf, expected_group)))
  expect_true(is_empty(setdiff(expected_group, kgroupdf)))

  expect_true(is_empty(setdiff(kfamilydf, expected_family)))
  expect_true(is_empty(setdiff(expected_family, kfamilydf)))

  expect_true(is_empty(setdiff(ksubfamilydf, expected_subfamily)))
  expect_true(is_empty(setdiff(expected_subfamily, ksubfamilydf)))
})

test_that("combine_nodes_and_edges gives an empty data frames if args are NULL", {
  df <- combine_nodes_and_edges(NULL, NULL, NULL, NULL, extract_kinome_df(kinome_data, "hs")) %>% select(-Name) %>% as.matrix() %>% is.na()

  expect_true(all(df == TRUE))
})
