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
  kdf <- ne_rhot_to_df_helper(NULL, "Manning_Name", extract_kinome_df(kinome_data, "hs"), NULL) %>% dplyr::select(-"Name", -"id") %>% as.matrix() %>% is.na()
  kgroupdf <- ne_rhot_to_df_helper(NULL, "Kinase_Group", extract_kinome_df(kinome_data, "hs"), "Group") %>% dplyr::select(-"Name", -"id") %>% as.matrix() %>% is.na()
  kfamilydf <- ne_rhot_to_df_helper(NULL, "Kinase_Family", extract_kinome_df(kinome_data, "hs"), "Family") %>% dplyr::select(-"Name", -"id") %>% as.matrix() %>% is.na()
  ksubfamilydf <- ne_rhot_to_df_helper(NULL, "Kinase_Subfamily", extract_kinome_df(kinome_data, "hs"), "Subfamily") %>% dplyr::select(-"Name", -"id") %>% as.matrix() %>% is.na()

  expect_true(all(kdf == TRUE))
  expect_true(all(kgroupdf == TRUE))
  expect_true(all(kfamilydf == TRUE))
  expect_true(all(ksubfamilydf == TRUE))
})

test_that("ne_rhot_to_df_helper gives data frames with correct names if first arg is NULL", {
  kdf <- ne_rhot_to_df_helper(NULL, "Manning_Name", extract_kinome_df(kinome_data, "hs"), NULL) %>% dplyr::pull("Name")
  kgroupdf <- ne_rhot_to_df_helper(NULL, "Kinase_Group", extract_kinome_df(kinome_data, "hs"), "Group") %>% dplyr::pull("Name")
  kfamilydf <- ne_rhot_to_df_helper(NULL, "Kinase_Family", extract_kinome_df(kinome_data, "hs"), "Family") %>% dplyr::pull("Name")
  ksubfamilydf <- ne_rhot_to_df_helper(NULL, "Kinase_Subfamily", extract_kinome_df(kinome_data, "hs"), "Subfamily") %>% dplyr::pull("Name")

  kinome_df <- extract_kinome_df(kinome_data, species_selection = "hs")

  expected_k <- dplyr::pull(kinome_df, "Manning_Name")
  expected_group <- paste0("Group_", dplyr::pull(kinome_df, "Kinase_Group")) %>% unique()
  expected_family <- paste0("Family_", dplyr::pull(kinome_df, "Kinase_Family")) %>% unique()
  expected_subfamily <- paste0("Subfamily_", dplyr::pull(kinome_df, "Kinase_Subfamily")) %>% unique()

  expect_true(rlang::is_empty(setdiff(kdf, expected_k)))
  expect_true(rlang::is_empty(setdiff(expected_k, kdf)))

  expect_true(rlang::is_empty(setdiff(kgroupdf, expected_group)))
  expect_true(rlang::is_empty(setdiff(expected_group, kgroupdf)))

  expect_true(rlang::is_empty(setdiff(kfamilydf, expected_family)))
  expect_true(rlang::is_empty(setdiff(expected_family, kfamilydf)))

  expect_true(rlang::is_empty(setdiff(ksubfamilydf, expected_subfamily)))
  expect_true(rlang::is_empty(setdiff(expected_subfamily, ksubfamilydf)))
})

test_that("combine_nodes_and_edges gives an empty data frames if args are NULL", {
  df <- combine_nodes_and_edges(NULL, NULL, NULL, NULL, extract_kinome_df(kinome_data, "hs")) %>% dplyr::select(-"Name", -"id") %>% as.matrix() %>% is.na()

  expect_true(all(df == TRUE))
})


test_that("nodes_and_edges() returns rhot object for species_selection = 'dm' and all nodes and edges", {
  kinome_df <- extract_kinome_df(kinome_data, "dm")

  exp_class <- c("rhandsontable", "htmlwidget")

  mn <- nodes_and_edges(kinome_df, "Manning_Name")
  ks <- nodes_and_edges(kinome_df, "Kinase_Subfamily")
  kf <- nodes_and_edges(kinome_df, "Kinase_Family")
  kg <- nodes_and_edges(kinome_df, "Kinase_Group")

  expect_equal(class(mn), exp_class)
  expect_equal(class(ks), exp_class)
  expect_equal(class(kf), exp_class)
  expect_equal(class(kg), exp_class)
})


test_that("nodes_and_edges() returns rhot with correct nrow for species_selection = 'dm' and all nodes and edges", {
  kinome_df <- extract_kinome_df(kinome_data, "dm")

  exp_nrow_mn <- nrow(kinome_df)
  exp_nrow_ks <- length(unique(kinome_df$Kinase_Subfamily))
  exp_nrow_kf <- length(unique(kinome_df$Kinase_Family))
  exp_nrow_kg <- length(unique(kinome_df$Kinase_Group))

  mn <- nodes_and_edges(kinome_df, "Manning_Name")$x$rDataDim[[1]]
  ks <- nodes_and_edges(kinome_df, "Kinase_Subfamily")$x$rDataDim[[1]]
  kf <- nodes_and_edges(kinome_df, "Kinase_Family")$x$rDataDim[[1]]
  kg <- nodes_and_edges(kinome_df, "Kinase_Group")$x$rDataDim[[1]]

  expect_equal(mn, exp_nrow_mn)
  expect_equal(ks, exp_nrow_ks)
  expect_equal(kf, exp_nrow_kf)
  expect_equal(kg, exp_nrow_kg)
})

test_that("nodes_and_edges() returns rhot with correct choices for species_selection = 'dm' and all nodes and edges", {
  kinome_df <- extract_kinome_df(kinome_data, "dm")

  exp_source_mn <- kinome_df$Manning_Name
  exp_source_ks <- unique(kinome_df$Kinase_Subfamily)
  exp_source_kf <- unique(kinome_df$Kinase_Family)
  exp_source_kg <- unique(kinome_df$Kinase_Group)

  mn <- nodes_and_edges(kinome_df, "Manning_Name")$x$columns[[1]]$source
  ks <- nodes_and_edges(kinome_df, "Kinase_Subfamily")$x$columns[[1]]$source
  kf <- nodes_and_edges(kinome_df, "Kinase_Family")$x$columns[[1]]$source
  kg <- nodes_and_edges(kinome_df, "Kinase_Group")$x$columns[[1]]$source

  # should have option where none is selected
  expect_equal(setdiff(mn, exp_source_mn), "")
  expect_equal(setdiff(ks, exp_source_ks), "")
  expect_equal(setdiff(kf, exp_source_kf), "")
  expect_equal(setdiff(kg, exp_source_kg), "")

  expect_equal(setdiff(exp_source_mn, mn), character())
  expect_equal(setdiff(exp_source_ks, ks), character())
  expect_equal(setdiff(exp_source_kf, kf), character())
  expect_equal(setdiff(exp_source_kg, kg), character())
})
