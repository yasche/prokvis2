test_that("map_names() returns df with expected colnames", {
  aliases <- extract_aliases_df(kinome_data, "hs")

  result <- map_names(kinome_data, "hs", "ABL", aliases)
  expected_colnames <- c("Input", "Manning_Name", "Include", "Aliases")

  expect_equal(colnames(result), expected_colnames)
})

test_that("map_names() returns expected results for some random inputs", {
  aliases_hs <- extract_aliases_df(kinome_data, "hs")
  aliases_dm <- extract_aliases_df(kinome_data, "dm")
  aliases_mm <- extract_aliases_df(kinome_data, "mm")
  aliases_sc <- extract_aliases_df(kinome_data, "sc")

  result_hs_abl <- map_names(kinome_data, "hs", "ABL", aliases_hs)$Manning_Name
  expected_hs_abl <- c("ABL1", "ABL2")

  result_dm_unia <- map_names(kinome_data, "dm", "O61443", aliases_dm)$Manning_Name
  expected_dm_unia <- "p38b"

  result_mm_unin <- map_names(kinome_data, "mm", "mk08_mouse", aliases_mm)$Manning_Name
  expected_mm_unin <- "JNK1"

  result_sc_empty <- map_names(kinome_data, "sc", "xxxxxxxxx", aliases_sc)
  expected_sc_empty <- readr::read_csv("Input,Manning_Name,Include,Aliases
", show_col_types = FALSE, col_types = "cclc")

  expect_equal(result_hs_abl, expected_hs_abl)
  expect_equal(result_dm_unia, expected_dm_unia)
  expect_equal(result_mm_unin, expected_mm_unin)
  expect_equal(result_sc_empty, expected_sc_empty)
})


test_that("name_map_df2rhot() returns an rhot object", {
  aliases_hs <- extract_aliases_df(kinome_data, "hs")
  name_map <- map_names(kinome_data, "hs", "ABL", aliases_hs)

  nm_rhot <- name_map_df2rhot(name_map, aliases_hs)
  exp_class <- c("rhandsontable", "htmlwidget")

  expect_equal(class(nm_rhot), exp_class)
})


test_that("name_map_rhot() returns an rhot object", {
  nm_rhot <- name_map_rhot(kinome_data, "hs", "ABL")

  exp_class <- c("rhandsontable", "htmlwidget")

  expect_equal(class(nm_rhot), exp_class)
})
