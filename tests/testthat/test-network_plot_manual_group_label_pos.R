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

test_that("Nudge extraction works", {
  kgroups <- c("AGC", "TKL")
  nudge_nums <- kinase_groups_to_custom_xy(kgroups)
  sim_input <- 1:4

  names(sim_input) <- nudge_nums

  res <- custom_xy_nums_to_nudge(nudge_nums, sim_input, kgroups)

  expected_res <- tibble::tibble(
    x_nudge = c(1L, 3L),
    y_nudge = c(2L, 4L),
    label = kgroups
  )

  expect_equal(res, expected_res)
})


test_that("manual_group_label_pos_input() creates a shiny input", {
  id <- "plots_ui_c"
  ns <- shiny::NS(id)
  kinase_groups <- c("AGC", "TKL")
  custom_color_nums <- kinase_groups_to_custom_xy(kinase_groups)


  res <- manual_group_label_pos_input(custom_color_nums, kinase_groups, ns, id)

  expect_equal(class(res), "list")
  expect_equal(length(res), length(kinase_groups) * 2)
})
