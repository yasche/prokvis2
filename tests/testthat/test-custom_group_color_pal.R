test_that("kinase_groups_to_custom_color_numbers() encoding works", {
  res <- kinase_groups_to_custom_color_numbers(c("AGC", "TKL"))

  nums <- stringr::str_remove(res, "^custom_group_col")

  expect_equal(length(res), 2)
  expect_true(all(stringr::str_detect(res, "^custom_group_col[0-9]{1,}")))
  expect_true(all(nums[[1]] == "1", nums[[2]] == "2"))
})

test_that("Color extraction works", {
  color_nums <- kinase_groups_to_custom_color_numbers(c("AGC", "TKL"))

  sim_input <- list("red", "blue")
  names(sim_input) <- color_nums

  res <- custom_color_nums_to_pal(color_nums, sim_input)

  expect_equal(res, c("red", "blue"))
})

test_that("custom_group_color_input() creates a shiny input", {
  id <- "plots_ui_c"
  ns <- shiny::NS(id)
  custom_color_nums <- kinase_groups_to_custom_color_numbers(c("AGC", "TKL"))
  kinase_groups <- c("AGC", "TKL")

  res <- custom_group_color_input(custom_color_nums, kinase_groups, ns, id)

  expect_equal(class(res), "list")
  expect_equal(length(res), 3)
})
