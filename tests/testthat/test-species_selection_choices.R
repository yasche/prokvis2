test_that("Correct species selection choices for internal `kinome_data`", {
  expected_selection <- c("dm", "hs", "mm", "sc", "gl", "lm", "dd", "cc", "sm", "aq", "tv", "ce")
  names(expected_selection) <- c("Drosophila melanogaster (Fruit Fly)",
                                 "Homo sapiens (Human)",
                                 "Mus musculus (Mouse)",
                                 "Saccharomyces cerevisiae (Baker's Yeast)",
                                 "Giardia lamblia",
                                 "Leishmania major",
                                 "Dictyostelium discoideum (Slime Mold)",
                                 "Coprinopsis cinerea (Gray Shag Mushroom)",
                                 "Selaginella moellendorffii",
                                 "Amphimedon queenslandica (Sponge)",
                                 "Trichomonas vaginalis",
                                 "Caenorhabditis elegans (Worm)")
  expect_equal(species_selection_choices(kinome_data), expected_selection)
})
