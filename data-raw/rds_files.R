## code to prepare `DATASET` dataset goes here

# Creation of data sets is documented elsewhere.

#

kinome_data <- purrr::map(paste(rds_path, list.files(rds_path), sep = "/"),
                          readr::read_rds)

extract_name_abr <- function(data) {
  scientific_name <- data$scientific_name

  tolower(stringr::str_remove_all(paste0(stringr::str_extract(scientific_name, "^[A-Z]"),
                                 stringr::str_extract(scientific_name, " [a-z]")),
         " "))
}

extract_name_abr(kinome_data[[1]])

species_abr <- purrr::map(kinome_data,
           extract_name_abr)

names(kinome_data) <- unlist(species_abr)

usethis::use_data(kinome_data, overwrite = TRUE)
