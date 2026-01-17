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

# problems when plotting tetrahymena -> remove for now

kinome_data[["tt"]] <- NULL

# change order so that "important" species are on the top

important_spec <- which(names(kinome_data) %in% c("dm", "mm", "hs", "sc"))

kinome_data_order <- c(names(kinome_data)[important_spec], names(kinome_data)[-important_spec])

setdiff(kinome_data_order, names(kinome_data))
setdiff(names(kinome_data), kinome_data_order)

kinome_data <- kinome_data[kinome_data_order]

usethis::use_data(kinome_data, overwrite = TRUE)
