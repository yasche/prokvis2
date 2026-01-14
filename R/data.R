#' A list object that contains kinome data for several model organisms
#'
#' @format ## `kinome_data`
#'
#' A list where the first level represents one species each. As of file version 1.0.0, each entry contains the following fields:
#' \describe{
#'   \item{version}{A character - version number, currently `1.0.0`.}
#'   \item{date}{A Date (S3) - When the entry was created.}
#'   \item{name}{A character -  The name of the species.}
#'   \item{scientific_name}{A character - The scientific name of the species.}
#'   \item{kinome}{A tibble - The kinome of the species, containing kinase name, group, family and subfamily, along with other information.}
#'   \item{aliases}{A tibble - Aliases for kinase names of that specific species. Used for the name mapping functionality.}
#'   \item{msa}{A phylo (S3) - A Multiple Sequence Alignment (MSA), used for the phylogenetic tree.}
#'   \item{sequences}{A character - Amino acid sequences of the kinases of a species.}
#'   \item{domains}{A character - Amino acid sequences of the kinase domains of a species.}
#'   ...
#' }
#'
#' @source Contains data from several publications by Manning et al. who kindly allowed the redistribution of the data.
"kinome_data"
