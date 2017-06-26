pkgs <- as.data.frame(available.packages())
package_names <- pkgs$Package

dotted <- as.character(package_names[stringr::str_count(package_names, "\\.") == 1])
length(dotted)
length(dotted)/nrow(package_names)

get_origin <- function(name){
  stringr::str_split(name, "\\.", simplify = TRUE)[1]
}
source <- purrr::map_chr(dotted, get_origin)


find_whether_source <- function(dotted, source){
  is_a_package <- source %in% package_names 
  
  dotted_imports <- dplyr::filter(pkgs, Package == dotted)$Imports
  dotted_imports <- stringr::str_split(dotted_imports, ",", simplify = TRUE)
  dotted_imports <- trimws(dotted_imports)
  is_imported_by_dotted <- source %in% dotted_imports
  
  source_imports <- dplyr::filter(pkgs, Package == source)$Imports
  source_imports <- stringr::str_split(source_imports, ",", simplify = TRUE)
  source_imports <- trimws(source_imports)
  source_imports <- stringr::str_replace(source_imports, "\\(.*", "")
  source_imports <- trimws(source_imports)
  is_imported_by_source <- dotted %in% source_imports
  
  is_a_package & (is_imported_by_dotted|is_imported_by_source)
}

which_sources <- purrr::map2_lgl(dotted, source, find_whether_source)

data.frame(source = source, dotted = dotted)[which_sources,]
