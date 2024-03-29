#' Merge Responses based on Raw Data
#' @importFrom magrittr %>%
#' @param a An input dataset
#' @param b Response number entered as a string
#'
#' @return The data set with merged responses - UK data (not used in the analysis)

mergecol = function(a, b){

  c = stringr::str_extract(colnames(a), stringr::str_c("(",b,").","*"))
  c = as.character(stats::na.omit(c))
  i <- NULL
  a = a %>%
    tidyr::unite(i, dplyr::all_of(c) , sep = "/")
  a$i = stringr::str_replace_all(a$i, "(/n)*" , "")
  a$i = stringr::str_replace_all(a$i, "(n/)", "")
  index = grep("i",colnames(a))
  colnames(a)[index] = b
  return(a)
}
