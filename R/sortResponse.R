usethis::use_package("utils")
#' Merges all the responses and outputs a dataset
#'
#' @param a The input dataset
#'
#' @return The data set with merged responses
#' @export
#' @importFrom stats na.omit
#' @importFrom utils View
sortresponse = function(a){
  undervector = c()
  lettervector = c()
  for (i in colnames(a)){
    d = substring(i,1,3)
    if (substring(d,3,3) == "_"){
      undervector = append(undervector,d)
    }
    else {
      lettervector = append(lettervector,d)
    }
  }
  for (i in lettervector){
    c = stringr::str_extract(colnames(a), stringr::str_c("(",i,").","*"))
    c = as.character(na.omit(c))
    if (length(c) > 1){
      a = mergecol(a,i)
    }
  }
  for (i in undervector){
    c = stringr::str_extract(colnames(a), stringr::str_c("(",i,").","*"))
    c = as.character(na.omit(c))
    if (length(c) > 1){
      a = mergecol(a,i)
    }
  }
  a
}
