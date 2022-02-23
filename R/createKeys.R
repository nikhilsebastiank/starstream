usethis::use_package("readxl")

#' Creates Unique Keys from Music Brainz Database
#' @importFrom utils write.csv
#' @return A .csv file containing the artist keys and artist names
#' @export
#'
createKeys = function(){
  artistdata = read_xlsx("data/artistkeys.xlsx")
  View(artistdata)
  write.csv(artistdata, "data/artistkeysmain.csv", row.names = FALSE)

}