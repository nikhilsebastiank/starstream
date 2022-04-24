usethis::use_package("dplyr")
usethis::use_package("lubridate")
#' Grammy Wins and Nominations
#' @importFrom magrittr %>%
#' @import lubridate
#' @import dplyr
#' @param datamain - The dataset used
#' @param a The Number of the Grammy Ceremony, For example 52 for 52nd Grammy
#' @param b The corresponding year of the ceremony
#' @return Returns a list containing the awards and nominations for each year for upto 10 years - Editable
#' @export
#'
#' @examples grammys(albumchartssub,52,2010)
grammys <- function(datamain,a=52,b=2010){
  albumcharts <- datamain
  grammyfull <- starstream::scrapegrammy(a,a+10)

  albumchartsByYear <- dplyr::select(albumcharts, year, artist)%>%
    group_by(year)

  albumchartsByYear$year <- as.Date(albumchartsByYear$year)

  albumchartslist = list()
  for (i in b:(b+10)){
    z = assign(paste("albumchartsbyYear",as.character(i), sep = ""),filter(albumcharts, lubridate::year(albumchartsByYear$year) == as.character(i)))
    albumchartslist[[paste("albumchartsbyYear",as.character(i), sep = "")]] = z
  }

  awardsfull = list()
  nominationsfull = list()

  for (j in 1:11){

    # Run this:
    awards = vector()
    nominations = vector ()
    for (i in 1:100){
      awarddeet = paste("grammyfull$awards$awards",a,sep="")
      nomindeet = paste("grammyfull$nominations$nominations",a,sep="")
      albumchartsdeet = albumchartslist[[j]]
      #noquoteawarddeet = noquote(awarddeet)
      #noquotenomindeet = noquote(nomindeet)
      grammyawarddeet = grammyfull$awards[[j]]
      grammynominationsdeet = grammyfull$nominations[[j]]


      k <- sum(stringr::str_count(grammyawarddeet,albumchartsdeet$artist[i]))
      l <- k + sum(stringr::str_count(grammynominationsdeet,albumchartsdeet$artist[i]))
      awards <- append(awards, k)
      nominations <- append(nominations, l)
      awardsfull[[as.character(b)]] <- awards
      nominationsfull[[as.character(b)]] <- nominations
    }# Go back to #Run this:

    a = a+1
    b = b+1
  }
  grammyawards <- list(awardsfull,nominationsfull)
  return(grammyawards)
}
