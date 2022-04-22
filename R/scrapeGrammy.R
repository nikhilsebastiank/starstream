usethis::use_package("dplyr")
usethis::use_package("rvest")
usethis::use_package("readr")
#' Scrapes Grammy data from Wikipedia
#' @import rvest
#' @import dplyr
#' @import readr
#' @importFrom magrittr %>%
#' @return List of all grammy nominations and wins from 2010 - 2020.
#' @export

scrapegrammy <- function(){

  awardsfull = list()
  nominationsfull = list()

  for (i in 42:52){
    if (i%%10 == 1){
      j = assign(paste("awards",as.character(i),sep=""),read_html(paste("https://en.wikipedia.org/wiki/",as.character(i),"st","_Annual_Grammy_Awards",sep=""))%>%
                   html_nodes("p+ ul li:nth-child(1) , dl+ ul b")%>%
                   html_text())
      k = parse_character(j)
      awardsfull[[paste("awards",as.character(i),sep="")]] <- k

      l = assign(paste("nominations",as.character(i),sep=""),read_html(paste("https://en.wikipedia.org/wiki/",as.character(i),"st","_Annual_Grammy_Awards",sep=""))%>%
                   html_nodes("p+ ul li+ li , dl+ ul li+ li")%>%
                   html_text())
      m = parse_character(l)
      nominationsfull[[paste("nominations",as.character(i),sep="")]] <- m

    }
    else if (i%%10 == 2){
      j = assign(paste("awards",as.character(i),sep=""),read_html(paste("https://en.wikipedia.org/wiki/",as.character(i),"nd","_Annual_Grammy_Awards",sep=""))%>%
                   html_nodes("p+ ul li:nth-child(1) , dl+ ul b")%>%
                   html_text())
      k = parse_character(j)
      awardsfull[[paste("awards",as.character(i),sep="")]] <- k

      l = assign(paste("nominations",as.character(i),sep=""),read_html(paste("https://en.wikipedia.org/wiki/",as.character(i),"nd","_Annual_Grammy_Awards",sep=""))%>%
                   html_nodes("p+ ul li+ li , dl+ ul li+ li")%>%
                   html_text())
      m = parse_character(l)
      nominationsfull[[paste("nominations",as.character(i),sep="")]] <- m

    }
    else if (i%%10 == 3){
      j = assign(paste("awards",as.character(i),sep=""),read_html(paste("https://en.wikipedia.org/wiki/",as.character(i),"rd","_Annual_Grammy_Awards",sep=""))%>%
                   html_nodes("p+ ul li:nth-child(1) , dl+ ul b")%>%
                   html_text())
      k = parse_character(j)
      awardsfull[[paste("awards",as.character(i),sep="")]] <- k

      l = assign(paste("nominations",as.character(i),sep=""),read_html(paste("https://en.wikipedia.org/wiki/",as.character(i),"rd","_Annual_Grammy_Awards",sep=""))%>%
                   html_nodes("p+ ul li+ li , dl+ ul li+ li")%>%
                   html_text())
      m = parse_character(l)
      nominationsfull[[paste("nominations",as.character(i),sep="")]] <- m

    }

    else {
      j = assign(paste("awards",as.character(i),sep=""),read_html(paste("https://en.wikipedia.org/wiki/",as.character(i),"th","_Annual_Grammy_Awards",sep=""))%>%
                   html_nodes("p+ ul li:nth-child(1) , dl+ ul b")%>%
                   html_text())
      k = parse_character(j)
      awardsfull[[paste("awards",as.character(i),sep="")]] <- k

      l = assign(paste("nominations",as.character(i),sep=""),read_html(paste("https://en.wikipedia.org/wiki/",as.character(i),"th","_Annual_Grammy_Awards",sep=""))%>%
                   html_nodes("p+ ul li+ li , dl+ ul li+ li")%>%
                   html_text())
      m = parse_character(l)
      nominationsfull[[paste("nominations",as.character(i),sep="")]] <- m

    }
    grammyfull = list("awards" = awardsfull,"nominations" =nominationsfull)
  }
  return(grammyfull)
}