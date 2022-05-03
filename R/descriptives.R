#' Descriptives
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @import stargazer
#' @return Table 2, descriptive statistics
#' @export

descriptives <- function(){
  charts <- read.csv("data/musicid.csv")

  #charts$year = strptime(as.character(charts$year), "%Y")
  #charts$year = year(charts$year)


  charts$year = lubridate::ymd(charts$year, truncated = 2L)

  medians <- charts %>%
    group_by(year) %>%
    summarise(
      mean = round(mean(indicativerevenue),2),
      median = round(median(indicativerevenue),2),
      sd = round(sd(indicativerevenue),2),
      max = round(max(indicativerevenue),2),
      min = round(min(indicativerevenue),2),
      range = round((max - min),2),
      IQrange = round((unname(quantile(indicativerevenue,0.75)) - unname(quantile(indicativerevenue,0.25))),2)
    ) %>%
    dplyr::filter(lubridate::year(year) >= 2010 & lubridate::year(year) <2021, )

  stargazer::stargazer(medians, summary = FALSE, rownames = FALSE, out = "results/graphs/Table2.txt", title = "Descriptive Statistics", digits = 1)
}
