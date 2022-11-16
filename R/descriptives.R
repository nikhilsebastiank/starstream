#' Create Table of Descriptive Statistics
#'
#'
#' `descriptives()` returns a table of descriptive statistics of IR for superstar musicians,
#' for the period 2010-2020.
#'
#'
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @import stargazer
#' @return Table 1, descriptive statistics for Indicative Revenue
#' @export

descriptives <- function(){
  charts <- chartssmall

  #charts$year = strptime(as.character(charts$year), "%Y")
  #charts$year = year(charts$year)


  charts$year = lubridate::ymd(charts$year, truncated = 2L)
  #charts$year = format(charts$year, format = "%Y")

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
    dplyr::filter(lubridate::year(year) >= 2010 & lubridate::year(year) <2021, )%>%
    dplyr::mutate(year = format(year, format = "%Y"))

  stargazer::stargazer(medians, summary = FALSE, rownames = FALSE, digits = 1)
}
