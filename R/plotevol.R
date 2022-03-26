usethis::use_package("ggplot2")
#' Creates Histogram to Visualize the Evolution of Income
#' @import ggplot2
#' @import tidyverse
#' @import readxl
#' @importFrom utils write.csv
#'
#' @param workingdir The working directory
#' @return A Histogram
#' @export
#'

plotevol = function(workingdir){
  charts = read.csv(paste(workingdir,"/","data/musicid.csv", sep = ""))
ggplot2::ggplot(data = charts)+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2020'), ggplot2::aes(x = indicativerevenue, color = '2020'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2019'), ggplot2::aes(x = indicativerevenue, color = '2019'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2018'), ggplot2::aes(x = indicativerevenue, color = '2018'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2017'), ggplot2::aes(x = indicativerevenue, color = '2017'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2016'), ggplot2::aes(x = indicativerevenue, color = '2016'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2015'), ggplot2::aes(x = indicativerevenue, color = '2015'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2014'), ggplot2::aes(x = indicativerevenue, color = '2014'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2013'), ggplot2::aes(x = indicativerevenue, color = '2013'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2012'), ggplot2::aes(x = indicativerevenue, color = '2012'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2011'), ggplot2::aes(x = indicativerevenue, color = '2011'))+
  ggplot2::geom_density(data = dplyr::filter(charts, year == '2010'), ggplot2::aes(x = indicativerevenue, color = '2010'))+
  ggplot2::labs(title = 'Evolution of Income Distribution for Superstars', x = 'Revenue', y = 'Density')
}
