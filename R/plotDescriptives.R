usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("lubridate")
#' Creates Descriptive Plots
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @importFrom utils read.csv
#' @importFrom stats median quantile
#' @importFrom utils globalVariables
#' @import readxl
#' @importFrom utils write.csv
#' @importFrom magrittr %>%
#' @param workingdir The working directory
#' @param results The directory for outputs
#' @return Creates and Saves Descriptive Plots for the Income Distributions of Superstars (2010-2020)
#' @export
#'

plotDescriptives = function(workingdir = "~/Dropbox/Projects/MasterThesis", results = paste(workingdir, "/results/graphs", sep = "")){
  charts = read.csv(paste(workingdir,"/","data/musicid.csv", sep = ""))
  evolution <- ggplot2::ggplot(data = charts)+
    ggplot2::geom_density(data = dplyr::filter(charts, year == '2020'), ggplot2::aes(x = indicativerevenue, color = '2020'))+
    ggplot2::geom_density(data = dplyr::filter(charts, year == '2019'), ggplot2::aes(x = indicativerevenue, color = '2019'))+
    #ggplot2::geom_density(data = dplyr::filter(charts, year == '2018'), ggplot2::aes(x = indicativerevenue, color = '2018'))+
    #ggplot2::geom_density(data = dplyr::filter(charts, year == '2017'), ggplot2::aes(x = indicativerevenue, color = '2017'))+
    ggplot2::geom_density(data = dplyr::filter(charts, year == '2016'), ggplot2::aes(x = indicativerevenue, color = '2016'))+
    #ggplot2::geom_density(data = dplyr::filter(charts, year == '2015'), ggplot2::aes(x = indicativerevenue, color = '2015'))+
    #ggplot2::geom_density(data = dplyr::filter(charts, year == '2014'), ggplot2::aes(x = indicativerevenue, color = '2014'))+
    ggplot2::geom_density(data = dplyr::filter(charts, year == '2013'), ggplot2::aes(x = indicativerevenue, color = '2013'))+
    #ggplot2::geom_density(data = dplyr::filter(charts, year == '2012'), ggplot2::aes(x = indicativerevenue, color = '2012'))+
    #ggplot2::geom_density(data = dplyr::filter(charts, year == '2011'), ggplot2::aes(x = indicativerevenue, color = '2011'))+
    ggplot2::geom_density(data = dplyr::filter(charts, year == '2010'), ggplot2::aes(x = indicativerevenue, color = '2010'))+
    ggplot2::labs(title = 'Evolution of Income Distribution for Superstars', x = 'Revenue', y = 'Density')
  evolution
  ggplot2::ggsave(path = results, width = 8, height = 5, device='png', dpi=700, filename = "evolution.png")

  # Convert year type to as.Date:
  charts$year = lubridate::ymd(charts$year, truncated = 2L)

  # Quantile Analysis
  medians <- charts %>%
    group_by(year) %>%
    summarise(
      mean = mean(indicativerevenue),
      median = median(indicativerevenue),
      q1 = quantile(indicativerevenue, 0.01),
      q2 = quantile(indicativerevenue, 0.1),
      q3 = quantile(indicativerevenue, 0.9),
      q4 = quantile(indicativerevenue, 0.99),
      ninetyToTen = q3/q2,
      ninetyToFifty = q3/median,
      fiftyToTen = median/q2,
      meanToMedian = mean/median
    ) %>%
    filter(lubridate::year(year) >= 2010 & lubridate::year(year) <2021)

  # Median Incomes

  medianincomes <- ggplot2::ggplot(data = medians, aes(year, median))+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::labs(title = 'Median Incomes for Superstars', x = 'Year', y = 'Median')
  medianincomes
  ggplot2::ggsave(path = results, width = 8, height = 5, device='png', dpi=700, filename = "medianincomes.png")

  # Mean to Median Ratio

  meanToMedianRatio <- ggplot2::ggplot(data = medians, aes(year, meanToMedian))+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::labs(title = 'Mean To Median Ratio for Superstars', x = 'Year', y = 'Mean to Median Ratio')
  meanToMedianRatio
  ggplot2::ggsave(path = results, width = 8, height = 5, device='png', dpi=700, filename = "meanToMedian.png")

  # Qunatile Analysis Over Time:
  # Can possibly do it for quintiles and also for other years maybe.

  quantileanalysis <- ggplot2::ggplot(data = medians, aes(year))+
    ggplot2::geom_point(aes(y = q4, color = "99th Percentile"))+
    ggplot2::geom_line(aes(y = q4, color = "99th Percentile"))+
    ggplot2::geom_point(aes(y = q3, color = "90th Percentile"))+
    ggplot2::geom_line(aes(y = q3, color = "90th Percentile"))+
    ggplot2::geom_point(aes(y = q2, color = "10th Percentile"))+
    ggplot2::geom_line(aes(y = q2, color = "10th Percentile"))+
    ggplot2::geom_point(aes(y = q1, color = "1st Percentile"))+
    ggplot2::geom_line(aes(y = q1, color = "1st Percentile"))+
    ggplot2::geom_point(aes(y = median, color = "50th Percentile"))+
    ggplot2::geom_line(aes(y = median, color = "50th Percentile"))+
    ggplot2::labs(title = 'Evolution of Quantiles', x = 'Year', y = 'Quantiles')
  quantileanalysis
  ggplot2::ggsave(path = results, width = 8, height = 5, device='png', dpi=700, filename = "quantiles.png")

  # Income ratios:

  incomeratios <- ggplot2::ggplot(data = medians, aes(year))+
    ggplot2::geom_point(aes(y = ninetyToTen, color = "90-10"))+
    ggplot2::geom_line(aes(y = ninetyToTen, color = "90-10"))+
    ggplot2::geom_point(aes(y = ninetyToFifty, color = "90-50"))+
    ggplot2::geom_line(aes(y = ninetyToFifty, color = "90-50"))+
    ggplot2::geom_point(aes(y = fiftyToTen, color = "50-10"))+
    ggplot2::geom_line(aes(y = fiftyToTen, color = "50-10"))+
    ggplot2::labs(title = 'Income Ratios for Superstars', x = 'Revenue', y = 'Density')
  incomeratios
  ggplot2::ggsave(path = results, width = 8, height = 5, device='png', dpi=700, filename = "incomeratios.png")
  plotlist = list(Evolution = evolution, MedianIncomes = medianincomes, MeanMedian = meanToMedianRatio, Quantiles = quantileanalysis, IncomeRatios = incomeratios)
  return(plotlist)
}
