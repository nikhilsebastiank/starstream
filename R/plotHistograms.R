#' Plot Histograms for Indicative Revenue
#' @return Histograms with Indicative Revenue on the Horizontal Axis (Appendix)
#' @export

plotHistograms = function(){
  charts = charts
  hist2020 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2020'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2019 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2019'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2018 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2018'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2017 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2017'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2016 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2016'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2015 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2015'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2014 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2014'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2013 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2013'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2012 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2012'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2011 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2011'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  hist2010 <- ggplot2::ggplot(data = charts) + ggplot2::geom_histogram(data = dplyr::filter(charts, year == '2010'), ggplot2::aes(x = indicativerevenue), bins = 15) + ggplot2::labs(x = "Indicative Revenue", y = "Frequency") + ggplot2::ylim(0,60) + ggplot2::xlim(0,60000)
  fullhist <- cowplot::plot_grid(hist2010,hist2011,hist2012,hist2013,hist2014,hist2015,hist2016,hist2017,hist2018,hist2019,hist2020, labels = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"), hjust = - 5, label_x = 0.01, ncol = 4)
  fullhist
}
