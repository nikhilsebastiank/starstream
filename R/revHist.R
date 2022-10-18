#' Plot Histograms for Earnings for Music Creators
#' @importFrom magrittr %>%
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @return A subset of the main data containing revenue data (UK Data - not used in the analysis)


revHist = function(){
  Creators = sortresponse() # Loads the sorted responses
  B8 <- NULL
  Q15 <- NULL
  Year1 <- NULL
  Year2 <- NULL
  Year3 <- NULL
  Creators1 = Creators %>% # Subsets revenue data
    select(B8, Q15)
  Creators1$B8 = as.factor(Creators1$B8)

  # Split string into 3 years
  Creators1 = mutate(Creators1,
                     Year1 = as.factor(unlist(lapply(strsplit(Creators1$Q15, split = "/"), function(x) {x[c(1)]}))),
                     Year2 = as.factor(unlist(lapply(strsplit(Creators1$Q15, split = "/"), function(x) {x[c(2)]}))),
                     Year3 = as.factor(unlist(lapply(strsplit(Creators1$Q15, split = "/"), function(x) {x[c(3)]}))))

  Creators1 = filter(Creators1, B8 %in% all_of(levels(B8))[1:10],
                     Year1 %in% all_of(levels(Year1))[1:10],
                     Year2 %in% all_of(levels(Year2))[1:10],
                     Year3 %in% all_of(levels(Year3))[1:10])

  newlevels = c("<1,000", "<5,000", "<10,000", "<20,000", "<30,000", "<40,000", "<50,000", "<75,000", "<100,000", ">100,000", "Q", "0", "n", "NotApplicable")

  # Revenue Before 2015

  Creators1$B8 = str_sort(Creators1$B8, numeric = TRUE)
  Creators1$B8 = as.factor(Creators1$B8)

  Creators1$B8 = factor(Creators1$B8, levels = c("£1 - 1,000", "£1,001 - £5,000", "£5,001 - £10,000", "£10,001 - £20,000", "£20,001 - £30,000", "£30,001 - £40,000" , "£40,001 - £50,000", "£50,001 - £75,000", "£75,001 - £100,000", "100000", "Before the growth of music streaming (2015), roughly how much did you earn from music in an average year?", "I did not earn anything from music in this period", "n", "Prefer not to say/not sure"))
  levels(Creators1$B8) = newlevels

  a = ggplot(data = Creators1, aes(x = B8))+
    geom_bar()+
    labs(x = "Before 2015")

  # Revenue 2018

  Creators1$Year1 = str_sort(Creators1$Year1, numeric = TRUE)
  Creators1$Year1 = as.factor(Creators1$Year1)

  Creators1$Year1 = factor(Creators1$Year1, levels = c("£1 - 1,000", "£1,001 - £5,000", "£5,001 - £10,000", "£10,001 - £20,000", "£20,001 - £30,000", "£30,001 - £40,000" , "£40,001 - £50,000", "£50,001 - £75,000", "£75,001 - £100,000", "100000", "Approximately how much did you personally earn overall from your music career (before taxes) in each of the past 3 years?\r\n \r\n\r\nIf some of your income is from advances received prior to or during this period, please allow for how this is apportioned to each of the 3 years. - 2018", "I did not earn anything from music in this year", "Not applicable", "Prefer not to sayot sure"))
  levels(Creators1$Year1) = newlevels

  b = ggplot(data = Creators1, aes(x = Year1))+
    geom_bar()+
    labs(x = "Year - 2018")

  # Revenue 2019

  Creators1$Year2 = str_sort(Creators1$Year2, numeric = TRUE)
  Creators1$Year2 = as.factor(Creators1$Year2)

  Creators1$Year2 = factor(Creators1$Year2, levels = c("£1 - 1,000", "£1,001 - £5,000", "£5,001 - £10,000", "£10,001 - £20,000", "£20,001 - £30,000", "£30,001 - £40,000" , "£40,001 - £50,000", "£50,001 - £75,000", "£75,001 - £100,000", "100000", "Approximately how much did you personally earn overall from your music career (before taxes) in each of the past 3 years?\r\n \r\n\r\nIf some of your income is from advances received prior to or during this period, please allow for how this is apportioned to each of the 3 years. - 2018", "I did not earn anything from music in this year", "Not applicable", "Prefer not to sayot sure"))
  levels(Creators1$Year2) = newlevels

  c = ggplot(data = Creators1, aes(x = Year2))+
    geom_bar()+
    labs(x = "Year - 2019")

  # Revenue 2020

  Creators1$Year3 = str_sort(Creators1$Year3, numeric = TRUE)
  Creators1$Year3 = as.factor(Creators1$Year3)

  Creators1$Year3 = factor(Creators1$Year3, levels = c("£1 - 1,000", "£1,001 - £5,000", "£5,001 - £10,000", "£10,001 - £20,000", "£20,001 - £30,000", "£30,001 - £40,000" , "£40,001 - £50,000", "£50,001 - £75,000", "£75,001 - £100,000", "100000", "Approximately how much did you personally earn overall from your music career (before taxes) in each of the past 3 years?\r\n \r\n\r\nIf some of your income is from advances received prior to or during this period, please allow for how this is apportioned to each of the 3 years. - 2018", "I did not earn anything from music in this year", "Not applicable", "Prefer not to sayot sure"))
  levels(Creators1$Year3) = newlevels

  d = ggplot(data = Creators1, aes(x = Year3))+
    geom_bar()+
    labs(x = "Year - 2020")
  #jpeg(file = '/Users/nikhilsebastian/Dropbox/Projects/MasterThesis/results/graphs/revenueHist.jpeg')
  #gridExtra::grid.arrange(a,b,c,d, nrow = 2,
  #top = grid::textGrob("Distribution of Revenue",gp=grid::gpar(fontsize=20,font=3)))
  p <- cowplot::plot_grid(a,b,c,d, nrow = 2, labels = "AUTO")
  title <- cowplot::ggdraw() + cowplot::draw_label("Distribution of Revenues - Based on UK Survey Data", fontface='bold')
  cowplot::plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
  ggsave(filename = '/Users/nikhilsebastian/Dropbox/Projects/MasterThesis/results/graphs/revenueHist.jpeg', last_plot(), device='jpeg', dpi=320, width = 15, height = 15)
}

# Need to check for non ASCII characters to fix (The pound symbol)
