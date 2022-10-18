#' Estimates Quantiles of the distribution by year
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @param yr The year to estimate the quantiles
#' @param workingdir The working directory
#' @return Quantiles: 0.01, 0.1, 0.5, 0.9


estimatequantiles = function(yr){
  charts = charts
  mdata = dplyr::filter(charts, year == yr)

  tags = c("[0-5000)", "[5000-10000)", "[10000-15000)", "[15000-20000)", "[20000-25000)", "[25000-30000)", "[30000-35000)", "[35000-40000)", "[40000-45000)", "[45000-50000)")
  mdatagroup <- tibble::as_tibble(mdata) %>%
    dplyr::mutate(tag = dplyr::case_when(
      indicativerevenue < 5000 ~ tags[1],
      indicativerevenue >= 5000 & indicativerevenue < 10000 ~ tags[2],
      indicativerevenue >= 10000 & indicativerevenue < 15000 ~ tags[3],
      indicativerevenue >= 15000 & indicativerevenue < 20000 ~ tags[4],
      indicativerevenue >= 20000 & indicativerevenue < 25000 ~ tags[5],
      indicativerevenue >= 25000 & indicativerevenue < 30000 ~ tags[6],
      indicativerevenue >= 30000 & indicativerevenue < 35000 ~ tags[7],
      indicativerevenue >= 35000 & indicativerevenue < 40000 ~ tags[8],
      indicativerevenue >= 40000 & indicativerevenue < 45000 ~ tags[9],
      indicativerevenue >= 45000 & indicativerevenue < 50000 ~ tags[10]
    ))

  mdatagroup$tag <- factor(mdatagroup$tag, levels = tags, ordered = FALSE)

  a = 20000
  b = 25000

  pa = nrow(dplyr::filter(mdatagroup, indicativerevenue < 20000))/100
  pb = nrow(dplyr::filter(mdatagroup, indicativerevenue < 25000))/100

  shape = (log(1-pa) - log(1-pb))/((log(b) - log(a)))
  scale = ((pb-pa)/((1/a^shape)-(1/b^shape)))^(1/shape)

  q1 = scale*(1-0.01)^(1/shape)
  q2 = scale*(1-0.1)^(1/shape)
  q3 = scale*(1-0.5)^(1/shape)
  q4 = scale*(1-0.9)^(1/shape)

  print(paste("Q1 =", q1))
  print(paste("Q2 =", q2))
  print(paste("Q3 =", q3))
  print(paste("Q4 =", q4))
  quantilelist = list("q1" = q1, "q2" = q2, "q3" = q3, "q4" = q4)
  return(quantilelist)
}
