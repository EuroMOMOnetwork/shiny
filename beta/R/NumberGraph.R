#' EuroMOMO number of deaths graph.
#'
#' @param dt Data.
#' @param c Country.
#' @param a ISOweek (YYYY-WMM) of data reporting
#' @param g group e.g. age group
#' @import data.table
#' @import ggplot2
#' @return graph
#' @export
NumberGraph <- function(dt, c, a, g) {
  library(data.table)
  library(ggplot2)

  dt <-  setDT(dt)[(country == c) & (aggregation == a) & (group == g),
                   .(nb, nbc, pnb,
                     sdm2 = max(0, pnb - 2*sqrt(Vexcess)),
                     sd2 = pnb + 2*sqrt(Vexcess),
                     sd4 = pnb + 4*sqrt(Vexcess)
                   ), keyby = ISOweek]

  dt$wk = as.numeric(as.factor(dt$ISOweek))
  graph <- ggplot(dt, aes(x = wk)) +
    geom_line(aes(y = nbc, colour="darkgreen"), linetype="solid") +
    geom_line(aes(y = pnb, colour="red"), linetype="solid") +
    geom_line(aes(y = sdm2, colour="black"), linetype="dashed") +
    geom_line(aes(y = sd2, colour="black"), linetype="dashed") +
    geom_line(aes(y = sd4, colour="blue"), linetype="dashed") +
    ggtitle(paste(c, g)) + theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(name = "ISOWeek", labels = dt[seq(min(dt$wk), max(dt$wk), by = 13),]$ISOweek, breaks = seq(min(dt$wk), max(dt$wk), by = 13)) +
    scale_y_continuous(name = "Number of deaths") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
    scale_color_identity(name = "",
                         breaks = c("darkgreen", "red", 'black', 'blue'),
                         labels = c("Observed", "Baseline", "2 z-scores", "4 z-scores"),
                         guide = "legend") +
    labs(caption = paste('Data reported:', a))

  return(graph)
}
