#' EuroMOMO z-score graph.
#'
#' @param dt Data.
#' @param c Country.
#' @param a ISOweek (YYYY-WMM) of data reporting
#' @param g group e.g. age group
#' @import data.table
#' @import ggplot2
#' @return graph
#' @export
ZscoreGraph <- function(dt, c, r, g) {
  library(data.table)
  library(ggplot2)

  dt <-  setDT(dt)[(country == c) & (reporting == r) & (group == g), .(zscore = (nbc - pnb)/sqrt(Vexcess)), keyby = ISOweek]

  dt$wk = as.numeric(as.factor(dt$ISOweek))
  graph <- ggplot(dt, aes(x = wk)) +
    geom_line(aes(y = zscore), linetype="solid", color="darkgreen", size = 1) +
    geom_hline(yintercept = -2, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0, linetype = "solid", color = "red") +
    geom_hline(yintercept = 2, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 4, linetype = "dashed", color = "blue") +
    ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(name = "ISOWeek",
                       labels = dt[seq(min(dt$wk), max(dt$wk), by = 6),]$ISOweek,
                       breaks = seq(min(dt$wk), max(dt$wk), by = 6)) +
    scale_y_continuous(name = "Z-score") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
    labs(caption = paste('EuroMOMO:', r))

  return(graph)
}
