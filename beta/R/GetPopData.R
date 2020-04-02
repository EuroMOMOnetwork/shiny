#' Population data by NUTS2 and ISOweek from EuroSTAT.
#'
#' @param StartYear Data from 1. January this year.
#' @param EndYear Data to 1. January this year.
#' @param NUTSdata Data table with NUTS-codes to have population by (optional). Else all NUTS-codes (0-2) available.
#' @import data.table
#' @import eurostat
#' @import dplyr
#' @import ISOweek
#' @return data table with:
#'  NUTS - NUTS0-2 code
#'  age - age 0-99 and total (999)
#'  ISOweek - time variable
#'  N - population number
#' @export
GetPopData <- function(StartYear, EndYear, NUTSdata = NULL) {
  library(data.table)
  # library(eurostat)
  # library(dplyr)
  # library(ISOweek)
  
  # NUTSdata <- CountryNUTS
  # NUTSdata <- NULL
  # StartYear <- 2015
  # EndYear <- 2021
  
  if (is.null(NUTSdata)) {
    pop_data <- setDT(eurostat::get_eurostat("demo_r_d2jan", time_format = "num"))[(time >= StartYear), .(values = mean(values)),
                                                                                   .(sex = as.character(sex), age = as.character(age), geo = as.character(geo), time)]
  } else {
    pop_data <- setDT(eurostat::get_eurostat("demo_r_d2jan", time_format = "num"))[(time >= StartYear) & (geo %in% NUTSdata$NUTS), .(values = mean(values)),
                                                                                   .(sex = as.character(sex), age = as.character(age), geo = as.character(geo), time)]
  }
  pop_data[, age := dplyr::recode(age, "Y_LT1" = "Y0", "TOTAL" = "Y999")]
  pop_data <- pop_data[(nchar(geo) <= 3) & (sex == "T") & !(age %in% c("UNK", "Y_OPEN")), .(N = sum(values)),
                       keyby = .(NUTS = geo, year = time, age = as.numeric(substr(age, 2, 4)))]
  
  if (is.null(NUTSdata)) {
    pop_proj <- setDT(eurostat::get_eurostat("proj_18np", time_format = "num"))[time <= EndYear, .(values = mean(values)),
                                                                                .(sex = as.character(sex), age = as.character(age), geo = as.character(geo), time)]
  } else {
    pop_proj <- setDT(eurostat::get_eurostat("proj_18np", time_format = "num"))[(time <= EndYear) & (geo %in% NUTSdata$NUTS), .(values = mean(values)),
                                                                                .(sex = as.character(sex), age = as.character(age), geo = as.character(geo), time)]
  }
  pop_proj[, age := dplyr::recode(age, "Y_LT1" = "Y0", "Y_GE100" = "Y99", "TOTAL" = "Y999")]
  pop_proj <- pop_proj[(sex == "T"), .(N = sum(values)),
                       keyby = .(NUTS = geo, year = time, age = as.numeric(substr(age, 2, 4)))]
  
  pop_data <- merge.data.table(pop_data, pop_proj, by = c('NUTS', 'year', 'age'), all = TRUE)
  pop_data$N <- ifelse(!is.na(pop_data$N.x), pop_data$N.x, pop_data$N.y)
  pop_data[, c('N.x', 'N.y')] <- NULL
  
  rm(pop_proj)

  pop_data$ISOweek <- ISOweek::ISOweek(as.Date(paste0(pop_data$year, '-01-01')))

  # Expand to cover all ISOweeks
  pop_data <- merge.data.table(pop_data,
                               expand.grid(NUTS = unique(pop_data$NUTS),
                                           age = as.numeric(levels(as.factor(pop_data$age))),
                                           ISOweek = ISOweek::ISOweek(seq(from = as.Date(paste0(min(pop_data$year), '-01-01')),
                                                                          to = as.Date(paste0(max(pop_data$year), '-01-01')), by="week")),
                                           stringsAsFactors = FALSE),
                               by = c('NUTS', 'age', 'ISOweek'), all = TRUE)
  pop_data$year <- NULL
  
  # Keep only (NUTS, age) with at least two non-NA
  pop_data[order(NUTS, age) , number_of_na := sum(!is.na(N)), by = .(NUTS, age)]
  pop_data <- pop_data[number_of_na >= 2]
  pop_data$number_of_na <- NULL
  
  # Interpolate
  pop_data$wk <- as.numeric(as.factor(pop_data$ISOweek))
  pop_data[, N := approx(wk, N, rule = 2, xout = seq(min(wk), max(wk)))$y, keyby = .(NUTS, age)]
  pop_data$wk <- NULL

  return(pop_data)
}
