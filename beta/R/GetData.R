#' EuroMOMO data plus population data on NUTS1 level.
#'
#' Use the functions:
#' - R/GetEuroMOMOdata.R
#' - R/GetPopData.R
#' and the csv-data with country-name and NUTS1 codes
#' - data/CountryNUTS.csv
#' 
#' @param week ISOweek of reporting.
#' @import data.table
#' @import ISOweek
#' @import sqldf
#' @return data table with:
#'  country - country name
#'  group - age group
#'  nb - registered deaths
#'  nbc - delay-adjusted deaths
#'  pnb - baseline
#'  Vexcess - Variance of (nbc - pnb)
#'  N - population
#'  NUTS - NUTS1 code
#'  NOTE: England consists of more NUTS1 codes
#' @export
GetData <- function(ReportingWeek) {
  library(data.table)
  # library(ISOweek)
  # library(sqldf)
  
  # EuroMOMO data
  source('R/GetEuroMOMOdata.R')
  data <- GetEuroMOMOdata(ReportingWeek)
  
  # NUTS for participating countries
  CountryNUTS <- read.csv("data/CountryNUTS.csv", header = TRUE, sep =";", as.is = TRUE, stringsAsFactors = FALSE)
  
  # Population data from EuroSTAT by country, NUTS and age
  EndYear <- as.numeric(substr(ISOweek::ISOweek(as.Date(ISOweek::ISOweek2date(paste0(ReportingWeek, '-4')))), 1, 4)) + 1
  StartYear <- as.numeric(substr(ISOweek::ISOweek(as.Date(ISOweek::ISOweek2date(paste0(ReportingWeek, '-4')))), 1, 4)) - 6
  source('R/GetPopData.R')
  pop_data <- merge(GetPopData(StartYear, EndYear, CountryNUTS), CountryNUTS, by = 'NUTS', all.x = TRUE, allow.cartesian = TRUE)

  data$group <- gsub('P', 'to100', data$group, ignore.case = TRUE)
  data <- data[, c("StartAge", "EndAge") := tstrsplit(group, "to", fixed=TRUE)][]
  data <- sqldf::sqldf("select a.reporting, a. country, a.[group], a.ISOweek, a.nb, a.nbc, a.pnb, a.Vexcess, sum(b.N) as N, b.NUTS from
                        data as a
                        left join
                        pop_data as b
                        on (a.country = b.NUTS_NAME) and (a.ISOweek = b.ISOweek) and
                        (((a.StartAge <= b.age) and (b.age <= a.EndAge)) or
                        ((a.[group] = 'Total') and (b.age = 999)))
                        group by a.reporting, a. country, a.[group], a.ISOweek, a.nb, a.nbc, a.pnb, a.Vexcess, b.NUTS
                      ")

  # Pooled population  
  data <- merge(setDT(data), setDT(data)[ country != 'Pooled', .(N = sum(N, na.rm = TRUE)),
                                          keyby = .(reporting, group, ISOweek)], by = c('reporting', 'group', 'ISOweek'), all.x = TRUE)
  data$N <- pmax(data$N.x, data$N.y, na.rm = TRUE)
  data[, c('N.x', 'N.y')] <- NULL
  
  # Sum population data for countries with more NUTS-codes (England)
  setDT(data)[, N := sum(N), keyby = .(country, reporting, group, ISOweek)]
  
  # Not full population
  data[(data$country == "Italy"), "N"] <- 0.15*data[(data$country == "Italy"), "N"]
  data[(data$country == "Spain"), "N"] <- 0.92*data[(data$country == "Spain"), "N"]
  
  return(data)
}
