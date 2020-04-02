#' EuroMOMO data.
#' Extract from database with reported data from countries.
#' Calculate pooled estimates
#' Note: Demand access to the EuroMOMO database
#'
#' @param week ISOweek of reporting.
#' @import data.table
#' @import ISOweek
#' @import RODBC
#' @import stringr
#' @return data table with:
#'  country - country name
#'  group - age group
#'  nb - registered deaths
#'  nbc - delay-adjusted deaths
#'  pnb - baseline
#'  Vexcess - Variance of (nbc - pnb)
GetEuroMOMOdata <- function(week) {
  library(data.table)
  # library(ISOweek)
  # library(RODBC)
  # library(stringr)
  
  # week <- '2020-W13'
  
  # Week of aggregation
  Aweek <- ISOweek::ISOweek(as.Date(ISOweek::ISOweek2date(paste0(week, '-7')) + 3))
  
  # EuroMOMO data from database 
  con <- RODBC::odbcConnect("euromomo", readOnlyOptimize = TRUE)
  EuroMOMOdata <- RODBC::sqlQuery(con, paste0("
      set transaction isolation level read uncommitted;
      set deadlock_priority 5;
      select country, [group], yodi, wodi, nb, nbc, pnb, zscore from IB_EuroMOMO.dbo.Record with(nolock)
      where (yoai = ", substr(Aweek, 1, 4), ") and (woai = ", substr(Aweek, 7, 8), ") and (yodi > ", substr(Aweek, 1, 4), " - 6) and
      (nbc is not NULL) and (pnb is not NULL) and (substring(country,1,13) <> 'All countries') and (country <> 'Europe') and
      ([group] in('0to4', '15to64', '5to14', '65P', '65to74', '75to84', '85P', 'Total'))
      order by country, [group], yodi, wodi
     "), stringsAsFactors = FALSE, as.is = TRUE)
  RODBC::odbcClose(con)
  rm(con)
  
  EuroMOMOdata$ISOweek <- paste0(EuroMOMOdata$yodi, '-W', sprintf('%02d', EuroMOMOdata$wodi))
  EuroMOMOdata <- setDT(EuroMOMOdata)[, .(country = stringr::str_to_title(country),
                                          group, ISOweek,
                                          nb = as.numeric(nb),
                                          nbc = as.numeric(nbc),
                                          pnb = as.numeric(pnb),
                                          Vexcess = (((as.numeric(nbc)^(2/3)-as.numeric(pnb)^(2/3))/as.numeric(zscore))^2)/(as.numeric(pnb)^(2*2/3-2)))
                                      ]
  # Pooling
  pooled <- NULL
  for (g in unique(EuroMOMOdata$group)) {
    d <- subset(EuroMOMOdata, group == g)
    # Use ISOweek where all countries have data
    d <- subset(cbind(d, min = max(setDT(d)[, .(min = min(ISOweek)), keyby = country]$min)), ISOweek >= min)
    pooled <- rbind(pooled,
                    setDT(d)[, .(nb = sum(nb),
                                 nbc = sum(nbc),
                                 pnb = sum(pnb),
                                 Vexcess = sum(Vexcess)),
                             keyby = .(group, ISOweek)]
    )
  }
  
  # Combine country and pooled data
  EuroMOMOdata <- cbind(reporting = week, rbind(EuroMOMOdata, cbind(country = "Pooled", pooled)))
  
  return(EuroMOMOdata)
}
