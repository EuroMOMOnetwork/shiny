# Combine EuroMOMO data and population data -------------------------------

source('R/GetData.R')
data <- GetData('2019-W01')
data <- rbind( data, GetData('2019-W02'))
data <- rbind( data, GetData('2019-W03'))
data <- rbind( data, GetData('2019-W04'))
data <- rbind( data, GetData('2019-W05'))
data <- rbind( data, GetData('2019-W06'))
data <- rbind( data, GetData('2019-W07'))
data <- rbind( data, GetData('2019-W08'))
data <- rbind( data, GetData('2019-W09'))
data <- rbind( data, GetData('2019-W10'))
data <- rbind( data, GetData('2019-W11'))
data <- rbind( data, GetData('2019-W12'))

saveRDS(data, file = 'data/Data.RDS')

X <- data[, N := sum(N), keyby = .(reporting, country, group, ISOweek, nb, nbc, pnb, Vexcess, NUTS)]



# Fake country ------------------------------------------------------------
data <- merge(data, cbind(country = unique(data$country),
                          FakeCountry = paste0('country',
                                               sample(as.numeric(unique(factor(data$country))), length(as.numeric(unique(factor(data$country))))))),
              by = 'country')
data$country <- data$FakeCountry
data$FakeCountry <- NULL

saveRDS(data, file = 'data/FakeData.RDS')



# Graph functions ---------------------------------------------------------

# Number graph
source('R/NumberGraph.R')

data <- readRDS(file = 'data/FakeData.RDS')
print(NumberGraph(data, 'England', '2019-W01', '15to64'))


# Z-score graph
source('R/ZscoreGraph.R')

data <- readRDS(file = 'data/FakeData.RDS')
print(ZscoreGraph(data, 'country3', '2019-W41', '15to64'))
