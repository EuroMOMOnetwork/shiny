# Combine EuroMOMO data and population data -------------------------------

source('R/GetData.R')
data <- GetData('2020-W06')
data <- cbind( data, GetData('2020-W07'))
data <- cbind( data, GetData('2020-W08'))
data <- cbind( data, GetData('2020-W09'))
data <- cbind( data, GetData('2020-W10'))
data <- cbind( data, GetData('2020-W11'))
data <- cbind( data, GetData('2020-W12'))

saveRDS(data, file = 'data/Data.RDS')

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
print(NumberGraph(data, 'country3', '2019-W41', '15to64'))


# Z-score graph
source('R/ZscoreGraph.R')

data <- readRDS(file = 'data/FakeData.RDS')
print(ZscoreGraph(data, 'country3', '2019-W41', '15to64'))
