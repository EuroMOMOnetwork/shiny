# Combine EuroMOMO data and population data -------------------------------

source('R/GetData.R')
data <- GetData('2020-W01')
data <- rbind( data, GetData('2020-W02'))
data <- rbind( data, GetData('2020-W03'))
data <- rbind( data, GetData('2020-W04'))
data <- rbind( data, GetData('2020-W05'))
data <- rbind( data, GetData('2020-W06'))
data <- rbind( data, GetData('2020-W07'))
data <- rbind( data, GetData('2020-W08'))
data <- rbind( data, GetData('2020-W09'))
data <- rbind( data, GetData('2020-W10'))
data <- rbind( data, GetData('2020-W11'))
data <- rbind( data, GetData('2020-W12'))
data <- rbind( data, GetData('2020-W13'))

saveRDS(data, file = 'data/Data.RDS')

saveRDS(subset(data, country == 'Pooled'), file = 'data/PooledData.RDS')


# Fake data ------------------------------------------------------------

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

FakeData <- subset(data, country != 'Pooled')
FakeData <- merge(FakeData, cbind(country = unique(FakeData$country),
                          FakeCountry = paste0('country',
                                               sample(as.numeric(unique(factor(FakeData$country))), length(as.numeric(unique(factor(FakeData$country))))))),
              by = 'country')
FakeData$country <- FakeData$FakeCountry
FakeData$FakeCountry <- NULL
FakeData <- rbind(FakeData, subset(data, country == 'Pooled'))

FakeData$NUTS <- NA

saveRDS(FakeData, file = 'data/FakeData.RDS')


# Graph functions ---------------------------------------------------------
data <- readRDS(file = 'data/Data.RDS')
data <- readRDS(file = 'data/PooledData.RDS')
data <- readRDS(file = 'data/FakeData.RDS')

source('R/GetData.R')
data <- GetData('2020-W13')

# Number graph
source('R/NumberGraph.R')
print(NumberGraph(data, 'Norway', '2020-W13', 'All ages'))

# Z-score graph
source('R/ZscoreGraph.R')
print(ZscoreGraph(data, 'Italy', '2020-W13', 'All ages'))

# Mortality rate graph
source('R/MRGraph.R')
print(MRGraph(data, 'Pooled', '2020-W13', 'All ages'))
