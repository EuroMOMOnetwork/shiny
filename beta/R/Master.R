# Combine EuroMOMO data and population data -------------------------------

source('R/GetData.R')
data <- GetData('2019-W02')
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
data <- rbind( data, GetData('2019-W13'))

# saveRDS(data, file = 'data/Data.RDS')

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

# saveRDS(data, file = 'data/Data.RDS')

# Fake country ------------------------------------------------------------
FakeData <- subset(data, country != 'Pooled')
FakeData <- merge(FakeData, cbind(country = unique(FakeData$country),
                          FakeCountry = paste0('country',
                                               sample(as.numeric(unique(factor(FakeData$country))), length(as.numeric(unique(factor(FakeData$country))))))),
              by = 'country')
FakeData$country <- FakeData$FakeCountry
FakeData$FakeCountry <- NULL
FakeData <- rbind(FakeData, subset(data, country == 'Pooled'))

saveRDS(FakeData, file = 'data/FakeData.RDS')


# Graph functions ---------------------------------------------------------

# Number graph
source('R/NumberGraph.R')

data <- readRDS(file = 'data/FakeData.RDS')
print(NumberGraph(data, 'England', '2019-W01', '15to64'))

# Z-score graph
source('R/ZscoreGraph.R')

data <- readRDS(file = 'data/FakeData.RDS')
print(ZscoreGraph(data, 'country3', '2019-W41', '15to64'))

# Mortality rate graph
source('R/MRGraph.R')

data <- readRDS(file = 'data/FakeData.RDS')
print(MRGraph(data, 'country3', '2019-W05', '15to64'))
