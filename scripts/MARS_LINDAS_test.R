#title: MARS_LINDAS_DF_test
#author: Gendre Matthieu

source('ressources/utils.R')
library(tidyr)

#Load LINDAS data
# define the SPARQL query (to run this on a federal computer, set proxy server's address)
data <- sparql(path = "ressources/sparql_query.rq",
               url = "https://int.lindas.admin.ch/query",
               proxy_url = 'http://proxy-bvcol.admin.ch')

#select data of interest
LINDAS_data <- data %>% 
  filter(grepl('Eggs', cubes)) %>%
  select(!c(cubes, observation))

#define date type column
year_index <- which(is.na(LINDAS_data$month))
LINDAS_data$datetype <- 'Month'
LINDAS_data$datetype[year_index] <- 'Year'

LINDAS_data$month[which(!is.na(LINDAS_data$month) & LINDAS_data$month < 10)] <- paste0('0', LINDAS_data$month[which(!is.na(LINDAS_data$month) & LINDAS_data$month < 10)])
LINDAS_data$month[which(is.na(LINDAS_data$month))] <- '01'
LINDAS_data$day <- '01'

LINDAS_data$date_id <- paste0(LINDAS_data$year,
                              LINDAS_data$month,
                              LINDAS_data$day)

#remove columns month, day and year
LINDAS_data <- LINDAS_data %>% select(-c('month', 'day', 'year'))


#load MARS data 
MARS_data <- read.table('../datasets/MARS_data_example_names.csv',
                        sep = ';',
                        header = T)

#lowercase columns names
colnames(MARS_data) <- tolower(colnames(MARS_data))
colnames(LINDAS_data) <- tolower(colnames(LINDAS_data))

#change columns names in MARS_data (match names in LINDAS_data)
names(LINDAS_data) <- gsub('label', '_name', names(LINDAS_data))

#translate colnames
names(MARS_data) <- ifelse(names(MARS_data) %in% names(translation_EN_DE), translation_EN_DE[names(MARS_data)], names(MARS_data))
names(LINDAS_data) <- ifelse(names(LINDAS_data) %in% names(translation_EN_DE), translation_EN_DE[names(LINDAS_data)], names(LINDAS_data))

#sort by product name and date
LINDAS_data <- LINDAS_data %>% arrange('Datum', 'Produkt')
MARS_data <- MARS_data %>% arrange('Datum', 'Produkt')

#create a new product name merging the columns keyindicator name, value chain and date type
MARS_data$Produkt <- paste(MARS_data$Produkt,
                           MARS_data$Kennzahl,
                           MARS_data$Wertschöpfungsstufe,
                           MARS_data$Datentype,
                           sep = '//')

LINDAS_data$Produkt <- paste(LINDAS_data$Produkt,
                             LINDAS_data$Kennzahl,
                             LINDAS_data$Wertschöpfungsstufe,
                             LINDAS_data$Datentype,
                             sep = '//')

#create a directory
dir.create('../test_outputs', showWarnings = FALSE)

#test LINDAS data
data_tester_MARS_LINDAS_DF(MARS_data,
                           LINDAS_data,
                           '../test_outputs/MARS_LINDAS_test')
