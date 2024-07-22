#title: MARS_PowerBI_test
#author: Gendre Matthieu

source('ressources/utils.R')
library(readxl)

#load old data
PowerBI_Daten <- readxl::read_xlsx('../datasets/PowerBI_example.xlsx')
colnames(PowerBI_Daten) <- gsub("\\[", "", colnames(PowerBI_Daten))
colnames(PowerBI_Daten) <- gsub("\\]", "", colnames(PowerBI_Daten))
PowerBI_Daten$Date <- gsub("-", "", PowerBI_Daten$Date)

#load MARS data
MARS_Daten <- read.table('../datasets/MARS_data_example_names.csv',
                         sep = ';', 
                         header = T)

#Adapt some columns names
colnames(PowerBI_Daten)[which(colnames(PowerBI_Daten) == 'KEYINDICATOR')] <- 'KeyIndicator'
colnames(PowerBI_Daten)[which(colnames(PowerBI_Daten) == 'QUANTITY')] <- 'Quantity'
colnames(PowerBI_Daten)[which(colnames(PowerBI_Daten) == 'Date')] <- 'Date_ID'
colnames(PowerBI_Daten) <- tolower(colnames(PowerBI_Daten))
colnames(MARS_Daten) <- tolower(colnames(MARS_Daten))

#Select columns
PowerBI_Daten <- PowerBI_Daten %>% select(colnames(MARS_Daten)[which(colnames(MARS_Daten) %in% colnames(PowerBI_Daten))])
MARS_Daten <- MARS_Daten %>% select(colnames(MARS_Daten)[which(colnames(MARS_Daten) %in% colnames(PowerBI_Daten))])

MARS_Daten <- MARS_Daten %>%
  mutate_if(sapply(MARS_Daten, is.integer), as.character)

PowerBI_Daten <- PowerBI_Daten %>%
  mutate(product_name = as.character(product_name))

#Merge ProduktName and KeyIndicator_Name or Name
MARS_Daten$product_name <- paste(MARS_Daten$product_name,
                                 MARS_Daten$keyindicatortype_name,
                                 MARS_Daten$valuechain_name,
                                 MARS_Daten$datetype,
                                 sep = '//')

PowerBI_Daten$product_name <- paste(PowerBI_Daten$product_name,
                                    PowerBI_Daten$keyindicatortype_name,
                                    PowerBI_Daten$valuechain_name,
                                    PowerBI_Daten$datetype,
                                    sep = '//')


#sort by Produkt name and date
PowerBI_Daten <- PowerBI_Daten %>% arrange('date_ID', 'product_name')
MARS_Daten <- MARS_Daten %>% arrange('date_ID', 'product_name')

#create a directory
dir.create('../test_outputs', showWarnings = FALSE)

#test PowerBI data
data_tester_MARS_PowerBI(MARS_Daten, 
                         PowerBI_Daten, 
                         '../test_outputs/PowerBI_pm_year')
