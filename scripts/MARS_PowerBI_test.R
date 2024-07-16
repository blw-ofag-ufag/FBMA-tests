#title: eggs_Markt_Struktur_test
#author: Gendre Matthieu

setwd('data_testing/')
source('../R_scripts/utils.R')
library(readxl)
library(dplyr)

#load old data
#PowerBI_Daten <- readxl::read_xlsx('PowerBI_Data/Eier_Prokopf.xlsx')
#PowerBI_Daten <- readxl::read_xlsx('PowerBI_Data/MEM_total_contribution.xlsx')
PowerBI_Daten <- readxl::read_xlsx('PowerBI_Data/Eier_Daten_all_DM.xlsx')
colnames(PowerBI_Daten) <- gsub("\\[", "", colnames(PowerBI_Daten))
colnames(PowerBI_Daten) <- gsub("\\]", "", colnames(PowerBI_Daten))
PowerBI_Daten$Date <- gsub("-", "", PowerBI_Daten$Date)

#load MARS data
#MARS_Daten <- read.table('MARS_Data/proKopf/proKopf_data_eggs.csv', sep = ';', header = T)
#MARS_Daten <- read.table('MARS_Data/MEM/MEM_total_contribution_per_eggs.csv', sep = ';', header = T)
MARS_Daten <- read.table('MARS_Data/LINDAS_testing/production_quantities_names.csv',sep = ';', header = T)
#MARS_Daten <- read.table('MARS_Data/LINDAS_testing/Eggs_MARS_allData_names.csv', sep = ';', header = T)


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

unique(PowerBI_Daten$product_name)

colnames(PowerBI_Daten)
colnames(MARS_Daten)

str(PowerBI_Daten)
str(MARS_Daten)

#sort by Produkt name and date
PowerBI_Daten <- PowerBI_Daten %>% arrange('date_ID', 'product_name')
MARS_Daten <- MARS_Daten %>% arrange('date_ID', 'product_name')

data_tester_MARS_PowerBI(MARS_Daten, PowerBI_Daten, 'test_outputs/PowerBI_pm_year')
