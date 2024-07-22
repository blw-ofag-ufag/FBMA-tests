#title: MARS_Datenfilter_test
#author: Gendre Matthieu

source('ressources/utils.R')

#load libraries
library(tidyr)

#read all Datenfilter datasets and merge in one.
file_list_LD <- list.files("Datenfilter_Datasets/",
                           pattern=".xlsx",
                           full.names = T)
keyindicators <- c('Preis',
                   'Beitrag',
                   'Prozentsatz',
                   'Menge')

for (f in 1:length(file_list_LD)){
  
  df <- readxl::read_xlsx(file_list_LD[f])
  print(file_list_LD[f])
  names(df)[which(names(df) %in% keyindicators)] <- 'KeyIndicator'
  
  if(f == 1){
    combined_files_DF <- df
  }
  
  else{
    combined_files_DF <- rbind(combined_files_DF, df)
  }
}

#Year data index
index_year <- which(!grepl('-', combined_files_DF$Datum))

#create a variable called datetype
combined_files_DF$datetype <- 'Month'
combined_files_DF$datetype[index_year] <- 'Year'

addCharacter <- combined_files_DF$Datum[index_year]
addCharacter <- paste0('01-', addCharacter)
combined_files_DF$Datum[which(!grepl('-', combined_files_DF$Datum))] <- addCharacter
combined_files_DF$Datum <- paste0('01-', combined_files_DF$Datum)

combined_files_DF <- combined_files_DF %>%
  tidyr::separate(Datum, c('Day','Month', 'Year'))

combined_files_DF$Datum <- paste0(combined_files_DF$Year,
                                      combined_files_DF$Month, 
                                      combined_files_DF$Day)

#delete columns Month, Day, Year
combined_files_DF <- combined_files_DF %>% select(-c('Month', 'Day', 'Year'))

#read all MARS dataset and merge in one. 
MARS_data <- read.table('../datasets/MARS_example_names.csv',
                        sep = ';', 
                        header = T)

#lowercase
colnames(MARS_data) <- tolower(colnames(MARS_data))
colnames(combined_files_DF) <- tolower(colnames(combined_files_DF))

#translate colnames from EN to DE
names(MARS_data) <- ifelse(names(MARS_data) %in% names(translation_EN_DE), translation_EN_DE[names(MARS_data)], names(MARS_data))

#sort by Produkt name and date
combined_files_DF <- combined_files_DF %>% arrange('Datum', 'Produkt')
MARS_data <- MARS_data %>% arrange('Datum', 'Produkt')

#Merge ProduktName and KeyIndicator_Name or Name
MARS_data$Produkt <- paste(MARS_data$Produkt,
                           MARS_data$Kennzahl,
                           MARS_data$Wertschöpfungsstufe,
                           MARS_data$Datetype,
                           sep = '//')

combined_files_DF$Produkt <- paste(combined_files_DF$Produkt,
                                   combined_files_DF$Kennzahl,
                                   combined_files_DF$Wertschöpfungsstufe,
                                   combined_files_DF$datetype,
                                   sep = '//')

#create a directory
dir.create('../test_outputs', showWarnings = FALSE)

#test Datenfilter data
data_tester_MARS_LINDAS_DF(MARS_data,
                           combined_files_DF,
                           '../test_outputs/MARS_Datenfilter_test')
