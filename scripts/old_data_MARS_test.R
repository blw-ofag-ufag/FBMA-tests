#title: old_data_MARS_test
#author: Gendre Matthieu

source('../ressources/utils.R')

#load old data
old_df <- readxl::read_xlsx('../datasets/old_data_example.xlsx')

old_df[old_df =='-'] <- NA
old_df <- as.data.frame(lapply(old_df, as.numeric))
old_df$Datum <- as.character(old_df$Datum)

#load MARS data
MARS_data <- read.table('../datasets/MARS_data_example.csv',
                        sep = ';',
                        header = T)

#Excel names = MARS names
dico_products_names <- c('Total_Konsumeier' = 'Total, Konsumeier',	
                         'Total_CH' = 'Total, CH',	
                         'Boden_Schaleneier_CH' = 'Boden, Schaleneier, CH',
                         'Total_CH_Import' = 'Total, CH + Import')

#format Date_ID columns (comment the line if you are testing monthly or daily data)
MARS_data$Date_ID <- gsub('0101', '', MARS_data$Date_ID)

#create a directory
dir.create('../test_outputs', showWarnings = FALSE)

#test MARS data
data_tester_Excel_MARS(old_df, 
                       MARS_data, 
                       dico_products_names, 
                       '../test_outputs/Test', 
                       nbr_digits = 2)

