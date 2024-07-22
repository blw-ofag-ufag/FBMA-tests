#title: utils
#author: Gendre Matthieu

#load libraries
library(readxl)
library(dplyr)
library(stringr)
library(httr)

#' @title Data tester Excel vs MARS
#'
#' @description This function compares the value for each products according to there name and date between two tables
#'
#' @usage data_tester_Excel_MARS(old_df, new_df, dico_products_names, filename, nbr_digits)
#'
#' @param old_df a data.frame. The first column must be the Datum as character and the other columns contain the data for each product.
#' @param new_df a data.frame. The data.frame must contain this columns: 
#'  'KeyIndicator',
#'  'Product_BK'          
#'  'Product_Name'         
#'  'Date_ID'              
#'  'Datetype'             
#'  'Quantity'             
#'  'ProductProperties_ID'
#'  'ProductOrigin_ID'     
#'  'ProductionSystem_ID'  
#'  'SalesRegion_ID'       
#'  'Usage_ID'             
#'  'Currency_ID'          
#'  'ValueChain_ID'        
#'  'ValueChain_Detail_ID'
#'  'CostComponent_ID'     
#'  'KeyIndicatorType_ID'  
#'  'Product_ID'           
#'  'ProductSubgroup_ID'   
#'  'ProductGroup_ID'      
#'  'Market_ID'  
#' @param dico_products_names character. Contains the correspondence of each product name between the 2 dataframes. 
#' @param filename string Path + file name 
#' @param nbr_digits integer. Number of digits for the rounding 
#' @return A .txt file containing the results of the testing for each product
data_tester_Excel_MARS <- function(old_df, new_df, dico_products_names, filename, nbr_digits) {
  
  # Create output file
  output_file <- paste0(filename, '.txt')
  f <- file(output_file, open = 'w')
  writeLines('****** Test Start ******', f)
  
  # Loop through each product column in old_df
  for (col_id in 2:ncol(old_df)) {
    product_name <- colnames(old_df)[col_id]
    writeLines(sprintf('\n%s', product_name), f)
    writeLines('------------------------', f)
    
    # Subset old_df for current product
    old_subdata <- old_df[, c(1, col_id)]
    colnames(old_subdata) <- c('Datum', 'OldValue')
    
    if (!product_name %in% names(dico_products_names)) {
      writeLines('No data in MARS\n', f)
      next
    }
    
    # Get corresponding product name in MARS
    mars_product_name <- dico_products_names[[product_name]]
    
    # Subset new_df for the current product
    new_subdata <- new_df %>%
      filter(Product_Name == mars_product_name) %>%
      select(KeyIndicator, Date_ID)
    
    if (nrow(new_subdata) == 0) {
      writeLines('No data in Excel\n', f)
      next
    }
    
    # Join old and new data on the date
    combined_data <- old_subdata %>%
      left_join(new_subdata, by = c('Datum' = 'Date_ID'))
    
    # Check values for each date
    combined_data <- combined_data %>%
      mutate(
        NewValue = as.numeric(KeyIndicator),
        Diff = round(OldValue, nbr_digits) - NewValue,
        Status = case_when(
          is.na(OldValue) & is.na(NewValue) ~ 'No data in Excel and MARS, please CHECK',
          is.na(OldValue) & !is.na(NewValue) ~ 'No data in Excel but data in MARS, please CHECK',
          !is.na(OldValue) & is.na(NewValue) ~ 'Data in Excel but no data in MARS, please CHECK',
          Diff == 0 ~ '',
          TRUE ~ sprintf('No match: %f // Excel value: %f // MARS value: %f', Diff, OldValue, NewValue)
        )
      )
    
      # Write mismatches to the file
      mismatches <- combined_data %>%
        filter(Status != '') %>%
        select(Datum, Status)
      
      if(nrow(mismatches) > 0){
        apply(mismatches, 1, function(row) {
        writeLines(sprintf('%s: %s', row['Datum'], row['Status']), f)
      })
    }
  }
  
  writeLines('\n****** Test Completed ******', f)
  close(f)
}


#' @title Data tester MARS vs LINDAS and Datenfilter
#'
#' @description This function compares the value for each product according to their name and date between two tables
#'
#' @usage data_tester_MARS_LINDAS_DF(MARS_df, LINDAS_df, filename)
#'
#' @param MARS_df a data.frame. The first column must be the Datum as character, then Produkt and Value 
#' @param LINDAS_df a data.frame. The data.frame must contain these columns: Datum, Produkt and Value 
#' @param filename string. Path + file name 
#' @param nbr_digits integer. Number of digits for the rounding 
#' @return A .txt file containing the results of the testing for each product
data_tester_MARS_LINDAS_DF <- function(MARS_df, LINDAS_df, filename){

  #keyindicator
  keyindicator <- unique(LINDAS_df$Kennzahl)

  #open file
  filename <- paste0(filename, '.txt')
  f = file(filename, open = 'w')

  writeLines('******Test start******', f)

  MARS_colnames <- colnames(MARS_df)
  #check if same columns in LINDAS_df and MARS_df
  dif <- colnames(MARS_df)[!(colnames(MARS_df) %in% colnames(LINDAS_df))]
  if(length(dif) > 0){
    writeLines(sprintf('The columns %s not in LINDAS', dif), f)
    MARS_colnames <- colnames(MARS_df)[!(colnames(MARS_df) %in% dif)]

  }

  #select columns and order columns according to the columns of MARS_df
  LINDAS_df <- LINDAS_df %>% select(MARS_colnames)
  MARS_df <- MARS_df %>% select(MARS_colnames)

  #remove whitespace
  MARS_df <- MARS_df %>% mutate(across(2,stringr::str_squish))
  LINDAS_df <- LINDAS_df %>% mutate(across(2,stringr::str_squish))

  products_MARS <- unique(MARS_df$Produkt)
  products_LINDAS <- unique(LINDAS_df$Produkt)

  #check values
  for(prod in 1:length(products_MARS)){
    product_name <- products_MARS[prod]
    writeLines(sprintf('\n%s', product_name), f)
    writeLines('------------------------', f)
    MARS_subdata <- MARS_df %>% filter(Produkt == product_name)


    if(!(product_name %in% products_LINDAS)){
      writeLines('No data for this product in LINDAS\n', f)
      next
    }

    LINDAS_subdata <- LINDAS_df %>%
      filter(Produkt == product_name)


    if(nrow(LINDAS_subdata) == 0){
      writeLines('No data for this product in MARS \n', f)
      next
    }

    else{
      #check by dates each values
      for(d in 1:nrow(MARS_subdata)){

        several_rows <- 0 #for writing in the file when data not identical

        datum <- MARS_subdata$Datum[d]

        MARS_subdata_date <- MARS_subdata %>%
          filter(Datum == datum)

        LINDAS_subdata_date <- LINDAS_subdata %>%
          filter(Datum == datum)

        MARS_value <- MARS_subdata[which(MARS_subdata$Datum == datum), 'KeyIndicator']
        LINDAS_value <- as.vector(LINDAS_subdata[which(LINDAS_subdata$Datum == datum), 'KeyIndicator'])
        val <- '0'

        if(sum(is.na(MARS_value) & length(LINDAS_value) == 0) == 2){
          next
          #val <- 'not present in MARS and LINDAS'
        }

        else if(sum(is.na(MARS_value) & length(LINDAS_value) != 0) == 2){
          val <- 'not present in MARS but present in LINDAS, please CHECK'
        }

        else if(sum(!(is.na(MARS_value)) & length(LINDAS_value) == 0) == 2){
          val <- 'present in MARS but not present in LINDAS, please CHECK'
        }

        else if(length(MARS_value) > 1){
          val <- 'duplicates in MARS'
        }

        else if(length(LINDAS_value) > 1){
          val <- 'duplicates in LINDAS'
        }

        else{

          comparison <- compare_rows(MARS_subdata_date, LINDAS_subdata_date, 'MARS', 'LINDAS')

          if(is.character(comparison)){
            next
          }

          else{
            several_rows <- 1
          }
        }
        #print(sprintf('%s: %s', datum, val))
        if(several_rows == 0){
          writeLines(sprintf('%s: %s', datum, val), f)
        }

        if(several_rows == 1){
          cat(sprintf('%s:', datum), file = f)
          for(lines in 1:nrow(comparison)){
            cat(as.character(comparison[lines, ]), file = f, append = TRUE)
            cat('\n', file = f, append = TRUE)
          }
        }

      }

    }

  }
  writeLines('\n******Test completed******', f)
  close(f)
}



#Test MARS and PowerBI data
data_tester_MARS_PowerBI <- function(MARS_df, PowerBI_df, filename){
  
  #keyindicator
  keyIndicator <- unique(PowerBI_df$keyindicatortype_name)
  
  #open file
  filename <- paste0(filename, '.txt')
  f = file(filename, open = 'w')
  
  writeLines('******Test start******', f)
  
  MARS_colnames <- colnames(MARS_df)
  #check if same columns in PowerBI_df and MARS_df
  dif <- colnames(MARS_df)[!(colnames(MARS_df) %in% colnames(PowerBI_df))]
  if(length(dif) > 0){
    writeLines(sprintf('The columns %s not in LINDAS', dif), f)
    MARS_colnames <- colnames(MARS_df)[!(colnames(MARS_df) %in% dif)]
    
  }
  
  #select columns and order columns according to the columns of MARS_df
  PowerBI_df <- PowerBI_df %>% select(MARS_colnames)
  MARS_df <- MARS_df %>% select(MARS_colnames)
  
  #remove whitespace
  MARS_df <- MARS_df %>% mutate(across(2,stringr::str_squish))
  PowerBI_df <- PowerBI_df %>% mutate(across(2,stringr::str_squish))
  
  products_MARS <- unique(MARS_df$product_name)
  products_LINDAS <- unique(PowerBI_df$product_name)
  
  #check values
  for(prod in 1:length(products_MARS)){
    product_n <- products_MARS[prod]
    writeLines(sprintf('\n%s', product_n), f)
    writeLines('------------------------', f)
    MARS_subdata <- MARS_df %>% filter(product_name == product_n)
    
    
    if(!(product_n %in% products_LINDAS)){
      writeLines('No data for this product in PowerBI\n', f)
      next
    }
    
    LINDAS_subdata <- PowerBI_df %>%
      filter(product_name == product_n)
    
    
    if(nrow(LINDAS_subdata) == 0){
      writeLines('No data for this product in MARS \n', f)
      next
    }
    
    else{
      #check by dates each values
      for(d in 1:nrow(MARS_subdata)){
        
        several_rows <- 0 #for writing in the file when data not identical
        
        datum <- MARS_subdata$date_id[d]
        
        MARS_subdata_date <- MARS_subdata %>%
          filter(date_id == datum)
        
        LINDAS_subdata_date <- LINDAS_subdata %>%
          filter(date_id == datum)
        
        
        MARS_value <- MARS_subdata[which(MARS_subdata$date_id == datum), 'keyindicator']
        LINDAS_value <- as.vector(LINDAS_subdata[which(LINDAS_subdata$date_id == datum), 'keyindicator'])
        val <- '0'
        
        if(sum(is.na(MARS_value) & length(LINDAS_value) == 0)==2){
          next
          #val <- 'not present in PowerBI and MARS'
        }
        
        else if(sum(is.na(MARS_value) & length(LINDAS_value) != 0)==2){
          val <- 'not present in MARS but present in PowerBI, please CHECK'
        }
        
        else if(sum(!(is.na(MARS_value)) & length(LINDAS_value) == 0)==2){
          val <- 'present in MARS but not present in PowerBI, please CHECK'
        }
        
        else if(length(MARS_value) > 1){
          val <- 'duplicates in MARS'
        }
        
        else if(length(LINDAS_value) > 1){
          val <- 'duplicates in PowerBI'
        } 
        
        else{
          
          comparison <- compare_rows(MARS_subdata_date, LINDAS_subdata_date, 'MARS', 'PowerBI')
          
          if(is.character(comparison)){
            next
          }
          
          else{
            several_rows <- 1
          }
        }
        #print(sprintf('%s: %s', datum, val))
        if(several_rows == 0){
          writeLines(sprintf('%s: %s', datum, val), f)
        }
        
        if(several_rows == 1){
          cat(sprintf('%s:', datum), file = f)
          for(lines in 1:nrow(comparison)){
            cat(as.character(comparison[lines, ]), file = f, append = TRUE)
            cat('\n', file = f, append = TRUE)
          }
        }
        
      }
      
    }
    
  }
  writeLines('\n******Test completed******', f)
  close(f)
}



#translation columns names EN -> DE
translation_EN_DE <- c('date_id' = 'Datum',
                       'market' = 'Markt',
                       'market_name' = 'Markt',
                       'productgroup_name' = 'Produktgruppe',
                       'productsubgroup_name' = 'Produktuntergruppe',
                       'product_name' = 'Produkt',
                       'keyindicatortype_name' = 'Kennzahl',  
                       'unit_name' = 'Einheit',              
                       'currency_name' = 'Währung',
                       'costcomponent_name' = 'Kostenkomponente',
                       'salesregion_name' = 'Verkaufsregion',
                       'valuechain_name'  = 'Wertschöpfungsstufe',
                       'valuechain_detail_name' = 'Wertschöpfungsstufe Detail',
                       'foreigntrade_name' = 'Aussenhandel',
                       'productorigin_name' = 'Produktherkunft',
                       'productionsystem_name' = 'Produktionssystem',
                       'productproperties_name' = 'Produkteigenschaften',
                       'usage_name' = 'Verwendungsart',
                       'datamethod_name' = 'Datenart',
                       'datasource_name' = 'Datenquelle',
                       'currency_name_abk' = 'Währung_Abk',
                       'datetype' = 'Datentype',
                       'measure' = 'keyindicator')


#' @title Compare rows function
#'
#' @description Compare rows for differences
#'
#' @param row1 a data.frame row
#' @param row2 a data.frame row
#' @param source1 string. Source name for the first dataframe
#' @param source2 string. Source name for the second dataframe
#' @return TRUE if rows match, FALSE otherwise
compare_rows <- function(df1, df2, df1_name, df2_name) {
  
  differences <- df1[1, ] != df2[1, ]
  diff_columns <- names(df1)[which(differences)]
  
  if (length(diff_columns) == 0) {
    return('Rows are identical')
  }
  
  result <- data.frame(
    column = diff_columns,
    V1 = unlist(df1[1, diff_columns], use.names = FALSE),
    V2 = unlist(df2[1, diff_columns], use.names = FALSE),
    stringsAsFactors = FALSE
  )
  
  names(result)[c(2,3)] <- c(df1_name, df2_name)
  
  return(result)
}



#SPARQL
sparql <- function(path = NULL, url , proxy_url = NULL, port_nbr = 8080) {
  
  # define the SPARQL query
  query <- path |> readLines() |> paste(collapse = '\n')
  
  # make the query
  response <- httr::POST(url,
                         add_headers('Accept' = 'text/csv'),
                         content_type('application/x-www-form-urlencoded; charset=UTF-8'),
                         body = list(query = query),
                         encode = 'form',
                         use_proxy(proxy_url, port = port_nbr))
  
  # return warning if query failed
  if(response$status_code >= 300) {
    warning('Query failed')
    return(NULL)
  }
  
  # read and parse the respnose (it's XML)
  result <- response |> httr::content(encoding = 'UTF-8')
  
  # convert results to data frame
  data <- as.data.frame(result)
  
  # return data frame
  return(data)
}

