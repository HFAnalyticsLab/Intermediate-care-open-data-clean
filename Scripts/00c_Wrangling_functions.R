## 00.c. WRANGLING FUNCTIONS

# Much of the data used in this project is sourced from Excel sheets which are not in amenable formats for analysis. 
# The below functions automate the process of feeding in each edition of the datasets after they're downloaded by the 
# functions in script 00b, and transforming them into formats more easily analyzed. 




# Acute hospital SitRep data functions

acute_sitrep_wrangle <- function(x = NULL){
  
  
  # Create new list of files in our directory now that we've scraped for new data. RECOMMENDED TO CHECK AND MAKE SURE NO MONTHS ARE REPEATED
  refreshed_current_files <- list.files(here('Raw_data/Acute_SitRep_data'), pattern='xlsx')  
  
  print(refreshed_current_files)
  
  import_list <- refreshed_current_files[refreshed_current_files %in% c('january2023.xlsx', 'february2023.xlsx', 'march2023.xlsx', 'april2023.xlsx', 'may2023.xlsx', 'june2023.xlsx', 'july2023.xlsx', 'august2023.xlsx', 'september2023.xlsx', 
                                                                      'october2023.xlsx', 'november2023.xlsx', 'december2023.xlsx')] # Select only 2023 months
  
  
  print(import_list)
  
  # Create function that reads in all sheets of each table in an amenable format, namely:
  # Table 4: Month total number of patients discharged by their intended discharge destination
  # Table 5: Weekly snapshot average of dischargeable people per day (LoS >14 days) not discharged, by reason
  # We need to separate data into ICB and trust-level, which awkwardly are published on the same sheets of the Excel files
  
  import_sheets_function <- function(file_name, table, level){
    
    if (table == 'Table 4'){
      raw_colnames <-  c('P0 - Domestic home without reablement support', 'P0 - Other without reablement support', 'P1 - Domestic home with reablement support', 'P1 - Other with reablement support',
                         'P1 - Hotel with reablement support', 'P2 - Care Home (24hr support)', 'P2 - Designated Setting (isolation before moving to care home)', 'P2 - Hospice (24hr support)', 'P2 - Community Rehab Setting (24hr support)',
                         'P3 - Care Home (new admission, likely permanent)', 'P3b - Care Home (existing resident discharged back)', 'P3b - Designated Setting (isolation before moving to care home as a new admission)')
    } else if (table == 'Table 5'){
      raw_colnames <- c('Awaiting a medical decision/ intervention including writing the discharge summary', 'Awaiting community equipment and adaptations to housing', 'Awaiting confirmation from community hub/single point of access that referral received and actioned', 'Awaiting Diagnostic test',
                        'Awaiting medicines to take home', 'Awaiting referral to community single point of access', 'Awaiting therapy decision to discharge', 'Awaiting transport', 'Declared as not meeting the criteria to reside at morning board round and then later in the day meets the criteria to reside so discharge stopped',
                        'Homeless/no right of recourse to public funds/no place to discharge to', 'Individual/ family not in agreement with discharge plans', 'No Plan', 'Pathway 1: awaiting availability of resource for assessment and start of care at home',
                        'Pathway 2: awaiting availability of rehabilitation bed in community hospital or other bedded setting', 'Pathway 3: awaiting availability of a bed in a residential or nursing home that is likely to be a permanent placement',
                        'Remains in hospital to avoid spread of infectious disease and because there is no other suitable location to discharge to', 'Repatriation/Transfer to another acute trust for specialist treatment or ongoing treatment', 'Safeguarding concern preventing discharge or Court of Protection')
    }
    
    
    if (table == 'Table 4' & !(file_name %in% c('september2023.xlsx')) & level %in% c('ICB', 'Trust')){
      
      discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 16, na = '-', col_names = FALSE)
      
    } else if ((table == 'Table 5' & level %in% c('ICB', 'Trust')) | (table == 'Table 4' & file_name %in% c('september2023.xlsx') & level %in% c('ICB', 'Trust'))) {
      
      discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 15, na = '-', col_names = FALSE)
      
    } else if(level == 'Region' & table == 'Table 4' & !(file_name %in% c('september2023.xlsx', 'october2023.xlsx', 'november2023.xlsx', 'december2023.xlsx'))){
      discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 6, na = '-', n_max = 8, col_names = FALSE)
      
    } else if ((level == 'Region' & table == 'Table 5') | (table == 'Table 4' & file_name %in% c('september2023.xlsx', 'october2023.xlsx', 'november2023.xlsx', 'december2023.xlsx') & level == 'Region')){
      discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 5, na = '-', n_max = 8, col_names = FALSE)
      
    } else {print('Error')}
    
    if (level %in% c('ICB', 'Trust')) {
      table_colnames <- c('Region', 'Org_code', 'Org_name', raw_colnames)
    } else if (level == 'Region'){
      table_colnames <- c('Region', raw_colnames)
    }
    
    
    names(discharges_all) <- table_colnames
    
    if (level %in% c('ICB', 'Trust')){
      stop_point <- which(discharges_all$Org_name == 'Org Name' & discharges_all$Org_name == 'Org Name')
      
      if (level == 'ICB'){
        discharge_df <- discharges_all[1:(stop_point-2),]
      } else if(level == 'Trust'){
        discharge_df <- discharges_all[stop_point+1:(nrow(discharges_all)-stop_point),]
      } else{print('Error')}
    } else if (level == 'Region'){
      discharge_df <- discharges_all
    }
    
    return(discharge_df)  
    
  }
  
  
  # Import all available months for both relevant tables at ICB and trust level
  
  table4_region <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 4', level = 'Region')})
  
  table4_ICB <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 4', level = 'ICB')})
  
  table4_trust <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 4', level = 'Trust')})
  
  table5_region <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 5', level = 'Region')})
  
  table5_ICB <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 5', level = 'ICB')})
  
  table5_trust <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 5', level = 'Trust')})
  
  # Add columns to regional tables
  
  table4_region <- lapply(table4_region, function(df){
    df <- df %>%
      mutate(Org_name = Region) %>%
      mutate(Org_code = Region) %>%
      select(Region, Org_code, Org_name, 2:13)
    return(df)
  })
  
  
  table5_region <- lapply(table5_region, function(df){
    df <- df %>%
      mutate(Org_name = Region) %>%
      mutate(Org_code = Region) %>%
      select(Region, Org_code, Org_name, 2:19)
    return(df)
  })
  
  # Add all tables to list
  
  all_tables_list <- list(table4_region = table4_region, table4_ICB = table4_ICB, table4_trust = table4_trust, table5_region = table5_region, table5_ICB = table5_ICB, table5_trust = table5_trust)
  
  
  # Apply uniform month labels to all tables
  
  month_labels <- sub('.xlsx', '', import_list)
  
  for (i in 1:length(all_tables_list)){
    names(all_tables_list[[i]]) <- month_labels   
  }
  
  # Pivot and label dataframes within lists, then combine all months into single df for each table/level combo
  
  all_tables_pivoted <- lapply(1:length(all_tables_list),function(i){
    
    lapply(1:length(month_labels), function(x){
      df<- all_tables_list[[i]][[x]] %>%
        mutate(period = month_labels[[x]]) %>%
        pivot_longer(4:length(all_tables_list[[i]][[x]]), names_to = 'metric', values_to = 'value',
                     values_ptypes = list(value=double()))
    }) 
  })
  
  all_months_combined <- lapply(1:length(all_tables_pivoted), function(i){
    
    df <- do.call(rbind, all_tables_pivoted[[i]]) %>%
      mutate(date = lubridate::my(period))
  })

  
}

# Community hospital SitRep data functions

community_sitrep_wrangle <- function(x = NULL){
  
  refreshed_current_files <- list.files(here('Raw_data/Community_SitRep_data'), pattern='xlsx')  
  
  print(refreshed_current_files)
  
  import_list <- import_list <- refreshed_current_files[refreshed_current_files %in% c('june2023.xlsx', 'july2023.xlsx', 'august2023.xlsx', 'september2023.xlsx', 
                                                                                       'october2023.xlsx', 'november2023.xlsx', 'december2023.xlsx')] # Select only 2023 months
  
  
  print(import_list)
  
  # Create function that reads in all sheets of each table in an amenable format, namely:
  # Table 4: Month total number of patients discharged by their intended discharge destination
  # Table 5: Weekly snapshot average of dischargeable people per day (LoS >14 days) not discharged, by reason
  # We need to separate data into ICB and trust-level, which awkwardly are published on the same sheets of the Excel files
  
  # The structure very slightly changed for the community sitrep files in July and August 2023, hence the additional if else statements in the import function
  # Whether these changes will continue must be checked for future vintages of the data
  
  
  import_sheets_function <- function(file_name, table, level){
    
    if (table == 'Table 4' & file_name %in% c('june2023.xlsx', 'july2023.xlsx', 'august2023.xlsx', 'september2023.xlsx')){
      raw_colnames <- c('P0 - Domestic home, no new support need', 'P0 - Other, no new support need', 'P1 - Domestic home, new reablement support', 'P1 - Other, new reablement support',
                        'P1 - Hospice at Home, new care or support need', 'P2 - Hospice (24hr support)', 'P2 - Community Rehab Setting (24hr support)', 'P2 - Care Home (24hr support)', 'P2 - Other non-home (24hr support)',
                        'P3 - Care Home (new admission, likely permanent)', 'P3b - Care Home (existing resident discharged back)')
      
    } else if(table == 'Table 5' & file_name %in% c('june2023.xlsx', 'july2023.xlsx', 'august2023.xlsx', 'september2023.xlsx')){
      raw_colnames <- c('Awaiting a medical decision/ intervention including writing the discharge summary', 'Awaiting a therapy decision/ intervention to proceed with discharge, including writing onward referrals, equipment ordering',
                        'Awaiting community equipment and adaptations to housing', 'Awaiting confirmation from community Transfer of Care Hub or receiving service that referral received and actioned', 'Awaiting Diagnostic test',
                        'Awaiting medicines to take home', 'Awaiting outcome of decision for CHC funding', 'Awaiting referral to community Transfer of Care Hub or receiving service', 'Awaiting transfer back to an acute trust', 'Awaiting transport',
                        'Homeless/no right of recourse to public funds/no place to discharge to', 'Individual/ family not in agreement with discharge plans', 'No Plan', 'Pathway 1: awaiting availability of resource for assessment and start of care at home',
                        'Pathway 2: awaiting availability of rehabilitation bed in community hospital or other bedded setting', 'Pathway 3: awaiting availability of a bed in a residential or nursing home that is likely to be a permanent placement',
                        'Remains in non-specialist Community bed to avoid spread of infectious disease and because there is no other suitable location to discharge to', 'Safeguarding concern preventing discharge or Court of Protection')
      
    } else if (table == 'Table 4' & file_name == 'october2023.xlsx'){
      raw_colnames <- c('P0 - Domestic home, no new support need', 'P0 - Other, no new support need', 'P1 - Domestic home, new reablement support', 'P1 - Domestic home or setting to continute with rehabilitation, reablement, and recovery',
                        'P1 - Domestic home or setting with a new care package to manage ongoing, long term care needs', 'P1 - Other, new reablement support', 'P1 - Hotel or other temporary accomodation to continue rehabilitation, reablement and recovery', 'P1 - Hotel or other temporary accomodation with a new care package to manage ongoing, long term care needs',
                        'P1 - Hospice at Home, new care or support need', 'P1 - Hospice at home to continue with rehabilitation, reablement and recovery and end-of-life care', 'P1 - Hospice at home for End-of-Life care',
                        'P2 - Hospice (24hr support)', 'P2 - Hospice for end-of-life care', 'P2 - Community Rehab Setting (24hr support)', 'P2 - Another pathway 2 bed to continue with rehabilitation, reablement and recovery', 'P2 - Care Home (24hr support)', 'P2 - homeless hostel or extra care facility to continue with rehabilitation, reablement and recovery', 'P2 - Other non-home (24hr support)',
                        'P3 - Care Home (new admission, likely permanent)', 'P3 - Discharge from rehabilitation, reablement and recovery services as a new admission to a care home for end-of-life care', 'P3b - Care Home (existing resident discharged back)')
      
    } else if(table == 'Table 5' & file_name == 'october2023.xlsx'){
      raw_colnames <- c('Awaiting a medical decision/intervention including writing the discharge summary', 'Awaiting a therapy decision/intervention to proceed with discharge, including writing onward referrals, equipment ordering', 'Awaiting referral to care transfer hub or receiving service', 'Awaiting medicines to take home', 'Awaiting transport', 'Awaiting confirmation from care transfer hub or receiving service that referral received and actioned.', 
                        'Pathway 1: awaiting availability of resource for assessment and start of care at home', 'Pathway 1: awaiting availability of resource for assessment and start of care at home (not a continuation of rehabilitation, recovery and reablement)',
                        'Pathway 1: Awaiting availability of resource for continuation of rehabilitation, reablement and recovery at home', 'Pathway 2: awaiting availability of another rehabilitation, reablement and recovery bed in a community bedded setting', 'Pathway 3: awaiting availability of a bed in a residential or nursing home that is likely to be a permanent placement', 
                        'Pathway 3: awaiting availability of a bed in a residential or nursing home for end-of-life care.', 'Awaiting equipment and adaptations to housing', 'Individual/family not in agreement with discharge plans', 
                        'Homeless/no right of recourse to public funds/no place to discharge to/lack of housing offers when previous residence no longer suitable', 'Safeguarding concern preventing discharge or Court of Protection', 'Awaiting readmission to an acute trust', 'No Plan', 
                        'Awaiting Diagnostic test', 'Remains in non-specialist Community bed to avoid spread of infectious disease and because there is no other suitable location to discharge to', 'Awaiting outcome of decision for CHC funding')
      
    } else if (table == 'Table 4' & !(file_name %in% c('june2023.xlsx', 'july2023.xlsx', 'august2023.xlsx', 'september2023.xlsx', 'october2023.xlsx'))){
      raw_colnames <- c('P0 - Domestic home, no new support need', 'P0 - Other, no new support need', 'P1 - Domestic home or setting to continute with rehabilitation, reablement, and recovery',
                        'P1 - Domestic home or setting with a new care package to manage ongoing, long term care needs', 'P1 - Hotel or other temporary accomodation to continue rehabilitation, reablement and recovery', 'P1 - Hotel or other temporary accomodation with a new care package to manage ongoing, long term care needs',
                        'P1 - Hospice at home to continue with rehabilitation, reablement and recovery and end-of-life care', 'P1 - Hospice at home for End-of-Life care',
                        'P2 - Hospice for end-of-life care', 'P2 - Another pathway 2 bed to continue with rehabilitation, reablement and recovery', 'P2 - homeless hostel or extra care facility to continue with rehabilitation, reablement and recovery',
                        'P3 - Care Home (new admission, likely permanent)', 'P3 - Discharge from rehabilitation, reablement and recovery services as a new admission to a care home for end-of-life care', 'P3b - Care Home (existing resident discharged back)')
      
    } else if(table == 'Table 5' & ! (file_name %in% c('june2023.xlsx', 'july2023.xlsx', 'august2023.xlsx', 'september2023.xlsx', 'october2023.xlsx'))){
      raw_colnames <- c('Awaiting a medical decision/intervention including writing the discharge summary', 'Awaiting a therapy decision/intervention to proceed with discharge, including writing onward referrals, equipment ordering', 'Awaiting referral to care transfer hub or receiving service', 'Awaiting medicines to take home', 'Awaiting transport', 'Awaiting confirmation from care transfer hub or receiving service that referral received and actioned.', 
                        'Pathway 1: awaiting availability of resource for assessment and start of care at home (not a continuation of rehabilitation, recovery and reablement)',
                        'Pathway 1: Awaiting availability of resource for continuation of rehabilitation, reablement and recovery at home', 'Pathway 2: awaiting availability of another rehabilitation, reablement and recovery bed in a community bedded setting', 'Pathway 3: awaiting availability of a bed in a residential or nursing home that is likely to be a permanent placement', 
                        'Pathway 3: awaiting availability of a bed in a residential or nursing home for end-of-life care.', 'Awaiting equipment and adaptations to housing', 'Individual/family not in agreement with discharge plans', 
                        'Homeless/no right of recourse to public funds/no place to discharge to/lack of housing offers when previous residence no longer suitable', 'Safeguarding concern preventing discharge or Court of Protection', 'Awaiting readmission to an acute trust', 'No Plan', 
                        'Awaiting Diagnostic test', 'Remains in non-specialist Community bed to avoid spread of infectious disease and because there is no other suitable location to discharge to', 'Awaiting outcome of decision for CHC funding')
    }
    
    
    if (table == 'Table 4' & file_name %in% c('july2023.xlsx', 'august2023.xlsx', 'october2023.xlsx', 'november2023.xlsx', 'december2023.xlsx') & level %in% c('ICB', 'Trust')){
      
      discharges_all <- read_excel(paste0('Raw_data/Community_SitRep_data/', file_name), sheet = table, skip = 15, na = '-', col_names = FALSE)
      
    } else if ((table == 'Table 5' & file_name %in% c('july2023.xlsx', 'august2023.xlsx') & level %in% c('ICB', 'Trust')) |(table == 'Table 4' & !(file_name %in% c('august2023.xlsx', 'july2023.xlsx')) & level %in% c('ICB', 'Trust'))) {
      
      discharges_all <- read_excel(paste0('Raw_data/Community_SitRep_data/', file_name), sheet = table, skip = 14, na = '-', col_names = FALSE)
      
    } else if ((table == 'Table 5' & !(file_name %in% c('august2023.xlsx', 'july2023.xlsx')) & level %in% c('ICB', 'Trust'))) {
      
      discharges_all <- read_excel(paste0('Raw_data/Community_SitRep_data/', file_name), sheet = table, skip = 13, na = '-', col_names = FALSE)
      
    } else if (table == 'Table 4' & file_name %in% c('july2023.xlsx', 'august2023.xlsx', 'october2023.xlsx', 'november2023.xlsx', 'december2023.xlsx') & level == 'Region'){
      
      discharges_all <- read_excel(paste0('Raw_data/Community_SitRep_data/', file_name), sheet = table, skip = 5, na = '-', n_max = 8, col_names = FALSE)
      
    } else if ((table == 'Table 5' & file_name %in% c('july2023.xlsx', 'august2023.xlsx') & level == 'Region') |(table == 'Table 4' & !(file_name %in% c('july2023.xlsx','august2023.xlsx')) & level == 'Region')) {
      
      discharges_all <- read_excel(paste0('Raw_data/Community_SitRep_data/', file_name), sheet = table, skip = 4, na = '-', n_max = 8, col_names = FALSE)
      
    } else if ((table == 'Table 5' & !(file_name %in% c('august2023.xlsx', 'july2023.xlsx')) & level == 'Region')) {
      
      discharges_all <- read_excel(paste0('Raw_data/Community_SitRep_data/', file_name), sheet = table, skip = 3, na = '-', n_max = 8, col_names = FALSE)
      
    } else {print('Error')}
    
    
    if (table == 'Table 5' & file_name %in% c('september2023.xlsx') & level %in% c('ICB', 'Trust')){
      discharges_all <- discharges_all %>%
        select(2:length(discharges_all))
    }
    
    if (level %in% c('ICB', 'Trust')) {
      table_colnames <- c('Region', 'Org_code', 'Org_name', raw_colnames)
    } else if (level == 'Region'){
      table_colnames <- c('Region', raw_colnames)
    }
    
    names(discharges_all) <- table_colnames
    
    if (level %in% c('ICB', 'Trust')){
      stop_point <- which(discharges_all$Org_name == 'Org Name' & discharges_all$Org_name == 'Org Name')
      
      if (level == 'ICB'){
        discharge_df <- discharges_all[1:(stop_point-2),]
      } else if(level == 'Trust'){
        discharge_df <- discharges_all[stop_point+1:(nrow(discharges_all)-stop_point),]
      } else{print('Error')}
    } else if (level == 'Region'){
      discharge_df <- discharges_all
    }
    
    return(discharge_df)  
    
  }
  
  
  
  
  # Import all available months for both relevant tables at ICB and trust level
  
  table4_region <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 4', level = 'Region')})
  
  table4_ICB <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 4', level = 'ICB')})
  
  table5_region <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 5', level = 'Region')})
  
  table5_ICB <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 5', level = 'ICB')})
  
  # Add columns to regional tables
  
  table4_region <- lapply(table4_region, function(df){
    df <- df %>%
      mutate(Org_name = Region) %>%
      mutate(Org_code = Region) %>%
      select(Region, Org_code, Org_name, 2:(length(df)))
    return(df)
  })
  
  
  table5_region <- lapply(table5_region, function(df){
    df <- df %>%
      mutate(Org_name = Region) %>%
      mutate(Org_code = Region) %>%
      select(Region, Org_code, Org_name, 2:(length(df)))
    return(df)
  })
  
  # Add all tables to list
  
  all_tables_list <- list(table4_region = table4_region, table4_ICB = table4_ICB, table5_region = table5_region, table5_ICB = table5_ICB)
  
  
  # Apply uniform month labels to all tables
  
  month_labels <- sub('.xlsx', '', import_list)
  
  for (i in 1:length(all_tables_list)){
    names(all_tables_list[[i]]) <- month_labels   
  }
  
  # Pivot and label dataframes within lists, then combine all months into single df for each table/level combo
  
  all_tables_pivoted <- lapply(1:length(all_tables_list),function(i){
    
    lapply(1:(length(month_labels)), function(x){
      df<- all_tables_list[[i]][[x]] %>%
        mutate(period = month_labels[[x]]) %>%
        pivot_longer(4:length(all_tables_list[[i]][[x]]), names_to = 'metric', values_to = 'value',
                     values_ptypes = list(value=double()))
    }) 
  })
  
  all_months_combined <- lapply(1:length(all_tables_pivoted), function(i){
    
    df <- do.call(rbind, all_tables_pivoted[[i]]) %>%
      mutate(date = lubridate::my(period))
  })
  
  return(all_months_combined)
  
}

# ASC data wrangling functions

ASCFR_wrangle <- function(x=NULL){

ascfr_data <- list.files('Raw_data/ASC_data', pattern='xlsx')

ascfr_data <- ascfr_data[!ascfr_data %in% c('ASCOF-time-series.xlsx')]


t21_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T16', skip = 7, col_names = FALSE)
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T21', skip = 6, col_names = FALSE)
  } else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T21', skip = 8, col_names = FALSE) 
  }
  
  
  names(df) <- c('age_band', 'region_code', 'region_name', 'Early Cessation of Service, NHS-funded, deceased: value', 'Early Cessation of Service, NHS-funded, deceased: percentage',
                 'Early Cessation of Service, not leading to long term support: value', 'Early Cessation of Service, not leading to long term support: percentage', 'Early Cessation of Service, leading to long term support: value',
                 'Early Cessation of Service, leading to long term support: percentage', 'Long Term Support: value', 'Long Term Support: percentage', 'No services provided, needs identified but self funding: value', 
                 'No services provided, needs identified but self funding: percentage', 'Ongoing Low Level Support: value', 'Ongoing Low Level Support: percentage', 'Short Term Support: value', 'Short Term Support: percentage',
                 'No services provided, needs identified but support declined: value', 'No services provided, needs identified but support declined: percentage',
                 'No services provided, signposted to other services: value', 'No services provided, signposted to other services: percentage', 'No services provided, no identified needs: value', 'No services provided, no identified needs: percentage',
                 'Total: value')
  
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
  
  df$age_band[3:11] <- '18-64'
  
  df$age_band[13:21] <- '65 and over'
  
  df <- df %>% 
    mutate_all(function(.){(str_replace(., "%", ""))})
  
  df <- df %>%
    pivot_longer(4:length(df), names_to = 'metric', values_to = 'value')
  
  df$value <- as.numeric(df$value)
  
  df <- df %>%
    separate_wider_delim(metric, delim = ': ', names = c('metric', 'metric_type'))
  
  return(df)
  
})     # Episodes for new clients, by what happened next and age band

t24_all <- lapply(ascfr_data, function(i){
  if  (i == 'ASCFR_2016-17.xlsx'){
    
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T24', skip = 6, col_names = FALSE)
  }else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T24', skip = 8, col_names = FALSE) }
  
  if  (i != 'ASCFR_2016-17.xlsx'){
    
    names(df) <- c('LA_code', 'area_code', 'LA_name', 'region_code', 'region_name', 'Episodes of ST-MAX per 100,000 adults', 'Episodes of ST-MAX',
                   'Population')
    
    df <- df[rowSums(is.na(df)) != ncol(df), ]
    
    df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
    
    df <- df %>% 
      mutate_all(function(.){(str_replace(., "//[x]", '0'))}) %>% 
      mutate_all(function(.){(str_replace(., "//[c]", '0'))})
    
    df <- df %>%
      pivot_longer(6:length(df), names_to = 'metric', values_to = 'value')
    
    df$value <- as.numeric(df$value)
    
    return(df)
    
  }})      # Number of episodes for new clients per 100,000 adults

t24_all <- t24_all[2:7]

t27_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T27', skip = 6, col_names = FALSE)
  } else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T27', skip = 8, col_names = FALSE) }
  
  if (i != 'ASCFR_2016-17.xlsx') {
    
    names(df) <- c('age_band', 'region_code', 'region_name', 'Early Cessation of Service, NHS-funded, deceased',
                   'Early Cessation of Service, not leading to long term support',  'Early Cessation of Service, leading to long term support', 'Move to nursing care (from community)',
                   'Move to residential care (from community)', 'Move to community', 'Level of long-term support increased', 'No change in long-term support', 'Level of long-term support decreased',
                   'All long term support ended, no ongoing eligible needs', 'Total' )
    
    
    df <- df[rowSums(is.na(df)) != ncol(df), ]
    
    df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
    
    df$age_band[3:11] <- '18-64'
    
    df$age_band[13:21] <- '65 and over'
    
    df <- df %>% 
      mutate_all(function(.){(str_replace(., "//*", '0'))})
    
    df <- df %>%
      pivot_longer(4:length(df), names_to = 'metric', values_to = 'value')
    
    df$value <- as.numeric(df$value)
    
    return(df)
    
  }}) 

t27_all <- t27_all[2:7]

t28_all <- lapply(ascfr_data, function(i){
  
  if  (i == 'ASCFR_2016-17.xlsx'){
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T28', skip = 7, col_names = FALSE)
  }else {
    
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T28', skip = 9, col_names = FALSE) }
  
  if (i != 'ASCFR_2016-17.xlsx') {
    
    names(df) <- c('LA_code', 'area_code', 'LA_name', 'region_code', 'region_name', 'Completed episodes of ST-Max per client: 18-64', 'Completed episodes of ST-Max: 18-64', 'Clients: 18-64',
                   'Completed episodes of ST-Max per client: 65+', 'Completed episodes of ST-Max: 65+', 'Clients: 65+',
                   'Completed episodes of ST-Max per client: Total', 'Completed episodes of ST-Max: Total', 'Clients: Total')
    
    df <- df[rowSums(is.na(df)) != ncol(df), ]
    
    df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
    
    df <- df %>% 
      mutate_all(function(.){(str_replace(., "//[x]", '0'))}) %>% 
      mutate_all(function(.){(str_replace(., "//[c]", '0'))})
    
    df <- df %>%
      pivot_longer(6:length(df), names_to = 'metric', values_to = 'value')
    
    df$value <- as.numeric(df$value)
    
    df <- df %>%
      separate_wider_delim(metric, delim = ': ', names = c('metric', 'age_band'))
    
    return(df)
  }})    # Number of episodes per client by age band, all clients

t28_all <- t28_all[2:7]

ascfr_tables <- list(t24_all, t27_all, t28_all)

ascfr_FYs_all <- c('2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31', '2023-03-31')

ascfr_FYs_short <- c('2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31', '2023-03-31')

for (x in 1:7){
  t21_all[[x]] <- t21_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t21_final <- do.call('rbind', t21_all)

for (i in 1:3){
  for (x in 1:6){
    ascfr_tables[[i]][[x]] <- ascfr_tables[[i]][[x]] %>%
      mutate(date = ascfr_FYs_short[[x]])
  }
  ascfr_tables[[i]] <- do.call('rbind', ascfr_tables[[i]])
}

t24_final <- ascfr_tables[[1]]

t27_final <- ascfr_tables[[2]]

t28_final <- ascfr_tables[[3]]




# England-level only tables 

t23_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T17', skip = 8, col_names = FALSE)
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T23', skip = 6, col_names = FALSE)
  } else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T23', skip = 8, col_names = FALSE) 
  }
  
  names(df) <- c('age_band', 'Sequel to ST-MAX', 'Blank', 'Physical Support, access and mobility only: value', 'Physical Support, access and mobility only: percentage',
                 'Physical Support, personal care support: value', 'Physical Support, personal care support: percentage', 'Sensory Support, support for visual impairment: value',
                 'Sensory Support, support for visual impairment: percentage', 'Sensory Support, support for hearing impairment: value', 'Sensory Support, support for hearing impairment: percentage',
                 'Sensory Support, support for dual impairment: value', 'Sensory Support, support for dual impairment: percentage', 'Support with memory and cognition: value', 'Support with memory and cognition: percentage',
                 'Learning disability support: value', 'Learning disability support: percentage', 'Mental health support: value', 'Mental health support: percentage', 'Social Support, substance misuse support: value',
                 'Social Support, substance misuse support: percentage', 'Social Support, asylum seeker support: value', 'Social Support, asylum seeker support: percentage', 'Social Support, support for isolation/Other: value',
                 'Social Support, support for isolation/Other: percentage', 'Total: value')
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df <- df %>%
      select(1:26)
  }else {
    
  }
  
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
  
  df <- df %>%
    filter(`Sequel to ST-MAX` == 'Total')
  
  df <- df %>% 
    mutate_all(function(.){(str_replace(., "%", ""))})
  
  df <- df %>%
    pivot_longer(4:length(df), names_to = 'metric', values_to = 'value')
  
  df$value <- as.numeric(df$value)
  
  df <- df %>%
    separate_wider_delim(metric, delim = ': ', names = c('metric', 'metric_type'))
  
  df <- df %>%
    select(-Blank)
  
  return(df)
  
})

for (x in 1:7){
  t23_all[[x]] <- t23_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t23_final <- do.call('rbind', t23_all)

t25_26_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df1 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T18', skip = 6, col_names = FALSE)
    df2 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T19', skip = 6, col_names = FALSE)
  } else if (i == 'ASCFR_2022-23.xlsx') {
    df1 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T25', skip = 6, col_names = FALSE) 
    df2 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T26', skip = 6, col_names = FALSE)
  } else {
    df1 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T25', skip = 8, col_names = FALSE) 
    df2 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T26', skip = 8, col_names = FALSE)
  }
  
  col_names <- c('LA_code', 'area_code', 'LA_name', 'region_code', 'region_name', 'Physical Support, access and mobility only', 
                 'Physical Support, personal care support', 'Sensory Support, support for visual impairment',
                 'Sensory Support, support for hearing impairment', 'Sensory Support, support for dual impairment',  
                 'Support with memory and cognition', 'Learning disability support: value','Mental health support', 'Social Support, substance misuse support',
                 'Social Support, asylum seeker support', 'Social Support, support for isolation/Other', 'Total')
  
  names(df1) <- col_names
  
  names(df2) <- col_names
  
  df1 <- df1 %>%
    filter(region_name == 'England') %>%
    mutate(age_band = '18-64')
  
  df2 <- df2 %>%
    filter(region_name == 'England') %>%
    mutate(age_band = '65+')
  
  df <- rbind(df1, df2)
  
  return(df)
  
})  # Number of completed episodes by primary support reason for existing clients, by age band

for (x in 1:7){
  t25_26_all[[x]] <- t25_26_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t25_26_final <- do.call('rbind', t25_26_all)

# Number of completed episodes by primary support reason for existing clients, by age band

return(list(t21_final, t23_final, t24_final, t25_26_final, t27_final, t28_final))

}

ASCOF_wrangle <- function(x=NULL){

ascof_FYs <- c('2016-03-31', '2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31', '2023-03-31')

variable_types <- c('Numerator', 'Denominator', 'Outcome')

initial_labels <- c()

for (i in ascof_FYs) {
  for (x in variable_types){
    item <- paste0(i, ':', x)
    initial_labels <- c(initial_labels, item)
  }
}

# Table 2B(1)

colnames_2b1 <- c('cassr_code', 'cassr', 'area_code')

variables_2b1 <- rep(c('Number of older people still at home 91 days after discharge from hospital into reablement services', 'Number of older people who received reablement services after discharge from hospital', 'Proportion of older people still at home 91 days after discharge from hospital into reablement services'), 8)

for (i in c(1:length(initial_labels))){
  item <- paste0(initial_labels[i], ':', variables_2b1[i])
  colnames_2b1 <- c(colnames_2b1, item)
}

table_2b1 <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2B(1)', skip = 6, col_names = FALSE, n_max = 172)

names(table_2b1) <- colnames_2b1

table_2b1 <- table_2b1 %>%
  pivot_longer(4:27, names_to = 'metric', values_to = 'value') %>%
  separate(metric, c('year', 'variable_type', 'metric'), sep = ':')


# Table 2B(2)

colnames_2b2 <- c('cassr_code', 'cassr', 'area_code')

variables_2b2 <- rep(c('Number of older people receiving reablement care following discharge from hospital', 'Number of older people discharged from hospital', 'Proportion of older people receiving reablement care following discharge from hospital'), 8)

for (i in c(1:length(initial_labels))){
  item <- paste0(initial_labels[i], ':', variables_2b2[i])
  colnames_2b2 <- c(colnames_2b2, item)
}

table_2b2 <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2B(2)', skip = 6, col_names = FALSE, n_max = 172)

names(table_2b2) <- colnames_2b2

table_2b2 <- table_2b2 %>%
  pivot_longer(4:27, names_to = 'metric', values_to = 'value') %>%
  separate(metric, c('year', 'variable_type', 'metric'), sep = ':')


# Table 2D

colnames_2d <- c('cassr_code', 'cassr', 'area_code')

variables_2d <- rep(c('Number of new short-term service users who no further support or support at a lower level after ST-MAX', 'Number of new service users who received short-term care to maximise independence', 'Proportion of new short-term service users who no further support or support at a lower level after ST-MAX'), 8)

for (i in c(1:length(initial_labels))){
  item <- paste0(initial_labels[i], ':', variables_2d[i])
  colnames_2d <- c(colnames_2d, item)
}

table_2d <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2D', skip = 6, col_names = FALSE, n_max = 172)

names(table_2d) <- colnames_2d

table_2d <- table_2d %>%
  pivot_longer(4:27, names_to = 'metric', values_to = 'value') %>%
  separate(metric, c('year', 'variable_type', 'metric'), sep = ':')

return(list(table_2b1, table_2b2, table_2d))

}


# Expenditure wrangling functions 

expenditure_ASC_wrangle <- function(x=NULL){
ascfr_data <- list.files('Raw_data/ASC_data', pattern='xlsx')

ascfr_data <- ascfr_data[!ascfr_data %in% c('ASCOF-time-series.xlsx')]

t32_all <- lapply(ascfr_data, function(i){
  
  if(i=='ASCFR_2016-17.xlsx'){
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T24', skip = 8, col_names = FALSE)
  } else if(i=='ASCFR_2022-23.xlsx'){
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T32', skip = 7, col_names = FALSE)
  } else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T32', skip = 9, col_names = FALSE) 
  }
  
  names(df) <- c('geography_code', 'LA_code', 'LA_name', 'region_code', 'region_name', 'Expenditure per 100,000 adults: previous year', 'Gross current expenditure: previous year','Population: previous year',
                 'Expenditure per 100,000 adults: current year', 'Gross current expenditure: current year','Population: current year')
  
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
  
  df <- df %>% 
    mutate_all(function(.){(str_replace(., "//[x]", '0'))}) %>% 
    mutate_all(function(.){(str_replace(., "//[c]", '0'))})
  
  df <- df %>%
    pivot_longer(6:length(df), names_to = 'metric', values_to = 'value')
  
  df$value <- as.numeric(df$value)
  
  df <- df %>%
    separate_wider_delim(metric, delim = ': ', names = c('metric', 'year'))
  
  return(df)
  
})

ascfr_FYs <- c('2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31', '2023-03-31')


for (x in 1:7){
  t32_all[[x]] <- t32_all[[x]] %>%
    mutate(date = ascfr_FYs[[x]])
}

t32_final <- do.call('rbind', t32_all) %>%
  filter((year == 'current year') | (year == 'previous year' & date == '2017-03-31'))

t32_final$date[t32_final$date == '2017-03-31' & t32_final$year == 'previous year'] <- '2016-03-31'

return(t32_final)

}
