################################################################################
################################################################################
# Script written in R 4.0.2

#  2. COMMUNITY SITREP DATA ANALYSIS

# In this script, we download any new editions of the discharges from community hospitals SitRep data from NHS England, process the data into an analyzable format, and create time series of key metrics.
# To run this script, you only need to have run 00_Setup_and_packages.R prior

# Much of this script functions in the same way as the preceding 01_Acute_Sitrep_analysis.R script, with minor variations to account for formatiing differences. 
# As it currently stand, this script works to scrape, download and wrangle sitrep data up until August 2023. For future data, please be 
# attentive to potential formatting differences which may complicate the wrangling stages.


################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages() & dir.exists(file.path(here('Raw_data/NCC_data')))) {
  print('Project setup run')
}else{
  source('Scripts/00_Setup_and_packages.R')}


################################################
######## SCRAPE AND DOWNLOAD LATEST DATA #######
################################################

## Identify which SitRep files are currently in our directory
current_files <- list.files(here('Raw_data/Community_SitRep_data'), pattern='xlsx')  


## Scrape the NHS webpage to identify which months are currently available

community_sitrep_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-community-data/'  # Link to NHS webpage

monthly_names <- read_html(community_sitrep_link) %>%    # Identify which months of data are listed on the web page
  html_elements('p') %>%
  html_text() %>%
  tolower() %>%
  as.data.frame() %>%
  rename(months = '.') %>%
  filter((grepl('community-discharge-sitrep-monthly', months)) == TRUE)

data_nodes <- read_html(community_sitrep_link) %>%  # Extract links to all the datasets available on the webpage
  html_elements('a') %>%
  html_attr('href') %>%
  as.data.frame() %>%
  rename(links = '.') %>%
  filter(grepl('.xlsx', links) == TRUE)

time_series_link <- data_nodes$links[grep('timeseries', data_nodes$links)] # Find time series in the list of data nodes

months_links_df <-  data_nodes %>%
  filter(grepl('timeseries', links)==FALSE) %>%
  cbind(., monthly_names) %>%
  select(months, links)

months <- sub(" ", "", word(months_links_df$months, 1, sep = "\\:")) # Create a list of months available on the webpage in the same format as the data download links


## Compare the list of available data to the data we already have in our directory

months_comparator <- tolower(data_nodes$links)

checklist_web <- lapply(1:length(months), function(i){      # Check that there is a download link for all months listed 
  grepl(months[[i]], months_comparator)
})

month_present_on_web <- sapply(1:length(checklist_web), function(i){   # Create vector describing whether a month has a download link on the web
  sum(checklist_web[[i]])
})

checklist_files <- lapply(1:length(months), function(i){                      # Check for monthly files in our raw data directory
  file.exists(paste0('Raw_data/Community_SitRep_data/', months[[i]], '.xlsx'))
})

month_present_in_files <- sapply(1:length(checklist_files), function(i){      # Create vector describing whether a month is in our data directory
  sum(checklist_files[[i]])
})

files_comparison_df <- data.frame(months, month_present_on_web, month_present_in_files, data_nodes$links[1:length(months)]) %>%    # Join all with download links and mark links to download
  rename(links = 4) %>%
  mutate(download = case_when(month_present_on_web >= 1 & month_present_in_files < 1 ~ "YES",
                              month_present_on_web >= 1 & month_present_in_files >= 1 ~ "NO",
                              TRUE ~ 'Error'))

links_to_download <- files_comparison_df %>%
  filter(download == 'YES')

if(nrow(links_to_download) >= 1){
  for (i in 1:nrow(links_to_download)){      # Download all links which we do not currently have in our directory
    
    download.file(url = links_to_download$links[[i]], destfile = paste0('Raw_data/Community_SitRep_data/', links_to_download$months[[i]], '.xlsx'))
  }
}

## Load in time series [CURRENTLY HASHED OUT AS WE HAVENT USED THEM AT ALL]
#download.file(url = time_series_link, destfile = 'Raw_data/Community_SitRep_data/latest_time_series.xlsx')  # Download latest version of time series data

rm(checklist_files, checklist_web, data_nodes, files_comparison_df, links_to_download, monthly_names)  # Clear up workspace

#################################################
######## LOAD UP FULL SERIES OF DATA ############
#################################################

# Create new list of files in our directory now that we've scraped for new data. RECOMMENDED TO CHECK AND MAKE SURE NO MONTHS ARE REPEATED
refreshed_current_files <- list.files(here('Raw_data/Community_SitRep_data'), pattern='xlsx')  

print(refreshed_current_files)

import_list <- refreshed_current_files[!refreshed_current_files == 'latest_time_series.xlsx'] # Remove time series from list to process with the below function

import_list <- import_list[!import_list == 'january2024.xlsx']

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
  
  lapply(1:(length(months)-1), function(x){
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

# Create individual dataframes for each table/level combo, including variable for separating out pathways

region_discharges_by_destination <- all_months_combined[[1]] %>%
  mutate(pathway = case_when((grepl('P0', metric)|(grepl('Pathway 0', metric))) == TRUE ~ 'P0',
                             (grepl('P1', metric)|(grepl('Pathway 1', metric))) == TRUE ~ 'P1',
                             (grepl('P2', metric)|(grepl('Pathway 2', metric))) == TRUE ~ 'P2',
                             (grepl('P3', metric)|(grepl('Pathway 3', metric))) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

ICB_discharges_by_destination <- all_months_combined[[2]] %>%
  mutate(pathway = case_when((grepl('P0', metric)|(grepl('Pathway 0', metric))) == TRUE ~ 'P0',
                             (grepl('P1', metric)|(grepl('Pathway 1', metric))) == TRUE ~ 'P1',
                             (grepl('P2', metric)|(grepl('Pathway 2', metric))) == TRUE ~ 'P2',
                             (grepl('P3', metric)|(grepl('Pathway 3', metric))) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

region_delayed_discharges_by_reason <- all_months_combined[[3]]

ICB_delayed_discharges_by_reason <- all_months_combined[[4]]

rm(table4_region, table4_ICB, table5_region, table5_ICB, all_tables_list, all_tables_pivoted, all_months_combined)  # Clear up workspace



## Load in time series [CURRENTLY HASHED OUT AS WE HAVENT USED THEM AT ALL]

#daily_timeseries <- read_excel('Raw_data/Community_SitRep_data/latest_time_series.xlsx', sheet = 'Daily Series', skip = 5) 

#weekly_timeseries <- read_excel('Raw_data/Community_SitRep_data/latest_time_series.xlsx', sheet = 'Weekly Series', skip = 6)

#weekly_timeseries$`Date (week commencing)` <- str_replace(weekly_timeseries$`Date (week commencing)`, '\\**', "")

#weekly_timeseries$`Date (week commencing)` <- str_replace(weekly_timeseries$`Date (week commencing)`, '\\*', "")

#################################################
################### ANALYSIS ####################
#################################################

## Total number of patients discharged on P1
region_discharges_by_destination %>%
  filter(pathway == 'P1' & Region == 'ENGLAND') %>%
  group_by(period, date) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

region_discharges_by_destination %>%
  filter(pathway == 'P1' & Region != 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value, color = Region)) +
  geom_line() +
  theme_minimal()

ICB_discharges_by_destination %>%
  replace_na(list(value = 0)) %>%
  filter(pathway == 'P1') %>%
  select(-period) %>%
  group_by(date, Org_code, Org_name) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = date, values_from = c(value))


## Total number of patients discharged on P2

region_discharges_by_destination %>%
  filter(pathway == 'P2' & Region == 'ENGLAND') %>%
  group_by(period, date) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value)) +
  geom_line(color = '#F8766D') +
  theme_minimal()


region_discharges_by_destination %>%
  filter(pathway == 'P2' & Region != 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value, color = Region)) +
  geom_line() +
  theme_minimal()

ICB_discharges_by_destination %>%
  replace_na(list(value = 0)) %>%
  filter(pathway == 'P2') %>%
  select(-period) %>%
  group_by(date, Org_code, Org_name) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = date, values_from = c(value))


# P1 discharges by destination 

region_discharges_by_destination %>%
  filter(pathway == 'P1' & Region == 'ENGLAND') %>%
  group_by(period, date, metric) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value, color = metric)) +
  geom_line() +
  theme_minimal()

# P2 discharges by destination 

region_discharges_by_destination %>%
  filter(pathway == 'P2' & Region == 'ENGLAND') %>%
  group_by(period, date, metric) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value, color = metric)) +
  geom_line() +
  theme_minimal()


## Percentage of discharges on P1 
region_discharges_by_destination %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(pathway == 'P1' & Region == 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>%
  ggplot(., aes(x = date, y = percent_of_discharges)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

region_discharges_by_destination %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(pathway == 'P1' & Region != 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>%
  ggplot(., aes(x = date, y = percent_of_discharges, color = Region)) +
  geom_line() +
  theme_minimal()

ICB_discharges_by_destination %>%
  replace_na(list(value = 0)) %>%
  group_by(period, date, Org_code, Org_name) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(pathway == 'P1') %>%
  group_by(period, date, Org_code, Org_name) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges)


## Percentage of discharges on P2 
region_discharges_by_destination %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(pathway == 'P2' & Region == 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>%
  ggplot(., aes(x = date, y = percent_of_discharges)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

region_discharges_by_destination %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(pathway == 'P2' & Region != 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>%
  ggplot(., aes(x = date, y = percent_of_discharges, color = Region)) +
  geom_line() +
  theme_minimal()

## NOTE: FIX DATE THING HERE, MAYBE DO GROUPING INSTEAD. FIGURE OUT MAP THING.
ICB_discharges_by_destination %>%
  replace_na(list(value = 0)) %>%
  group_by(period, date, Org_code, Org_name) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(pathway == 'P2') %>%
  group_by(period, date, Org_code, Org_name) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>%
  filter(date == max(ymd(Date))) %>%
  ggplot(., aes(x = percent_of_discharges)) +
  geom_histogram() +
  theme_minimal()



###TABLE 5 

# Number of delayed discharges awaiting availability of care on pathway 1

region_delayed_discharges_by_reason %>%
  filter(grepl('Pathway 1', metric) & Region == 'ENGLAND') %>%
  group_by(period, date) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

region_delayed_discharges_by_reason %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(grepl('Pathway 1', metric) & Region != 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value, color = Region)) +
  geom_line() +
  theme_minimal()


# Number of delayed discharges awaiting availability of care on pathway 2

region_delayed_discharges_by_reason %>%
  filter(grepl('Pathway 2', metric) & Region == 'ENGLAND') %>%
  group_by(period, date) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

region_delayed_discharges_by_reason %>%
  filter(grepl('Pathway 2', metric) & Region != 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value, color = Region)) +
  geom_line() +
  theme_minimal()

# Proportion of delayed discharges awaiting availability of care on pathway 1

region_delayed_discharges_by_reason %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(grepl('Pathway 1', metric) & Region == 'ENGLAND') %>%
  group_by(period, date) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>%
  ggplot(., aes(x = date, y = percent_of_discharges)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

region_delayed_discharges_by_reason %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(grepl('Pathway 1', metric) & Region != 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>% 
  ggplot(., aes(x = date, y = percent_of_discharges, color = Region)) +
  geom_line() +
  theme_minimal()


# Proportion of delayed discharges awaiting availability of care on pathway 2

region_delayed_discharges_by_reason %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(grepl('Pathway 2', metric) & Region == 'ENGLAND') %>%
  group_by(period, date) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>%
  ggplot(., aes(x = date, y = percent_of_discharges)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

region_delayed_discharges_by_reason %>%
  group_by(period, date, Region) %>%
  mutate(all_discharges = sum(value)) %>%
  dplyr::ungroup() %>%
  filter(grepl('Pathway 2', metric) & Region != 'ENGLAND') %>%
  group_by(period, date, Region) %>%
  summarise(value = sum(value), all_discharges = sum(all_discharges)) %>%
  mutate(percent_of_discharges = value/all_discharges) %>% 
  ggplot(., aes(x = date, y = percent_of_discharges, color = Region)) +
  geom_line() +
  theme_minimal()


################################################
############# VISUALISATIONS ###################
################################################

ggplot(daily_timeseries, aes(x = Date, y = `P1 - Domestic home, new reablement support`)) +
  geom_line() +
  theme_minimal()

ICB_discharges_by_destination %>%
  filter(pathway == 'P1') %>%
  replace_na(list(value = 0)) %>%
  group_by(period, date) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value)) +
  geom_line() +
  theme_minimal()




##########################################################


### ANALYSIS FOR LONG READ


#1. Average monthly number of patients discharged  - total and by pathway - for latest 12 months
avg_disch_by_pathway = ICB_discharges_by_destination %>%
  filter(date >= (max(date) - months(12))) %>%
  group_by(period, pathway) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(pathway) %>%
  summarise(mean = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(pathway) %>%
  mutate(pc = mean/tot*100)

avg_disch_by_pathway %>% ungroup() %>% summarise(tot=sum(mean))

avg_disch_by_pathway %>%
  ggplot(., aes(x=pathway, y=mean, fill=pathway)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title='Discharges by pathway', y='Average monthly discharges, Sep 22 to Aug 23')


#2. Stratified by destination
avg_disch_by_pathway_and_destination = ICB_discharges_by_destination %>%
  filter(date >= (max(date) - months(12))) %>%
  group_by(period, metric, pathway) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by( pathway, metric)  %>%
  summarise(mean = mean(value, na.rm=TRUE))

avg_disch_by_pathway_and_destination %>%
  filter((pathway=='P1' | pathway=='P2') & mean!=0) %>%
  mutate(destination = case_when(str_detect(metric, 'Domestic home') ~ 'Home',
                                 str_detect(metric, 'Hotel') ~ 'Hotel',
                                 str_detect(metric, 'Other') ~ 'Other',
                                 str_detect(metric, 'Care Home') ~ 'Care home',
                                 str_detect(metric, 'Community Rehab') ~ 'Community rehab',
                                 str_detect(metric, 'Designated Setting') ~ 'Isolation befor care home')) %>%
  ggplot(., aes(x=destination, y=mean, fill=pathway)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title='Discharges by destination', y='Average monthly discharges, Sep 22 to Aug 23')



#3. Number of P1 and P2 discharges by month 
ICB_discharges_by_destination %>%
  filter(pathway=='P2' | pathway=='P1') %>%
  group_by(period, date, pathway) %>%
  summarise(value = sum(value, na.rm=TRUE), .groups='keep') %>%
  ggplot(., aes(x = date, y = value, color=pathway, group=pathway)) +
  geom_line() +
  theme_minimal() +
  xlab('Month and year') +
  ylab('Number of discharges')

#4. Percentage of dicharges that are on P1 and P2 by ICB (latest month) - could also do n per pop
ICB_discharges_by_destination %>%
  filter(date==max(date)) %>%
  mutate(Org_name = str_remove_all(Org_name, 'NHS'),
         Org_name = str_remove_all(Org_name, 'INTEGRATED CARE BOARD')) %>%
  group_by(Org_name) %>%
  mutate(tot=sum(value, na.rm=TRUE)) %>%
  filter(pathway=='P1') %>%
  group_by(pathway, Org_name) %>%
  summarise(pc = sum(value, na.rm=TRUE)/tot*100) %>%
  distinct() %>%
  ggplot(., aes(y = reorder(Org_name, pc), x = pc)) + 
  geom_bar(stat = 'identity', fill='lightblue') +
  theme_minimal() +
  xlab('Percentage of discharges that were to pathway 1, Aug 23') +
  ylab('ICB')

#4. Percentage of dicharges that are on P1 and P2 by ICB (latest month) - could also do n per pop
ICB_discharges_by_destination %>%
  filter(date==max(date)) %>%
  mutate(Org_name = str_remove_all(Org_name, 'NHS'),
         Org_name = str_remove_all(Org_name, 'INTEGRATED CARE BOARD')) %>%
  group_by(Org_name) %>%
  mutate(tot=sum(value, na.rm=TRUE)) %>%
  filter(pathway=='P2') %>%
  group_by(pathway, Org_name) %>%
  summarise(pc = sum(value, na.rm=TRUE)/tot*100) %>% 
  distinct() %>%
  ggplot(., aes(y = reorder(Org_name, pc), x = pc)) + 
  geom_bar(stat = 'identity', fill='lightblue') +
  theme_minimal() +
  xlab('Percentage of discharges that were to pathway 2, Aug 23') +
  ylab('ICB')


# Stacked
ICB_discharges_by_destination %>%
  filter(date==max(date)) %>%
  mutate(Org_name = str_remove_all(Org_name, 'NHS'),
         Org_name = str_remove_all(Org_name, 'INTEGRATED CARE BOARD')) %>%
  group_by(Org_name) %>%
  mutate(tot=sum(value, na.rm=TRUE)) %>%
  filter(pathway=='P1' | pathway== 'P2') %>%
  group_by(pathway, Org_name) %>%
  summarise(pc = sum(value, na.rm=TRUE)/tot*100) %>% 
  distinct() %>%
  ggplot(., aes(y = reorder(Org_name, pc), x = pc, fill = pathway)) + 
  geom_bar(stat = 'identity') +
  theme_minimal() +
  xlab('Percentage of discharges that were to pathway 1 or 2, Aug 23') +
  ylab('ICB')



#5. Average monthly number of delayed discharges for latest 12 months

head(ICB_delayed_discharges_by_reason)

ICB_delayed_discharges_by_reason %>% select(metric) %>% distinct()

avg_delayed_by_pathway = ICB_delayed_discharges_by_reason %>%
  filter(date >= (max(date) - months(12))) %>%
  group_by(period, metric) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(metric) %>%
  summarise(mean = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(metric) %>%
  mutate(pc = mean/tot*100) 

avg_delayed_by_pathway %>% ungroup() %>% summarise(tot=sum(mean))

avg_delayed_by_pathway %>%
  filter(str_detect(metric, 'Pathway 1') | str_detect(metric, 'Pathway 2')) %>%
  ggplot(., aes(x=metric, y=mean, fill=metric)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title='Average delayed discharges by pathway, Sep 22 to Aug 23', y='', x='') +
  theme(legend.position = "bottom", legend.direction ='vertical', axis.text.x = element_blank())


#6. Percentage of all discharges that are delayed

avg_delayed_by_pathway %>%
  mutate(metric = case_when(str_detect(metric, 'Pathway') ~ metric,
                            TRUE ~ 'Other (15 categories, all <5%)')) %>%
  ggplot(., aes(x=metric, y=pc, fill=metric)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title='Average delayed discharges by reason, Sep 22 to Aug 23', y='', x='') +
  theme(legend.position = "bottom", legend.direction ='vertical', axis.text.x = element_blank())

#7. Number of delayed discharges that are due to IC by month
ICB_delayed_discharges_by_reason %>%
  filter(str_detect(metric, 'Pathway 1') | str_detect(metric, 'Pathway 2')) %>%
  group_by(date, metric) %>%
  summarise(value = sum(value, na.rm=TRUE), .groups='keep') %>%
  ggplot(., aes(x = date, y = value, color=metric, group=metric)) +
  geom_line() +
  theme_minimal() +
  xlab('Month and year') +
  ylab('Number of delayed discharges') +
  theme(legend.position = "bottom", legend.direction ='vertical')



