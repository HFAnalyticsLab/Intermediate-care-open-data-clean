################################################################################
################################################################################
# Script written in R 4.0.2

#  1. ACUTE SITREP DATA ANALYSIS

# In this script, we download any new editions of the discharges from acute hospitals SitRep data from NHS England, process the data into an analyzable format, and create time series of key metrics.
# To run this script, you only need to have run 00_Setup_and_packages.R prior

# In the scraping portion, we scrape the NHSE webpage for Delayed Discharge SitRep data and install any available datasets which
# we do not currently have in our working directory. This will act to both download all available data on a first run, 
# and download only any new editions of the dataset on subsequent runs. The singular time series dataset is updated with each run. 

# There are two tables we are interested in in the SitRep data: Table 4, detailing discharges by pathway, and Table 5, showing 
# discharge delays by reason. These tables are both formatted in a slightly difficult way for reading and analyzing in R - 
# both have irregular shapes, numerous empty cells for formatting, and have multiple tables (showing the same metrics at regional, ICB and trust level)
# on the same sheet. The wrangling portion of this script reads in each monthly sheet for Tables 4 and 5 and fixes their formatting 
# into an amenable shape for R. It then pivots and combines all monthly sheets into more easily filterable dataframes featuring 
# all metrics in long format, divided into ICB and trust level. 

# As it currently stands, this script works to scrape, download and wrangle sitrep data up until September 2023. For future data, please be 
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
current_files <- list.files(here('Raw_data/Acute_SitRep_data'), pattern='xlsx')  


## Scrape the NHS webpage to identify which months are currently available

acute_sitrep_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/'  # Link to NHS webpage

monthly_names <- read_html(acute_sitrep_link) %>%    # Identify which months of data are listed on the web page
  html_elements('p') %>%
  html_text() %>%
  tolower() %>%
  as.data.frame() %>%
  rename(months = '.') %>%
  filter((grepl('daily-discharge-sitrep-monthly', months)) == TRUE)

months <- sub(" ", "", word(monthly_names$months, 1, sep = "\\:")) # Create a list of months available on the webpage in the same format as the data download links
  
data_nodes <- read_html(acute_sitrep_link) %>%  # Extract links to all the datasets available on the webpage
  html_elements('a') %>%
  html_attr('href') %>%
  as.data.frame() %>%
  rename(links = '.') %>%
  filter(grepl('.xlsx', links) == TRUE)

time_series_link <- data_nodes$links[grep('timeseries', data_nodes$links)] # Find time series in the list of data nodes

discharge_ready_date_files <- data_nodes$links[grep('Discharge-Ready-Date', data_nodes$links)]  # In October 2023, they included an additional, unlabeled dataset link. This must be removed. 

data_nodes <- data_nodes %>%
  mutate(drd_remove = case_when(links %in% discharge_ready_date_files ~ 1,
                                TRUE ~ 0)) %>%
  filter(drd_remove == 0)
  

## Compare the list of available data to the data we already have in our directory

months_comparator <- tolower(data_nodes$links)

checklist_web <- lapply(1:length(months), function(i){      # Check that there is a download link for all months listed 
  grepl(months[[i]], months_comparator)
})

month_present_on_web <- sapply(1:length(checklist_web), function(i){   # Create vector describing whether a month has a download link on the web
  sum(checklist_web[[i]])
})

checklist_files <- lapply(1:length(months), function(i){                      # Check for monthly files in our raw data directory
  file.exists(paste0('Raw_data/Acute_SitRep_data/', months[[i]], '.xlsx'))
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
    
    download.file(url = links_to_download$links[[i]], destfile = paste0('Raw_data/Acute_SitRep_data/', links_to_download$months[[i]], '.xlsx'))
  }
}


download.file(url = time_series_link, destfile = 'Raw_data/Acute_SitRep_data/latest_time_series.xlsx')  # Download latest version of time series data

rm(checklist_files, checklist_web, data_nodes, files_comparison_df, links_to_download, monthly_names)  # Clear up workspace

#################################################
######## LOAD UP FULL SERIES OF DATA ############
#################################################

# Create new list of files in our directory now that we've scraped for new data. RECOMMENDED TO CHECK AND MAKE SURE NO MONTHS ARE REPEATED
refreshed_current_files <- list.files(here('Raw_data/Acute_SitRep_data'), pattern='xlsx')  

print(refreshed_current_files)

import_list <- refreshed_current_files[!refreshed_current_files == 'latest_time_series.xlsx'] # Remove time series from list to process with the below function


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
  
  lapply(1:length(months), function(x){
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
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

ICB_discharges_by_destination <- all_months_combined[[2]] %>%
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

trust_discharges_by_destination <- all_months_combined[[3]] %>%
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

region_delayed_discharges_by_reason <- all_months_combined[[4]]

ICB_delayed_discharges_by_reason <- all_months_combined[[5]]

trust_delayed_discharges_by_reason <- all_months_combined[[6]]

rm(table4_region, table4_ICB, table4_trust, table5_region, table5_ICB, table5_trust, all_tables_list, all_tables_pivoted, all_months_combined)  # Clear up workspace


#################################################
################### ANALYSIS ####################
#################################################



#### ANALYSIS FOR LONG READ

#1. Average monthly number of patients discharged  - total and by pathway - for 2023
# Using ICB table because region table does not have England for nov/dec
avg_disch_by_pathway = ICB_discharges_by_destination %>%
  filter(year(date)==2023) %>%
  group_by(period, pathway) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(pathway) %>%
  summarise(mean = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(pathway) %>%
  mutate(pc = mean/tot*100)

# QA: Cross-checking with regional tables. Results slightly differ.
region_avg_disch_by_pathway = region_discharges_by_destination %>%
  filter(year(date)==2023 & Region == 'ENGLAND (Type 1 Trusts)') %>%
  group_by(period, pathway) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(pathway) %>%
  summarise(mean = mean(value, na.rm=TRUE), no_months = n_distinct(period)) %>%   # Ensuring that all 12 months are being included
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(pathway) %>%
  mutate(pc = mean/tot*100)


#5. Average monthly number of delayed discharges for latest 12 months

head(ICB_delayed_discharges_by_reason)

ICB_delayed_discharges_by_reason %>% select(metric) %>% distinct()

avg_delayed_by_pathway = ICB_delayed_discharges_by_reason %>%
  filter(year(date)==2023) %>%
  group_by(period, metric) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(metric) %>%
  summarise(mean = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(metric) %>%
  mutate(pc = mean/tot*100) 


# QA: Cross-check with regional tables
region_avg_delayed_by_pathway = region_delayed_discharges_by_reason %>%
  filter(year(date)==2023  & Region == 'ENGLAND (Type 1 Trusts)') %>%
  group_by(period, metric) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(metric) %>%
  summarise(mean = mean(value, na.rm=TRUE), no_months = n_distinct(period)) %>%
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(metric) %>%
  mutate(pc = mean/tot*100) 


#6. Percentage of all discharges that are delayed

avg_delayed_by_pathway %>%
  mutate(metric = case_when(str_detect(metric, 'Pathway') ~ metric,
                            TRUE ~ 'Other (15 categories, all <5%)')) %>%
  ggplot(., aes(x=metric, y=pc, fill=metric)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title='Average delayed discharges by reason, 2023', y='', x='') +
  theme(legend.position = "bottom", legend.direction ='vertical', axis.text.x = element_blank())


# QA with regional tables 

region_avg_delayed_by_pathway %>%
  mutate(metric = case_when(str_detect(metric, 'Pathway') ~ metric,
                            TRUE ~ 'Other (15 categories, all <5%)')) %>%
  ggplot(., aes(x=metric, y=pc, fill=metric)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title='Average delayed discharges by reason, 2023', y='', x='') +
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


region_delayed_discharges_by_reason %>%
  filter((str_detect(metric, 'Pathway 1') | str_detect(metric, 'Pathway 2')) & Region == 'ENGLAND (Type 1 Trusts)') %>%
  group_by(date, metric) %>%
  summarise(value = sum(value, na.rm=TRUE), .groups='keep') %>%
  ggplot(., aes(x = date, y = value, color=metric, group=metric)) +
  geom_line() +
  theme_minimal() +
  xlab('Month and year') +
  ylab('Number of delayed discharges') +
  theme(legend.position = "bottom", legend.direction ='vertical')


