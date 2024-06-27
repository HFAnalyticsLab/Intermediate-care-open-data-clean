# LEEDS DISCHARGE ANALYSIS

rm(list = ls())

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages() & dir.exists(file.path(('Raw_data/')))) {
  print('Project setup run')
}else{
  source('Scripts/00_Setup_and_packages.R')}

# Data download functions
source('Scripts/00b_Scraping_and_download_functions.R')

# Wrangling functions
source('Scripts/00c_Wrangling_functions.R')


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

months <- months[1:26]

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


#################################################
######## LOAD UP FULL SERIES OF DATA ############
#################################################

refreshed_current_files <- list.files(here('Raw_data/Acute_SitRep_data'), pattern='xlsx')  

print(refreshed_current_files)

import_list <- refreshed_current_files


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
  
  
  if (table == 'Table 4' & !(file_name %in% c('september2023.xlsx'))){
    
    discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 16, na = '-', col_names = FALSE)
    
  } else if ((table == 'Table 5') & file_name != 'april2024.xlsx' | (table == 'Table 4' & file_name %in% c('september2023.xlsx'))) {
    
    discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 15, na = '-', col_names = FALSE)
    
  } else if (table == 'Table 5' & file_name == 'april2024.xlsx') {
    
    discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 15, na = '-', col_names = FALSE)
    discharges_all <- discharges_all[2:length(names(discharges_all))]
    
  } else {print('Error')}
  
  table_colnames <- c('Region', 'Org_code', 'Org_name', raw_colnames)

  names(discharges_all) <- table_colnames
  
  stop_point <- which(discharges_all$Org_name == 'Org Name' & discharges_all$Org_name == 'Org Name')
  
  discharge_df <- discharges_all[stop_point+1:(nrow(discharges_all)-stop_point),]
    
  
  return(discharge_df)  
  
}


# Import all available months for both relevant tables at ICB and trust level

table4_trust <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 4', level = 'Trust')})

table5_trust <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 5', level = 'Trust')})



# Add all tables to list

all_tables_list <- list(table4_trust = table4_trust, table5_trust = table5_trust)


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



# Create individual dataframes for each table/level combo, including variable for separating out pathways

trust_discharges_by_destination <- all_months_combined[[1]] %>%
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

trust_delayed_discharges_by_reason <- all_months_combined[[2]]

trust_delayed_discharges_by_reason$value[is.na(trust_delayed_discharges_by_reason$value)] <- 0

trust_discharges_by_destination$value[is.na(trust_discharges_by_destination$value)] <- 0

rm(all_months_combined)  # Clear up workspace

#################################################
################### ANALYSIS ####################
#################################################

leeds_and_similar_destination <- trust_discharges_by_destination %>%
                                filter(Org_code %in% c('RYR', 'R1H', 'RR8', 'RWE', 'RX1')) %>%
  group_by(Org_code, date) %>%
  mutate(total_monthly_discharges = sum(value)) %>%
  ungroup() %>%
  mutate(percent_of_monthly_total = value/total_monthly_discharges)

leeds_and_similar_reasons <- trust_delayed_discharges_by_reason %>%
  filter(Org_code %in% c('RYR', 'R1H', 'RR8', 'RWE', 'RX1')) %>%
  mutate(IC_or_no = case_when(grepl('Pathway 1', metric) == TRUE ~ 'Pathway 1',
                              grepl('Pathway 2', metric) == TRUE ~ 'Pathway 2',
                              grepl('Pathway 3', metric) == TRUE ~ 'Pathway 3',
                              TRUE ~ 'Other reasons')) %>%
  group_by(IC_or_no, Org_code, Org_name, period, date) %>%
  summarise(value = sum(value)) %>%
  group_by(Org_code, date) %>%
  mutate(total_monthly_discharges = sum(value)) %>%
  ungroup() %>%
  mutate(percent_of_monthly_total = value/total_monthly_discharges)



# By destination

leeds_and_similar_destination %>%
  group_by(Org_name, period, date, pathway) %>%
  summarise(value = sum(value)) %>%
  group_by(Org_name, date) %>%
  mutate(total_monthly_discharges = sum(value)) %>%
  ungroup() %>%
  mutate(percent_of_monthly_total = value/total_monthly_discharges) %>%
  filter(pathway == 'P1') %>%
  ggplot() +
    geom_line(aes(x = date, y = percent_of_monthly_total, color = Org_name)) +
    theme_minimal()

leeds_and_similar_destination %>%
  group_by(Org_name, period, date, pathway) %>%
  summarise(value = sum(value)) %>%
  group_by(Org_name, date) %>%
  mutate(total_monthly_discharges = sum(value)) %>%
  ungroup() %>%
  mutate(percent_of_monthly_total = value/total_monthly_discharges) %>%
  filter(pathway == 'P0') %>%
  ggplot() +
  geom_line(aes(x = date, y = percent_of_monthly_total, color = Org_name)) +
  theme_minimal()



### Reasons

# percents
  
leeds_and_similar_reasons %>%
    filter(IC_or_no == 'Pathway 2') %>%
    ggplot() +
    geom_line(aes(x = date, y = percent_of_monthly_total, color = Org_name)) +
    theme_minimal()

leeds_and_similar_reasons %>%
  filter(IC_or_no == 'Pathway 1') %>%
  ggplot() +
  geom_line(aes(x = date, y = percent_of_monthly_total, color = Org_name)) +
  theme_minimal()

# values

leeds_and_similar_reasons %>%
  filter(IC_or_no == 'Pathway 2') %>%
  ggplot() +
  geom_line(aes(x = date, y = value, color = Org_name)) +
  theme_minimal()

leeds_and_similar_reasons %>%
  filter(IC_or_no == 'Pathway 1') %>%
  ggplot() +
  geom_line(aes(x = date, y = value, color = Org_name)) +
  theme_minimal()




