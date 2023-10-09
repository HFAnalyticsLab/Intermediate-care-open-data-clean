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

# As it currently stands, this script works to scrape, download and wrangle sitrep data up until August 2023. For future data, please be 
# attentive to potential formatting differences which may complicate the wrangling stages.

################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages()) {   
}else{
  source('00_Setup_and_packages.R')}


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
  filter((grepl('daily-discharge', months)) == TRUE)

months <- sub(" ", "", word(monthly_names$months, 1, sep = "\\:")) # Create a list of months available on the webpage in the same format as the data download links
  
data_nodes <- read_html(acute_sitrep_link) %>%  # Extract links to all the datasets available on the webpage
  html_elements('a') %>%
  html_attr('href') %>%
  as.data.frame() %>%
  rename(links = '.') %>%
  filter(grepl('.xlsx', links) == TRUE)

time_series_link <- data_nodes$links[grep('timeseries', data_nodes$links)] # Find time series in the list of data nodes


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
    raw_colnames <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 5, n_max = 0)
    discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 15, na = '-')
    
  } else if (table == 'Table 5') {
    raw_colnames <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 4, n_max = 0)
    discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 14, na = '-')
    
  } else {print('Error')}
  
  table_colnames <- c('Region', 'Org_code', 'Org_name', names(raw_colnames))
  
  names(discharges_all) <- table_colnames
  
  stop_point <- which(discharges_all$Org_name == 'Org Name' & discharges_all$Org_name == 'Org Name')
  
  if (level == 'ICB'){
    discharge_df <- discharges_all[1:(stop_point-2),]
  } else if(level == 'Trust'){
    discharge_df <- discharges_all[stop_point+1:(nrow(discharges_all)-stop_point),]
  } else{print('Error')}
  
  return(discharge_df)  
  
}


# Import all available months for both relevant tables at ICB and trust level

table4_ICB <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 4', level = 'ICB')})

table4_trust <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 4', level = 'Trust')})

table5_ICB <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 5', level = 'ICB')})

table5_trust <- lapply(1:length(import_list), function(i){import_sheets_function(file_name = import_list[i], table = 'Table 5', level = 'Trust')})

all_tables_list <- list(table4_ICB = table4_ICB, table4_trust = table4_trust, table5_ICB = table5_ICB, table5_trust = table5_trust)


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

ICB_discharges_by_destination <- all_months_combined[[1]] %>%
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

trust_discharges_by_destination <- all_months_combined[[2]] %>%
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

ICB_delayed_discharges_by_reason <- all_months_combined[[3]]

trust_delayed_discharges_by_reason <- all_months_combined[[4]]

rm(table4_ICB, table4_trust, table5_ICB, table5_trust, all_tables_list, all_tables_pivoted, all_months_combined)  # Clear up workspace


## Load in time series

daily_timeseries <- read_excel('Raw_data/Acute_SitRep_data/latest_time_series.xlsx', sheet = 'Daily Series', skip = 5) 

weekly_timeseries <- read_excel('Raw_data/Acute_SitRep_data/latest_time_series.xlsx', sheet = 'Weekly Series', skip = 6)


#################################################
################### ANALYSIS ####################
#################################################

ICB_discharges_by_destination %>%
  filter(pathway == 'P1') %>%
  group_by(period, date) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value)) +
  geom_line() +
  theme_minimal()


################################################
############# VISUALISATIONS ###################
################################################


ggplot(daily_timeseries, aes(x = Date, y = `P1 - Domestic home with reablement support`)) +
  geom_line() +
  theme_minimal()

ggplot(weekly_timeseries, aes(x = `Week commencing`, y = `Pathway 1: awaiting availability of resource for assessment and start of care at home`)) +
  geom_line() +
  theme_minimal()

ICB_discharges_by_destination %>%
  filter(pathway == 'P1') %>%
  group_by(period, date) %>%
  summarise(value = sum(value)) %>%
  ggplot(., aes(x = date, y = value)) +
  geom_line() +
  theme_minimal()



