########################################

#  1. ACUTE SITREP DATA ANALYSIS

# In this script, we scrape the NHSE webpage for Delayed Discharge SitRep data and install any available datasets which
# we do not currently have in our working directory. This will act to both download all available data on a first run, 
# and download 

# There are two tables we are interested in in the SitRep data: Table 4, detailing ___, and Table 5, showing ___. 




# Check if project setup has been run, and run it if not
if (setup_run == 'RUN') {   
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

files_comparison_df <- data.frame(months, month_present_on_web, month_present_in_files, data_nodes$links[1:17]) %>%    # Join all with download links and mark links to download
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
    discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 15)
    
  } else if (table == 'Table 5') {
    raw_colnames <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 4, n_max = 0)
    discharges_all <- read_excel(paste0('Raw_data/Acute_SitRep_data/', file_name), sheet = table, skip = 14)
    
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
