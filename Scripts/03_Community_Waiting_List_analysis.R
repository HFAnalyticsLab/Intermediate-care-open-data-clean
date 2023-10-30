################################################################################
################################################################################
# Script written in R 4.0.2

#  2. COMMUNITY SITREP DATA ANALYSIS

# In this script, we download any new editions of the discharges from community hospitals SitRep data from NHS England, process the data into an analyzable format, and create time series of key metrics.
# To run this script, you only need to have run 00_Setup_and_packages.R prior

# Much of this script functions in the same way as the preceding 01_Acute_Sitrep_analysis.R script, with minor variations to account for formatiing differences. 
# As it currently stand, this script works to scrape, download and wrangle sitrep data up until August 2023. For future data, please be 
# attentive to potential formatting differences which may complicate the wrangling stages.

# WARNING: dates are currently loading in in a strange format from the weekly timeseries, will need fixing

################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages()) {
  print('Project setup run')
}else{
  source('Scripts/00_Setup_and_packages.R')}


################################################
######## SCRAPE AND DOWNLOAD LATEST DATA #######
################################################

## Identify which SitRep files are currently in our directory
current_files <- list.files(here('Raw_data/Community_Waiting_List_data'), pattern='xlsx')  


## Scrape the NHS webpage to identify which months are currently available

community_waitinglist_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/community-health-services-waiting-lists/'  # Link to NHS webpage

monthly_names <- read_html(community_waitinglist_link) %>%    # Identify which months of data are listed on the web page
  html_elements('p') %>%
  html_text() %>%
  tolower() %>%
  as.data.frame() %>%
  rename(months = '.') %>%
  filter((grepl('community-health', months)) == TRUE)

data_nodes <- read_html(community_waitinglist_link) %>%  # Extract links to all the datasets available on the webpage
  html_elements('a') %>%
  html_attr('href') %>%
  as.data.frame() %>%
  rename(links = '.') %>%
  filter(grepl('.xlsx', links) == TRUE)

months_links_df <- cbind(monthly_names, data_nodes) %>%
  filter(grepl('timeseries', links)==FALSE)

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
  file.exists(paste0('Raw_data/Community_Waiting_List_data/', months[[i]], '.xlsx'))
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
    
    download.file(url = links_to_download$links[[i]], destfile = paste0('Raw_data/Community_Waiting_List_data/', links_to_download$months[[i]], '.xlsx'))
  }
}


rm(checklist_files, checklist_web, data_nodes, files_comparison_df, links_to_download, monthly_names)  # Clear up workspace

#################################################
######## LOAD UP FULL SERIES OF DATA ############
#################################################

# Create new list of files in our directory now that we've scraped for new data. RECOMMENDED TO CHECK AND MAKE SURE NO MONTHS ARE REPEATED
refreshed_current_files <- list.files(here('Raw_data/Community_Waiting_List_data'), pattern='xlsx')  

print(refreshed_current_files)

import_list <- refreshed_current_files[!refreshed_current_files == 'latest_time_series.xlsx'] # Remove time series from list to process with the below function


# Read and combine relevant sheets
# Table 3: National overview of waiting list metrics by service, August 2023
# Table 4: Total waiting list by service, August 2023

read_and_create_community_waitlist <- function(files, sheet_names) {
  read_one_file <- function(file_name, sheet_name) {
    # Extract the period from the file name
    period <- sub("community-health-services-waiting-lists-", "", file_name)
    period <- sub("\\.xlsx", "", period)
    
    file_path <- paste0('Raw_data/Community_Waiting_List_data/', file_name)
    sheet_data <- read_excel(file_path, sheet = sheet_name, skip = 4, na = '', col_names = TRUE, .name_repair = 'universal')
    
    # Convert all columns to character to ensure consistency
    sheet_data <- sheet_data %>%
      mutate(across(everything(), as.character))
    
    # Add a new column for the period
    sheet_data <- sheet_data %>%
      mutate(Period = period)
    
    return(sheet_data)
  }
  
  all_dataframes <- lapply(sheet_names, function(sheet_name) {
    sheet_data_list <- lapply(files, function(file_name) {
      read_one_file(file_name, sheet_name)
    })
    
    # Combine the list of data frames into a single data frame
    combined_data <- do.call(bind_rows, sheet_data_list)
    
    return(combined_data)
  })
  
  return(all_dataframes)
}

# List of Excel file names
file_names <- import_list

# List of sheet names you want to read
sheet_names <- c("Table 3", "Table 4")

# Call the function to read and create data frames for multiple sheets from multiple files
all_community_waiting <- read_and_create_community_waitlist(file_names, sheet_names)

# Assign each data frame to a separate variable
community_waiting_by_service <- all_community_waiting[[1]]
community_waiting_subnational <- all_community_waiting[[2]]

rm(all_community_waiting, months_links_df)

# Clean up tables

community_waiting_by_service_clean = community_waiting_by_service %>%
  rename(service = '...1') %>%
  filter(service %in% c('England' , 'Adult services' , '(A) Intermediate care and reablement') & 
           Period!='2023-24-august') %>%
  mutate(Period = str_remove_all(Period, '-revised'),
         month = str_remove_all(Period, "^(2023-24-|2022-23-)"),
         year = case_when(  month %in% c('january', 'february', 'march') ~ as.numeric(substr(Period, 6, 7)) + 2000,
                            TRUE ~ as.numeric(substr(Period, 1, 4))),
         date = as.Date(paste("01", month, year, sep = "-"), format = "%d-%B-%Y")) %>%
  select(-Period, -month, -year) %>%
  pivot_longer(cols=!c('service', 'date'), names_to='waiting_category', values_to='number_waiting') %>%
  mutate(number_waiting=as.numeric(number_waiting))
         


# Get data by region and ICB
community_waiting_subnational_clean = community_waiting_subnational %>%
  select(geography_type = '...1', geography_name = '...2', waiting_for_intermediate_care ='.A..Intermediate.care.and.reablement', Period) %>%
  fill(geography_type, .direction='down') %>%
  filter(Period!='2023-24-august' & geography_type %in% c('Region', 'ICB')) %>%
  mutate(Period = str_remove_all(Period, '-revised'),
         month = str_remove_all(Period, "^(2023-24-|2022-23-)"),
         year = case_when(  month %in% c('january', 'february', 'march') ~ as.numeric(substr(Period, 6, 7)) + 2000,
                            TRUE ~ as.numeric(substr(Period, 1, 4))),
         date = as.Date(paste("01", month, year, sep = "-"), format = "%d-%B-%Y")) %>%
  select(-Period, -month, -year) %>%
  mutate(waiting_for_intermediate_care=as.numeric(waiting_for_intermediate_care))

  

#################################################
################### ANALYSIS ####################
#################################################

# Trends over time for all CS and trend for IC

# IC waiting list compared to total waiting list for all ages and adults only
community_waiting_by_service_clean %>%
  filter(waiting_category == 'Total.waiting.list') %>%
  ggplot(., aes(x = date, y = number_waiting, group=service, color=service)) +
  geom_line() +
  theme_minimal()

# Trend for IC only
community_waiting_by_service_clean %>% 
  filter(waiting_category == 'Total.waiting.list' & service =='(A) Intermediate care and reablement') %>%
  ggplot(., aes(x = date, y = number_waiting)) +
  geom_line() + 
  theme_minimal()

community_waiting_by_service_clean %>% 
  filter(service =='(A) Intermediate care and reablement' & waiting_category!='Total.waiting.list') %>%
  ggplot(., aes(x = date, y = number_waiting, color=waiting_category)) +
  geom_line() + 
  theme_minimal()

community_waiting_by_service_clean %>%
  filter(service =='(A) Intermediate care and reablement' & date==max(date)  & waiting_category!='Total.waiting.list') %>%
  mutate(tot=sum(number_waiting),
         percentage = (number_waiting / tot) * 100)  

  

################################################
############# VISUALISATIONS ###################
################################################





##########################################################


