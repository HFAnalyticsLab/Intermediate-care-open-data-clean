## 00.b. WEB SCRAPING FUNCTIONS

# This script provides functions which download the relevant data for each of our analysis scripts. 
# For the delayed discharges SitRep datasets, the functions in this script scrape the webpages for these 
# data collections and check for any new editions, downloading any which cannot be found in our current raw data folder.
# For the ASC and expenditure analysis scripts, the functions here download the relevant data directly form. 
# These are annual datasets which do not have many editions and are not regularly updated, and as such do not require web scrapers. 


# Acute hospital SitRep webscraper

acute_sitrep_scrape <- function(x = NULL){
  
  
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
  
  rm(checklist_files, checklist_web, data_nodes, files_comparison_df, links_to_download, monthly_names)  # Clear up workspace
  
} 


# Community hopital SitRep webscraper


community_sitrep_scrape <- function(x= NULL){
  
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
  
  rm(checklist_files, checklist_web, data_nodes, files_comparison_df, links_to_download, monthly_names)  # Clear up workspace
  
  
}


## ASC and expenditure data download

ASC_datadownload <- function(x=NULL){

  # 2016/17
if (file.exists('Raw_data/ASC_data/ASCFR_2016-17.xlsx')){print('2016/17 ASCFR data already loaded')
} else{download.file('https://files.digital.nhs.uk/excel/o/4/reference_data_tables_combined.xlsx', 'Raw_data/ASC_data/ASCFR_2016-17.xlsx')}


# 2017/18

if (file.exists('Raw_data/ASC_data/ASCFR_2017-18.xlsx')){print('2017/18 ASCFR data already loaded')
} else{
  download.file('https://files.digital.nhs.uk/4F/11BD6D/SALT%20and%20ASCFR%20Reference%20Tables%20%28Suppressed%29%20v2.xlsx', 'Raw_data/ASC_data/ASCFR_2017-18.xlsx')
}

# 2018/19

if (file.exists('Raw_data/ASC_data/ASCFR_2018-19.xlsx')){print('2018/19 ASCFR data already loaded')
} else {
  download.file('https://files.digital.nhs.uk/C4/9FBB8A/ASCFR%20and%20SALT%20Reference%20Tables%202018-19.xlsx', 'Raw_data/ASC_data/ASCFR_2018-19.xlsx')
}

# 2019/20

if (file.exists('Raw_data/ASC_data/ASCFR_2019-20.xlsx')){print('2019/20 ASCFR data already loaded')
} else{
  download.file('https://files.digital.nhs.uk/BF/7AEF16/ASCFR%20and%20SALT%20Reference%20Tables%202019-20.xlsx', 'Raw_data/ASC_data/ASCFR_2019-20.xlsx')
}

# 2020/21

if (file.exists('Raw_data/ASC_data/ASCFR_2020-21.xlsx')){print('2020/21 ASCFR data already loaded')
} else{
  download.file('https://files.digital.nhs.uk/B5/81EEB4/ASCFR%20and%20SALT%20Reference%20Tables%202020-21%20v2%2022-11-2022.xlsx', 'Raw_data/ASC_data/ASCFR_2020-21.xlsx')
}

# 2021/22

if (file.exists('Raw_data/ASC_data/ASCFR_2021-22.xlsx')){print('2021/22 ASCFR data already loaded')
} else{
  download.file('https://files.digital.nhs.uk/AF/4C9A4F/ASCFR%20and%20SALT%20Data%20Tables%202021-22.xlsx', 'Raw_data/ASC_data/ASCFR_2021-22.xlsx')
}

#2022/23

if (file.exists('Raw_data/ASC_data/ASCFR_2022-23.xlsx')){print('2022/23 ASCFR data already loaded')
} else{
  download.file('https://files.digital.nhs.uk/03/C96CF8/ASCFR%20and%20SALT%20Data%20Tables%202022-23%20v2.xlsx', 'Raw_data/ASC_data/ASCFR_2022-23.xlsx')
}

### DOWNLOAD ASCOF DATA


if (file.exists('Raw_data/ASC_data/ASCOF-time-series.xlsx')){print('ASCOF time series data already loaded')
} else{
  download.file('https://files.digital.nhs.uk/EE/17E313/meas-from-asc-of-eng-2022-23-timeseries.xlsx', 'Raw_data/ASC_data/ASCOF-time-series.xlsx')
}

}


