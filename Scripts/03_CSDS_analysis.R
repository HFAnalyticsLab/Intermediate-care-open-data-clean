################################################################################
################################################################################
# Script written in R 4.0.2

#  3. COMMUNITY SERVICES DATASET DATA ANALYSIS

################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages()) { 
  print('Project setup run')   
}else{
  source('00_Setup_and_packages.R')
  print('Project setup run')
  }


################################################
######## SCRAPE AND DOWNLOAD LATEST DATA #######
################################################


## Identify which CSDS files are currently in our directory
current_files <- list.files(here('Raw_data/CSDS_data'), pattern='csv')


## Scrape the NHS webpage to identify which months are currently available

CSDS_mainpage_link <- 'https://digital.nhs.uk/data-and-information/publications/statistical/community-services-statistics-for-children-young-people-and-adults'  # Link to NHS webpage


monthly_names <- read_html(CSDS_mainpage_link) %>%                # Identify which months of data are listed on the web page
html_nodes(xpath="//a[contains(@class, 'cta__button')]") %>%
  html_text() %>%
  tolower() %>%
  as.data.frame() %>%
  rename(labels = '.')

monthly_links <- read_html(CSDS_mainpage_link) %>%                      # Extract links to all the dataset subpages available on the webpage
  html_nodes(xpath="//a[contains(@class, 'cta__button')]/@href") %>%
  html_text() %>%
  paste0("https://digital.nhs.uk",.) %>%
  as.data.frame() %>%
  rename(links = '.')

months_links_df <- cbind(monthly_names, monthly_links) %>%              # Create dataframe joining monthly labels with their links, amd isolate months in an amenable format from
  filter(grepl('     ', labels) == FALSE) %>%                           # the end of each link. Use these to create date format versions of each of these labels (used to separate
  mutate(names_only = sub('https://digital.nhs.uk/data-and-information/publications/statistical/community-services-statistics-for-children-young-people-and-adults/', 
                          '', links)) %>%                               # the scraping and downloading below into pre and post Jan 2020 timeframes, as formatting changed then)  
  mutate(dates_as_dates = lubridate::my(names_only)) 



## Compare the list of available data to the data we already have in our directory

checklist_files <- lapply(1:nrow(months_links_df), function(i){                      # Check for monthly files in our raw data directory
    file.exists(paste0('Raw_data/CSDS_data/', months_links_df$names_only[[i]], '.csv'))
  })

month_present_in_files <- sapply(1:length(checklist_files), function(i){      # Create vector describing whether a month is in our data directory
  sum(checklist_files[[i]])
})

months_links_df <- cbind(months_links_df, month_present_in_files) %>%        # Compare existing files to those available online, and filter out any which are already downloaded
  filter(month_present_in_files == 0)

# The function below takes any available monthly datasets which we have not already downloaded, and uses the links we scraped from the main CSDS webpage to navigate to that data's 
# publication page. For datasets published after January 2020, we scrape this page for another link taking us to the repository of datasets, where we identify the relevant csv link, 
# and download it. For publications prior to January 2020, we simply navigate to the publication page, scrape to find the csv download link, and download directly from there.
# This process all gets a bit complicated by the fact that these files are sometimes saved as .csv and sometimes as .zip, which is the reason for the several ifelse statements within.
# Any cases where the csv/dataset page links we're after can't be found are flagged when this function is run.
# As mentioned above, Oct-Dec 2017 data will not be downloaded by this function, and warnings will be received to that effect when run. 

scrape_download_function <- function(i){
  
  if (months_links_df$dates_as_dates[[i]] <= '2020-01-01'){
    
    month <- months_links_df$names_only[[i]]
    
    subpage_link <- months_links_df$links[[i]]
    
    subpage_scrape <- read_html(subpage_link) %>%
      html_nodes(xpath="//a[contains(@class, 'nhsd-a-box-link')]/@href") %>%
      html_text() %>%
      as.data.frame() %>%
      rename(links = '.') %>%
      filter(grepl('-data.csv', links) == TRUE)
    
    if (nrow(subpage_scrape) != 1){
      print(paste0('Warning: check ', month))
    } else{
      download.file(subpage_scrape$links[1], paste0('Raw_data/CSDS_data/', month, '.csv'))
    }
    
    
    
    
  } else{
    month <- months_links_df$names_only[[i]]
    
    subpage_link <- months_links_df$links[[i]]
    
    subpage_scrape <- read_html(subpage_link) %>%
      html_nodes(xpath="//a[contains(@class, 'nhsd-a-link')]/@href") %>%
      html_text() %>%
      paste0("https://digital.nhs.uk",.) %>%
      as.data.frame() %>%
      rename(links = '.') %>%
      filter(grepl('/datasets', links) == TRUE|grepl('/data-set', links) == TRUE)
    
      datasets_link <- subpage_scrape$links[grepl(paste0(month,'/'), subpage_scrape$links) == TRUE]
      
      datasets_scrape <- read_html(datasets_link) %>%
        html_nodes(xpath="//a[contains(@class, 'nhsd-a-link')]/@href") %>%
        html_text() %>%
        as.data.frame() %>%
        rename(links = '.') %>%
        filter(grepl('exp-core-data', links) == TRUE|grepl('exp-data.csv', links) == TRUE|grepl('exp-data.zip', links) == TRUE|grepl('Refresh_Core', links) == TRUE)    # One time they labeled the csv data 'Refresh_Core_V2.csv', hence the last term. Not the most elegant solution but currently functions!
      
      if (nrow(datasets_scrape) != 1){
        
        print(paste0('Warning: check ', month))
        
      } else{
        
        if (grepl(datasets_scrape$links[1], '.zip')){
          
          temp <- tempfile()
          
          download.file(datasets_scrape$links[[1]], temp)
          
          inside_zip <- unzip(temp, list = TRUE)
          
          if (nrow(inside_zip != 1)){ print(paste0('Warning: check zip for ', month))
          }else{unzip(temp, exdir = 'Raw_data/CSDS_data/')
            
            file.rename(paste0('Raw_data/CSDS_data/', inside_zip$Name[[1]]), paste0('Raw_data/CSDS_data/', month, '.csv'))
          }
          
        } else {
          
          download.file(datasets_scrape$links[1], paste0('Raw_data/CSDS_data/', month, '.csv'))
          
        }
        }
      
  }
  
  }


if(nrow(months_links_df >= 1)){
  lapply(1:nrow(months_links_df), scrape_download_function)     # Feed all undownloade files available on the NHSD page into the sraping and download function
}


################################################
################ LOAD IN ALL DATA ##############
################################################

all_csds_csv_byprovider <- lapply(1:length(current_files), function(i){
  csv <- read_csv(paste0('Raw_data/CSDS_data/', current_files[[i]]))
  
  names(csv) <- tolower(names(csv))
  
  names(csv) <- sub('organisation', 'org', names(csv))
  
  names(csv) <- sub(' ', '_', names(csv))
  
  if('Provider' %in% levels(as.factor(csv$org_level))){
    csv <- csv %>%
      filter(org_level %in% c('Provider','All Submitters'))
  } else {
    csv <- csv %>%
      filter(org_level %in% c('Trust','AT'))
  }
  
  return(csv)
       })

names(all_csds_csv_byprovider) <- lapply(1:length(current_files), function(i){sub('.csv', '', current_files[[i]])})

all_csds_csv_ICB <- lapply(1:length(current_files), function(i){
  read_csv(paste0('Raw_data/CSDS_data/', current_files[[i]]))
  
})


