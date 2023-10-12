################################################################################
################################################################################
# Script written in R 4.0.2

#  3. COMMUNITY SERVICES DATASET DATA ANALYSIS

# In this script, we download any new editions of the community services dataset (CSDS) from NHS England, process the data into an analyzable format, and create time series of key metrics.
# To run this script, you only need to have run 00_Setup_and_packages.R prior

# In the scraping portion, we scrape the NHSE webpage for community services data and install any available datasets which
# we do not currently have in our working directory. This will act to both download all available data on a first run, 
# and download only any new editions of the dataset on subsequent runs. The singular time series dataset is updated with each run. 

# NOTE: I don't think there's anything we can get out of the editions of the dataset from before august 2020. If we're confident in this 
# conclusion, will alter the scraper to only download editions of the data from beyond that point.

# NOTE: We seem to see a transition from submitters recording IC in the Intermediate care service measure to the specific categories over time, creating a false appearance of 
# decline in that measure and growth in the others. Maybe we need to sum these? Truncate the time series?

# Note: need to add some workspace clearouts here where appropriate due to large numbers of files
################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages()) { 
  print('Project setup run')   
}else{
  source('Scripts/00_Setup_and_packages.R')
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
  mutate(dates_as_dates = lubridate::my(names_only))                    # Labels with a large black space are omitted - these are from the "Upcoming publications" section and so are of no interest to us



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
  lapply(1:nrow(months_links_df), scrape_download_function)     # Feed all undownloaded files available on the NHSD page into the sraping and download function
}


################################################
################ LOAD IN ALL DATA ##############
################################################

refreshed_current_files <- list.files(here('Raw_data/CSDS_data'), pattern='csv')  

print(refreshed_current_files)

all_csds_csv_byprovider <- lapply(1:length(refreshed_current_files), function(i){
  csv <- read_csv(paste0('Raw_data/CSDS_data/', refreshed_current_files[[i]]))
  
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

names(all_csds_csv_byprovider) <- lapply(1:length(refreshed_current_files), function(i){sub('.csv', '', refreshed_current_files[[i]])})


# I don't think we can find anything useable in the pre-august 2020 editions of the dataset

csds_provider_currentformat <- all_csds_csv_byprovider[c('august-2020', 'september-2020', 'october-2020', 'november-2020', 'december-2020', 'january-2021', 'february-2021',
                                                  'march-2021', 'april-2021', 'may-2021', 'june-2021', 'july-2021', 'august-2021', 'september-2021', 'october-2021', 'november-2021', 'december-2021',
                                                  'january-2022', 'february-2022', 'march-2022', 'april-2022', 'may-2022', 'june-2022', 'july-2022', 'august-2022', 'september-2022', 'october-2022', 
                                                  'november-2022', 'december-2022',  'january-2023', 'february-2023', 'march-2023', 'april-2023', 'may-2023', 'june-2023', 'july-2023')] 


csds_provider_IConly <- lapply(1:length(csds_provider_currentformat), function(i){
  
  df <- csds_provider_currentformat[[i]] %>%
    filter(count_of == 'UniqueCareContacts' & measure %in% c(18, 51, 52, 53, 54, 48, 45))
    
  
  return(df)
  
})


csds_IC_all <- do.call('rbind', csds_provider_IConly)


#####################################################
################ ANALYZE DATA #######################
#####################################################

# NOTE: We seem to see a transition from submitters recording IC in the Intermediate care service measure to the specific categories over time, creating a false appearance of 
# decline in that measure and growth in the others. Maybe we need to sum these? Truncate the time series?

csds_IC_all %>%
  filter(measure == 18 & org_level == 'All Submitters') %>%
  ggplot(., aes(x = ymd(reporting_period_start), y = measure_value)) +
  geom_line() +
  theme_minimal()

