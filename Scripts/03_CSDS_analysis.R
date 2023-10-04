################################################################################
################################################################################
# Script written in R 4.0.2

#  3. COMMUNITY SERVICES DATASET DATA ANALYSIS

################################################################################
################################################################################


# Check if project setup has been run, and run it if not
if (setup_run == 'RUN') {   
}else{
  source('00_Setup_and_packages.R')}


################################################
######## SCRAPE AND DOWNLOAD LATEST DATA #######
################################################

rm(list=ls())
## Identify which CSDS files are currently in our directory
current_files <- list.files(here('Raw_data/CSDS_data'), pattern='csv')  


## Scrape the NHS webpage to identify which months are currently available

CSDS_mainpage_link <- 'https://digital.nhs.uk/data-and-information/publications/statistical/community-services-statistics-for-children-young-people-and-adults'  # Link to NHS webpage

monthly_names <- read_html(CSDS_mainpage_link) %>%
html_nodes(xpath="//a[contains(@class, 'cta__button')]") %>%
  html_text() %>%
  tolower() %>%
  as.data.frame()
rename(months = '.')

months <- sub(" ", "", word(monthly_names$months, 1, sep = "\\:")) # Create a list of months available on the webpage in the same format as the data download links

monthly_links <- read_html(CSDS_mainpage_link) %>%
  html_nodes(xpath="//a[contains(@class, 'cta__button')]/@href") %>%
  html_text() %>%
  paste0("https://digital.nhs.uk",.) %>%
  as.data.frame()

month <- 'july-2023'

test_link <- 'https://digital.nhs.uk/data-and-information/publications/statistical/community-services-statistics-for-children-young-people-and-adults/july-2023'

test_scrape <- read_html(test_link) %>%
  html_nodes(xpath="//a[contains(@class, 'nhsd-a-link')]/@href") %>%
  html_text() %>%
  paste0("https://digital.nhs.uk",.) %>%
  as.data.frame() %>%
  rename(links = '.') %>%
  filter(grepl('datasets', links) == TRUE|grepl('data-sets', links) == TRUE)

datasets_link <- test_scrape$links[grepl(paste0(month,'/datasets'), test_scrape$links) == TRUE]

datasets_scrape <- read_html(datasets_link) %>%
  html_nodes(xpath="//a[contains(@class, 'nhsd-a-link')]/@href") %>%
  html_text() %>%
  paste0("https://digital.nhs.uk",.) %>%
  as.data.frame() %>%
  rename(links = '.') %>%
  filter(grepl('exp-core-data', links) == TRUE|grepl('exp-data', links) == TRUE)







