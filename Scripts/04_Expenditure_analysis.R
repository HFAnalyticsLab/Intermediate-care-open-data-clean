################################################################################
################################################################################
# Script written in R 4.0.2

#  4. EXPENDITURE ANALYSIS: ASC-FRs and GDP deflator

# This script downloads and wrangles all relevant editions of the Adult Social Care Financial Returns publication and the latest GDP deflator (December 2023 edition at time of publication),
# then combines data from these sources to derive real unit costs (2022-23 prices) of local authority-provided reablement over time. 

################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages() & dir.exists(file.path(('Raw_data/')))) { 
  print('Project setup run')
}else{
  source('Scripts/00_Setup_and_packages.R')}

# Data download functions
source('Scripts/00b_Scraping_and_download_functions.R')


# Wrangling functions
source('Scripts/00c_Wrangling_functions.R')

################################################
############### DOWNLOAD DATA ##################
################################################

### DOWNLOAD ASCR-FR DATA

ASC_datadownload()

# Download deflator

download.file('https://assets.publishing.service.gov.uk/media/659c102bc23a1000128d0cb8/GDP_Deflators_Qtrly_National_Accounts_December_2023_update.xlsx', 'Raw_data/GDP_deflator.xlsx')

##########################################################################
################# WRANGLE DATA INTO AMENABLE FORMAT ######################
##########################################################################


## WRANGLE ASC-FR DATA

t32_final <- expenditure_ASC_wrangle()

# Additional ASC data 

all_ascfr_data <- ASCFR_wrangle()

t28_final <- all_ascfr_data[[6]]

rm(all_ascfr_data)

# Wrangle deflator 

deflator <- read_excel('Raw_data/GDP_deflator.xlsx', skip = 6) %>%
  select(year = 1, deflator = 2)

deflator <- deflator[1:68,]  

###################################################
################### ANALYSIS ######################
###################################################

max_date <- max(t32_final$date)

deflator_shortened <- deflator[58:68,]

deflator_shortened$deflator <- as.numeric(deflator_shortened$deflator)


# ASC-FRs

england_ascfr_expenditure <- t32_final %>%
  filter(metric == 'Gross current expenditure' & region_name == 'England') %>%
  mutate(date = as.Date(date)) %>%
  mutate(value = as.numeric(value)*1000) %>%
  mutate(deflator_dates = c('2015-16', '2016-17', '2017-18','2018-19', '2019-20', '2020-21', '2021-22', '2022-23')) %>%
  left_join(., deflator_shortened, by = c('deflator_dates'='year')) %>%
  mutate(real_expenditure = value/deflator*100)

episodes <- t28_final %>%
  filter(region_name == 'England' & metric == 'Completed episodes of ST-Max' & age_band == 'Total')

england_ascfr_expenditure$episodes <- c(NA, NA, episodes$value)

england_ascfr_expenditure <- england_ascfr_expenditure %>%
  mutate(spend_per_episode = value/episodes) %>%
  mutate(real_spend_per_episode = real_expenditure/episodes)

# Change in real expenditure vs current year
england_ascfr_expenditure$real_expenditure[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_expenditure[england_ascfr_expenditure$date == '2022-03-31']-1
england_ascfr_expenditure$real_expenditure[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_expenditure[england_ascfr_expenditure$date == '2021-03-31']-1

# Change in unit costs v current year
england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2022-03-31']-1
england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2021-03-31']-1
england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2020-03-31']-1


## TABLE OUTPUTS

# Figure 5: Real unit costs

output_figure5 <- england_ascfr_expenditure %>%
  select(date, real_spend_per_episode) %>%
  drop_na()

write_csv(output_figure5, 'Outputs/figure5.csv')
 

