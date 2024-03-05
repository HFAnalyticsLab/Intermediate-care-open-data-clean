################################################################################
################################################################################
# Script written in R 4.0.2

#  5. EXPENDITURE ANALYSIS: NATIONAL COST COLLECTION AND ASC-FRs

# This script downloads all relevant editions of the National Cost Collection and Adult Social Care Financial Returns (ASC-FR) datasets. 
# and a time series of the Adult Social Care Outcomes Framework dataset.

################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages() & dir.exists(file.path(here('Raw_data/NCC_data')))) { 
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

t24_final <- all_ascfr_data[[3]]

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

england_ascfr_expenditure$episodes <- c(NA, NA, 246035, 255275, 261605, 246600, 252150, 251255)

england_ascfr_expenditure <- england_ascfr_expenditure %>%
  mutate(spend_per_episode = value/episodes) %>%
  mutate(real_spend_per_episode = real_expenditure/episodes)
  
ggplot(england_ascfr_expenditure, aes(x = date, y = value)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

ggplot(england_ascfr_expenditure, aes(x = date, y = real_expenditure)) +
  geom_line(color = '#F8766D') +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) 

england_ascfr_expenditure$real_expenditure[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_expenditure[england_ascfr_expenditure$date == '2022-03-31']-1
england_ascfr_expenditure$real_expenditure[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_expenditure[england_ascfr_expenditure$date == '2021-03-31']-1

england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2022-03-31']-1
england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2021-03-31']-1
england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2023-03-31']/england_ascfr_expenditure$real_spend_per_episode[england_ascfr_expenditure$date == '2020-03-31']-1

t32_final %>%
  filter(metric == 'Expenditure per 100,000 adults' & region_name == 'England') %>%
  ggplot(., aes(x = as.Date(date), y = as.numeric(value))) +
  geom_line(color = '#F8766D') +
  theme_minimal()

t32_final %>%
  filter(metric == 'Gross current expenditure' & region_name == 'England') %>%
  ggplot(., aes(x = as.Date(date), y = as.numeric(value*1000))) +
  geom_line(color = '#F8766D') +
  theme_minimal()

t32_final %>%
filter(region_name == 'England' & metric == 'Expenditure per 100,000 adults' & is.na(LA_code))
  ggplot(., aes(x = reorder(region_name,  -value), y = as.numeric(value*1000))) +
  geom_col(fill = '#F8766D') +
  theme_minimal() +
  coord_flip() +
  xlab('Regions') +
  ylab('Expenditure on ST-MAX per 100,000 adults')


# NCC 

england_NCC_expenditure <- NCC_combined %>%
  filter(metric == 'activity' | metric == 'national_average_unit_cost') %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(expenditure = as.numeric(activity)*as.numeric(national_average_unit_cost))

england_NCC_expenditure_short <- NCC_combined %>%
  filter((metric == 'activity' | metric == 'national_average_unit_cost') & date != '2022-03-31') %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(expenditure = as.numeric(activity)*as.numeric(national_average_unit_cost))

ggplot(data = england_NCC_expenditure_short, aes(x=date, y=expenditure, color = currency_name))+
  geom_line() +
  theme_minimal()

# unit costs

ggplot(data = england_NCC_expenditure_short, aes(x=date, y=national_average_unit_cost, color = currency_name))+
  geom_line() +
  theme_minimal()

# Real terms NCC


real_NCC_expenditure <- england_NCC_expenditure_short %>%
  left_join(., deflator_shortened, by = c('period'='year')) %>%
  mutate(real_expenditure = expenditure/deflator*100) %>%
  mutate(real_unit_cost = national_average_unit_cost/deflator*100)

ggplot(data = real_NCC_expenditure, aes(x=date, y=real_expenditure, color = currency_name))+
  geom_line() +
  theme_minimal()

ggplot(data = real_NCC_expenditure, aes(x=date, y=real_unit_cost, color = currency_name))+
  geom_line() +
  theme_minimal() +
  xlab('Date') +
  ylab('Unit Cost')


ggplot()+
  geom_line(data = real_NCC_expenditure, aes(x=date, y=real_expenditure, color = currency_name)) +
  geom_line(data = england_ascfr_expenditure, aes(x = date, y = real_expenditure, color = 'ST-MAX (Local Authorities)')) +
  theme_minimal() +
  xlab('Date') +
  ylab('Expenditure') +
  theme(legend.text = element_text(size = 11))


## TABLE OUTPUTS

# Figure 6: Real unit costs

output_figure6 <- england_ascfr_expenditure %>%
  select(date, real_spend_per_episode) %>%
  drop_na()

write_csv(output_figure6, 'Outputs/figure6_unitcosts.csv')
 

