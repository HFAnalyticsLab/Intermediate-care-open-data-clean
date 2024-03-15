################################################################################
################################################################################
# Script written in R 4.0.2

#  1. ACUTE SITREP DATA ANALYSIS

# In this script, we download any new editions of the discharges from acute hospitals SitRep data from NHS England, process the data into an analyzable format, and create time series of key metrics.

# In the scraping portion, we scrape the NHSE webpage for Delayed Discharge SitRep data and install any available datasets which
# we do not currently have in our working directory. This will act to both download all available data on a first run, 
# and download only any new editions of the dataset on subsequent runs. 

# There are two tables we are interested in in the SitRep data: Table 4, detailing discharges by pathway, and Table 5, showing 
# discharge delays by reason. These tables are both formatted in a slightly difficult way for reading and analyzing in R - 
# both have irregular shapes, numerous empty cells for formatting, and have multiple tables (showing the same metrics at regional, ICB and trust level)
# on the same sheet. The wrangling portion of this script reads in each monthly sheet for Tables 4 and 5 and fixes their formatting 
# into an amenable shape for R. It then pivots and combines all monthly sheets into more easily filterable dataframes featuring 
# all metrics in long format, divided into ICB and trust level. 

# As it currently stands, this script works to scrape, download and wrangle sitrep data up until December 2023. If adapting for future data, please be 
# attentive to potential formatting differences which may complicate the wrangling stages.

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
######## SCRAPE AND DOWNLOAD LATEST DATA #######
################################################

acute_sitrep_scrape()

#################################################
######## LOAD UP FULL SERIES OF DATA ############
#################################################

all_months_combined <- acute_sitrep_wrangle()

# Create individual dataframes for each table/level combo, including variable for separating out pathways

region_discharges_by_destination <- all_months_combined[[1]] %>%
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

ICB_discharges_by_destination <- all_months_combined[[2]] %>%
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

trust_discharges_by_destination <- all_months_combined[[3]] %>%
  mutate(pathway = case_when(grepl('P0', metric) == TRUE ~ 'P0',
                             grepl('P1', metric) == TRUE ~ 'P1',
                             grepl('P2', metric) == TRUE ~ 'P2',
                             grepl('P3', metric) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

region_delayed_discharges_by_reason <- all_months_combined[[4]]

ICB_delayed_discharges_by_reason <- all_months_combined[[5]]

trust_delayed_discharges_by_reason <- all_months_combined[[6]]

rm(all_months_combined)  # Clear up workspace

#################################################
################### ANALYSIS ####################
#################################################



#### ANALYSIS FOR LONG READ

#1. Average monthly number of patients discharged  - total and by pathway - for 2023


avg_disch_by_pathway = region_discharges_by_destination %>%
  filter(year(date)==2023 & Region == 'ENGLAND (Type 1 Trusts)') %>%
  group_by(period, pathway) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(pathway) %>%
  summarise(mean = mean(value, na.rm=TRUE), no_months = n_distinct(period)) %>%   # Ensuring that all 12 months are being included
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(pathway) %>%
  mutate(pc = mean/tot*100)


region_discharges_by_destination %>% ungroup() %>%
  filter(year(date)==2023 & Region == 'ENGLAND (Type 1 Trusts)') %>% 
  summarise(avg = sum(value, na.rm=TRUE)/12) 




#5. Average monthly number of delayed discharges for latest 12 months

avg_delayed_by_pathway = region_delayed_discharges_by_reason %>%
  filter(year(date)==2023  & Region == 'ENGLAND (Type 1 Trusts)') %>%
  group_by(metric) %>%
  summarise(mean = mean(value, na.rm=TRUE), no_months = n_distinct(period)) %>%
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(metric) %>%
  mutate(pc = mean/tot*100) 

region_delayed_discharges_by_reason %>% ungroup() %>%
  filter(year(date)==2023 & Region == 'ENGLAND (Type 1 Trusts)') %>% 
  summarise(avg = sum(value, na.rm=TRUE)/12) # gives 8,733 


#6. Percentage of all discharges that are delayed

avg_delayed_by_pathway %>%
  mutate(metric = case_when(str_detect(metric, 'Pathway') ~ metric,
                            TRUE ~ 'Other (15 categories, all <5%)')) %>%
  ggplot(., aes(x=metric, y=pc, fill=metric)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(title='Average delayed discharges by reason, 2023', y='', x='') +
  theme(legend.position = "bottom", legend.direction ='vertical', axis.text.x = element_blank())

#7. Number of delayed discharges that are due to IC by month

region_delayed_discharges_by_reason %>%
  filter((str_detect(metric, 'Pathway 1') | str_detect(metric, 'Pathway 2')) & Region == 'ENGLAND (Type 1 Trusts)') %>%
  group_by(date, metric) %>%
  summarise(value = sum(value, na.rm=TRUE), .groups='keep') %>%
  ggplot(., aes(x = date, y = value, color=metric, group=metric)) +
  geom_line() +
  theme_minimal() +
  xlab('Month and year') +
  ylab('Number of delayed discharges') +
  theme(legend.position = "bottom", legend.direction ='vertical')


### Output for figure 4
weekly_delayed <- avg_delayed_by_pathway %>%
  filter(str_detect(metric, 'Pathway 1')|str_detect(metric, 'Pathway 2')) %>%
  mutate(pathway = case_when(str_detect(metric, 'Pathway 1') ~ 'P1',
                             str_detect(metric, 'Pathway 2') ~ 'P2'))

output_figure4 <- avg_disch_by_pathway %>%
filter(pathway %in% c('P1', 'P2')) %>%
  left_join(., weekly_delayed, by = 'pathway') %>%
  mutate(weekly_discharges = mean.x/4) %>%
  select(pathway, metric, weekly_discharges, weekly_delayed = mean.y) %>%
  mutate(pc_delayed = weekly_delayed/(weekly_discharges+weekly_delayed)*100) 

write_csv(output_figure4, 'Outputs/figure4.csv')  

