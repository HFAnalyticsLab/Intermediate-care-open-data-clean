################################################################################
################################################################################
# Script written in R 4.0.2

#  2. COMMUNITY SITREP DATA ANALYSIS

# In this script, we download any new editions of the discharges from community hospitals SitRep data from NHS England, process the data into an analyzable format, and create time series of key metrics.
# To run this script, you only need to have run 00_Setup_and_packages.R prior

# Much of this script functions in the same way as the preceding 01_Acute_Sitrep_analysis.R script, with minor variations to account for formatiing differences. 
# As it currently stand, this script works to scrape, download and wrangle sitrep data up until August 2023. For future data, please be 
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

community_sitrep_scrape()

#################################################
######## LOAD UP FULL SERIES OF DATA ############
#################################################

all_months_combined <- community_sitrep_wrangle()

# Create individual dataframes for each table/level combo, including variable for separating out pathways

region_discharges_by_destination <- all_months_combined[[1]] %>%
  mutate(pathway = case_when((grepl('P0', metric)|(grepl('Pathway 0', metric))) == TRUE ~ 'P0',
                             (grepl('P1', metric)|(grepl('Pathway 1', metric))) == TRUE ~ 'P1',
                             (grepl('P2', metric)|(grepl('Pathway 2', metric))) == TRUE ~ 'P2',
                             (grepl('P3', metric)|(grepl('Pathway 3', metric))) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

ICB_discharges_by_destination <- all_months_combined[[2]] %>%
  mutate(pathway = case_when((grepl('P0', metric)|(grepl('Pathway 0', metric))) == TRUE ~ 'P0',
                             (grepl('P1', metric)|(grepl('Pathway 1', metric))) == TRUE ~ 'P1',
                             (grepl('P2', metric)|(grepl('Pathway 2', metric))) == TRUE ~ 'P2',
                             (grepl('P3', metric)|(grepl('Pathway 3', metric))) == TRUE ~ 'P3',
                             TRUE ~ 'Other'))

region_delayed_discharges_by_reason <- all_months_combined[[3]]

ICB_delayed_discharges_by_reason <- all_months_combined[[4]]

rm(table4_region, table4_ICB, table5_region, table5_ICB, all_tables_list, all_tables_pivoted, all_months_combined)  # Clear up workspace


#################################################
################### ANALYSIS ####################
#################################################


##########################################################


### ANALYSIS FOR LONG READ


#1. Average monthly number of patients discharged  - total and by pathway - for 2023
avg_disch_by_pathway = region_discharges_by_destination %>%
  filter(year(date)==2023 & Org_name=='ENGLAND') %>%
  group_by(period, pathway) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(pathway) %>%
  summarise(mean = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(pathway) %>%
  mutate(pc = mean/tot*100)

avg_disch_by_pathway %>% ungroup() %>% summarise(tot=sum(mean))

View(avg_disch_by_pathway)





#5. Average monthly number of delayed discharges for 2023

head(ICB_delayed_discharges_by_reason)

region_delayed_discharges_by_reason %>% filter(year(date)==2023 & Org_name=='ENGLAND' ) %>% select(period) %>% distinct()

avg_delayed_by_pathway = region_delayed_discharges_by_reason %>%
  filter(year(date)==2023 & Org_name=='ENGLAND') %>%
  mutate(metric=case_when(str_detect(metric, 'Pathway 1') ~ 'Pathway 1',
                          str_detect(metric, 'Pathway 2') ~ 'Pathway 2',
                          str_detect(metric, 'Pathway 3') ~ 'Pathway 3',
                          TRUE ~ 'other')) %>%
  group_by(period, metric) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>%
  group_by(metric) %>%
  summarise(mean = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(tot = sum(mean)) %>%
  group_by(metric) %>%
  mutate(pc = mean/tot*100) 

avg_delayed_by_pathway %>% ungroup() %>% summarise(tot=sum(mean))  # I'm getting 2151 here vs 2700 in comments - needs checking

avg_delayed_by_pathway %>% filter(metric=='Pathway 1' | metric=='Pathway 2' | metric=='Pathway 3')

region_delayed_discharges_by_reason %>% ungroup() %>%
  filter(year(date)==2023 & Org_name=='ENGLAND') %>% 
  summarise(avg = sum(value, na.rm=TRUE)/7) # gives 1737 - checked and this is correct
  