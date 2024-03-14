################################################################################
################################################################################
# Script written in R 4.0.2

#  3. ADULT SOCIAL CARE FINANCIAL RETURNS AND OUTCOMES FRAMEWORK DATA ANALYSIS

# This script downloads and wrangles all relevant editions of the Adult Social Care Financial Returns (ASC-FR) dataset, 
# and a time series of the Adult Social Care Outcomes Framework dataset.

################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages() & dir.exists(file.path(('Raw_data/NCC_data')))) { 
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

ASC_datadownload()

##########################################################################
################# WRANGLE DATA INTO AMENABLE FORMAT ######################
##########################################################################


## WRANGLE ASC-FR DATA

all_ascfr_data <- ASCFR_wrangle()

t21_final <- all_ascfr_data[[1]]

t23_final <- all_ascfr_data[[2]]

t24_final <- all_ascfr_data[[3]]

t25_26_final <- all_ascfr_data[[4]]

t27_final <- all_ascfr_data[[5]]

t28_final <- all_ascfr_data[[6]]

rm(all_ascfr_data)

## WRANGLE ASC-OF DATA

## Table 2B(1): Proportion of older people still at home 91 days after discharge from hospital into reablement services
## Table 2B(2): Proportion of older people receiving reablement care following discharge from hospital
## Table 2D: Proportion of new short-term service users who no further support or support at a lower level

all_ASCOF_data <- ASCOF_wrangle()

table_2b1 <- all_ASCOF_data[[1]]

table_2b2 <- all_ASCOF_data[[2]]

table_2d <- all_ASCOF_data[[3]]
                           
rm(all_ASCOF_data)

#####################################################
################ ANALYZE DATA #######################
#####################################################

## OUTPUT TABLES

max_date <- max(t21_final$date)

# Total number of clients per month

t28_final %>%
  filter(region_name == 'England' & metric == 'Clients' & age_band == 'Total' & date == max_date) %>%
  mutate(monthly = value/12)

# Figure 1: Episodes by age band and support reason

output_figure1 <- t23_final %>%
  filter(metric != 'Total' & metric_type == 'value' & date == max_date & age_band != 'All ages') %>%
  select(6, 3, 1, 5) %>%
  mutate(agg_supportreason = case_when(grepl('Physical Support', metric) == TRUE ~ 'Physical Support',
                                       grepl('Social Support', metric) == TRUE ~ 'Social Support',
                                       grepl('Sensory Support', metric) == TRUE ~ 'Sensory Support',
                                       TRUE ~ metric)) %>%
  group_by(agg_supportreason, age_band) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = age_band, values_from = value) %>%
  mutate(all_ages = `18 to 64` + `65 and over`)

write_csv(output_figure1, 'Outputs/figure1.csv')

fig1_pc <- t23_final %>%
  filter(metric != 'Total' & metric_type == 'percentage' & date == max_date)  %>%
  mutate(agg_supportreason = case_when(grepl('Physical Support', metric) == TRUE ~ 'Physical Support',
                                       grepl('Social Support', metric) == TRUE ~ 'Social Support',
                                       grepl('Sensory Support', metric) == TRUE ~ 'Sensory Support',
                                       TRUE ~ metric)) %>%
  group_by(agg_supportreason, age_band) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = age_band, values_from = value)
  

# Figure 3: What happened next
output_figure3 <- t21_final %>% 
  filter(date == max_date & metric_type == 'value' & region_name == 'England' & age_band == 'All ages' & metric != 'Total') %>%
  select(7,4, episodes = 6)

fig3_pc <-t21_final %>% 
  filter(date == max_date & metric_type == 'percentage' & region_name == 'England' & age_band == 'All ages' & metric != 'Total')

write_csv(output_figure3, 'Outputs/figure3.csv')
