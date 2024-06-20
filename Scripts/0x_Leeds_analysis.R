# LEEDS DISCHARGE ANALYSIS

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


