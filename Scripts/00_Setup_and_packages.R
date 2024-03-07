## Install and load packages

packages <- c('tidyverse', 'readxl', 'rvest', 'here', 'lubridate', 'data.table', 'plotly', 'sf', 'scales')

installed_packages <- packages %in% row.names(installed.packages())

if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)


## Set up folder structure

# Parent folders

ifelse(!dir.exists(file.path(here('Raw_data/'))), dir.create(file.path(here('Raw_data/'))), print('Raw data directory already exists'))  

ifelse(!dir.exists(file.path(here('Outputs/'))), dir.create(file.path(here('Outputs/'))), print('Outputs directory already exists'))

# Sub folders

data_subfolders <- c('Acute_SitRep_data', 'Community_SitRep_data', 'CSDS_data', 'ASC_data')


lapply(data_subfolders, function(i){
  ifelse(!dir.exists(file.path(here('Raw_data/', i))), dir.create(file.path(here('Raw_data/', i))), print('Directory already exists'))
})


