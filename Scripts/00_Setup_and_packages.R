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

data_subfolders <- c('Acute_SitRep_data', 'Community_SitRep_data', 'CSDS_data', 'ASC_data', 'NCC_data', 'Maps')

lapply(data_subfolders, function(i){
  ifelse(!dir.exists(file.path(here('Raw_data/', i))), dir.create(file.path(here('Raw_data/', i))), print('Directory already exists'))
})


## Download maps (these will be used several times throughout)

# ICB map

ICB_map_link <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BFC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'

if (file.exists('Raw_data/Maps/ICB_map.geojson')){
  print('ICB map already downloaded.')
} else {
  download.file(ICB_map_link, destfile = 'Raw_data/Maps/ICB_map.geojson')
}

# Regional map

region_map_link <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Regions_December_2022_Boundaries_EN_BFC_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'

if (file.exists('Raw_data/Maps/Region_map.geojson')){
  print('Region map already downloaded.')
} else {
  download.file(region_map_link, destfile = 'Raw_data/Maps/Region_map.geojson')
}

# Local Authority map

LA_map_link <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2023_EW_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'

if (file.exists('Raw_data/Maps/LA_map.geojson')){
  print('LA map already downloaded.')
} else {
  download.file(LA_map_link, destfile = 'Raw_data/Maps/LA_map.geojson')
}



