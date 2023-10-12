################################################################################
################################################################################
# Script written in R 4.0.2

#  4. ADULT SOCIAL CARE FINANCIAL RETURNS AND OUTCOMES FRAMEWORK DATA ANALYSIS

# This script downloads all relevant editions of the Adult Social Care Financial Returns (ASC-FR) dataset, 
# and a time series of the Adult Social Care Outcomes Framework dataset.

# These datasets are also available as csvs which are more easily machine-readable than the xlsx reference tables,
# but even with the data dictionaries their interpretation is quite opaque - as such, since we're only interested in 
# a few quite specific measures, for now it seems more worth wrangling the excel sheets into an R readable format.

################################################################################
################################################################################

rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages()) { 
  print('Project setup run')
}else{
  source('Scripts/00_Setup_and_packages.R')}


################################################
############### DOWNLOAD DATA ##################
################################################

### DOWNLOAD ASCR-FR DATA

# 2016/17
if (file.exists('Raw_data/ASC_data/ASCFR_2016-17.xlsx')){print('2016/17 ASCFR data already loaded')
  } else{download.file('https://files.digital.nhs.uk/excel/o/4/reference_data_tables_combined.xlsx', 'Raw_data/ASC_data/ASCFR_2016-17.xlsx')}


# 2017/18

if (file.exists('Raw_data/ASC_data/ASCFR_2017-18.xlsx')){print('2017/18 ASCFR data already loaded')
} else{
download.file('https://files.digital.nhs.uk/4F/11BD6D/SALT%20and%20ASCFR%20Reference%20Tables%20%28Suppressed%29%20v2.xlsx', 'Raw_data/ASC_data/ASCFR_2017-18.xlsx')
}

# 2018/19

if (file.exists('Raw_data/ASC_data/ASCFR_2018-19.xlsx')){print('2018/19 ASCFR data already loaded')
} else {
download.file('https://files.digital.nhs.uk/C4/9FBB8A/ASCFR%20and%20SALT%20Reference%20Tables%202018-19.xlsx', 'Raw_data/ASC_data/ASCFR_2018-19.xlsx')
}

# 2019/20

if (file.exists('Raw_data/ASC_data/ASCFR_2019-20.xlsx')){print('2019/20 ASCFR data already loaded')
} else{
download.file('https://files.digital.nhs.uk/BF/7AEF16/ASCFR%20and%20SALT%20Reference%20Tables%202019-20.xlsx', 'Raw_data/ASC_data/ASCFR_2019-20.xlsx')
}

# 2020/21

if (file.exists('Raw_data/ASC_data/ASCFR_2020-21.xlsx')){print('2020/21 ASCFR data already loaded')
} else{
download.file('https://files.digital.nhs.uk/B5/81EEB4/ASCFR%20and%20SALT%20Reference%20Tables%202020-21%20v2%2022-11-2022.xlsx', 'Raw_data/ASC_data/ASCFR_2020-21.xlsx')
}

# 2021/22

if (file.exists('Raw_data/ASC_data/ASCFR_2021-22.xlsx')){print('2021/22 ASCFR data already loaded')
} else{
download.file('https://files.digital.nhs.uk/AF/4C9A4F/ASCFR%20and%20SALT%20Data%20Tables%202021-22.xlsx', 'Raw_data/ASC_data/ASCFR_2021-22.xlsx')
}


### DOWNLOAD ASCOF DATA


if (file.exists('Raw_data/ASC_data/ASCOF-time-series.xlsx')){print('ASCOF time series data already loaded')
} else{
download.file('https://files.digital.nhs.uk/FE/612651/meas-from-asc-of-eng-2021-22-time-sers-anx-v2.xlsx', 'Raw_data/ASC_data/ASCOF-time-series.xlsx')
}


##########################################################################
################# WRANGLE DATA INTO AMENABLE FORMAT ######################
##########################################################################

## WRANGLE ASC-FR DATA

# 2016/17



# 2017/18



# 2018/19

t21_1819 <- read_excel('Raw_data/ASC_data/ASCFR_2018-19.xlsx', sheet = 'T21', skip = 8, col_names = FALSE)

t23_1819 <- read_excel('Raw_data/ASC_data/ASCFR_2018-19.xlsx', sheet = 'T23', skip = 8, col_names = FALSE)

t24_1819 <- read_excel('Raw_data/ASC_data/ASCFR_2018-19.xlsx', sheet = 'T24', skip = 8, col_names = FALSE)

t28_1819 <- read_excel('Raw_data/ASC_data/ASCFR_2018-19.xlsx', sheet = 'T28', skip = 8, col_names = FALSE)

t32_1819 <- read_excel('Raw_data/ASC_data/ASCFR_2018-19.xlsx', sheet = 'T32', skip = 8, col_names = FALSE)

# 2019/20



# 2020/21



# 2021/22




## WRANGLE ASC-OF DATA

## Table 2B(1): Proportion of older people still at home 91 days after discharge from hospital into reablement services
## Table 2B(2): Proportion of older people receiving reablement care following discharge from hospital
## Table 2D: Proportion of new short-term service users who no further support or support at a lower level

FYs <- c('2014-15', '2015-16', '2016-17', '2017-18', '2018-19', '2019-20', '2020-21', '2021-22')

variable_types <- c('Numerator', 'Denominator', 'Outcome')

initial_labels <- c()

for (i in FYs) {
  for (x in variable_types){
    item <- paste0(i, ':', x)
    initial_labels <- c(initial_labels, item)
  }
}

# Table 2B(1)

colnames_2b1 <- c('cassr_code', 'cassr', 'area_code')

variables_2b1 <- rep(c('Number of older people still at home 91 days after discharge from hospital into reablement services', 'Number of older people who received reablement services after discharge from hospital', 'Proportion of older people still at home 91 days after discharge from hospital into reablement services'), 8)

for (i in c(1:length(initial_labels))){
  item <- paste0(initial_labels[i], ':', variables_2b1[i])
  colnames_2b1 <- c(colnames_2b1, item)
}

table_2b1 <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2B(1)', skip = 6, col_names = FALSE, n_max = 157)

names(table_2b1) <- colnames_2b1

table_2b1 <- table_2b1 %>%
  pivot_longer(4:27, names_to = 'metric', values_to = 'value') %>%
  separate(metric, c('year', 'variable_type', 'metric'), sep = ':')


# Table 2B(2)

colnames_2b2 <- c('cassr_code', 'cassr', 'area_code')

variables_2b2 <- rep(c('Number of older people receiving reablement care following discharge from hospital', 'Number of older people discharged from hospital', 'Proportion of older people receiving reablement care following discharge from hospital'), 8)

for (i in c(1:length(initial_labels))){
  item <- paste0(initial_labels[i], ':', variables_2b2[i])
  colnames_2b2 <- c(colnames_2b2, item)
}

table_2b2 <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2B(2)', skip = 6, col_names = FALSE, n_max = 157)

names(table_2b2) <- colnames_2b2

table_2b2 <- table_2b2 %>%
  pivot_longer(4:27, names_to = 'metric', values_to = 'value') %>%
  separate(metric, c('year', 'variable_type', 'metric'), sep = ':')


# Table 2D

colnames_2d <- c('cassr_code', 'cassr', 'area_code')

variables_2d <- rep(c('Number of new short-term service users who no further support or support at a lower level after ST-MAX', 'Number of new service users who received short-term care to maximise independence', 'Proportion of new short-term service users who no further support or support at a lower level after ST-MAX'), 8)

for (i in c(1:length(initial_labels))){
  item <- paste0(initial_labels[i], ':', variables_2d[i])
  colnames_2d <- c(colnames_2d, item)
}

table_2d <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2D', skip = 6, col_names = FALSE, n_max = 157)

names(table_2d) <- colnames_2d

table_2d <- table_2d %>%
  pivot_longer(4:27, names_to = 'metric', values_to = 'value') %>%
  separate(metric, c('year', 'variable_type', 'metric'), sep = ':')



#####################################################
################ ANALYZE DATA #######################
#####################################################












