################################################################################
################################################################################
# Script written in R 4.0.2

#  4. ADULT SOCIAL CARE FINANCIAL RETURNS AND OUTCOMES FRAMEWORK DATA ANALYSIS

# This script downloads all relevant editions of the Adulst Social Care Financial Returns (ASC-FR) dataset, 
# and a time series of the Adult Social Care Outcomes Framework dataset.

# These datasets are also available as csvs which are more easily machine-readable than the xlsx reference tables,
# but even with the data dictionaries their interpretation is quite opaque - as such, since we're only interested in \
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
