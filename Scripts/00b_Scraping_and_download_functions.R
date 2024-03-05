## 00.b. WEB SCRAPING FUNCTIONS

# This script provides functions which download the relevant data for each of our analysis scripts. 
# For the delayed discharges SitRep datasets, the functions in this script scrape the webpages for these 
# data collections and check for any new editions, downloading any which cannot be found in our current raw data folder.
# For the ASC and expenditure analysis scripts, the functions here download the relevant data directly form. 
# These are annual datasets which do not have many editions and are not regularly updated, and as such do not require web scrapers. 


# Acute hospital SitRep webscraper




# Community hopital SitRep webscraper






## ASC data download

ASC_datadownload <- function(x=NULL){

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

#2022/23

if (file.exists('Raw_data/ASC_data/ASCFR_2022-23.xlsx')){print('2022/23 ASCFR data already loaded')
} else{
  download.file('https://files.digital.nhs.uk/03/C96CF8/ASCFR%20and%20SALT%20Data%20Tables%202022-23%20v2.xlsx', 'Raw_data/ASC_data/ASCFR_2022-23.xlsx')
}

### DOWNLOAD ASCOF DATA


if (file.exists('Raw_data/ASC_data/ASCOF-time-series.xlsx')){print('ASCOF time series data already loaded')
} else{
  download.file('https://files.digital.nhs.uk/EE/17E313/meas-from-asc-of-eng-2022-23-timeseries.xlsx', 'Raw_data/ASC_data/ASCOF-time-series.xlsx')
}

}


# Expenditure data download


