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



### DOWNLOAD NATIONAL COST COLLECTION DATA

# 2013/14

if (file.exists('Raw_data/NCC_data/NCC_2013-14.xlsx')){print('2013/14 NCC data already loaded')
} else{
  download.file('https://webarchive.nationalarchives.gov.uk/ukgwa/20200506033032mp_/https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/397469/03a_2013-14_National_Schedule_-_CF-NET_updated.xls', 'Raw_data/NCC_data/NCC_2013-14.xls')
}

# 2014/15

if (file.exists('Raw_data/NCC_data/NCC_2014-15.xlsx')){print('2014/15 NCC data already loaded')
} else{
  download.file('https://webarchive.nationalarchives.gov.uk/ukgwa/20200506073612mp_/https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/480791/2014-15_National_Schedules.xlsx', 'Raw_data/NCC_data/NCC_2014-15.xlsx')
}

# 2015/16

if (file.exists('Raw_data/NCC_data/NCC_2015-16.xlsx')){print('2015/16 NCC data already loaded')
} else{
  download.file('https://webarchive.nationalarchives.gov.uk/ukgwa/20200506094432mp_/https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/577084/National_schedule_of_reference_costs_-_main_schedule.xlsx', 'Raw_data/NCC_data/NCC_2015-16.xlsx')
}

# 2016/17

if (file.exists('Raw_data/NCC_data/NCC_2016-17.xlsx')){print('2016/17 NCC data already loaded')
} else{
  temp <- tempfile()
  download.file('https://webarchive.nationalarchives.gov.uk/ukgwa/20200501111106mp_/https://improvement.nhs.uk/documents/6467/201617_ReferenceCostData.zip', temp)
  unzip(temp, files = c('2 - National schedule of reference costs - the main schedule.xlsx'), exdir = 'Raw_data/NCC_data/')
  file.rename('Raw_data/NCC_data/2 - National schedule of reference costs - the main schedule.xlsx', 'Raw_data/NCC_data/NCC_2016-17.xlsx')
  unlink(temp)
}

# 2017/18

if (file.exists('Raw_data/NCC_data/NCC_2017-18.xlsx')){print('2017/18 NCC data already loaded')
} else{
  temp <- tempfile()
  download.file('https://webarchive.nationalarchives.gov.uk/ukgwa/20200501111106mp_/https://improvement.nhs.uk/documents/6468/201718_reference_costs_data_and_guidance.zip', temp)
  unzip(temp, files = c('2 - National schedule of reference costs.xlsx'), exdir = 'Raw_data/NCC_data/')
  file.rename(from = 'Raw_data/NCC_data/2 - National schedule of reference costs.xlsx', to ='Raw_data/NCC_data/NCC_2017-18.xlsx')
  unlink(temp)
}


# 2018/19

if (file.exists('Raw_data/NCC_data/NCC_2018-19.xlsx')){print('2018/19 NCC data already loaded')
} else{
  download.file('https://www.england.nhs.uk/wp-content/uploads/2020/08/2_-_National_schedule_of_NHS_costs_V2.xlsx', 'Raw_data/NCC_data/NCC_2018-19.xlsx')
}


# 2019/20

if (file.exists('Raw_data/NCC_data/NCC_2019-20.xlsx')){print('2019/20 NCC data already loaded')
} else{
  download.file('https://www.england.nhs.uk/wp-content/uploads/2021/06/National_Schedule_of_NHS_Costs_FY1920.xlsx', 'Raw_data/NCC_data/NCC_2019-20.xlsx')
}

# 2020/21

if (file.exists('Raw_data/NCC_data/NCC_2020-21.xlsx')){print('2020/21 NCC data already loaded')
} else{
  download.file('https://www.england.nhs.uk/wp-content/uploads/2022/07/2_National_schedule_of_NHS_costs_FY20-21.xlsx', 'Raw_data/NCC_data/NCC_2020-21.xlsx')
}

# 2021/22

if (file.exists('Raw_data/NCC_data/NCC_2021-22.xlsx')){print('2021/22 NCC data already loaded')
} else{
  download.file('https://www.england.nhs.uk/wp-content/uploads/2023/04/2_National_schedule_of_NHS_costs_FY21-22_v3.xlsx', 'Raw_data/NCC_data/NCC_2021-22.xlsx')
}


# Download deflator

if (file.exists('Raw_data/GDP_deflator.xlsx')){print('GDP deflator already loaded')
} else{
  download.file('https://assets.publishing.service.gov.uk/media/651ad0456a423b000df4c6e2/GDP_Deflators_Qtrly_National_Accounts_September_2023_update.xlsx', 'Raw_data/GDP_deflator.xlsx')
}


##########################################################################
################# WRANGLE DATA INTO AMENABLE FORMAT ######################
##########################################################################


## WRANGLE ASC-FR DATA

ascfr_data <- list.files('Raw_data/ASC_data', pattern='xlsx')

ascfr_data <- ascfr_data[!ascfr_data %in% c('ASCOF-time-series.xlsx')]

t32_all <- lapply(ascfr_data, function(i){
  
  if(i=='ASCFR_2016-17.xlsx'){
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T24', skip = 8, col_names = FALSE)
  } else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T32', skip = 9, col_names = FALSE) 
  }
  
  names(df) <- c('geography_code', 'LA_code', 'LA_name', 'region_code', 'region_name', 'Expenditure per 100,000 adults: previous year', 'Gross current expenditure: previous year','Population: previous year',
                 'Expenditure per 100,000 adults: current year', 'Gross current expenditure: current year','Population: current year')
  
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
  
  df <- df %>% 
    mutate_all(function(.){(str_replace(., "//[x]", '0'))}) %>% 
    mutate_all(function(.){(str_replace(., "//[c]", '0'))})
  
  df <- df %>%
    pivot_longer(6:length(df), names_to = 'metric', values_to = 'value')
  
  df$value <- as.numeric(df$value)
  
  df <- df %>%
    separate_wider_delim(metric, delim = ': ', names = c('metric', 'year'))
  
  return(df)
  
})

ascfr_FYs <- c('2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31')


for (x in 1:6){
  t32_all[[x]] <- t32_all[[x]] %>%
      mutate(date = ascfr_FYs[[x]])
  }

t32_final <- do.call('rbind', t32_all) %>%
  filter((year == 'current year') | (year == 'previous year' & date == '2017-03-31'))

t32_final$date[t32_final$date == '2017-03-31' & t32_final$year == 'previous year'] <- '2016-03-31'

## WRANGLE NCC DATA

NCC_files <- list.files('Raw_data/NCC_data')

NCC_read_all <- lapply(NCC_files, function(i){
  
  if(i == 'NCC_2014-15.xlsx'){
    df <- read_excel(paste0('Raw_data/NCC_data/', i), sheet = 'CHS', skip = 3)
  } else{
    df <- read_excel(paste0('Raw_data/NCC_data/', i), sheet = 'CHS', skip = 4)
  }
  
  names(df) <- tolower(names(df))
  
  names(df) <- str_replace_all(names(df), ' ', '_')
  
  FY <- sub('NCC_', '', i)
  FY <- sub('.xlsx', '', FY)
  FY <- sub('.xls', '', FY)
  
  df <- df %>%
   filter(service_code %in% c('IC')) %>%
    pivot_longer(5:length(df), names_to = 'metric', values_to = 'value',
                 values_transform = list(value=as.numeric)) %>%
    mutate(period = FY)
  
  names(df) <- c('service_code', 'service_name', 'currency_code', 'currency_name', 'metric', 'value', 'period')
  
  return(df)
  })

NCC_combined <- do.call('rbind', NCC_read_all)

NCC_combined$metric[NCC_combined$metric == 'activity1'|NCC_combined$metric == 'number_of_contacts'|NCC_combined$metric == 'number_of_fces'] <- 'activity'

NCC_combined$date <- ymd(paste0('20', substr(NCC_combined$period, 6,7), '-03-31'))


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
  mutate(deflator_dates = c('2015-16', '2016-17', '2017-18','2018-19', '2019-20', '2020-21', '2021-22')) %>%
  left_join(., deflator_shortened, by = c('deflator_dates'='year')) %>%
  mutate(real_expenditure = value/deflator*100)

ggplot(england_ascfr_expenditure, aes(x = date, y = value)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

ggplot(england_ascfr_expenditure, aes(x = date, y = real_expenditure)) +
  geom_line(color = '#F8766D') +
  theme_minimal()

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
filter(region_name != 'England' & metric == 'Expenditure per 100,000 adults' & is.na(LA_code) & date == max_date) %>%
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
 

