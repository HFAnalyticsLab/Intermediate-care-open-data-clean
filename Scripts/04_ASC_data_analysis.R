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

# 2022/23

#if (file.exists('Raw_data/ASC_data/ASCFR_2022-23.xlsx')){print('2022/23 ASCFR data already loaded')
#} else{
#  download.file('https://files.digital.nhs.uk/99/CE12BB/ASCFR%20and%20SALT%20Data%20Tables%202022-23.xlsx', 'Raw_data/ASC_data/ASCFR_2022-23.xlsx')
#}

### DOWNLOAD ASCOF DATA


if (file.exists('Raw_data/ASC_data/ASCOF-time-series.xlsx')){print('ASCOF time series data already loaded')
} else{
download.file('https://files.digital.nhs.uk/FE/612651/meas-from-asc-of-eng-2021-22-time-sers-anx-v2.xlsx', 'Raw_data/ASC_data/ASCOF-time-series.xlsx')
}



##########################################################################
################# WRANGLE DATA INTO AMENABLE FORMAT ######################
##########################################################################


## WRANGLE ASC-FR DATA

ascfr_data <- list.files('Raw_data/ASC_data', pattern='xlsx')

ascfr_data <- ascfr_data[!ascfr_data %in% c('ASCOF-time-series.xlsx')]

t21_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T16', skip = 7, col_names = FALSE)
  }else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T21', skip = 8, col_names = FALSE) 
  }
  
  
  names(df) <- c('age_band', 'region_code', 'region_name', 'Early Cessation of Service, NHS-funded, deceased: value', 'Early Cessation of Service, NHS-funded, deceased: percentage',
                'Early Cessation of Service, not leading to long term support: value', 'Early Cessation of Service, not leading to long term support: percentage', 'Early Cessation of Service, leading to long term support: value',
                'Early Cessation of Service, leading to long term support: percentage', 'Long Term Support: value', 'Long Term Support: percentage', 'No services provided, needs identified but self funding: value', 
                'No services provided, needs identified but self funding: percentage', 'Ongoing Low Level Support: value', 'Ongoing Low Level Support: percentage', 'Short Term Support: value', 'Short Term Support: percentage',
                'No services provided, needs identified but support declined: value', 'No services provided, needs identified but support declined: percentage',
                'No services provided, signposted to other services: value', 'No services provided, signposted to other services: percentage', 'No services provided, no identified needs: value', 'No services provided, no identified needs: percentage',
                'Total: value')
  
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
  
  df$age_band[3:11] <- '18-64'
  
  df$age_band[13:21] <- '65 and over'
  
  df <- df %>% 
    mutate_all(function(.){(str_replace(., "%", ""))})
  
  df <- df %>%
    pivot_longer(4:length(df), names_to = 'metric', values_to = 'value')
  
  df$value <- as.numeric(df$value)
  
  df <- df %>%
    separate_wider_delim(metric, delim = ': ', names = c('metric', 'metric_type'))
   
  return(df)
  
})     # Episodes for new clients, by what happened next and age band

t24_all <- lapply(ascfr_data, function(i){
  if  (i == 'ASCFR_2016-17.xlsx'){
    
  } else {
  
  df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T24', skip = 8, col_names = FALSE)
  
  names(df) <- c('LA_code', 'area_code', 'LA_name', 'region_code', 'region_name', 'Episodes of ST-MAX per 100,000 adults', 'Episodes of ST-MAX',
                 'Population')
  
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
  
  df <- df %>% 
    mutate_all(function(.){(str_replace(., "//[x]", '0'))}) %>% 
    mutate_all(function(.){(str_replace(., "//[c]", '0'))})
  
  df <- df %>%
   pivot_longer(6:length(df), names_to = 'metric', values_to = 'value')
  
  df$value <- as.numeric(df$value)
  
  return(df)
  
}})      # Number of episodes for new clients per 100,000 adults

t24_all <- t24_all[2:6]

t27_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    
  }else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T27', skip = 8, col_names = FALSE) 
    
    
    names(df) <- c('age_band', 'region_code', 'region_name', 'Early Cessation of Service, NHS-funded, deceased',
                   'Early Cessation of Service, not leading to long term support',  'Early Cessation of Service, leading to long term support', 'Move to nursing care (from community)',
                   'Move to residential care (from community)', 'Move to community', 'Level of long-term support increased', 'No change in long-term support', 'Level of long-term support decreased',
                   'All long term support ended, no ongoing eligible needs', 'Total' )
    
    
    df <- df[rowSums(is.na(df)) != ncol(df), ]
    
    df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
    
    df$age_band[3:11] <- '18-64'
    
    df$age_band[13:21] <- '65 and over'
    
    df <- df %>% 
      mutate_all(function(.){(str_replace(., "//*", '0'))})
    
    df <- df %>%
      pivot_longer(4:length(df), names_to = 'metric', values_to = 'value')
    
    df$value <- as.numeric(df$value)
    
    return(df)
    
  }}) 

t27_all <- t27_all[2:6]

t28_all <- lapply(ascfr_data, function(i){
  
  if  (i == 'ASCFR_2016-17.xlsx'){
    
  } else {
  
  df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T28', skip = 9, col_names = FALSE)
  
  names(df) <- c('LA_code', 'area_code', 'LA_name', 'region_code', 'region_name', 'Completed episodes of ST-Max per client: 18-64', 'Completed episodes of ST-Max: 18-64', 'Clients: 18-64',
                 'Completed episodes of ST-Max per client: 65+', 'Completed episodes of ST-Max: 65+', 'Clients: 65+',
                 'Completed episodes of ST-Max per client: Total', 'Completed episodes of ST-Max: Total', 'Clients: Total')
  
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
  
  df <- df %>% 
    mutate_all(function(.){(str_replace(., "//[x]", '0'))}) %>% 
    mutate_all(function(.){(str_replace(., "//[c]", '0'))})
  
  df <- df %>%
    pivot_longer(6:length(df), names_to = 'metric', values_to = 'value')
  
  df$value <- as.numeric(df$value)
  
  df <- df %>%
   separate_wider_delim(metric, delim = ': ', names = c('metric', 'age_band'))
  
  return(df)
}})    # Number of episodes per client by age band, all clients

t28_all <- t28_all[2:6]

ascfr_tables <- list(t24_all, t27_all, t28_all)

ascfr_FYs_all <- c('2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31')

ascfr_FYs_short <- c('2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31')

for (x in 1:6){
 t21_all[[x]] <- t21_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t21_final <- do.call('rbind', t21_all)

for (i in 1:3){
  for (x in 1:5){
    ascfr_tables[[i]][[x]] <- ascfr_tables[[i]][[x]] %>%
      mutate(date = ascfr_FYs_short[[x]])
  }
  ascfr_tables[[i]] <- do.call('rbind', ascfr_tables[[i]])
}

t24_final <- ascfr_tables[[1]]

t27_final <- ascfr_tables[[2]]

t28_final <- ascfr_tables[[3]]




# England-level only tables 

t23_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T17', skip = 8, col_names = FALSE)
  }else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T23', skip = 8, col_names = FALSE) 
  }
  
 names(df) <- c('age_band', 'Sequel to ST-MAX', 'Blank', 'Physical Support, access and mobility only: value', 'Physical Support, access and mobility only: percentage',
                'Physical Support, personal care support: value', 'Physical Support, personal care support: percentage', 'Sensory Support, support for visual impairment: value',
                'Sensory Support, support for visual impairment: percentage', 'Sensory Support, support for hearing impairment: value', 'Sensory Support, support for hearing impairment: percentage',
                'Sensory Support, support for dual impairment: value', 'Sensory Support, support for dual impairment: percentage', 'Support with memory and cognition: value', 'Support with memory and cognition: percentage',
                'Learning disability support: value', 'Learning disability support: percentage', 'Mental health support: value', 'Mental health support: percentage', 'Social Support, substance misuse support: value',
                'Social Support, substance misuse support: percentage', 'Social Support, asylum seeker support: value', 'Social Support, asylum seeker support: percentage', 'Social Support, support for isolation/Other: value',
                'Social Support, support for isolation/Other: percentage', 'Total: value')
 
 if (i == 'ASCFR_2016-17.xlsx') {
   df <- df %>%
     select(1:26)
 }else {
   
 }
 
 df <- df[rowSums(is.na(df)) != ncol(df), ]
 
 df <- df[rowSums(is.na(df)) != ncol(df)-1, ]
 
 df <- df %>%
  filter(`Sequel to ST-MAX` == 'Total')
 
 df <- df %>% 
  mutate_all(function(.){(str_replace(., "%", ""))})
 
 df <- df %>%
  pivot_longer(4:length(df), names_to = 'metric', values_to = 'value')
 
 df$value <- as.numeric(df$value)
 
 df <- df %>%
   separate_wider_delim(metric, delim = ': ', names = c('metric', 'metric_type'))
 
 df <- df %>%
   select(-Blank)
 
 return(df)
 
})

for (x in 1:6){
  t23_all[[x]] <- t23_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t23_final <- do.call('rbind', t23_all)

t25_26_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df1 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T18', skip = 6, col_names = FALSE)
    df2 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T19', skip = 6, col_names = FALSE)
  }else {
    df1 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T25', skip = 8, col_names = FALSE) 
    df2 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T26', skip = 8, col_names = FALSE)
  }
  
  col_names <- c('LA_code', 'area_code', 'LA_name', 'region_code', 'region_name', 'Physical Support, access and mobility only', 
                 'Physical Support, personal care support', 'Sensory Support, support for visual impairment',
                 'Sensory Support, support for hearing impairment', 'Sensory Support, support for dual impairment',  
                 'Support with memory and cognition', 'Learning disability support: value','Mental health support', 'Social Support, substance misuse support',
                 'Social Support, asylum seeker support', 'Social Support, support for isolation/Other', 'Total')
  
 names(df1) <- col_names
 
 names(df2) <- col_names
 
 df1 <- df1 %>%
   filter(region_name == 'England') %>%
   mutate(age_band = '18-64')
 
 df2 <- df2 %>%
   filter(region_name == 'England') %>%
   mutate(age_band = '65+')
 
 df <- rbind(df1, df2)
  
  return(df)
  
})  # Number of completed episodes by primary support reason for existing clients, by age band

for (x in 1:6){
  t25_26_all[[x]] <- t25_26_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t25_26_final <- do.call('rbind', t25_26_all)


# Number of completed episodes by primary support reason for existing clients, by age band


## WRANGLE ASC-OF DATA

## Table 2B(1): Proportion of older people still at home 91 days after discharge from hospital into reablement services
## Table 2B(2): Proportion of older people receiving reablement care following discharge from hospital
## Table 2D: Proportion of new short-term service users who no further support or support at a lower level


ascof_FYs <- c('2015-03-31', '2016-03-31', '2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31')

variable_types <- c('Numerator', 'Denominator', 'Outcome')

initial_labels <- c()

for (i in ascof_FYs) {
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

max_date <- max(t21_final$date)

region_map <- read_sf('Raw_data/Maps/Region_map.geojson')

### ASC-FR DATA

# Number of episodes

t28_final %>%
  filter(region_name == 'England' & metric == 'Completed episodes of ST-Max' & age_band == 'Total')  # Compute percentage decline during COVID here
  ggplot(., aes(x = as_date(date), y = as.numeric(value))) +
  geom_line(color = '#F8766D') +
  theme_minimal() +
  xlab('Date') +
  ylab('Episodes of ST-MAX')

# Number of episodes by age band

t28_final %>%
    filter(region_name == 'England' & metric == 'Completed episodes of ST-Max' & is.na(LA_code) & date == max_date & age_band != 'Total') %>%
    ggplot(., aes(x = reorder(age_band,  -value), y = as.numeric(value))) +
    geom_col(fill = '#F8766D') +
    theme_minimal() +
    coord_flip() +
    xlab('Age band') +
    ylab('Episodes of ST_MAX')


# Number of episodes by region and age band
  
t28_final %>%
    filter(region_name != 'England' & metric == 'Completed episodes of ST-Max' & is.na(LA_code) & date == max_date & age_band != 'Total')
    ggplot(., aes(x = reorder(region_name,  -value), y = as.numeric(value), fill = age_band)) +
    geom_col() +
    theme_minimal() +
    coord_flip() +
    xlab('Regions') +
    ylab('Episodes of ST_MAX')
  
t28_final %>%
   filter(region_name != 'England' & metric == 'Completed episodes of ST-Max' & is.na(LA_code) & date == max_date & age_band != 'Total') %>%
  pivot_wider(names_from = age_band, values_from = value) %>%
  mutate(percent_under65 = `18-64`/(`18-64`+`65+`))

# Episodes of ST-MAX per client

t28_final %>%
  filter(region_name == 'England' & metric == 'Completed episodes of ST-Max per client' & age_band == 'Total') %>%
  ggplot(., aes(x = as_date(date), y = as.numeric(value))) +
  geom_line(color = '#F8766D') +
  theme_minimal() +
  xlab('Date') +
  ylab('Episodes of ST-MAX per client')

# Episodes of ST-MAX per client by age band

t28_final %>%
  filter(region_name == 'England' & metric == 'Completed episodes of ST-Max per client' & is.na(LA_code) & date == max_date & age_band != 'Total')

  ggplot(., aes(x = reorder(age_band,  -value), y = as.numeric(value))) +
  geom_col(fill = '#F8766D') +
  theme_minimal() +
  coord_flip() +
  xlab('Age band') +
  ylab('Episodes of ST_MAX per client')

# Episodes of ST-MAX per client by region

t28_final %>%
  filter(region_name != 'England' & metric == 'Completed episodes of ST-Max per client' & age_band == 'Total' & is.na(LA_code) & date == max_date) %>%
  ggplot(., aes(x = reorder(region_name,  -value), y = as.numeric(value))) +
  geom_col(fill = '#F8766D') +
  theme_minimal() +
  coord_flip() +
  xlab('Regions') +
  ylab('Episodes of ST_MAX per client')

# Episodes of ST-MAX per client by LA

ggplotly(
  t28_final %>%
  filter(region_name != 'England' & metric == 'Completed episodes of ST-Max per client' & age_band == 'Total' & !is.na(LA_code) & date == max_date) %>%
  ggplot(., aes(x = reorder(LA_name,  -value), y = as.numeric(value), text = paste0(reorder(LA_name,  -value), ': ', round(value, 2)))) +
  geom_col(fill = '#F8766D') +
  theme_minimal() +
    theme(axis.text.x=element_blank()) +
    xlab('Local Authorities') +
    ylab('Episodes of ST_MAX per client')
  , tooltip = 'text') %>%
  layout(hoverlabel =list(bgcolor='white', font_color='black'))

# Episodes of ST-MAX per 100,000 adults (Total)

t24_final %>%
  filter(region_name == 'England' & metric == 'Episodes of ST-MAX per 100,000 adults') %>%
  ggplot(., aes(x = as_date(date), y = as.numeric(value))) +
  geom_line(color = '#F8766D') +
  theme_minimal() +
  xlab('Date') +
  ylab('Episodes of ST-MAX per 100,000 adults')

# Episodes of ST-MAX per 100,000 adults (regions)

t24_final %>%
  filter(region_name != 'England' & metric == 'Episodes of ST-MAX per 100,000 adults' & is.na(LA_code) & !(region_code %in% c('A', 'D', 'E', 'H', 'J (IL)', 'J (OL)')) & date == max_date) %>%
  ggplot(., aes(x = reorder(region_name, -value), y = as.numeric(value))) +
  geom_col(fill = '#F8766D') +
  theme_minimal() +
  coord_flip() +
  xlab('Regions') +
  ylab('Episodes of ST_MAX per 100,000 adults')

# Episodes of ST-MAX per 100,000 adults (LA-level)

ggplotly(
  t24_final %>%
           filter(region_name != 'England' & metric == 'Episodes of ST-MAX per 100,000 adults' & !is.na(LA_code) & date == max_date) %>%
           ggplot(., aes(x = reorder(LA_name,  -value), y = as.numeric(value), text = paste0(reorder(LA_name,  -value), ': ', round(value, 2)))) +
           geom_col(fill = '#F8766D') +
           theme_minimal() +
           theme(axis.text.x=element_blank()) +
           xlab('Local Authorities') +
           ylab('Episodes of ST-MAX per 100,000 adults')
         , tooltip = 'text') %>%
  layout(hoverlabel =list(bgcolor='white', font_color='black'))


# Episodes of ST-MAX by what happened next

t21_final %>% 
  filter(date == max_date & metric_type == 'value' & region_name == 'England' & age_band == 'All ages' & metric != 'Total') %>%
  ggplot(., aes(x = reorder(metric, -value), y = value, fill = reorder(metric, -value))) +
  geom_col() +
  theme_minimal()  +
  scale_fill_discrete(name = 'What happened after receipt of ST-MAX') +
  xlab('') +
  ylab('Number of episodes') +
  theme(axis.text.x=element_blank())

# Episodes of ST-MAX by what happened next (percentage)

t21_final %>% 
  filter(date == max_date & metric_type == 'percentage' & region_name == 'England' & age_band == 'All ages' & metric != 'Total') %>%
  ggplot(., aes(x = reorder(metric, -value), y = value, fill = reorder(metric, -value))) +
  geom_col() +
  theme_minimal()  +
  scale_fill_discrete(name = 'What happened after receipt of ST-MAX') +
  xlab('') +
  ylab('% of episodes') +
  theme(axis.text.x=element_blank())

# Episodes of ST-MAX for new clients by primary support reason

t23_final %>%
  filter(metric != 'Total' & age_band == 'All ages' & metric_type == 'value' & date == max_date) %>%
  ggplot(., aes(x = reorder(metric, -value), y = value, fill = reorder(metric, -value))) +
  geom_col() +
  theme_minimal()  +
  scale_fill_discrete(name = 'Primary support reason') +
  xlab('') +
  ylab('Number of episodes') +
  theme(axis.text.x=element_blank())


# Episodes of ST-MAX for new clients by primary support reason and age band
  t23_final %>%
    filter(metric != 'Total' & age_band == '18 to 64' & metric_type == 'percentage' & date == max_date) %>%
  ggplot(., aes(x = reorder(metric, -value), y = value, fill = reorder(metric, -value))) +
    geom_col() +
    theme_minimal()  +
    scale_fill_discrete(name = 'Primary support reason') +
    xlab('') +
    ylab('Percentage of episodes') +
    theme(axis.text.x=element_blank())
  
  t23_final %>%
    filter(metric != 'Total' & age_band == '65 and over' & metric_type == 'percentage' & date == max_date) %>%
    ggplot(., aes(x = reorder(metric, -value), y = value, fill = reorder(metric, -value))) +
    geom_col() +
    theme_minimal()  +
    scale_fill_discrete(name = 'Primary support reason') +
    xlab('') +
    ylab('Percentage of episodes') +
    theme(axis.text.x=element_blank())

# Episodes of ST-MAX for new clients by primary support reason and age band (combined, number)  
  t23_final %>%
    filter(metric != 'Total' & metric_type == 'value' & date == max_date & age_band != 'All ages') %>%
    ggplot(., aes(x = reorder(metric, value), y = value, fill = age_band)) +
    geom_bar(position="dodge", stat="identity") +
    theme_minimal()  +
    coord_flip() +
    scale_fill_discrete(name = 'Primary support reason') +
    xlab('') +
    ylab('Number of episodes') +
    scale_fill_discrete(name = "Age band")
  
  # Episodes of ST-MAX for new clients by primary support reason and age band (combined, percentage)  
  t23_final %>%
    filter(metric != 'Total' & metric_type == 'percentage' & date == max_date & age_band != 'All ages') %>%
    ggplot(., aes(x = reorder(metric, value), y = value, fill = age_band)) +
    geom_bar(position="dodge", stat="identity") +
    theme_minimal()  +
    coord_flip() +
    scale_fill_discrete(name = 'Primary support reason') +
    xlab('') +
    ylab('% of episodes') +
    scale_fill_discrete(name = "Age band")
  
### ASC-OF data

## Table 2B(1): Proportion of older people still at home 91 days after discharge from hospital into reablement services
table_2b1 %>%
  filter(cassr == 'ENGLAND' & variable_type == 'Outcome') %>%
  ggplot(., aes(x = as_date(year), y = as.numeric(value))) +
  geom_line(color = '#F8766D') +
  theme_minimal()

## Proportion of older people still at home 91 days after discharge from hospital into reablement services by region

ggplotly(
table_2b1 %>%
  filter(cassr != 'ENGLAND' & variable_type == 'Outcome' & year == max_date) %>%
  ggplot(., aes(x = reorder(cassr,  -as.numeric(value)), y = as.numeric(value), text = paste0(reorder(cassr,  -as.numeric(value)), ': ', value))) +
  geom_col(fill = '#F8766D') +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  xlab('Local Authorities') +
  ylab('Percent still at home after 91 days')
  , tooltip = 'text') %>%
 layout(hoverlabel =list(bgcolor='white', font_color='black'))



## Table 2B(2): Proportion of older people receiving reablement care following discharge from hospital
table_2b2 %>%
  filter(cassr == 'ENGLAND' & variable_type == 'Outcome') %>%
  ggplot(., aes(x = as_date(year), y = as.numeric(value))) +
  geom_line(color = '#F8766D') +
  theme_minimal() +
  xlab('Date') +
  ylab('% of older people receiving reablement care')

table_2b2 %>%
  filter(cassr == 'ENGLAND' & variable_type == 'Numerator') %>%
  ggplot(., aes(x = as_date(year), y = as.numeric(value))) +
  geom_line(color = '#F8766D') +
  theme_minimal() +
  xlab('Date') +
  ylab('Number of older people receiving reablement care')

## Proportion of older people receiving reablement care following discharge from hospital by LA

ggplotly(
  table_2b2 %>%
    filter(cassr != 'ENGLAND' & variable_type == 'Outcome' & year == max_date) %>%
    ggplot(., aes(x = reorder(cassr,  -as.numeric(value)), y = as.numeric(value), text = paste0(reorder(cassr,  -as.numeric(value)), ': ', round(as.numeric(value), 2)))) +
    geom_col(fill = '#F8766D') +
    theme_minimal() +
    theme(axis.text.x=element_blank()) +
    xlab('Local Authorities') +
    ylab('Percent of over 65s receiving reablement on discharge')
  , tooltip = 'text') %>%
  layout(hoverlabel =list(bgcolor='white', font_color='black'))

# Prop receiving no or less intense care following reablement

table_2d %>%
  filter(cassr == 'ENGLAND' & variable_type == 'Outcome') %>%
  ggplot(., aes(x = as_date(year), y = as.numeric(value))) +
  geom_line(color = '#F8766D') +
  theme_minimal()
  







