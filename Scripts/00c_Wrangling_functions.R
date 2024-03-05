## 00.c. WRANGLING FUNCTIONS

# Much of the data used in this project is scourced from Excel sheets which are not in amenable formats for analysis. 
# The below functions automate the process of feeding in each edition of the datasets after they're downloaded by the 
# functions in script 00b, and transforming them into formats more easily analyzed. 






# ASC data wrangling functions

ASCFR_wrangle <- function(x=NULL){

ascfr_data <- list.files('Raw_data/ASC_data', pattern='xlsx')

ascfr_data <- ascfr_data[!ascfr_data %in% c('ASCOF-time-series.xlsx')]


t21_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T16', skip = 7, col_names = FALSE)
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T21', skip = 6, col_names = FALSE)
  } else {
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
    
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T24', skip = 6, col_names = FALSE)
  }else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T24', skip = 8, col_names = FALSE) }
  
  if  (i != 'ASCFR_2016-17.xlsx'){
    
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

t24_all <- t24_all[2:7]

t27_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T27', skip = 6, col_names = FALSE)
  } else {
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T27', skip = 8, col_names = FALSE) }
  
  if (i != 'ASCFR_2016-17.xlsx') {
    
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

t27_all <- t27_all[2:7]

t28_all <- lapply(ascfr_data, function(i){
  
  if  (i == 'ASCFR_2016-17.xlsx'){
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T28', skip = 7, col_names = FALSE)
  }else {
    
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T28', skip = 9, col_names = FALSE) }
  
  if (i != 'ASCFR_2016-17.xlsx') {
    
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

t28_all <- t28_all[2:7]

ascfr_tables <- list(t24_all, t27_all, t28_all)

ascfr_FYs_all <- c('2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31', '2023-03-31')

ascfr_FYs_short <- c('2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31', '2023-03-31')

for (x in 1:7){
  t21_all[[x]] <- t21_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t21_final <- do.call('rbind', t21_all)

for (i in 1:3){
  for (x in 1:6){
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
  } else if(i == 'ASCFR_2022-23.xlsx'){ 
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T23', skip = 6, col_names = FALSE)
  } else {
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

for (x in 1:7){
  t23_all[[x]] <- t23_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t23_final <- do.call('rbind', t23_all)

t25_26_all <- lapply(ascfr_data, function(i){
  
  if (i == 'ASCFR_2016-17.xlsx') {
    df1 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T18', skip = 6, col_names = FALSE)
    df2 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T19', skip = 6, col_names = FALSE)
  } else if (i == 'ASCFR_2022-23.xlsx') {
    df1 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T25', skip = 6, col_names = FALSE) 
    df2 <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T26', skip = 6, col_names = FALSE)
  } else {
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

for (x in 1:7){
  t25_26_all[[x]] <- t25_26_all[[x]] %>%
    mutate(date = ascfr_FYs_all[[x]])
}

t25_26_final <- do.call('rbind', t25_26_all)

# Number of completed episodes by primary support reason for existing clients, by age band

return(list(t21_final, t23_final, t24_final, t25_26_final, t27_final, t28_final))

}

ASCOF_wrangle <- function(x=NULL){

ascof_FYs <- c('2016-03-31', '2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31', '2023-03-31')

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

table_2b1 <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2B(1)', skip = 6, col_names = FALSE, n_max = 172)

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

table_2b2 <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2B(2)', skip = 6, col_names = FALSE, n_max = 172)

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

table_2d <- read_excel('Raw_data/ASC_data/ASCOF-time-series.xlsx', sheet = '2D', skip = 6, col_names = FALSE, n_max = 172)

names(table_2d) <- colnames_2d

table_2d <- table_2d %>%
  pivot_longer(4:27, names_to = 'metric', values_to = 'value') %>%
  separate(metric, c('year', 'variable_type', 'metric'), sep = ':')

return(list(table_2b1, table_2b2, table_2d))

}


# Expenditure wrangling functions 

expenditure_ASC_wrangle <- function(x=NULL){
ascfr_data <- list.files('Raw_data/ASC_data', pattern='xlsx')

ascfr_data <- ascfr_data[!ascfr_data %in% c('ASCOF-time-series.xlsx')]

t32_all <- lapply(ascfr_data, function(i){
  
  if(i=='ASCFR_2016-17.xlsx'){
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T24', skip = 8, col_names = FALSE)
  } else if(i=='ASCFR_2022-23.xlsx'){
    df <- read_excel(paste0('Raw_data/ASC_data/', i), sheet = 'T32', skip = 7, col_names = FALSE)
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

ascfr_FYs <- c('2017-03-31', '2018-03-31', '2019-03-31', '2020-03-31', '2021-03-31', '2022-03-31', '2023-03-31')


for (x in 1:7){
  t32_all[[x]] <- t32_all[[x]] %>%
    mutate(date = ascfr_FYs[[x]])
}

t32_final <- do.call('rbind', t32_all) %>%
  filter((year == 'current year') | (year == 'previous year' & date == '2017-03-31'))

t32_final$date[t32_final$date == '2017-03-31' & t32_final$year == 'previous year'] <- '2016-03-31'

return(t32_final)

}
