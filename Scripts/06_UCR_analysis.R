library(aws.s3)

# Crisis response
# table 1 = % met target
# table 2 = count of referrals
# table 3 = count of contacts

url = 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/11/2-hour-Urgent-Community-Response-Performance-Metrics_2023-24_September.xlsx'

temp_file = tempfile(fileext = '.xlsx')
download.file(url, temp_file, mode='wb')

crisis_resp_2hr_raw = read_excel(temp_file, sheet='Table 1', skip=5, .name_repair='universal')

crisis_referrals_raw = read_excel(temp_file, sheet='Table 2', skip=5, .name_repair='universal')


####### Analysis
# 2 hour target
crisis_resp_2hr = crisis_resp_2hr_raw %>%
  pivot_longer(cols=!c('ODS.Code', 'Organisation.Name', 'Organisation.Type'), names_to='monthyr', values_to='percent') %>%
  filter(!is.na(percent)) %>%
  mutate(monthyr = str_remove(monthyr, '..provisional..'),
         month = substr(monthyr, 1,3),
         yr = as.numeric(substr(monthyr, 5, 6)),
         month = match(tolower(month), tolower(month.abb)),
         date = as.Date(paste( 1, month, yr, sep = "-")),
         Organisation.Name = str_to_title(str_remove_all(Organisation.Name, 'COMMISSIONING REGION')))%>%
  select(-monthyr, -month, -yr)

colnames(crisis_resp_2hr)

crisis_resp_2hr %>% filter(Organisation.Name == 'National') %>% mutate(percent=round(percent, 2))

crisis_resp_2hr %>% filter(Organisation.Type == 'Region') %>% mutate(percent=round(percent, 2)) %>%
  filter(date==max(date)) %>% select(Organisation.Name, percent)


# referral counts
crisis_referrals = crisis_referrals_raw %>%
  pivot_longer(cols=!c('ODS.Code', 'Organisation.Name', 'Organisation.Type'), names_to='monthyr', values_to='referrals') %>%
  filter(!is.na(referrals)) %>%
  mutate(monthyr = str_remove(monthyr, '..provisional..'),
         month = substr(monthyr, 1,3),
         yr = as.numeric(substr(monthyr, 5, 6)),
         month = match(tolower(month), tolower(month.abb)),
         date = as.Date(paste( 1, month, yr, sep = "-")),
         Organisation.Name = str_to_title(str_remove_all(Organisation.Name, 'COMMISSIONING REGION')))%>%
  select(-monthyr, -month, -yr)

View(crisis_referrals)

crisis_referrals %>% filter(Organisation.Name == 'National') 

crisis_referrals %>% filter(Organisation.Type == 'Region') %>% 
  filter(date==max(date)) %>% select(Organisation.Name, referrals)



  