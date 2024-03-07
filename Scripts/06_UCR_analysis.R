# Crisis response
# table 1 = % met target
# table 2 = count of referrals
# table 3 = count of contacts


rm(list=ls()) # Clear up workspace

# Check if project setup has been run, and run it if not
if ('rvest' %in% .packages() & dir.exists(file.path(('Raw_data/')))) { 
  print('Project setup run')
}else{
  source('Scripts/00_Setup_and_packages.R')}

url = 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/02/2-hour-Urgent-Community-Response-Performance-Metrics_2023-24_December.xlsx'

temp_file = tempfile(fileext = '.xlsx')
download.file(url, temp_file, mode='wb')

crisis_referrals_raw = read_excel(temp_file, sheet='Table 2', skip=5, .name_repair='universal')

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

mean(crisis_referrals$referrals[crisis_referrals$Organisation.Name == 'National'])

  