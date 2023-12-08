# Bringing in age and sex population of ICBs

url = 'https://www.england.nhs.uk/wp-content/uploads/2022/04/b-projected-populations-22-23.xlsx'
temp_file = tempfile(fileext = '.xlsx')
download.file(url, temp_file, mode='wb')

pop_icb = read_excel(temp_file,  sheet='Projected ICB population', skip=15)

colnames(pop_icb)

age_pop_icb = pop_icb %>%
  filter(!is.na(R22)) %>%
  rename(total=`Pop22/23`, ICB22NM = 'Integrated Care Board') %>%
  select(-Male, -Female) %>%
  pivot_longer(cols=!c('R22', 'ICB22', "ICB22NM", 'total'), names_to = 'sex_age', values_to='pop') %>%
  mutate(age=str_remove_all(sex_age, 'm|f')) %>%
  select(-sex_age) %>%
  group_by( ICB22, R22, ICB22NM, total, age) %>%
  summarise(pop=sum(pop)) %>%
  mutate(pc=pop/total*100)

older_adults_icb = age_pop_icb %>%
  mutate(age_75plus = case_when(age %in% c('75-79', '80-84', '85+') ~ 1,
                                   TRUE ~ 0),
         age_65plus = case_when(age %in% c('65-69', '70-74', '75-79', '80-84', '85+') ~ 1,
                                   TRUE ~ 0)) %>%
  group_by(ICB22, age_75plus) %>%
  mutate(pop75 = sum(pop),
         pop75pc = pop75/total*100) %>%
  group_by(ICB22, age_65plus) %>%
  mutate(pop65 = sum(pop),
         pop65pc = pop65/total*100) %>%
  ungroup() %>%
  filter(age_75plus==1 & age_65plus == 1) %>%
  select(-age, -pop, -pc, -age_75plus, -age_65plus) %>%
  distinct()




  
  
  
