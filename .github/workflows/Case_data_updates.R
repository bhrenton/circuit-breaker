library(tidyverse)
library(zoo)
case_data1 <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>% 
  select(submission_date, state, new_case) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y")) %>% 
  arrange(date,state) %>% 
  group_by(state) %>% 
  mutate(avgnewcases=rollmeanr(new_case, k = 10,fill = NA)) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  filter(state!="NY") %>% 
  filter(state!="NYC") %>% 
  select(date,state,avgnewcases,new_case)
case_data2 <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>% 
  select(submission_date, state, new_case) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y")) %>% 
  arrange(date,state) %>% 
  filter(state=="NY"|state=="NYC") %>% 
  group_by(date) %>% 
  summarise(new_case=sum(new_case)) %>% 
  mutate(state="NY") %>% 
  group_by(state) %>% 
  mutate(avgnewcases=rollmeanr(new_case, k = 10,fill = NA)) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  select(date,state,avgnewcases,new_case)
case_data<-rbind(case_data1,case_data2) %>% 
  arrange(date,state) %>% 
  select(date,state,avgnewcases,new_case)
write.csv(case_data, "state_case_timeseries.csv", row.names = F, na = "")
countycases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  select(-c("UID","iso2","iso3","code3","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key"))
countycases_pivot <- pivot_longer(data = countycases, cols = -c(1),
                                  names_to = "Date",
                                  values_to = "Cases") %>% 
  mutate(FIPS = str_pad(FIPS,5,pad = "0")) %>% 
  select(FIPS, Date, Cases) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  group_by(FIPS) %>% 
  mutate(newcases=Cases-lag(Cases)) %>% 
  group_by(FIPS) %>% 
  mutate(avgnewcases=rollmeanr(newcases, k = 10,fill = NA)) %>% 
  filter(Date>=as.Date("2021-10-01")) %>% 
  select(Date,FIPS,newcases,avgnewcases)
write.csv(countycases_pivot, "county_case_timeseries.csv", row.names = F, na = "")