library(tidyverse)
library(zoo)
library(usdata)
case_data_ihr1 <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>% 
  select(submission_date, state, new_case) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y")) %>% 
  mutate(key = paste(date,state)) %>% 
  arrange(date,state) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  filter(state!="NY") %>% 
  filter(state!="NYC") %>% 
  select(date,state,new_case)
case_data_ihr2 <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>% 
  select(submission_date, state, new_case) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y")) %>% 
  arrange(date,state) %>% 
  filter(state=="NY"|state=="NYC") %>% 
  group_by(date) %>% 
  summarise(new_case=sum(new_case)) %>% 
  mutate(state="NY") %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  select(date,state,new_case)
case_data_ihr<-rbind(case_data_ihr1,case_data_ihr2) %>% 
  arrange(date,state) %>% 
  group_by(state) %>% 
  mutate(new_cases=rollsumr(new_case,k=7, fill=NA)) %>% 
  mutate(ihrdate=date+7) %>% 
  mutate(key=paste(ihrdate,state)) %>% 
  select(state,key,new_cases)
hospitalizations_ihr <- read_csv("https://healthdata.gov/resource/g62h-syeh.csv?$limit=50000") %>% 
  select(state,date,previous_day_admission_adult_covid_confirmed,previous_day_admission_pediatric_covid_confirmed) %>% 
  arrange(date,state) %>% 
  mutate(previous_day_hospitalizations=previous_day_admission_adult_covid_confirmed+previous_day_admission_pediatric_covid_confirmed) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  group_by(state) %>% 
  mutate(new_hospitalizations_7=rollsumr(previous_day_hospitalizations,k=7, fill=NA)) %>% 
  mutate(key=paste(date,state)) %>% 
  select(state,key,new_hospitalizations_7)
ihr<-left_join(case_data_ihr,hospitalizations_ihr,by="key") %>% 
  filter(!is.na(new_hospitalizations_7)) %>% 
  mutate(ihr=new_hospitalizations_7/new_cases) %>% 
  select(key,ihr)
case_data1 <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>% 
  select(submission_date, state, new_case) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y")) %>% 
  arrange(date,state) %>% 
  group_by(state) %>% 
  mutate(avgnewcases=rollmeanr(new_case, k = 7,fill = NA),sumnewcases=rollsumr(new_case,k=7, fill=NA)) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  mutate(date=date+1) %>% 
  filter(state!="NY") %>% 
  filter(state!="NYC") %>% 
  mutate(key=paste(date,state)) %>% 
  select(date,state,avgnewcases,sumnewcases,new_case,key)
case_data2 <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>% 
  select(submission_date, state, new_case) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y")) %>% 
  arrange(date,state) %>% 
  filter(state=="NY"|state=="NYC") %>% 
  group_by(date) %>% 
  summarise(new_case=sum(new_case)) %>% 
  mutate(state="NY") %>% 
  group_by(state) %>% 
  mutate(avgnewcases=rollmeanr(new_case, k = 7,fill = NA),sumnewcases=rollsumr(new_case,k=7, fill=NA)) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  mutate(date=date+1) %>% 
  mutate(key=paste(date,state)) %>% 
  select(date,state,avgnewcases,sumnewcases,new_case,key)
case_data<-rbind(case_data1,case_data2) %>% 
  arrange(date,state)
hospitalizations <- read_csv("https://healthdata.gov/resource/g62h-syeh.csv?$limit=50000") %>% 
  select(state,date,inpatient_beds,inpatient_beds_used,previous_day_admission_adult_covid_confirmed,previous_day_admission_pediatric_covid_confirmed) %>% 
  mutate(new_hospitalizations=previous_day_admission_adult_covid_confirmed+previous_day_admission_pediatric_covid_confirmed) %>%
  mutate(open_beds=inpatient_beds-inpatient_beds_used) %>% 
  rename(beds_used=inpatient_beds_used,total_beds=inpatient_beds) %>% 
  arrange(date,state) %>% 
  group_by(state) %>% 
  mutate(pctopen=open_beds/total_beds) %>% 
  group_by(state) %>% 
  mutate(new_hospitalizations_7=rollsumr(new_hospitalizations,k=7, fill=NA)) %>%
  filter(date>=as.Date("2021-10-01")) %>% 
  mutate(key=paste(date,state)) %>% 
  select(date,state,new_hospitalizations,new_hospitalizations_7,total_beds,beds_used,open_beds,pctopen,key)
cases_hospitalizations=left_join(case_data,hospitalizations,by="key") %>% 
  select(date.x,state.x,avgnewcases,sumnewcases,new_case,new_hospitalizations,new_hospitalizations_7,total_beds,beds_used,open_beds,pctopen,key) %>% 
  rename(date=date.x,state=state.x)
case_data1a <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>% 
  select(submission_date, state, new_case) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y")) %>% 
  arrange(date,state) %>% 
  group_by(state) %>% 
  mutate(baselinecases=rollsumr(new_case,k=7, fill=NA)) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  mutate(date=date+1) %>% 
  filter(state!="NY") %>% 
  filter(state!="NYC") %>% 
  mutate(baselinedate=date+10) %>% 
  mutate(key=paste(baselinedate,state)) %>% 
  select(baselinedate,state,baselinecases,key)
case_data2a <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD") %>% 
  select(submission_date, state, new_case) %>%
  mutate(date = as.Date(submission_date, format = "%m/%d/%Y")) %>% 
  arrange(date,state) %>% 
  filter(state=="NY"|state=="NYC") %>% 
  group_by(date) %>% 
  summarise(new_case=sum(new_case)) %>% 
  mutate(state="NY") %>% 
  group_by(state) %>% 
  mutate(baselinecases=rollsumr(new_case,k=7, fill=NA)) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  mutate(date=date+1) %>% 
  mutate(baselinedate=date+10) %>% 
  mutate(key=paste(baselinedate,state)) %>% 
  select(baselinedate,state,baselinecases,key)
case_data_baseline<-rbind(case_data1a,case_data2a) %>% 
  arrange(baselinedate,state) %>% 
  select(key,baselinecases)
cases_hospitalizations_ihr=left_join(cases_hospitalizations,ihr,by="key") %>% 
  select(date,state,avgnewcases,sumnewcases,new_case,new_hospitalizations,new_hospitalizations_7,total_beds,beds_used,open_beds,pctopen,ihr,key)
cases_hospitalizations_baseline<-left_join(cases_hospitalizations_ihr,case_data_baseline,by="key") %>% 
  select(date,state.x,new_case,sumnewcases,avgnewcases,baselinecases,new_hospitalizations,new_hospitalizations_7,total_beds,beds_used,open_beds,pctopen,ihr,key) %>%
  rename(state=state.x) %>% 
  mutate(circuit_increase_case7=open_beds*(1/ihr)) %>% 
  mutate(circuit_increase_case1=circuit_increase_case7/7) %>% 
  mutate(circuit_newcase7=circuit_increase_case7+baselinecases) %>% 
  mutate(circuit_newcase1=circuit_newcase7/7) %>% 
  mutate(casethresholdpct=avgnewcases/circuit_newcase1*100) %>% 
  mutate(circuit_increase_hospital7=open_beds) %>% 
  mutate(circuit_increase_hospital1=circuit_increase_hospital7/7) %>% 
  mutate(circuit_newhospital7=circuit_increase_hospital7+new_hospitalizations_7) %>% 
  mutate(circuit_newhospital1=circuit_newhospital7/7)
icu_data <- read_csv("https://healthdata.gov/resource/g62h-syeh.csv?$limit=50000") %>% 
  arrange(date,state) %>% 
  select(date,state,total_adult_patients_hospitalized_confirmed_covid,staffed_icu_adult_patients_confirmed_covid,total_staffed_adult_icu_beds,staffed_adult_icu_bed_occupancy) %>% 
  filter(date>=as.Date("2021-10-01")) %>% 
  mutate(icuratio=(staffed_icu_adult_patients_confirmed_covid/(total_adult_patients_hospitalized_confirmed_covid-staffed_icu_adult_patients_confirmed_covid))/3) %>% 
  mutate(openicubeds=total_staffed_adult_icu_beds-staffed_adult_icu_bed_occupancy) %>% 
  rename(totalicubeds=total_staffed_adult_icu_beds) %>% 
  mutate(key=paste(date,state)) %>% 
  select(key,icuratio,totalicubeds,openicubeds)
cases_hospitalizations_baseline_icu<-left_join(cases_hospitalizations_baseline,icu_data,by="key") %>% 
  mutate(circuit_increase_icucase7=openicubeds/(ihr*icuratio)) %>% 
  mutate(circuit_icunewcase7=circuit_increase_icucase7+baselinecases) %>% 
  mutate(circuit_icunewcase1=circuit_icunewcase7/7)
final<-cases_hospitalizations_baseline_icu %>% 
  select(date,state,circuit_newcase7,circuit_newcase1,circuit_newhospital7,circuit_newhospital1,total_beds,open_beds,totalicubeds,openicubeds,avgnewcases,new_case,circuit_icunewcase1) %>% 
  filter(state!="AS") %>% 
  filter(state!="VI") %>% 
  filter(!is.na(total_beds)) %>% 
  filter(date>=as.Date("2021-11-01")) %>% 
  mutate(`Percent Occupied`=(total_beds-open_beds)/total_beds*100) %>% 
  mutate(`ICU Percent Occupied`=(totalicubeds-openicubeds)/totalicubeds*100) %>% 
  mutate(`At Threshold?`=ifelse(avgnewcases>=circuit_newcase1,"Yes","No")) %>%
  mutate(`At ICU Threshold?`=ifelse(avgnewcases>=circuit_icunewcase1,"Yes","No")) %>%
  mutate(Category = ifelse(`Percent Occupied`>=100,"At Capacity",ifelse(`At Threshold?`=="Yes"&`Percent Occupied`<100,"Forecasted to Exceed Capacity",ifelse(new_case>=circuit_newcase1 & `At Threshold?`=="No","Unsustainable","Has Capacity")))) %>% 
  mutate(`ICU Category` = ifelse(`ICU Percent Occupied`>=100,"At Capacity",ifelse(`At ICU Threshold?`=="Yes"&`ICU Percent Occupied`<100,"Forecasted to Exceed Capacity",ifelse(new_case>=circuit_icunewcase1 & `At ICU Threshold?`=="No","Unsustainable","Has Capacity")))) %>% 
  rename(`Total number of new cases in 7 days to reach circuit breaker`=circuit_newcase7) %>% 
  rename(`Average daily cases to reach circuit breaker`=circuit_newcase1) %>% 
  rename(`Total number of new hospitalizations in 7 days to reach circuit breaker`=circuit_newhospital7) %>% 
  rename(`Average daily hospitalizations to reach circuit breaker`=circuit_newhospital1) %>% 
  rename(`Total Beds`=total_beds) %>% 
  rename(`Open Beds`=open_beds) %>% 
  rename(`Average New Daily Cases`=avgnewcases) %>% 
  rename (`New Cases Yesterday (State)`=new_case) %>% 
  rename(`Average daily cases to reach ICU circuit breaker`=circuit_icunewcase1) %>% 
  rename(`Open ICU Beds`=openicubeds) %>% 
  rename(`Total ICU Beds`=totalicubeds)
countycases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  select(-c("UID","iso2","iso3","code3","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key"))
countycases_pivot <- pivot_longer(data = countycases, cols = -c(1),
                                  names_to = "Date",
                                  values_to = "Cases") %>% 
  mutate(FIPS = str_pad(FIPS,5,pad = "0")) %>% 
  select(FIPS, Date, Cases) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  filter(Date>=as.Date("2021-10-01")) %>% 
  group_by(FIPS) %>% 
  mutate(newcases=Cases-lag(Cases)) %>% 
  select(Date,FIPS,newcases)
hospital_facility <- read_csv("https://healthdata.gov/resource/anag-cw7u.csv?$limit=600000") %>% 
  select(hospital_pk,collection_week,fips_code,state,inpatient_beds_7_day_avg,inpatient_beds_used_7_day_avg) %>% 
  mutate(collection_week = as.Date(collection_week, format = "%m/%d/%y")) %>% 
  filter(collection_week>=as.Date("2021-10-22")) %>% 
  mutate(hospital_date=collection_week+6) %>% 
  filter(inpatient_beds_7_day_avg>=0) %>% 
  filter(inpatient_beds_used_7_day_avg>=0) %>% 
  group_by(hospital_date,fips_code) %>% 
  summarise(total_beds=sum(inpatient_beds_7_day_avg), beds_used=sum(inpatient_beds_used_7_day_avg)) %>% 
  mutate(open_beds=total_beds-beds_used,pctopen=open_beds/total_beds)
countycases_reference <-  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  select(FIPS,Province_State,Combined_Key) %>% 
  mutate(FIPS = str_pad(FIPS,5,pad = "0")) 
county_beds<-left_join(countycases_reference,hospital_facility,by=c("FIPS"="fips_code")) %>% 
  filter(!is.na(total_beds)) %>% 
  mutate(abbr=state2abbr(Province_State)) %>% 
  mutate(key=paste(hospital_date,FIPS))
countycasesavg <- countycases_pivot %>% 
  group_by(FIPS) %>% 
  mutate(avgnewcases=rollmeanr(newcases,k=7,fill=NA)) %>% 
  group_by(FIPS) %>% 
  mutate(sumnewcases=rollsumr(newcases,k=7,fill=NA)) %>% 
  filter(!is.na(newcases)) %>% 
  mutate(key=paste(Date,FIPS)) %>% 
  select(Date,FIPS,avgnewcases,sumnewcases,newcases,key) %>% 
  rename(newcasesyesterday=newcases)
case_hospital_reference<-read_csv("County Timeseries Reference Hospital Dates.csv") %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  mutate(closesthospitaldate = as.Date(closesthospitaldate, format = "%m/%d/%y"))
newcasesa<-countycases_pivot %>% 
  group_by(FIPS) %>% 
  mutate(sumnewcases=rollsumr(newcases,k=7,fill=NA)) %>% 
  filter(!is.na(newcases)) %>% 
  mutate(Datea=Date+7) %>% 
  mutate(key=paste(Datea,FIPS)) %>% 
  select(key,sumnewcases) %>% 
  rename(newcasesa=sumnewcases)
newcasesb<-countycases_pivot %>% 
  group_by(FIPS) %>% 
  mutate(sumnewcases=rollsumr(newcases,k=7,fill=NA)) %>% 
  filter(!is.na(newcases)) %>% 
  mutate(Dateb=Date+7) %>% 
  mutate(key=paste(Dateb,FIPS)) %>% 
  select(key,sumnewcases) %>% 
  rename(newcasesb=sumnewcases)
cases_hospital2<-left_join(countycasesavg,case_hospital_reference,by="Date")
cases_hospital2a<-left_join(cases_hospital2,newcasesa,by="key") %>% 
  select(-FIPS.y) %>% 
  rename(FIPS=FIPS.x) %>% 
  filter(!is.na(FIPS)) %>% 
  mutate(hospitalkey=paste(closesthospitaldate,FIPS))
cases_hospitalab<-left_join(cases_hospital2a,newcasesb,by=c("hospitalkey"="key")) %>% 
  select(-FIPS.y) %>% 
  rename(FIPS=FIPS.x)
countycasesbaseline <- countycasesavg %>%
  select(Date,FIPS,sumnewcases) %>% 
  mutate(baselinedate=Date+10) %>% 
  mutate(key=paste(baselinedate,FIPS)) %>% 
  select(key,sumnewcases) %>% 
  rename(baselinecases=sumnewcases)
casesb_baseline<-left_join(cases_hospitalab,countycasesbaseline,by="key") %>% 
  select(Date,FIPS.x,avgnewcases,sumnewcases,newcasesyesterday,closesthospitaldate,newcasesa,newcasesb,baselinecases,key,hospitalkey) %>% 
  rename(FIPS=FIPS.x)
cases_baseline_beds<-left_join(casesb_baseline,county_beds,by=c("hospitalkey"="key")) %>% 
  filter(!is.na(FIPS.x)) %>% 
  select(Date,FIPS.x,avgnewcases,sumnewcases,newcasesyesterday,closesthospitaldate,newcasesb,baselinecases,key,hospitalkey,Combined_Key,hospital_date,total_beds,open_beds,pctopen,abbr) %>% 
  rename(FIPS=FIPS.x) %>% 
  filter(!is.na(Combined_Key)) %>% 
  mutate(ihrkey=paste(Date,abbr))
cases_baseline_beds_ihra<-left_join(cases_baseline_beds,ihr,by=c("ihrkey"="key")) %>% 
  rename(ihra=ihr) %>% 
  mutate(ihrhospitalkey=paste(hospital_date,abbr))
cases_baseline_beds_ihrb<-left_join(cases_baseline_beds_ihra,ihr,by=c("ihrhospitalkey"="key")) %>% 
  rename(ihrb=ihr)
county_cases_beds_nowcast<-cases_baseline_beds_ihrb %>% 
  filter(!is.na(FIPS)) %>% 
  mutate(openbedsnowcast=open_beds-((sumnewcases*ihra)-(newcasesb*ihrb))) %>% 
  mutate(circuit_increase_hospital7=openbedsnowcast) %>% 
  mutate(circuit_increase_hospital1=circuit_increase_hospital7/7) %>%  
  mutate(circuit_newhospital7=openbedsnowcast+(baselinecases*ihra)) %>%
  mutate(circuit_newhospital1=circuit_newhospital7/7) %>% 
  mutate(circuit_increase_case7=openbedsnowcast*(1/ihra)) %>% 
  mutate(circuit_increase_case1=circuit_increase_case7/7) %>% 
  mutate(circuit_newcase7=circuit_increase_case7+baselinecases) %>% 
  mutate(circuit_newcase1=circuit_newcase7/7) 
final_county_ts <- county_cases_beds_nowcast %>% 
  select(Date,FIPS,Combined_Key,abbr,circuit_newcase7,circuit_newcase1,circuit_newhospital7,circuit_newhospital1,total_beds,open_beds,openbedsnowcast,avgnewcases,newcasesyesterday) %>% 
  mutate(`Percent Occupied (Nowcast)`=(total_beds-openbedsnowcast)/total_beds*100) %>% 
  mutate(`Percent Occupied (last HHS update)`=(total_beds-open_beds)/total_beds*100) %>% 
  mutate(`At Threshold?`=ifelse(avgnewcases>=circuit_newcase1,"Yes","No")) %>%
  mutate(Category = ifelse(`Percent Occupied (Nowcast)`>=100,"At Capacity",ifelse(`At Threshold?`=="Yes"&`Percent Occupied (Nowcast)`<100,"Forecasted to Exceed Capacity",ifelse(newcasesyesterday>=circuit_newcase1 & `At Threshold?`=="No","Unsustainable","Has Capacity")))) %>% 
  mutate(casethresholdpct=avgnewcases/circuit_newcase1*100) %>% 
  rename(`Total number of new cases in 7 days to reach circuit breaker`=circuit_newcase7) %>% 
  rename(`Average daily cases to reach circuit breaker`=circuit_newcase1) %>% 
  rename(`Total number of new hospitalizations in 7 days to reach circuit breaker`=circuit_newhospital7) %>% 
  rename(`Average daily hospitalizations to reach circuit breaker`=circuit_newhospital1) %>% 
  rename(`Total Beds`=total_beds) %>% 
  rename(`Open Beds (last HHS Update)`=open_beds) %>% 
  rename(`Open Beds (Nowcast)`=openbedsnowcast) %>% 
  rename(`Average New Daily Cases`=avgnewcases) %>% 
  rename (`New Cases Yesterday`=newcasesyesterday) %>% 
  rename(County=Combined_Key) %>% 
  filter(!is.na(Category))
county_ts2<-final_county_ts %>% 
  mutate(categorykey=ifelse(Category=="At Capacity"|Category=="Forecasted to Exceed Capacity",1,0)) %>% 
  group_by(FIPS) %>% 
  mutate(categorysum=rollsumr(categorykey,k=7,fill=NA)) %>% 
  mutate(Warning=ifelse(categorysum>=1,"This county was either at capacity or forecasted to exceed capacity in at least 1 of the last 7 days.",NA)) %>% 
  mutate(Category = ifelse(`Percent Occupied (Nowcast)`>=100,"At Capacity",ifelse(`At Threshold?`=="Yes"&`Percent Occupied (Nowcast)`<100,"Forecasted to Exceed Capacity",ifelse(`New Cases Yesterday`>=`Average daily cases to reach circuit breaker` & `At Threshold?`=="No","Unsustainable",ifelse(!is.na(Warning)&Category=="Has Capacity","Continued Risk","Has Capacity"))))) %>% 
  select(-categorykey,-categorysum) %>% 
  filter(Date>=as.Date("2022-01-01"))
county_daily <-county_ts2 %>% 
  filter(Date==max(Date)) %>% 
  select(-Date)
county_state_estimates<-final_county_ts %>% 
  mutate(Date=Date+1) %>% 
  group_by(Date,abbr) %>% 
  summarise(totalbedscounty=sum(`Total Beds`),openbedsnowcast=sum(`Open Beds (Nowcast)`)) %>% 
  mutate(key=paste(Date,abbr))
cases_hospitalizations_baseline_icu2<-cases_hospitalizations_baseline_icu %>% 
  mutate(key=paste(date,state))
state_county_final_estimates<-left_join(cases_hospitalizations_baseline_icu2,county_state_estimates,by="key") %>% 
  mutate(circuit_increase_case7=openbedsnowcast*(1/ihr)) %>% 
  mutate(circuit_increase_case1=circuit_increase_case7/7) %>% 
  mutate(circuit_newcase7=circuit_increase_case7+baselinecases) %>% 
  mutate(circuit_newcase1=circuit_newcase7/7) %>% 
  mutate(casethresholdpct=avgnewcases/circuit_newcase1*100) %>% 
  mutate(circuit_increase_hospital7=openbedsnowcast) %>% 
  mutate(circuit_increase_hospital1=circuit_increase_hospital7/7) %>% 
  mutate(circuit_newhospital7=circuit_increase_hospital7+new_hospitalizations_7) %>% 
  mutate(circuit_newhospital1=circuit_newhospital7/7) %>% 
  mutate(circuit_increase_icucase7=openicubeds/(ihr*icuratio)) %>% 
  mutate(circuit_icunewcase7=circuit_increase_icucase7+baselinecases) %>% 
  mutate(circuit_icunewcase1=circuit_icunewcase7/7) %>% 
  select(date,state,circuit_newcase7,circuit_newcase1,circuit_newhospital7,circuit_newhospital1,totalbedscounty,open_beds,pctopen,openbedsnowcast,totalicubeds,openicubeds,avgnewcases,new_case,circuit_icunewcase1,casethresholdpct) %>% 
  filter(state!="AS") %>% 
  filter(state!="VI") %>% 
  filter(state!="PR") %>% 
  filter(!is.na(totalbedscounty)) %>% 
  mutate(`Percent Occupied (State Nowcast)`=(totalbedscounty-openbedsnowcast)/totalbedscounty*100) %>%
  mutate(`Percent Occupied (State HHS Data)`=(1-pctopen)*100) %>%
  mutate(`ICU Percent Occupied`=(totalicubeds-openicubeds)/totalicubeds*100) %>% 
  mutate(`At Threshold?`=ifelse(avgnewcases>=circuit_newcase1,"Yes","No")) %>%
  mutate(`At ICU Threshold?`=ifelse(avgnewcases>=circuit_icunewcase1,"Yes","No")) %>%
  mutate(Category = ifelse(`Percent Occupied (State Nowcast)`>=100,"At Capacity",ifelse(`At Threshold?`=="Yes"&`Percent Occupied (State Nowcast)`<100,"Forecasted to Exceed Capacity",ifelse(new_case>=circuit_newcase1 & `At Threshold?`=="No","Unsustainable","Has Capacity")))) %>% 
  mutate(`ICU Category` = ifelse(`ICU Percent Occupied`>=100,"At Capacity",ifelse(`At ICU Threshold?`=="Yes"&`ICU Percent Occupied`<100,"Forecasted to Exceed Capacity",ifelse(new_case>=circuit_icunewcase1 & `At ICU Threshold?`=="No","Unsustainable","Has Capacity")))) %>% 
  select(-pctopen) %>% 
  rename(`Total number of new cases in 7 days to reach circuit breaker`=circuit_newcase7) %>% 
  rename(`Average daily cases to reach circuit breaker`=circuit_newcase1) %>% 
  rename(`Total number of new hospitalizations in 7 days to reach circuit breaker`=circuit_newhospital7) %>% 
  rename(`Average daily hospitalizations to reach circuit breaker`=circuit_newhospital1) %>% 
  rename(`Total Beds`=totalbedscounty) %>% 
  rename(`Open Beds`=openbedsnowcast) %>% 
  rename(`Open Beds (HHS Data)`=open_beds) %>% 
  rename(`Average New Daily Cases`=avgnewcases) %>% 
  rename (`New Cases Yesterday (State)`=new_case) %>% 
  rename(`Average daily cases to reach ICU circuit breaker`=circuit_icunewcase1) %>% 
  rename(`Open ICU Beds`=openicubeds) %>% 
  rename(`Total ICU Beds`=totalicubeds) %>% 
  mutate(categorykey=ifelse(Category=="At Capacity"|Category=="Forecasted to Exceed Capacity",1,0)) %>% 
  group_by(state) %>% 
  mutate(categorysum=rollsumr(categorykey,k=7,fill=NA)) %>% 
  mutate(Warning=ifelse(categorysum>=1,"This state was either at capacity or forecasted to exceed capacity in at least 1 of the last 7 days.",NA)) %>% 
  mutate(Category = ifelse(`Percent Occupied (State Nowcast)`>=100,"At Capacity",ifelse(`At Threshold?`=="Yes"&`Percent Occupied (State Nowcast)`<100,"Forecasted to Exceed Capacity",ifelse(`New Cases Yesterday (State)`>=`Average daily cases to reach circuit breaker` & `At Threshold?`=="No","Unsustainable",ifelse(!is.na(Warning)&Category=="Has Capacity","Continued Risk","Has Capacity"))))) %>% 
  select(-categorykey,-categorysum)
state_daily <-state_county_final_estimates %>% 
  filter(date==max(date)) %>% 
  select(-date)
countytimeseries_selected<-final_county_ts %>% 
  select(Date,FIPS, County, Category,`Percent Occupied (Nowcast)`,`Percent Occupied (last HHS update)`)
write.csv(county_daily, "circuit_breaker_county_daily.csv", row.names = F, na = "")
write.csv(state_daily, "circuit_breaker_state_daily.csv", row.names = F, na = "")
write.csv(county_ts2, "circuit_breaker_county_timeseries.csv", row.names = F, na = "")
write.csv(state_county_final_estimates, "circuit_breaker_state_timeseries.csv", row.names = F, na = "")
write.csv(countytimeseries_selected, "circuit_breaker_county_timeseries_selected.csv", row.names = F, na = "")
