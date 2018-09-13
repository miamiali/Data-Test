# Date: Thursday, September 13th, 2018
# By: Mia Li
# Description: code for data test
# Version of R Studio used: 1.1.456 

#install and load packages required for this analysis####
install.packages(c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr","purrr", "gridExtra","broom","lazyeval", "Hmisc"))
r1<-c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr","purrr", "gridExtra","broom","lazyeval", "Hmisc")
lapply(r1,require,character.only=TRUE)


#import two patient datasets####
pat_encounters <- read.csv(file = "H:/Personal/New folder/Data test/Test_DataCore.csv")
pat_vital <- read.csv(file = "H:/Personal/New folder/Data test/Test_DataCore_VitalStats.csv")

#step 3 run descriptive analysis on the variables in both datasets
#run summary() on both datasets to see the what variables are included, what are their types, and what are their distributions like
summary(pat_encounters) #There are 13 variables; PATIENT_ID is the identifier for each patient; CLINIC_CODE, DATA_OF_DEATH, and DISCHARGE_TYPE have more missing values than other variables
summary(pat_vital) #There are 2 variables; PATIENT_ID is the identifier for each patient

#Count the number of unique patients in both datasets
pat_encounters %>%
  summarise(n = n_distinct(PATIENT_ID)) #n = 80

pat_vital %>%
  summarise(n = n_distinct(PATIENT_ID)) #n =98

#q1 number of patients####
#Join the two datasets and compare the difference by using PATIENT_ID
length(pat_vital$PATIENT_ID[pat_vital$PATIENT_ID %in% pat_encounters$PATIENT_ID]) #9 patients appear in both datasets, meaning there are 80+98-9 = 169 patients in both datasets

#demographics of the patients from the first dataset
#convert the data of birth variable to date format 
pat_encounters %<>%
  separate(BIRTHDATE, c("BIRTHDATE","DOB_TIME"), sep = ":", extra = "merge") %>%
  mutate(BIRTHDATE = dmy(BIRTHDATE))

#write a function to convert DOB of 20** to 19**
conv_yr_dob <- function(x, year = 2018) {
  y <- year(x)%%100
  year(x) <- ifelse(y > year %% 100, 1900 + y, 2000 + y)
  x
}

pat_encounters$BIRTHDATE <- conv_yr_dob(pat_encounters$BIRTHDATE)
#q2 demographics ####
#calculate age, then investigage the distribution of age, gender and race
pat_encounters %<>%
  mutate(AGE = 2015-year(BIRTHDATE))

pat_dem <- pat_encounters %>%
  group_by(PATIENT_ID) %>%
  filter(row_number() == 1) %>%
  select(c(1,6,8,9,15))

pat_dem %>%
  group_by() %>%
  summarise(mean_age = mean(AGE),
            median_age = median(AGE),
            min_age = min(AGE),
            max_age = max(AGE),
            sd_age = sd(AGE))

table(pat_dem$SEX)
SEX <- table(pat_dem$SEX)
prop.table(SEX)

table(pat_dem$RACE)
RACE <- table(pat_dem$RACE)
prop.table(RACE[RACE!="Unknown"])

#q3&4 How many of the patients died? ####
#examine the variable DATE OF DEATH
table(pat_encounters$DATE_OF_DEATH)
#filter patients with a nonempty death date
pat_encounters %>%
  filter(DATE_OF_DEATH != "") %>%
  summarise(n_dead = n_distinct(PATIENT_ID)) # 4 patients are dead in the encounters dataset

#check if these 4 patients are in the vital dataset
pat_encounters %>%
  filter(DATE_OF_DEATH != "") %>%
  semi_join(pat_vital, by = "PATIENT_ID") %>%
  summarise(n_dead_both = n_distinct(PATIENT_ID)) #all 4 patients showed up in the vital dataset

#check missing in vital data
pat_vital %>%
  summarise(n = sum(is.na(DATE_OF_DEATH))) #no missing
#there are 98 patients who are dead

#q5 For the patients who died, when did they visit the last visit date for the patients who have died? Create a table which lists number of days from last visit date to date of death. Interpret####

##convert admission date and discharge date variables into date format
pat_encounters %<>%
  separate(ADMISSION_DATE_TIME, c("AD_DATE","AD_TIME"), sep = ":", extra = "merge") %>%
  separate(DISCHARGE_DATE_TIME, c("DIS_DATE", "DIS_TIME"), sep = ":", extra =  "merge") %>%
  mutate(AD_DATE = dmy(AD_DATE),
         DIS_DATE = dmy(DIS_DATE),
         DATE_OF_DEATH = dmy(DATE_OF_DEATH))
#find the last visit who have died
pat_dead <- pat_encounters %>%
  filter(!is.na(DATE_OF_DEATH)) %>%
  group_by(PATIENT_ID) %>%
  arrange(AD_DATE) %>%
  filter(row_number() == n())%>%
  mutate(DIFF_LAST_DEATH = as.Date(DATE_OF_DEATH) - as.Date(AD_DATE))


#q6 Calculate total days spent at the hospital for each patient. Analyze /Interpret ####
pat_encounters %<>%
  mutate(DAYS_IN_HOS = as.Date(DIS_DATE) - as.Date(AD_DATE) + 1) %>%
  group_by(PATIENT_ID) %>%
  mutate(SUM_IN_HOS = sum(DAYS_IN_HOS, na.rm = TRUE))
#might be data entry errors?
#plot the total days spent at hospital for each patient
pat_encounters %>%
  group_by(PATIENT_ID) %>%
  filter(row_number() == 1) %>%
  ggplot(aes(x = 1, y = as.numeric(SUM_IN_HOS))) +
  geom_boxplot(width = 1) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size=15),
        panel.grid = element_blank())+
  labs(y = "Total number of days in hospital")


pat_encounters %>%
  group_by(PATIENT_ID) %>%
  filter(row_number() == 1) %>%
  group_by() %>%
  summarise(mean_SUM = mean(SUM_IN_HOS),
            median_SUM = median(SUM_IN_HOS),
            min_SUM = min(SUM_IN_HOS),
            max_SUM = max(SUM_IN_HOS),
            sd_SUM = sd(SUM_IN_HOS),
            q3 = quantile(SUM_IN_HOS,0.75)) #449.25 is the 75 percentile

days_spent <- pat_encounters %>%
  group_by(PATIENT_ID) %>%
  filter(row_number() == 1) %>%
  mutate(super_user = ifelse(SUM_IN_HOS >449.25, "Super User", "Non Super User"))

table_gender <- table(days_spent$super_user, days_spent$SEX)
chisq.test(table_gender)

t.test(as.numeric(days_spent$AGE)~days_spent$super_user)

sum(as.numeric(days_spent$SUM_IN_HOS[days_spent$SUM_IN_HOS > 449.25]))/sum(as.numeric(days_spent$SUM_IN_HOS))

#q7 Investigate inpatient visits (patients who stayed for more than a day at the hospital) for each patient. Output a list of the top 10 patient_ids who spent maximum days in the hospital in the past year (2015)####
pat_encounters %>%
  filter(year(AD_DATE) == 2015 | year(DIS_DATE) == 2015) %>%
  mutate(DIFF_AD_DIS = ifelse(year(AD_DATE) == 2014, as.Date(DIS_DATE) - as.Date("2015-1-1") + 1, as.Date(DIS_DATE) - as.Date(AD_DATE) + 1)) %>%
  group_by(PATIENT_ID) %>%
  mutate(sum_days_2005 = sum(DIFF_AD_DIS, na.rm = TRUE)) %>%
  arrange(desc(sum_days_2005))%>%
  select(PATIENT_ID, sum_days_2005)%>%
  distinct(PATIENT_ID, sum_days_2005) %>%
  top_n(10)

pat_encounters %>%
  group_by(year(AD_DATE)) %>%
  mutate(INPATIENT = ifelse(DAYS_IN_HOS > 1, "INPATIENT", "NONINPATIENT"),
         COUNT_VISIT_YEAR = n(),
         YEAR_AD = year(AD_DATE),
         YEAR_DIS =  year(DIS_DATE),
         YEAR_DEATH = year(DATE_OF_DEATH)) %>% 
  filter(!is.na(INPATIENT)) %>%
  ggplot(aes(x = as.factor(YEAR_AD), fill = as.factor(INPATIENT))) +
  geom_bar(aes(y = ..count..), stat =  "count", position = "dodge", width = 0.8)+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        legend.title = element_blank(),
        panel.grid = element_blank())+
  labs(x = "Year of Admission",
       y = "No. Hospital Visit")

pat_encounters %>%
  filter(SUM_IN_HOS < 5000) %>%
  group_by(PATIENT_ID) %>%
  ggplot(aes(x=SUM_IN_HOS))+
  geom_bar(stat = "bin")
 
#the one superuser's total days in hospital accounts for 36% of all days in hospital

