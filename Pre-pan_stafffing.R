options(stringsAsFactors = FALSE, scripen = 999)

#Set working directory
getwd
list.files()

save.image("PBJstaffing.RData")
load("PBJstaffing.RData")

#Install packages if needed
install.packages("tidyverse")
install.packages("dplyr")
install.packages("gplotz")
install.packages("knitr")
install.packages("psych")
install.packages("lme4")
install.packages("boot")
install.packages("ggeffects")
install.packages("DescTools")
install.packages("arsenal")

#load
library(tidyverse)
library(dplyr)
library(gplotz)
library(knitr)
library(psych)
library(lme4)
library(boot)
library(ggeffects)
library(DescTools)
library(arsenal)

####2017
##Combine quarterly data into yearly data files by Ohio
#Import Data
Q117=read.csv("2017Q1.csv")
Q217=read.csv("2017Q2.csv")
Q317=read.csv("2017Q3.csv")
Q417=read.csv("2017Q4.csv")

#Change column names into the same names
Q117=Q117 %>% rename(Hrs_RNDON = hrs_RNDON)
Q417=Q417 %>% rename(Hrs_RN_ctr = hrs_RN_ctr)

#Combine into 1 year
identical(Q117,Q217)
identical(Q317,Q417)
identical(Q117,Q317)
comparedf(Q317,Q417)
F2017=rbind(Q117,Q217)
L2017=rbind(Q317,Q417)

#One more thing to rename
comparedf(F2017,L2017)  
L2017=L2017%>% rename(hrs_RNadmin = hrs_RNAdmin)

#Final bind for 2017
T2017=rbind(F2017,L2017)

#Filter by Ohio
Ohio2017=T2017[T2017$STATE == "OH",]

write.csv(Ohio2017,"Ohio2017.csv",row.names = T,
          fileEncoding = "UTF-8")

##Add columns for hours per resident day (HPRD); methodology from KHN
#Nursing hours: total hours for RNs, LPNs
#excluding hours for RN Director of Nursing (RNDON), RN admin and LPN admin
#Divide by daily resident census (MDScenus)

is.character(Ohio2017$hrs_LPN)
is.character(Ohio2017$hrs_RN)

#Make hrs_LPN numeric

Ohio2017$hrs_LPN = as.numeric(Ohio2017$hrs_LPN)
is.character(Ohio2017$hrs_LPN)

#Add column for HPRD nurses
Ohio2017$Nurse_HPRD=NA
x=Ohio2017$hrs_RN+ Ohio2017$hrs_LPN
Ohio2017$Nurse_HPRD=x/Ohio2017$MDScensus


#Aide hours: total hours for CNAs,medication aides 
#and nurse aides in training (NAtrn)

is.character(Ohio2017$hrs_CNA)
is.character(Ohio2017$Hrs_CNA_emp)

#Remove the rows with "NA" for hrs_CMA becuase we
#don't know what they mean, so not fair to include. Only X rows with
#NAs in this dataset. There are 54 NAs (0.016%)

Ohio2017CNA = Ohio2017[complete.cases(Ohio2017$hrs_CNA),]

#Check to if there any NAs still
is.character(Ohio2017CNA$hrs_CNA)
is.character(Ohio2017CNA$hrs_MedAide)
is.character(Ohio2017CNA$hrs_NA_trn)


#Add column for HPRD 
Ohio2017CNA$Aide_HPRD = NA
y=Ohio2017CNA$hrs_CNA + Ohio2017CNA$hrs_MedAide + Ohio2017CNA$hrs_NA_trn
Ohio2017CNA$Aide_HPRD=y/Ohio2017CNA$MDScensus

##Nurse ratios for 2017
#Best nurse staffing days
#Create a list of the 10 best ratios for each facility
nursebest2017 = Ohio2017%>%select(PROVNAME, Nurse_HPRD)%>%
  group_by(PROVNAME)%>%arrange(desc(Nurse_HPRD))%>%slice(1:10)

# find average ratio for each facility and put into a data frame
NB2017ratio = nursebest2017 %>% group_by(PROVNAME)%>%
  summarise(avg_ratio_best=mean(Nurse_HPRD))

#Divide by 24 to find the ratio of staff per resident
NB2017ratioFINAL = NB2017ratio%>% mutate(best_ratio=24/avg_ratio_best)

#Worst nurse staffing days
nurseworst2017 = Ohio2017 %>% select(PROVNAME, Nurse_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(Nurse_HPRD) %>%  slice(1:10)

#Find average ratio for each facility and put into a data frame
NW2017ratio=nurseworst2017 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_worst=mean(Nurse_HPRD))

#Divide by 24 to find the ratio of staffing per resident
NW2017ratioFINAL = NW2017ratio %>% mutate(worst_ratio=24/avg_ratio_worst)

#Join and export CSV for best and worst for aides in 2017
Nurses2017= NB2017ratioFINAL %>% 
  left_join(NW2017ratioFINAL, by = "PROVNAME")
write.csv(Nurses2017,"nurse_staffing_2017.csv",row.names = T,
          fileEncoding = "UTF-8")

##Aide ratios in 2017
#Best aide staffing days in 2017
aidebest2017 = Ohio2017CNA %>% select(PROVNAME, Aide_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(desc(Aide_HPRD)) %>% slice(1:10)

#Find average ratio for each facility and put into a data frame
AB2017ratio=aidebest2017 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_best=mean(Aide_HPRD))

#Divide by 24 to find the ratio of staffing per resident
AB2017ratioFINAL = AB2017ratio %>% mutate(best_ratio=24/avg_ratio_best)

#Worst aide staffing days in 2017
aideworst2017 = Ohio2017CNA %>% select(PROVNAME, Aide_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(Aide_HPRD) %>%  slice(1:10)

#Find average ratio for each facility and put into a data frame
AW2017ratio=aideworst2017 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_worst=mean(Aide_HPRD))

#Divide by 24 to find the ratio of staffing per resident
AW2017ratioFINAL = AW2017ratio %>% mutate(worst_ratio=24/avg_ratio_worst)

#Join and export CSV for best and worst for aides in 2017
Aides2017= AB2017ratioFINAL %>% 
  left_join(AW2017ratioFINAL, by = "PROVNAME")
write.csv(Aides2017,"aide_staffing_2017.csv",row.names = T,
          fileEncoding = "UtF-8")




###2018
#Import Data
Q118=read.csv("PBJ_Daily_Nurse_Staffing_CY_2018_Q1.csv")
Q218=read.csv("PBJ_Daily_Nurse_Staffing_CY_2018Q2.csv")
Q318=read.csv("PBJ_Daily_Nurse_Staffing_CY_2018Q3.csv")
Q418=read.csv("PBJ_Daily_Nurse_Staffing_CY_2018Q4.csv")

#Change column names into the same names because they hate me
Q118=Q118 %>% rename(Hrs_RNDON = hrs_RNDON)
Q418=Q418 %>% rename(Hrs_RN_ctr = hrs_RN_ctr)

#Combine into 1 year
F2018=rbind(Q118,Q218)

F2018$Location.1 = NULL

L2018=rbind(Q318,Q418)

#Final bind for 2018
comparedf(F2018,L2018)

F2018=F2018 %>% rename(Hrs_RNDON = hrs_RNDON)
F2018=F2018 %>% rename(Hrs_RNadmin = hrs_RNadmin)
F2018=F2018 %>% rename(Hrs_RN = hrs_RN)
F2018=F2018 %>% rename(hrs_LPNadmin = Hrs_LPN_admin)
F2018=F2018 %>% rename(Hrs_NA_trn = hrs_NA_trn) %>% 
  rename(Hrs_MedAide = hrs_MedAide)%>%
  rename(Hrs_LPN = hrs_LPN)
F2018=F2018 %>% rename(Hrs_CNA = hrs_CNA)
F2018=F2018 %>% rename(Hrs_NAtrn = Hrs_NA_trn)


T2018=rbind(F2018,L2018)

#Filter by Ohio
Ohio2018=T2018[T2018$STATE == "OH",]
write.csv(Ohio2018, "Ohio2018.csv",row.names = T,
          fileEncoding = "UtF-8")

##Add columns for hours per resident day (HPRD); methodology from KHN
#Nursing hours: total hours for RNs, LPNs
#excluding hours for RN Director of Nursing (RNDON), RN admin and LPN admin
#Divide by daily resident census (MDScenus)

is.character(Ohio2018$Hrs_LPN)
is.character(Ohio2018$Hrs_RN)

#Make Hrs_LPN numeric

Ohio2018$Hrs_LPN = as.numeric(Ohio2018$Hrs_LPN)
is.character(Ohio2018$hrs_LPN)

#Add column for HPRD nurses
Ohio2018$Nurse_HPRD=NA
z=Ohio2018$Hrs_RN+ Ohio2018$Hrs_LPN
Ohio2018$Nurse_HPRD=z/Ohio2018$MDScensus

#Aide hours: total hours for CNAs,medication aides 
#and nurse aides in training (NAtrn)

is.character(Ohio2018$Hrs_CNA)

Ohio2018$Hrs_CNA = as.numeric(Ohio2018$Hrs_CNA)

#Remove the rows with "NA" for hrs_CMA becuase we
#don't know what they mean, so not fair to include. Only X rows with
#NAs in this dataset. There are 44 NAs (0.013%)

Ohio2018CNA = Ohio2018[complete.cases(Ohio2018$Hrs_CNA),]

#Check to if there any NAs still
is.character(Ohio2018CNA$hrs_CNA)
is.character(Ohio2018CNA$hrs_MedAide)
is.character(Ohio2018CNA$hrs_NA_trn)


#Add column for HPRD Aides
Ohio2018CNA$Aide_HPRD = NA
w=Ohio2018CNA$Hrs_CNA + Ohio2018CNA$Hrs_MedAide + Ohio2018CNA$Hrs_NAtrn
Ohio2018CNA$Aide_HPRD=w/Ohio2018CNA$MDScensus

##Nurse ratios for 2018
#Best nurse staffing days
#Create a list of the 10 best ratios for each facility
nursebest2018 = Ohio2018 %>% select(PROVNAME, Nurse_HPRD) %>% 
  group_by(PROVNAME)%>% arrange(desc(Nurse_HPRD))%>%  slice(1:10)

# find average ratio for each facility and put into a data frame
NB2018ratio = nursebest2018 %>% group_by(PROVNAME)%>%
  summarise(avg_ratio_best=mean(Nurse_HPRD))

#Divide by 24 to find the ratio of staff per resident
NB2018ratioFINAL = NB2018ratio%>% mutate(ratio_best=24/avg_ratio_best)

#Worst nurse staffing days
nurseworst2018 = Ohio2018 %>% select(PROVNAME, Nurse_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(Nurse_HPRD) %>%  slice(1:10)

#Find average ratio for each facility and put into a data frame
NW2018ratio=nurseworst2018 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_worst=mean(Nurse_HPRD))

#Divide by 24 to find the ratio of staffing per resident
NW2018ratioFINAL = NW2018ratio %>% mutate(ratio_worst=24/avg_ratio_worst)

Nurse2018= NB2018ratioFINAL %>% 
  left_join(NW2018ratioFINAL, by = "PROVNAME")
write.csv(Nurse2018, "nurse_staffing_2018.csv",row.names = T,
          fileEncoding = "UtF-8")

##Aide ratios in 2018
#Best aide staffing days in 2018
aidebest2018 = Ohio2018CNA %>% select(PROVNAME, Aide_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(desc(Aide_HPRD)) %>% slice(1:10)

#Find average ratio for each facility and put into a data frame
AB2018ratio=aidebest2018 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_best=mean(Aide_HPRD))

#Divide by 24 to find the ratio of staffing per resident
AB2018ratioFINAL = AB2018ratio %>% mutate(ratio_best=24/avg_ratio_best)

#Worst aide staffing days in 2017
aideworst2018 = Ohio2018CNA %>% select(PROVNAME, Aide_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(Aide_HPRD) %>% slice(1:10)

#Find average ratio for each facility and put into a data frame
AW2018ratio=aideworst2018 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_worst=mean(Aide_HPRD))

#Divide by 24 to find the ratio of staffing per resident
AW2018ratioFINAL = AW2018ratio %>% mutate(ratio_worst=24/avg_ratio_worst)

Aides2018= AB2018ratioFINAL %>% 
  left_join(AW2018ratioFINAL, by = "PROVNAME")
write.csv(Aides2018, "aides_staffing_2018.csv",row.names = T,
          fileEncoding = "UtF-8")



###2019
##Combine quarterly data into yearly data files by Ohio

#Import Data
Q119=read.csv("PBJ_Daily_Nurse_Staffing_CY_2019Q1.csv")
Q219=read.csv("PBJ_Daily_Nurse_Staffing_CY_2019_Q2.csv")
Q319=read.csv("PBJ_Daily_Nurse_Staffing_CY_2019_Q3.csv")
Q419=read.csv("PBJ_Daily_Nurse_Staffing_CY_2019Q4.csv")

#Combine into 1 year
F2019=rbind(Q119,Q219)
L2019=rbind(Q319,Q419)
T2019=rbind(F2019,L2019)

#Filter by Ohio
Ohio2019=T2019[T2019$STATE == "OH",]

write.csv(Ohio2019, "Ohio2019.csv",row.names = T,
          fileEncoding = "UtF-8")


##Add columns for hours per resident day (HPRD); methodology from KHN
#Nursing hours: total hours for RNs, LPNs
#excluding hours for RN Director of Nursing (RNDON), RN admin and LPN admin
#Divide by daily resident census (MDScenus)

is.character(Ohio2019$Hrs_LPN)
is.character(Ohio2019$Hrs_RN)

#Make hrs_LPN numeric

Ohio2019$Hrs_LPN = as.numeric(Ohio2019$Hrs_LPN)
is.character(Ohio2017$hrs_LPN)

#Add column for HPRD nurses
Ohio2019$Nurse_HPRD=NA
s=Ohio2019$Hrs_RN+Ohio2019$Hrs_LPN
Ohio2019$Nurse_HPRD=s/Ohio2019$MDScensus

#Aide hours: total hours for CNAs,medication aides 
#and nurse aides in training (NAtrn)

is.character(Ohio2019$Hrs_CNA)
is.character(Ohio2017CNA$Hrs_MedAide)
is.character(Ohio2017CNA$Hrs_NA_trn)

Ohio2019$Hrs_CNA = as.numeric(Ohio2019$Hrs_CNA)

#No NAs for hrs_CMA, so add column for HPRD 
Ohio2019$Aide_HPRD = NA
t=Ohio2019$Hrs_CNA + Ohio2019$Hrs_MedAide + Ohio2019$Hrs_NAtrn
Ohio2019$Aide_HPRD=t/Ohio2019$MDScensus

##Nurse ratios for 2019
#Best nurse staffing days
#Create a list of the 10 best ratios for each facility
nursebest2019 = Ohio2019 %>% select(PROVNAME, Nurse_HPRD) %>% 
  group_by(PROVNAME)%>% arrange(desc(Nurse_HPRD))%>%  slice(1:10)

# find average ratio for each facility and put into a data frame
NB2019ratio = nursebest2019 %>% group_by(PROVNAME)%>%
  summarise(avg_ratio_best=mean(Nurse_HPRD))

#Divide by 24 to find the ratio of staff per resident
NB2019ratioFINAL = NB2019ratio%>% mutate(ratio_best=24/avg_ratio_best)

#Worst nurse staffing days
nurseworst2019 = Ohio2019 %>% select(PROVNAME, Nurse_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(Nurse_HPRD) %>%  slice(1:10)

#Find average ratio for each facility and put into a data frame
NW2019ratio=nurseworst2019 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_worst=mean(Nurse_HPRD))

#Divide by 24 to find the ratio of staffing per resident
NW2019ratioFINAL = NW2019ratio %>% mutate(ratio_worst=24/avg_ratio_worst)

Nurse2019= NB2019ratioFINAL %>% 
  left_join(NW2019ratioFINAL, by = "PROVNAME")
write.csv(Nurse2019, "nurse_staffing_2019.csv",row.names = T,
          fileEncoding = "UtF-8")

##Aide ratios in 2019
#Best aide staffing days in 2018
aidebest2019 = Ohio2019 %>% select(PROVNAME, Aide_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(desc(Aide_HPRD)) %>% slice(1:10)

#Find average ratio for each facility and put into a data frame
AB2019ratio=aidebest2019 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_best=mean(Aide_HPRD))

#Divide by 24 to find the ratio of staffing per resident
AB2019ratioFINAL = AB2019ratio %>% mutate(ratio_best=24/avg_ratio_best)

#Worst aide staffing days in 2019
aideworst2019 = Ohio2019 %>% select(PROVNAME, Aide_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(Aide_HPRD) %>% slice(1:10)

#Find average ratio for each facility and put into a data frame
AW2019ratio=aideworst2019 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_worst=mean(Aide_HPRD))

#Divide by 24 to find the ratio of staffing per resident
AW2019ratioFINAL = AW2019ratio %>% mutate(ratio_worst=24/avg_ratio_worst)

Aides2019= AB2019ratioFINAL %>% 
  left_join(AW2019ratioFINAL, by = "PROVNAME")
write.csv(Aides2019, "aides_staffing_2019.csv",row.names = T,
          fileEncoding = "UtF-8")

###2020
##Combine quarterly data into yearly data files by Ohio

#Import Data
Q120=read.csv("PBJ_Daily_Nurse_Staffing_CY_2020Q1.csv")
Q220=read.csv("PBJ_Daily_Nurse_Staffing_CY_2020Q2.csv")
Q320=read.csv("PBJ_Daily_Nurse_Staffing_CY_2020Q3.csv")
Q420=read.csv("PBJ_Daily_Nurse_Staffing_CY_2020Q4.csv")

#Combine into 1 year
F2020=rbind(Q120,Q220)
L2020=rbind(Q320,Q420)
T2020=rbind(F2020,L2020)

#Filter by Ohio
Ohio2020=T2020[T2020$STATE == "OH",]

write.csv(Ohio2020, "Ohio2020.csv",row.names = T,
          fileEncoding = "UtF-8")

Ohio2020= read.csv("Ohio2020.csv")

##Add columns for hours per resident day (HPRD); methodology from KHN
#Nursing hours: total hours for RNs, LPNs
#excluding hours for RN Director of Nursing (RNDON), RN admin and LPN admin
#Divide by daily resident census (MDScenus)

is.character(Ohio2020$Hrs_LPN)
is.character(Ohio2020$Hrs_RN)

#Make hrs_LPN numeric

Ohio2019$Hrs_LPN = as.numeric(Ohio2019$Hrs_LPN)
is.character(Ohio2017$hrs_LPN)

#Add column for HPRD nurses
Ohio2020$Nurse_HPRD=NA
b=Ohio2020$Hrs_RN+Ohio2020$Hrs_LPN
Ohio2020$Nurse_HPRD=b/Ohio2020$MDScensus

#Aide hours: total hours for CNAs,medication aides 
#and nurse aides in training (NAtrn)

is.character(Ohio2020$Hrs_CNA)
is.character(Ohio2020$Hrs_MedAide)
is.character(Ohio2020$Hrs_NA_trn)

Ohio2020$Hrs_CNA = as.numeric(Ohio2020$Hrs_CNA)

#No NAs for hrs_CNA, so add column for HPRD 
Ohio2020$Aide_HPRD = NA
d=Ohio2020$Hrs_CNA + Ohio2020$Hrs_MedAide + Ohio2020$Hrs_NAtrn
Ohio2020$Aide_HPRD=d/Ohio2020$MDScensus

##Nurse ratios for 2020
#Best nurse staffing days
#Create a list of the 10 best ratios for each facility
nursebest2020 = Ohio2020 %>% select(PROVNAME, Nurse_HPRD) %>% 
  group_by(PROVNAME)%>% arrange(desc(Nurse_HPRD))%>%  slice(1:10)

# find average ratio for each facility and put into a data frame
NB2020ratio = nursebest2020 %>% group_by(PROVNAME)%>%
  summarise(avg_ratio_best=mean(Nurse_HPRD))

#Divide by 24 to find the ratio of staff per resident
NB2020ratioFINAL = NB2020ratio%>% mutate(ratio_best=24/avg_ratio_best)

#Worst nurse staffing days
nurseworst2020 = Ohio2020 %>% select(PROVNAME, Nurse_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(Nurse_HPRD) %>%  slice(1:10)

#Find average ratio for each facility and put into a data frame
NW2020ratio=nurseworst2020 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_worst=mean(Nurse_HPRD))

#Divide by 24 to find the ratio of staffing per resident
NW2020ratioFINAL = NW2020ratio %>% mutate(ratio_worst=24/avg_ratio_worst)

Nurse2020= NB2020ratioFINAL %>% 
  left_join(NW2020ratioFINAL, by = "PROVNAME")
write.csv(Nurse2020, "nurse_staffing_2020.csv",row.names = T,
          fileEncoding = "UtF-8")

##Aide ratios in 2020
#Best aide staffing days in 2020
aidebest2020 = Ohio2020 %>% select(PROVNAME, Aide_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(desc(Aide_HPRD)) %>% slice(1:10)

#Find average ratio for each facility and put into a data frame
AB2020ratio=aidebest2020 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_best=mean(Aide_HPRD))

#Divide by 24 to find the ratio of staffing per resident
AB2020ratioFINAL = AB2020ratio %>% mutate(ratio_best=24/avg_ratio_best)

#Worst aide staffing days in 2019
aideworst2020 = Ohio2020 %>% select(PROVNAME, Aide_HPRD) %>% 
  group_by(PROVNAME) %>% arrange(Aide_HPRD) %>% slice(1:10)

#Find average ratio for each facility and put into a data frame
AW2020ratio=aideworst2020 %>% group_by(PROVNAME) %>%
  summarise(avg_ratio_worst=mean(Aide_HPRD))

#Divide by 24 to find the ratio of staffing per resident
AW2020ratioFINAL = AW2020ratio %>% mutate(ratio_worst=24/avg_ratio_worst)

Aides2020= AB2020ratioFINAL %>% 
  left_join(AW2020ratioFINAL, by = "PROVNAME")
write.csv(Aides2020, "aides_staffing_2020.csv",row.names = T,
          fileEncoding = "UtF-8")

#Next I want to compare facilities' best and worst staff ratios
#And see how much they changed. Goal is to be able to say "x% of
#Ohio nursing homes saw an increase in their staff to patient ratio
#from 2017 to 2020" since we can't compare ratios between facilities. Not
#fair to compare numbers between nursing homes because homes have different
#populations, necessary ratios, etc. (explanation in KHN methodology)

#an increase means there were more residents per staff (worse)
#a decrease means there were less residents per staff (better)

#Difference between 2017 and 2020
Aides2017=read.csv("aide_staffing_2017.csv")

Aideschange = Aides2017 %>%  
  inner_join(Aides2020, by = "PROVNAME")

Aideschange= Aideschange %>% rename(Best_ratio_17 =best_ratio) %>% 
  rename(Worst_ratio_17=worst_ratio)%>%
  rename(Best_ratio_20 =ratio_best) %>%
  rename(Worst_ratio_20 =ratio_worst)

#Add column for percent change for best staff ratio days
Aideschange$Best_per_change=NA
Aideschange$Best_per_change=Aideschange$Best_ratio_20-Aideschange$Best_ratio_17
Aideschange$Best_per_change=Aideschange$Best_per_change/Aideschange$Best_ratio_17*100

#Do the same for worst
Aideschange$Worst_per_change=NA
Aideschange$Worst_per_change=Aideschange$Worst_ratio_20-Aideschange$Worst_ratio_17
Aideschange$Worst_per_change=Aideschange$Worst_per_change/Aideschange$Worst_ratio_17*100

#How many nursing homes' best staffing ratios increased from 2017 to 2020?
length(Aideschange$Best_per_change[Aideschange$Best_per_change>"0"])
283/764*100
#37% of nursing homes saw the number of residents per staff on their 
#best staffing days increase from 2017 to 2020
length(Aideschange$Best_per_change[Aideschange$Best_per_change<"0"])
481/764*100
#63% of nursing homes saw the number of residents per staff on their 
#best staffing days decrease from 2017 to 2020

#How many nursing homes' worst staffing ratios increased from 2017 to 2020?
length(Aideschange$Worst_per_change[Aideschange$Worst_per_change>"0"])
459/764*100
#60% of nursing homes saw the number of residents per staff on their 
#worst staffing days increase from 2017 to 2020

length(Aideschange$Worst_per_change[Aideschange$Worst_per_change<"0"])
305/764*100
#40% of nursing homes saw the number of residents per staff on their 
#worst staffing days decrease from 2017 to 2020
