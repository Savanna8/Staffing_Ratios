# set up R work space

options(stringsAsFactors = FALSE, scripen = 999)

# set working directory

# call packages

library(tidyverse) 
library(dplyr)
library(DescTools)
library(psych)
library(fastDummies)

#Shortage timeline
#Goal: Make csv that has Ohio's and the US' percentage of reported aid shortages
#for every single week. 5/24/20 to 8/1/21

#Import newest version of data

"New"=read.csv("COVID_19_Nursing_Home_Data_2021_08_15.csv")

#Separate for only NH that submitted data
DataNew = New[New$submitted_data == "Y",]

#Remove unnecessary columns, focusing on staffing shortages
Newsmall = DataNew[-7:-65]
Newsmall = Newsmall[-10:-220]

#Separate files by year by separating out the month, day, year

Newsmall = transform(Newsmall,Date = as.character(week_ending))
Newsplit = strsplit(Newsmall$week_ending, split = "-")
Newsmall = transform(Newsmall, 
                     Date1=sapply(Newsplit,"[[",1), 
                     Date2=sapply(Newsplit, "[[",2),
                     Date3=sapply(Newsplit, "[[",3))

#Remove Aug weeks

totalAug=Newsmall[!(Newsmall$week_ending == "2021-08-15 00:00:00" |
                      Newsmall$week_ending == "2021-08-08 00:00:00"),]

#Do dummy columns so we can make sure that we only include those who gave a Y/N
#Exclude those who left that column blank

dumtotalAug = dummy_cols(totalAug, select_columns=c("shortage_of_aides"))

#Make a file with just Ohio

OhioAug=dumtotalAug[dumtotalAug$provider_state == "OH",]

#Group by week

Ohiotime = OhioAug %>% group_by(week_ending) %>%
  summarise(OH_count_shortages=sum(shortage_of_aides_Y),
            OH_percentage_shortages=OH_count_shortages/
              sum(shortage_of_aides_Y,shortage_of_aides_N)*100)

#Now US: dumtotalAug is all of the US so jump into summarizing

UStime = dumtotalAug %>% group_by(week_ending) %>%
  summarise(US_count_shortages=sum(shortage_of_aides_Y),
            US_percentage_shortages=US_count_shortages/sum(shortage_of_aides_Y,shortage_of_aides_N)*100)

#Now join them

Timeline = Ohiotime %>% inner_join(UStime, by = "week_ending")

#Export CSV
write.csv(Timeline, "Shortage_Timeline.csv", fileEncoding = "UTF-8")


