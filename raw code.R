# Google Data Analytics bellabeat Case Study R Code
# Aseem Mehrotra


# Install & Load Packages
install.packages("tidyverse")
install.packages("readxl")
install.packages("reshape2")
install.packages("scales")
install.packages("sqldf")
install.packages("janitor")
install.packages("lubridate")
library(tidyverse)
library(readxl)
library(reshape2)
library(scales)
library(sqldf)
library(janitor)
library(lubridate)


# Read Distinct rows in csv Files and Store in Data Frames
dailyAct <- unique(read.csv("dailyActivity_merged.csv"))
dailyCal <- unique(read.csv("dailyCalories_merged.csv"))
dailyInt <- unique(read.csv("dailyIntensities_merged.csv"))
dailyStep <- unique(read.csv("dailySteps_merged.csv"))
heartRate <- unique(read.csv("heartrate_seconds_merged.csv"))
slpDay <- unique(read.csv("sleepDay_merged.csv"))
wtInfo <- unique(read.csv("weightLogInfo_merged.csv"))

# Sneak into the Daily Data
colnames(dailyAct)
head(dailyAct)
colnames(dailyCal)
head(dailyCal)
colnames(dailyInt)
head(dailyInt)
colnames(dailyStep)
head(dailyStep)
# Column names of dailyCal and dailyInt already exists in dailyAct

# Sneak into the Heart Rate
colnames(heartRate)
head(heartRate)

# Sneak into the Weight Info
colnames(wtInfo)
head(wtInfo)

# Sneak into the Sleep Records
colnames(slpDay)
head(slpDay)

# Upon looking into the Data, it is evident that the data can be matched to one another on the basis of "Id" attribute

# Clean Data (Unique Values are ensured during Read phase of Data)
# Null Values in each Column of Data Frame
lapply(dailyAct, function(x) { length(which(is.na(x)))})
lapply(dailyCal, function(x) { length(which(is.na(x)))})
lapply(dailyInt, function(x) { length(which(is.na(x)))})
lapply(dailyStep, function(x) { length(which(is.na(x)))})
lapply(heartRate, function(x) { length(which(is.na(x)))})
lapply(slpDay, function(x) { length(which(is.na(x)))})
lapply(wtInfo, function(x) { length(which(is.na(x)))})
# Weight Log Info database wtInfo has 65 missing or NULL values in "Fat" column.

# Process Data
dailyActCal <- dailyAct %>% 
  select(Id, ActivityDate, Calories)
check1a <- nrow(sqldf("SELECT * FROM dailyActCal INTERSECT SELECT * FROM dailyCal"))
check1b <- nrow(dailyAct)
check1c <- nrow(dailyCal)
check1a
check1b
check1c
# As dailyAct, dailyCal & dailyActCal has similar number of observations: 940, thus dailyAct can be used for Analysis

dailyActInt <- dailyAct %>% 
  select(Id, ActivityDate, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes, SedentaryActiveDistance, LightActiveDistance, ModeratelyActiveDistance, VeryActiveDistance)
check2a <- nrow(sqldf("SELECT * FROM dailyActInt INTERSECT SELECT * FROM dailyInt"))
check2b <- nrow(dailyAct)
check2c <- nrow(dailyInt)
check2a
check2b
check2c
# As dailyAct, dailyInt & dailyActInt has similar number of observations: 940, thus dailyAct can be used for Analysis
# Only dailyAct dataframe can be used in Analysis and it will cover all 3 dataframes dailyAct, dailyCal & dailyInt

# Left Join dailyAct with heartRate data frame
dailyHR <- sqldf("SELECT Id, Value AS HeartRate, TRIM(SUBSTR(Time,1,9)) AS Date FROM heartRate")
dailyActHR <- sqldf("SELECT A.*, B.HeartRate FROM dailyAct AS A LEFT JOIN dailyHR AS B ON A.Id = B.Id AND A.ActivityDate = B.Date")

dailySlp <- sqldf("SELECT Id, TRIM(SUBSTR(SleepDay,1,9)) AS Date, TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed FROM slpDay")
dailyActHRSlp <- sqldf("SELECT A.*, B.TotalSleepRecords, B.TotalMinutesAsleep, B.TotalTimeInBed FROM dailyActHR AS A LEFT JOIN dailySlp AS B ON A.Id = B.Id AND A.ActivityDate = B.Date")

wtLog <- sqldf("SELECT Id, TRIM(SUBSTR(Date,1,9)) AS Date, WeightKg, BMI FROM wtInfo")
dailyActHRSlpWt <- sqldf("SELECT A.*, B.WeightKg, B.BMI FROM dailyActHRSlp AS A LEFT JOIN wtLog AS B ON A.Id = B.Id AND A.ActivityDate = B.date")

# dailyActHRSlpWt is the master database for all daily attributes along with Heart Rate, Sleep and Weight. This data frame will be used for Analysis

# Clean column names in Master Dataframe
dailyActHRSlpWt <- dailyActHRSlpWt %>% 
  clean_names()

# Sneak into Master Dataframe
names(dailyActHRSlpWt)
head(dailyActHRSlpWt)
summarise(dailyActHRSlpWt)
glimpse(dailyActHRSlpWt)

# Change activity_date data type to date type in Master Dataframe
dailyActHRSlpWt <- dailyActHRSlpWt %>% 
  mutate(activity_date = mdy(activity_date))
# Change ID from numeric to Character for making it a Category
dailyActHRSlpWt <- dailyActHRSlpWt %>% 
  mutate(id = as.character(id))

# Sneak into Master Dataframe
names(dailyActHRSlpWt)
head(dailyActHRSlpWt)
summarise(dailyActHRSlpWt)
glimpse(dailyActHRSlpWt)
summary(dailyActHRSlpWt)

# Analysis

# Calories Burned vs Total Steps by ID: Scatter Plot
dailyActHRSlpWt %>% 
  group_by(id) %>% 
  ggplot(mapping = aes(x=total_steps, y=calories))+
  geom_point(mapping = aes(color = id))+
  labs(x="Total Steps", y= "Calories Burned", title = "Calories vs Steps", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", color = "ID")+
  annotate("rect", xmin = 20000, xmax = 32000, ymin = 0, ymax = 1000, alpha = 0.2, fill = "blue")+
  annotate("text", x= 21000, y= 800, label = "Higher the Steps Count,  ", color = "blue", hjust = "left")+
  annotate("text", x= 21000, y= 600, label = "more the ", color = "blue", hjust = "left")+
  annotate("text", x= 21000, y= 400, label = "Calories burned", color = "blue", hjust = "left")+
  theme(legend.box.background = element_rect(color = "Blue", size = 2), legend.text = element_text(face = "bold"))

# Calories vs Steps by User ID: Box Plot
dailyActHRSlpWt %>% 
  select(id, total_steps, calories) %>% 
  group_by(id) %>% 
  ggplot(mapping = aes(x = total_steps, y= calories))+
  geom_boxplot(aes(fill=id))+
  labs(x= "Steps", y= "Calories Burned",title = "Calories vs Steps", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", fill = "ID")

# Higher the Steps, more the Calories Burned


# Calories Burned by User ID: Density Plot
dailyActHRSlpWt %>%
  select(id, total_steps, calories) %>% 
  group_by(id) %>% 
  ggplot(aes(x= calories))+
  geom_density(aes(fill=factor(id), alpha = 0.95))+
  labs(x="CALORIES", y= "DENSITY", title = "Calories Burned Density", subtitle = "By User ID: 95% Confidence", caption = "Data Source: Kaggle Notebook by Mobius", scientific = FALSE, fill = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 0), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))+
  facet_wrap(~id)

# Calories Burned by User ID: Bar Graph
dailyActHRSlpWt %>% 
  group_by(id) %>% 
  select(id, calories) %>% 
  ggplot(mapping = aes(id))+
  geom_bar(mapping = aes(fill = id))+
  labs(x="ID", y= "Calories Burned", title = "Calories vs Steps", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", scientific = FALSE, fill = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 2), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))


# Total Steps by User ID: Density Plot
dailyActHRSlpWt %>%
  select(id, total_steps) %>% 
  ggplot(aes(x= total_steps))+
  scale_y_continuous(labels= scales::comma)+
  geom_density(aes(fill=factor(id), alpha = 0.95))+
  labs(x="STEPS", y= "DENSITY", title = "Total Steps Density", subtitle = "By User ID: 95% Confidence", caption = "Data Source: Kaggle Notebook by Mobius", scientific = FALSE, fill = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 0), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))+
  facet_wrap(~id)

# Total Steps by User ID: Bar Graph
dailyActHRSlpWt %>%
  select(id, total_steps) %>% 
  ggplot(aes(x= id))+
  scale_y_continuous(labels= scales::comma)+
  geom_bar(aes(fill=factor(id)))+
  labs(x="STEPS", y= "ID", title = "Total Steps", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", scientific = FALSE, fill = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 0), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

# Weight Trend By User ID
dailyActHRSlpWt %>% 
  select(id, activity_date, weight_kg) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(activity_date, weight_kg))+
  geom_line(aes(color = id))+
  labs(x="Date", y= "Weight", title = "Weight Trend", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", color = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 0), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))
# Only 8 users out of 33 Log Weight Info

# Sleep Trend By User ID
dailyActHRSlpWt %>% 
  select(id, activity_date, total_minutes_asleep) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(activity_date, total_minutes_asleep))+
  geom_point(aes(color = id))+
  labs(x="Date", y= "Sleep", title = "Sleep Trend", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", color = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 0), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))+
  facet_wrap(~id)
# 24 users out of 33 used Fitbit for Sleep Log
# However only 12 users are regularly using the fitbit for Sleep Log

# Heart Rate Trend By User ID
dailyActHRSlpWt %>% 
  select(id, activity_date, heart_rate) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(activity_date, heart_rate))+
  geom_point(aes(color = id))+
  labs(x="Date", y= "Heart Rate", title = "Heart Rate Trend", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", color = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 0), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))+
  facet_wrap(~id)
# 14 users out of 33 users used Fitbit for Heart Rate Tracking

# Use of Fitbit for Sleep & Heart Rate tracking is Fairly high as compared to Weight Tracking

# Time to get sleep
dailyActHRSlpWt %>% 
  select(id, activity_date, total_minutes_asleep, total_time_in_bed) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(activity_date, total_time_in_bed-total_minutes_asleep))+
  geom_point(aes(color = id))+
  labs(x="Time in Bed", y= "Minutes taken to Sleep", title = "Sleep Analysis", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", color = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 0), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))+
  facet_wrap(~id)
# Maximum users took less than 50 minutes on average to get asleep while on Bed


# Sedentary Minutes by User ID: Violin Plot
dailyActHRSlpWt %>% 
  select(id, sedentary_minutes) %>% 
  ggplot(aes(x= id, y= sedentary_minutes))+
  geom_violin(aes(fill =id))+
  labs(x="ID", y= "Minutes", title = "Sedantary Minutes", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", scientific = FALSE, fill = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 2), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

# Lightly Active Minutes by User ID: Violin Plot
dailyActHRSlpWt %>% 
  select(id, lightly_active_minutes) %>% 
  ggplot(aes(x= id, y= lightly_active_minutes))+
  geom_violin(aes(fill =id))+
  labs(x="ID", y= "Minutes", title = "Lightly Active Minutes", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", scientific = FALSE, fill = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 2), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

# Fairly Active Minutes by User ID: Violin Plot
dailyActHRSlpWt %>% 
  select(id, fairly_active_minutes) %>% 
  ggplot(aes(x= id, y= fairly_active_minutes))+
  geom_violin(aes(fill =id))+
  labs(x="ID", y= "Minutes", title = "Fairly Active Minutes", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", scientific = FALSE, fill = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 2), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))

# Very Active Minutes by User ID: Violin Plot
dailyActHRSlpWt %>% 
  select(id, very_active_minutes) %>% 
  ggplot(aes(x= id, y= very_active_minutes))+
  geom_violin(aes(fill =id))+
  labs(x="ID", y= "Minutes", title = "Very Active Minutes", subtitle = "By User ID", caption = "Data Source: Kaggle Notebook by Mobius", scientific = FALSE, fill = "ID")+
  theme(legend.box.background = element_rect(color = "Blue", size = 2), legend.text = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))
