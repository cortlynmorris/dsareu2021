library(reshape2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(dslabs)
library(ggrepel)
library(ggthemes)
library(maps)
library(scales)

# Mac code for reading in files (Cortlyn)
setwd("/Users/cmorris/Desktop/dsreu2021/rstudiodirectory/ResearchProject")

persons <- read.csv(file="Persons_Involved_in_Crashes.csv")
glimpse(persons)

locations <- read.csv(file="Reported_Crash_Locations.csv")
glimpse(locations)

crashes <- persons %>% left_join(locations, by="key_crash")

# Windows code for reading in files (Kelly)
library(readr)
persons <- read_csv("~/NCAT REU/Mostafa/Data/Persons_Involved_in_Crashes.csv")

locations <- read_csv("~/NCAT REU/Mostafa/Data/Reported_Crash_Locations.csv")

crashes <- persons %>% left_join(locations, by="key_crash")

# Windows code for files (Ayan)
persons <- read_csv("C:/Users/ayang/Desktop/Project FIles REU A&T/REU Research Project1/Persons_Involved_in_Crashes.csv")


locations <- read_csv("C:/Users/ayang/Desktop/Project FIles REU A&T/REU Research Project1/Reported_Crash_Locations.csv")


crashes <- persons %>% left_join(locations, by="key_crash")

# Beginning of data alanysis

crashes %>% 
  filter(VehicleType != "Unknown", VehicleType != "") %>%
  count(VehicleType) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         VehicleType = reorder(VehicleType,logtrans)) %>% 
  ggplot(aes(x=VehicleType, y=logtrans, fill=VehicleType)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  geom_text(aes(label=logtrans),nudge_y=0.5) + 
  labs(title = "Frequency of Crashes by Vehicle Type", 
       x = "Vehicle Type", 
       y = "Count (log10 Scale)") 

crashes %>% 
  filter(WeatherCondition1 != "NA") %>%
  count(WeatherCondition1) %>% 
  mutate(logtrans = round(log10(n), digits = 2),
         WeatherCondition1 = reorder(WeatherCondition1,logtrans)) %>% 
  ggplot(aes(x=WeatherCondition1, y=logtrans, fill=WeatherCondition1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  geom_text(aes(label=logtrans),nudge_y=0.5) +
  labs(title = "Frequency of Crashes by Weather Condition",
       x = "Weather Condition",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(WeatherCondition1 != "NA", WeatherContributedToCrash == "Yes") %>%
  count(WeatherCondition1) %>% 
  mutate(logtrans = round(log10(n), digits = 2),
         WeatherCondition1 = reorder(WeatherCondition1,logtrans)) %>% 
  ggplot(aes(x=WeatherCondition1, y=logtrans, fill=WeatherCondition1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  geom_text(aes(label=logtrans),nudge_y=0.5) +
  labs(title = "Frequency of Crashes by Weather Condition",
       x = "Weather Condition",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(TrafficControlType != "NA", TrafficControlType != "", 
         TrafficControlType != "NaN") %>%
  count(TrafficControlType) %>% 
  mutate(logtrans = round(log10(n), digits = 2),
         TrafficControlType = reorder(TrafficControlType,logtrans)) %>% 
  ggplot(aes(x=TrafficControlType, y=logtrans, fill=TrafficControlType)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  geom_text(aes(label=logtrans),nudge_y=0.5) +
  labs(title = "Frequency of Crashes by Traffic Control Type",
       x = "Traffic Control Type",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(RoadFeature != "NA", RoadFeature != "", RoadFeature != "NaN") %>%
  count(RoadFeature) %>% 
  mutate(logtrans = round(log10(n), digits = 2),
         RoadFeature = reorder(RoadFeature,logtrans)) %>% 
  ggplot(aes(x=RoadFeature, y=logtrans, fill=RoadFeature)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  geom_text(aes(label=logtrans),nudge_y=0.5) +
  labs(title = "Frequency of Crashes by Road Feature",
       x = "Road Feature",
       y = "Count (log10 Scale)")

crashes %>%
  filter(PersonType == "Driver", Age != "NA", Age != "NaN", Age != "") %>%
  ggplot() +
  geom_histogram(aes(x=Age), binwidth = 5, col="red", fill="darkgrey") +
  labs(title="Histogram for Drivers' Ages", x="Age", y="Frequency")

crashes %>% 
  filter(AlcoholResultType != "NA", AlcoholResultType != "", 
         AlcoholResultType != "NaN", AlcoholResultType != "Contaminated sample/unusable") %>%
  count(AlcoholResultType) %>% 
  mutate(logtrans = round(log10(n), digits = 2),
         AlcoholResultType = reorder(AlcoholResultType,logtrans)) %>% 
  ggplot(aes(x=AlcoholResultType, y=logtrans, fill=AlcoholResultType)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  geom_text(aes(label=logtrans),nudge_y=0.5) +
  labs(title = "Frequency of Crashes by Alcohol Result Type",
       x = "Alcohol Result Type",
       y = "Count (log10 Scale)")

crashes %>%
  filter(AlcoholResultType != "NA", AlcoholResultType != "Unknown", 
         Crash_Date_DOW != "NA", Crash_Date_DOW != "NaN", Crash_Date_DOW != "") %>%
  group_by(AlcoholResultType) %>%
  ggplot(aes(fill=AlcoholResultType, x=Crash_Date_DOW)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Alcohol Result Type Frequency by Crash Date DOW",
       x = "Crash Date DOW",
       y = "Percentage")

crashes %>% 
  filter(AlcoholResultType != "NA", AlcoholResultType != "", 
         AlcoholResultType != "NaN", AlcoholResultType != 
           "Contaminated sample/unusable", AlcoholResultType != "Pending",
         AlcoholResultType != "Unknown") %>%
  count(AlcoholResultType) %>% 
  mutate(logtrans = round(log10(n), digits = 2),
         AlcoholResultType = reorder(AlcoholResultType,logtrans)) %>% 
  ggplot(aes(x=AlcoholResultType, y=logtrans, fill=AlcoholResultType)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  geom_text(aes(label=logtrans),nudge_y=0.5) +
  labs(title = "Frequency of Crashes by Alcohol Result Type",
       x = "Alcohol Result Type",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(Crash_Date_DOW != "NA") %>%
  mutate(Crash_Date_DOW = factor(Crash_Date_DOW, levels = c("Sunday","Monday", "Tuesday", "Wednesday", 
                                   "Thursday", "Friday", "Saturday"))) %>%
  ggplot() +
  geom_bar(aes(Crash_Date_DOW, fill=Crash_Date_DOW), show.legend = FALSE) +
  geom_text(stat="count", aes(x=Crash_Date_DOW, label=..count..), vjust=-0.25) +
  labs(title = "Frequency of Crashes by Day of Week",
       x = "Day of Week",
       y = "Count")

crashes %>% 
  filter(Crash_Date_Month != "NA") %>%
  mutate(Crash_Date_Month = factor(Crash_Date_Month, 
                                   levels = c("January","February", "March", 
                                              "April", "May", "June", "July",
                                              "August", "September", "October",
                                              "November", "December"))) %>%
  ggplot() +
  geom_bar(aes(Crash_Date_Month, fill=Crash_Date_Month), show.legend = FALSE) +
  geom_text(stat="count", aes(x=Crash_Date_Month, label=..count..), vjust=-0.25) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Frequency of Crashes by Month",
       x = "Month",
       y = "Count")

crashes %>%
  filter(Crash_Date_Hour != "NA", Crash_Date_Hour != "") %>%
  ggplot() +
  geom_bar(aes(Crash_Date_Hour)) +
  labs(title="Frequency of Crashes by Hour", x="Hour", y="Count")

crashes %>%
  filter(Crash_Date_Hour != "NA", Crash_Date_Hour != "") %>%
  mutate(shift = ifelse(
    Crash_Date_Hour >= 6 & Crash_Date_Hour < 14, "Shift 1", 
    ifelse(Crash_Date_Hour >= 14 & Crash_Date_Hour < 22, "Shift 2", "Shift 3"))) %>%
  ggplot() +
  geom_bar(aes(x=shift, fill = shift)) +
  labs(title="Frequency of Crashes by Shift", x="Shift", y="Count")

crashes %>% 
  filter(PersonType == "Driver" | PersonType == "Passenger", Age != "", Age != "NA") %>% 
  ggplot() +
  geom_histogram(aes(x=Age), binwidth = 5, col="red", fill="darkgrey") +
  labs(x="Age", y="Frequency", title="Drivers' vs. Passengers' Age") +
  theme(legend.position = "top") +
  facet_wrap(~PersonType, dir = "v")

crashes %>% 
  filter(PersonType == "Driver" | PersonType == "Passenger", 
         Gender != "NA", Gender != "Unknown") %>% 
  ggplot() +
  geom_bar(aes(x=Gender), col="red", fill="darkgrey") +
  labs(x="Gender", y="Frequency", title="Drivers' vs. Passengers' Gender") +
  theme(legend.position = "top") +
  facet_wrap(~PersonType)

crashes %>% 
  filter(VehicleSeizure != "NA") %>%
  count(VehicleSeizure) %>% 
  mutate(logtrans = round(log10(n), digits = 2)) %>% 
  ggplot(aes(x=VehicleSeizure, y=logtrans, fill=VehicleSeizure)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  labs(title = "Frequency of Crashes by Vehicle Seizure",
       x = "Vehicle Seizure",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(AirbagDeployed != "NA", AirbagDeployed != "Unknown") %>%
  count(AirbagDeployed) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         AirbagDeployed = reorder(AirbagDeployed,logtrans)) %>% 
  ggplot(aes(x=AirbagDeployed, y=logtrans, fill=AirbagDeployed)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Airbag Deployed",
       x = "Airbag Deployed",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(Ejection != "NA", Ejection != "Unknown") %>%
  count(Ejection) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         Ejection = reorder(Ejection,logtrans)) %>% 
  ggplot(aes(x=Ejection, y=logtrans, fill=Ejection)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Ejection",
       x = "Ejection",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(Gender != "NA", Gender != "Unknown", Gender != "") %>%
  count(Gender) %>% 
  mutate(Percent = round(n/sum(n)*100,2), 
         Gender = reorder(Gender,Percent)) %>% 
  ggplot(aes(x=Gender, y=Percent, fill=Gender)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Percent),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Gender",
       x = "Gender",
       y = "Percent")


crashes %>% 
  filter(Race != "NA", Race != "Unknown", Race != "") %>%
  count(Race) %>% 
  mutate(Percent = round(n/sum(n)*100,2), 
         Race = reorder(Race,Percent)) %>% 
  ggplot(aes(x=Race, y=Percent, fill=Race)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Percent), nudge_y = 0.05) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Race",
       x = "Race",
       y = "Percent")

#old code 
# crashes %>% 
#   filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType == "Driver") %>%
#   count(Injury)
# crashes %>% 
#   filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType == "Passenger") %>%
#   count(Injury)
# crashes %>% 
#   filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType == "Pedestrian") %>%
#   count(Injury)
# crashes %>% 
#   filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType == "Pedalcyclist") %>%
#   count(Injury)
# crashes %>% 
#   filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType == "Other*") %>%
#   count(Injury)
# persontype2 <- c(rep("Driver" , 5) , rep("Passenger" , 5) , rep("Pedestrian" , 5) ,
#                  rep("Pedalcyclist" , 5) , rep("Other" , 5) )
# injury2 <- rep(c("Type A" , "Type B" , "Type C", "Killed", "No Injury") , 5)
# value <- c(851, 7295, 22020, 105, 227811, 232, 2307, 9163, 31, 72373, 183, 596, 554,
#            62, 154, 19, 168, 123, 3, 68, 10, 48, 34, 1, 141)
# data <- data.frame(persontype2,injury2,value)
# 
# data %>%
#   mutate(
#          injury2 = reorder(injury2, -value),
#          persontype2 = factor(persontype2,
#                               levels = c("Driver", "Passenger", "Pedestrian", 
#                                          "Pedalcyclist", "Other"))) %>%
#   ggplot(aes(fill=injury2, y=value, x=persontype2)) +
#   geom_bar(position="dodge", stat="identity") +
#   geom_text(aes(label=value), nudge_x = -0.35) +
#   coord_flip()

#Basic injury frequency plot 
crashes %>% 
  filter(Injury != "NA", Injury != "Unknown", Injury != "") %>%
  count(Injury) %>% 
  mutate(logtrans = round(log10(n), digits = 2)) %>% 
  ggplot(aes(x=Injury, y=logtrans, fill=Injury)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Injury",
       x = "Injury",
       y = "Count (log10 Scale)")

#Injury with other variables in percent stacked (fill) plots 
#new code 
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", ContributingCircumstance1 != "NA", 
         ContributingCircumstance1 != "NaN", 
         ContributingCircumstance1 != "Unable to determine", 
         ContributingCircumstance1 != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=ContributingCircumstance1)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Contributing Circumstance",
       x = "Contributing Circumstance",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", PersonType != "NA", 
        PersonType != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=PersonType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Person Type",
       x = "Person Type",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Ejection != "NA", 
         Ejection != "NaN", Ejection != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=Ejection)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Ejection",
       x = "Ejection",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", RoadFeature != "NA", 
         RoadFeature != "NaN", RoadFeature != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=RoadFeature)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Road Feature",
       x = "Road Feature",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", RoadClassification != "NA", 
         RoadClassification != "NaN", RoadClassification != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=RoadClassification)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Road Classification",
       x = "Road Classification",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", WeatherCondition1 != "NA", 
         WeatherCondition1 != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=WeatherCondition1)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Weather Condition",
       x = "Weather Condition",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", AlcoholResultType != "NA", 
         AlcoholResultType != "NaN", AlcoholResultType != "Unknown",
         AlcoholResultType != "Contaminated sample/unusable", 
         AlcoholResultType != "Pending") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=AlcoholResultType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Alcohol Result Type",
       x = "Alcohol Result Type",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", TrafficControlType != "NA", 
         TrafficControlType != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=TrafficControlType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() +
  labs(title = "Injury Frequency by Traffic Control Type",
       x = "Traffic Control Type",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", VehicleType != "NA", 
         VehicleType != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=VehicleType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Vehicle Type",
       x = "Vehicle Type",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Protection != "NA", 
         Protection != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=Protection)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Protection",
       x = "Protection",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", VisionObstruction != "NA", 
         VisionObstruction != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=VisionObstruction)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Vision Obstruction",
       x = "Vision Obstruction",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", AirbagDeployed != "NA", 
         AirbagDeployed != "NaN", AirbagDeployed != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=AirbagDeployed)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Airbag Deployed",
       x = "Airbag Deployed",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", FirstHarmfulEvent != "NA", 
         FirstHarmfulEvent != "NaN", FirstHarmfulEvent != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=FirstHarmfulEvent)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequncy by First Harmful Event",
       x = "First Harmful Event",
       y = "Percentage")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", MostHarmfulEvent != "NA", 
         MostHarmfulEvent != "NaN", MostHarmfulEvent != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=MostHarmfulEvent)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequncy by Most Harmful Event",
       x = "Most Harmful Event",
       y = "Percentage")

crashes %>% 
  filter(Protection != "NA", Protection != "Unable to determine") %>%
  count(Protection) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         Protection = reorder(Protection,logtrans)) %>% 
  ggplot(aes(x=Protection, y=logtrans, fill=Protection)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Protection",
       x = "Protection",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(Trapped != "NA", Trapped != "Unknown") %>%
  count(Trapped) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         Trapped = reorder(Trapped,logtrans)) %>% 
  ggplot(aes(x=Trapped, y=logtrans, fill=Trapped)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Trapped",
       x = "Trapped",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(VisionObstruction != "NA", VisionObstruction != "Unknown") %>%
  count(VisionObstruction) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         VisionObstruction = reorder(VisionObstruction,logtrans)) %>% 
  ggplot(aes(x=VisionObstruction, y=logtrans, fill=VisionObstruction)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Vision Obstruction",
       x = "Vision Obstruction",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(VisionObstruction != "NA", VisionObstruction != "Unknown", 
         VisionObstruction != "", WeatherContributedToCrash == "Yes") %>%
  count(VisionObstruction) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         VisionObstruction = reorder(VisionObstruction,logtrans)) %>% 
  ggplot(aes(x=VisionObstruction, y=logtrans, fill=VisionObstruction)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Vision Obstruction",
       x = "Vision Obstruction",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(ContributingCircumstance1 != "NA", ContributingCircumstance1 != "Unknown") %>%
  count(ContributingCircumstance1) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         ContributingCircumstance1 = reorder(ContributingCircumstance1,logtrans)) %>% 
  ggplot(aes(x=ContributingCircumstance1, y=logtrans, fill=ContributingCircumstance1)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Contributing Circumstance 1",
       x = "Contributing Circumstance 1",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(ContributingCircumstance2 != "NA", 
         ContributingCircumstance2 != "Unknown") %>%
  count(ContributingCircumstance2) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         ContributingCircumstance2 = reorder(ContributingCircumstance2,
                                             logtrans)) %>% 
  ggplot(aes(x=ContributingCircumstance2, y=logtrans, 
             fill=ContributingCircumstance2)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Contributing Circumstance 2",
       x = "Contributing Circumstance 2",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(ContributingCircumstance3 != "NA", 
         ContributingCircumstance3 != "Unknown") %>%
  count(ContributingCircumstance3) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         ContributingCircumstance3 = 
           reorder(ContributingCircumstance3,logtrans)) %>% 
  ggplot(aes(x=ContributingCircumstance3, y=logtrans, 
             fill=ContributingCircumstance3)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Contributing Circumstance 3",
       x = "Contributing Circumstance 3",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(LocationRelationToRoad != "NA", LocationRelationToRoad != "Unknown") %>%
  count(LocationRelationToRoad) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         LocationRelationToRoad = reorder(LocationRelationToRoad,logtrans)) %>% 
  ggplot(aes(x=LocationRelationToRoad, y=logtrans, fill=LocationRelationToRoad)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Location of Crash in Relation to Road",
       x = "Location Relation to Road",
       y = "Count (log10 Scale)")


crashes %>% 
  filter(LocationInNearIndicator != "NA", LocationInNearIndicator != "Unknown") %>%
  count(LocationInNearIndicator) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         LocationInNearIndicator = reorder(LocationInNearIndicator,logtrans)) %>% 
  ggplot(aes(x=LocationInNearIndicator, y=logtrans, fill=LocationInNearIndicator)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Crashes Near Indicator",
       x = "Location in Near Indicator",
       y = "Count (log10 Scale)")


crashes %>% 
  filter(LocationRampIndicator != "NA", LocationRampIndicator != "Unknown") %>%
  count(LocationRampIndicator) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         LocationRampIndicator = reorder(LocationRampIndicator,logtrans)) %>% 
  ggplot(aes(x=LocationRampIndicator, y=logtrans, fill=LocationRampIndicator)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Crash Location from Ramp",
       x = "Location Ramp Indicator",
       y = "Count (log10 Scale)")

crashes %>%
  filter(LocationFeetFromRoad != "NA", LocationFeetFromRoad != "Unknown", 
         LocationFeetFromRoad != "") %>%
  ggplot() +
  geom_histogram(aes(x=LocationFeetFromRoad), binwidth = 100, col="red", fill="darkgrey") +
  labs(title="Histogram for Location Feet From Road")

crashes %>% 
  filter(LocationDirectionFromRoad != "NA", LocationDirectionFromRoad != "Unknown",
         LocationDirectionFromRoad != "") %>%
  count(LocationDirectionFromRoad) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         LocationDirectionFromRoad= reorder(LocationDirectionFromRoad,logtrans)) %>% 
  ggplot(aes(x=LocationDirectionFromRoad, y=logtrans, fill=LocationDirectionFromRoad)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Direction of Crash",
       x = "Location Direction From Road",
       y = "Count (log10 Scale)")


crashes %>% 
  filter(LocationAtFromIndicator != "NA", LocationAtFromIndicator != "Unknown") %>%
  count(LocationAtFromIndicator) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         LocationAtFromIndicator= reorder(LocationAtFromIndicator,logtrans)) %>% 
  ggplot(aes(x=LocationAtFromIndicator, y=logtrans, fill=LocationAtFromIndicator)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Indicator Crash Location",
       x = "Location At/From Indicator",
       y = "Count (log10 Scale)")


crashes %>% 
  filter(LocationDirectionToRoad != "NA", LocationDirectionToRoad != "Unknown") %>%
  count(LocationDirectionToRoad) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         LocationDirectionToRoad= reorder(LocationDirectionToRoad,logtrans)) %>% 
  ggplot(aes(x=LocationDirectionToRoad, y=logtrans, fill=LocationDirectionToRoad)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Direction of Crash From Road",
       x = "Location Direction To Road",
       y = "Count (log10 Scale)")


crashes %>% 
  filter(FirstHarmfulEvent != "NA", FirstHarmfulEvent != "Unknown") %>%
  count(FirstHarmfulEvent) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         FirstHarmfulEvent= reorder(FirstHarmfulEvent,logtrans)) %>% 
  ggplot(aes(x=FirstHarmfulEvent, y=logtrans, fill=FirstHarmfulEvent)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "First Harmful Crash Event ",
       x = "First Harmful Event",
       y = "Count (log10 Scale)")


crashes %>% 
  filter(MostHarmfulEvent != "NA", MostHarmfulEvent != "Unknown") %>%
  count(MostHarmfulEvent) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         MostHarmfulEvent= reorder(MostHarmfulEvent,logtrans)) %>% 
  ggplot(aes(x=MostHarmfulEvent, y=logtrans, fill=MostHarmfulEvent)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Most Harmful Crash Event ",
       x = "Most Harmful Event",
       y = "Count (log10 Scale)")


crashes %>% 
  filter(RoadClassification != "NA", RoadClassification != "Unknown",
         RoadClassification != "NaN", RoadClassification != "") %>%
  count(RoadClassification) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         RoadClassification= reorder(RoadClassification,logtrans)) %>% 
  ggplot(aes(x=RoadClassification, y=logtrans, fill=RoadClassification)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Classification of the Road ",
       x = "Road Classification",
       y = "Count (log10 Scale)")


crashes %>% 
  filter(Crash_Date_Year != "Unknown", Crash_Date_Year != "2011") %>%
  ggplot(aes(Crash_Date_Year, fill=as.factor(Crash_Date_Year))) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat="count", aes(x=Crash_Date_Year, label=..count..), vjust=-0.25) + 
  geom_abline(intercept = 387995/(77/12), slope = 0, lty="dashed") +
  scale_x_continuous(breaks=2015:2021) +
  labs(title = "Frequency of Crashes by Year", 
       x = "Year", 
       y = "Count")

crashes %>% 
  filter(PersonType == "Driver") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(Drivers = n, Frequency = nn) %>%
  mutate(Drivers = reorder(Drivers, Frequency)) %>%
  ggplot(aes(x=Drivers, y=Frequency, fill=Drivers)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y=0.1) +
  coord_flip() +
  labs(title = "Number of Drivers per Crash",
       x = "Number of Drivers",
       y = "Count")

crashes %>% 
  filter(PersonType == "Passenger") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(Passengers = n, Frequency = nn) %>%
  mutate(Passengers = reorder(Passengers,Frequency)) %>% 
  ggplot(aes(x=Passengers, y=Frequency, fill=Passengers)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y=0.5) +
  coord_flip() +
  labs(title = "Number of Passengers per Crash",
       x = "Number of Passengers",
       y = "Count")

crashes %>% 
  filter(PersonType == "Pedestrian") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>% 
  rename(Pedestrians = n, Frequency = nn) %>%
  mutate(Pedestrians = reorder(Pedestrians,Frequency)) %>% 
  ggplot(aes(x=Pedestrians, y=Frequency, fill=Pedestrians)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y=0.5) +
  coord_flip() +
  labs(title = "Number of Pedestrians per Crash",
       x = "Number of Pedestrians",
       y = "Count")

crashes %>% 
  filter(PersonType == "Pedalcyclist") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(Pedalcyclists = n, Frequency = nn) %>%
  mutate(Pedalcyclists = reorder(Pedalcyclists,Frequency)) %>% 
  ggplot(aes(x=Pedalcyclists, y=Frequency, fill=Pedalcyclists)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y=0.5) +
  coord_flip() +
  labs(title = "Number of Pedalcyclists per Crash",
       x = "Number of Pedalcyclists",
       y = "Count")

crashes %>% 
  filter(PersonType == "Other*") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(Other = n, Frequency = nn) %>%
  mutate(Other = reorder(Other,Frequency)) %>% 
  ggplot(aes(x=Other, y=Frequency, fill=Other)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y=0.5) +
  coord_flip() +
  labs(title = "Number of Others per Crash",
       x = "Number of Others",
       y = "Count")

crashes %>% 
  filter(PersonType == "Unknown") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(Unknown = n, Frequency = nn) %>%
  mutate(Unknown = reorder(Unknown,Frequency)) %>% 
  ggplot(aes(x=Unknown, y=Frequency, fill=Unknown)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y=0.5) +
  coord_flip() +
  labs(title = "Number of Unknown per Crash",
       x = "Number of Unknown",
       y = "Count")

crashes %>% 
  filter(Injury == "Killed") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(Killed = n, Frequency = nn) %>%
  mutate(Killed = reorder(Killed, Frequency)) %>%
  ggplot(aes(x=Killed, y=Frequency, fill=Killed)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y = 0.5) +
  coord_flip() +
  labs(title = "Number Killed per Crash",
       x = "Number Killed",
       y = "Count")

crashes %>% 
  filter(Injury == "A type injury (disabling)") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(TypeA = n, Frequency = nn) %>%
  mutate(TypeA = reorder(TypeA, Frequency)) %>%
  ggplot(aes(x=TypeA, y=Frequency, fill=TypeA)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y = 0.5) +
  coord_flip() +
  labs(title = "Number of Type A Injuries per Crash",
       x = "Number of Type A Injuries",
       y = "Count")

crashes %>% 
  filter(Injury == "B type injury (evident)") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(TypeB = n, Frequency = nn) %>%
  mutate(TypeB = reorder(TypeB, Frequency)) %>%
  ggplot(aes(x=TypeB, y=Frequency, fill=TypeB)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y = 0.5) +
  coord_flip() +
  labs(title = "Number of Type B Injuries per Crash",
       x = "Number of Type B Injuries",
       y = "Count")

crashes %>% 
  filter(Injury == "C type injury (possible)") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(TypeC = n, Frequency = nn) %>%
  mutate(TypeC = reorder(TypeC, Frequency)) %>%
  ggplot(aes(x=TypeC, y=Frequency, fill=TypeC)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y = 0.5) +
  coord_flip() +
  labs(title = "Number of Type C Injuries per Crash",
       x = "Number of Type C Injuries",
       y = "Count")

crashes %>% 
  filter(Injury == "No injury") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(NoInjury = n, Frequency = nn) %>%
  mutate(NoInjury = reorder(NoInjury, Frequency)) %>%
  ggplot(aes(x=NoInjury, y=Frequency, fill=NoInjury)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y = 0.5) +
  coord_flip() +
  labs(title = "Number of People with No Injuries per Crash",
       x = "Number of People with No Injuries",
       y = "Count")

crashes %>% 
  filter(Injury == "Unknown") %>%
  group_by(key_crash) %>%
  count() %>% 
  group_by(n) %>%
  count() %>%
  rename(Unknown = n, Frequency = nn) %>%
  mutate(Unknown = reorder(Unknown, Frequency)) %>%
  ggplot(aes(x=Unknown, y=Frequency, fill=Unknown)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=Frequency),nudge_y = 0.5) +
  coord_flip() +
  labs(title = "Number of Unknown Injuries per Crash",
       x = "Number of Unknown Injuries",
       y = "Count")

###Mapping crashes 
library(maps)
#install.packages("sf")
library(sf)

#North Carolina Map 
crashes %>% 
  filter(LocationLatitude != "0", LocationLongitude != "0",
         LocationLatitude != "NA", LocationLatitude != "",
         LocationLongitude != "NA", LocationLongitude != "",
         LocationLatitude > 35.4, LocationLatitude < 36.2,
         LocationLongitude < -78, LocationLongitude > -79) %>%
  ggplot(aes(LocationLongitude, LocationLatitude)) +
  borders('county', 'north carolina', fill = "light blue") +
  geom_point(size=0.5, col = 'black', show.legend = F) +
  coord_quickmap()

#Wake county map 
nc_counties <- map_data("county", "north carolina") %>%
  select(lon = long, lat, group, id = subregion)
head(nc_counties)

wake_county <- nc_counties %>%
  filter(nc_counties$id == "wake")

wake_map <- wake_county %>% 
  ggplot() +
  geom_polygon(aes(lon, lat), color = "darkblue", fill = "lightblue")
wake_map

crashes_filter <- crashes %>%
  filter(LocationLatitude != "0", LocationLongitude != "0",
         LocationLatitude != "NA", LocationLatitude != "",
         LocationLongitude != "NA", LocationLongitude != "",
         LocationLatitude > 35.4, LocationLatitude < 36.2,
         LocationLongitude < -78, LocationLongitude > -79)

wake_map <- wake_map + 
  geom_point(data = crashes_filter, 
             aes(x=LocationLongitude, y= LocationLatitude), 
             colour = "blue", alpha = 1/10)
wake_map

#Heat map (zoomed out)
library(ggplot2)
#install.packages("ggmap")
library(ggmap)
library(RColorBrewer)

map_bounds <- c(-78.98, 35.51, -78.24, 36.06)

coords.map <- get_stamenmap(map_bounds, zoom = 10, maptype = "toner-lite")

coords.map <- ggmap(coords.map, extent="panel", legend="none")
coords.map <- coords.map + stat_density2d(data=crashes_filter,  
                                          aes(x=LocationLongitude, 
                                              y=LocationLatitude, 
                                              fill=..level.., 
                                              alpha=..level..), 
                                          geom="polygon")
coords.map <- coords.map +   scale_fill_gradientn(colours=rev(brewer.pal(7, "RdBu")))

coords.map <- coords.map + theme_bw()

coords.map

#Heat map (zoomed in) 
crashes_filter <- crashes %>%
  filter(LocationLatitude != "0", LocationLongitude != "0",
         LocationLatitude != "NA", LocationLatitude != "",
         LocationLongitude != "NA", LocationLongitude != "",
         LocationLatitude > 35.4, LocationLatitude < 36.2,
         LocationLongitude < -78, LocationLongitude > -79)

map_bounds <- c(-78.8, 35.72, -78.5, 35.9) #coordinates of wake county

coords.map <- get_stamenmap(map_bounds, zoom = 13, maptype = "toner-lite")

coords.map <- ggmap(coords.map, extent="panel")
coords.map <- coords.map + stat_density2d(data=crashes_filter,  
                                          aes(x=LocationLongitude, 
                                              y=LocationLatitude, 
                                              fill=..level..),
                                          alpha=0.3, 
                                          geom="polygon") 

coords.map <- coords.map + scale_fill_gradientn(colours=rev(brewer.pal(7, "RdYlGn")))

coords.map <- coords.map + theme_bw() + 
  ggtitle("Heat Map of Crash Locations") + 
  xlab("Latitude") + ylab("Longitude")

coords.map

#Interactive map of wake county crashes 
library(plotly)

ggplotly(coords.map) %>%
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  )

###Visualizing text data using word clouds 

#Installing and loading necessary packages 
install.packages('wordcloud2')
library(wordcloud2)

#Word cloud for location road name on  
devtools::install_github("gaospecial/wordcloud2")

location_road_name_on_freq <- crashes %>%
  count(LocationRoadNameOn)

set.seed(101)
location_road_name_on_freq %>%
  wordcloud2(shape = 'circle', backgroundColor = "black", minSize = 5)

#Word cloud for location road name at 
devtools::install_github("gaospecial/wordcloud2")

location_road_name_at_freq <- crashes %>%
  count(LocationRoadNameAt)

set.seed(102)
location_road_name_at_freq %>%
  wordcloud2(shape = 'circle', backgroundColor = "black", minSize = 5)

#Time series plot
crashes_ts = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))

crashes_ts %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90))

#Crash Date Hour Shift Plot
crashes %>%
  filter(Crash_Date_Hour != "NA", Crash_Date_Hour != "") %>%
  mutate(shift = case_when(
    Crash_Date_Hour >= 6 & Crash_Date_Hour < 14 ~ "6:00 a.m. - 1:59 p.m.",
    Crash_Date_Hour >= 14 & Crash_Date_Hour < 22 ~ "2:00 p.m. - 9:59 p.m.",
    TRUE ~ "10:00 p.m. - 5:59 a.m.")) %>%
  mutate(shift = factor(shift, levels = c("6:00 a.m. - 1:59 p.m.", 
                                          "2:00 p.m. - 9:59 p.m.", 
                                          "10:00 p.m. - 5:59 a.m."))) %>%
  ggplot() +
  geom_bar(aes(x=shift, fill = shift), show.legend = F) +
  labs(title="Frequency of Crashes by Shift", x="Shift", y="Count")

#Injury Frequency by Driver Age
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType == "Driver", Age != "NA",
         Age != "", Age != "Unknown", Age >= 5) %>%
  group_by(Injury) %>%
  mutate(age = case_when(
    Age >= 5 & Age < 15 ~ "5-14",
    Age >= 15 & Age < 25 ~ "15-24",
    Age >= 25 & Age < 35 ~ "25-34",
    Age >= 35 & Age < 45 ~ "35-44",
    Age >= 45 & Age < 55 ~ "45-54",
    Age >= 55 & Age < 65 ~ "55-64",
    Age >= 65 & Age < 75 ~ "65-74",
    Age >= 75 & Age < 85 ~ "75-84",
    Age >= 85 & Age < 95 ~ "85-94",
    Age >= 95 & Age < 105 ~ "95-104",
    Age >= 105 & Age < 115 ~ "105-114",
    TRUE ~ "115-124")) %>%
  mutate(age = factor(age, levels = c("5-14", "15-24", "25-34", "35-44", "45-54",
                                      "55-64", "65-74", "75-84", "85-94", 
                                      "95-104", "105-114", "115-124"))) %>%
  ggplot(aes(fill=Injury, x=age)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Driver Age",
       x = "Age",
       y = "Percentage")

#Frequency of Driver Age
crashes %>%
  filter(PersonType == "Driver", Age != "NA",
         Age != "", Age != "Unknown", Age >= 5) %>%
  mutate(age = case_when(
    Age >= 5 & Age < 15 ~ "5-14",
    Age >= 15 & Age < 25 ~ "15-24",
    Age >= 25 & Age < 35 ~ "25-34",
    Age >= 35 & Age < 45 ~ "35-44",
    Age >= 45 & Age < 55 ~ "45-54",
    Age >= 55 & Age < 65 ~ "55-64",
    Age >= 65 & Age < 75 ~ "65-74",
    Age >= 75 & Age < 85 ~ "75-84",
    Age >= 85 & Age < 95 ~ "85-94",
    Age >= 95 & Age < 105 ~ "95-104",
    Age >= 105 & Age < 115 ~ "105-114",
    TRUE ~ "115-124")) %>%
  mutate(age = factor(age, levels = c("5-14", "15-24", "25-34", "35-44", "45-54",
                                      "55-64", "65-74", "75-84", "85-94", 
                                      "95-104", "105-114", "115-124"))) %>%
  ggplot(aes(fill=age, x=age)) + 
  geom_bar(show.legend = FALSE) + 
  geom_text(stat="count", aes(x=age, label=..count..), vjust=-0.25) +
  labs(title = "Frequency of Driver Age",
       x = "Age",
       y = "Count") + 
  theme(axis.text.x = element_text(angle = 90))

#Injury Frequency by Passenger Age
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType == "Passenger", Age != "NA",
         Age != "", Age != "Unknown") %>%
  group_by(Injury) %>%
  mutate(age = case_when(
    Age >= 0 & Age < 10 ~ "0-9",
    Age >= 10 & Age < 20 ~ "10-19",
    Age >= 20 & Age < 30 ~ "20-29",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    Age >= 50 & Age < 60 ~ "50-59",
    Age >= 60 & Age < 70 ~ "60-69",
    Age >= 70 & Age < 80 ~ "70-79",
    Age >= 80 & Age < 90 ~ "80-89",
    Age >= 90 & Age < 100 ~ "90-99",
    Age >= 100 & Age < 110 ~ "100-109",
    Age >= 110 & Age < 120 ~ "110-119",
    TRUE ~ "120-129")) %>%
  mutate(age = factor(age, levels = c("0-9", "10-19", "20-29", "30-39", "40-49", 
                                      "50-59", "60-69", "70-79", "80-89", 
                                      "90-99", "100-109", "110-119",
                                      "120-129"))) %>%
  ggplot(aes(fill=Injury, x=age)) + 
  geom_bar() + 
  coord_flip() + 
  labs(title = "Injury Frequency by Passenger Age",
       x = "Age",
       y = "Percentage")

#Frequency of Passenger Age
crashes %>%
  filter(PersonType == "Passenger", Age != "NA",
         Age != "", Age != "Unknown") %>%
  mutate(age = case_when(
    Age >= 0 & Age < 10 ~ "0-9",
    Age >= 10 & Age < 20 ~ "10-19",
    Age >= 20 & Age < 30 ~ "20-29",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    Age >= 50 & Age < 60 ~ "50-59",
    Age >= 60 & Age < 70 ~ "60-69",
    Age >= 70 & Age < 80 ~ "70-79",
    Age >= 80 & Age < 90 ~ "80-89",
    Age >= 90 & Age < 100 ~ "90-99",
    Age >= 100 & Age < 110 ~ "100-109",
    Age >= 110 & Age < 120 ~ "110-119",
    TRUE ~ "120-129")) %>%
  mutate(age = factor(age, levels = c("0-9", "10-19", "20-29", "30-39", "40-49", 
                                      "50-59", "60-69", "70-79", "80-89", 
                                      "90-99", "100-109", "110-119",
                                      "120-129"))) %>%
  ggplot(aes(fill=age, x=age)) + 
  geom_bar(show.legend = FALSE) + 
  geom_text(stat="count", aes(x=age, label=..count..), vjust=-0.25) +
  labs(title = "Frequency of Passenger Age",
       x = "Age",
       y = "Percentage") + 
  theme(axis.text.x = element_text(angle = 90))

#Regular Time Series Plots
#install.packages("fpp2")
library(fpp2)

crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))

crashes_annual = crashes_ts %>%
  separate(Date, 
           into = c("Year", "Month"), sep = 4, remove = FALSE) %>%
  select(-Month)

crashes_ts %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90))

crashes_annual %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Year, scales = "free_x")

#Forecasting attempt with Dr. Mostafa (does not work)
library(zoo)
zoo(crashes_ts, seq(from = as.Date("2015-01-01"), to = as.Date("2021-05-31"), by = 1))

ts_crashes <- window(as.ts(crashes_ts$Date), start=2015)

ts_crashes <- ts(data = crashes_ts, start = 1, frequency = 365)

fit1 <- HoltWinters(ts_crashes)

fit1

plot(forecast(fit1))

fit.crashes <- tslm(ts_crashes ~ trend)
fcast <- forecast(fit.crashes)
autoplot(fcast) +
  ggtitle("Forecasts of daily number of car crashes in Wake County, NC using regression") +
  xlab("Year") + ylab("number of crashes per day")


ts_crashes2 <- window(as.ts(crashes_ts$Date), start=2015/01/01)
fit <- HoltWinters(ts_crashes2, beta=FALSE, gamma=FALSE)

crash_count <- crashes_ts %>%
  group_by(Date)

ts_crashes <- window(as.ts(crash_count), start=2015)
fit.crashes <- tslm(ts_crashes ~ trend + season)
fcast <- forecast(fit.crashes)
autoplot(fcast) +
  ggtitle("Forecasts of daily number of car crashes in Wake County, NC using regression") +
  xlab("Year") + ylab("number of crashes per day")

#Different attempt at forecasting daily (THIS WORKS) 
#Loading necessary packages 
library(forecast)
library(fpp2)
library(TTR)

##Working with entire time series (THIS WORKS)
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))

#Converting crashes_ts to a time series object 
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)

#Converting crashes_ts to a time series object 
crashests <- as.ts(crashes_ts)

#Least squares estimation
fit.crashes2 <- tslm(count ~ Date, data=crashests)
summary(fit.crashes2)

#Fitted values (linear plot through ts)
autoplot(crashests[,'count'], series="Data") +
  autolayer(fitted(fit.crashes2), series="Fitted") +
  xlab("Date") + ylab("") +
  ggtitle("Number of Daily Car Crashes (With Pandemic Data)") +
  guides(colour=guide_legend(title=" ")) + 
  scale_x_continuous(breaks = seq(as.Date("2015/01/01"), as.Date("2021/05/31"), 
                                  "day"))

####CANNOT CHANGE X AXIS????

#Prediction with HoltWinters() and plotting forecast
covid <- HoltWinters(crashests2)

summary(covid)

plot(fitted(covid), main = "Box Jenkins Decomposition of Daily Crashes (With Pandemic Data)")

fcast <- forecast(covid, 210)

summary(fcast)

autoplot(fcast) +
  ggtitle("Forecasts of Daily Car Crashes Using Holt Winters (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes")

#Prediction with stlf (Seasonal and Trend Decomposition using Loess 
#Forecasting Model)
m <- crashests2 %>%
  stlf(lambda = 0, h = 210) 

m %>%
  autoplot() + 
  ggtitle("Seasonal and Trend Decomposition Using Loess Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes")

m %>% summary()

####Working with non COVID time series daily (THIS WORKS)
#Regular non COVID time series daily 
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))

crashes_ts.noncovid %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90))

#Converting crashes_ts to a time series object 
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), end = c(2020,59),
                 frequency = 365)

#Converting crashes_ts to a time series object 
crashests.noncovid2 <- as.ts(crashes_ts.noncovid, start = c(2015,1), end = c(2020,59), 
                            frequency = 365)

#Least squares estimation
fit.crashes.noncovid2 <- tslm(count ~ Date, data=crashests.noncovid2)
summary(fit.crashes.noncovid2)

#Fitted values (linear plot through ts)
autoplot(crashests.noncovid2[,'count'], series="Data") +
  autolayer(fitted(fit.crashes.noncovid2), series="Fitted") +
  xlab("Date") + ylab("") +
  ggtitle("Number of Daily Car Crashes (Without Pandemic Data)") +
  guides(colour=guide_legend(title=" "))

#Prediction with HoltWinters() and plotting forecast
noncovid <- HoltWinters(crashests.noncovid)

summary(noncovid)

plot(fitted(noncovid), main = "Box Jenkins Decomposition of Daily Crashes (Without Pandemic Data)") 

fcast.noncovid <- forecast(noncovid, 700)

summary(fcast)

autoplot(fcast.noncovid) +
  ggtitle("Forecasts of Car Crashes Using HoltWinters (Without Pandemic Data)") +
  xlab("Year") + ylab("Crashes")

#Plotting the time series and forecast together STLF
p <- crashests.noncovid %>%
  stlf(lambda = 0, h = 700) 

p %>%
  autoplot() + 
  ggtitle("Seasonal and Trend Decomposition Using Loess Forecasting Model for Daily Car Crashes (Without Pandemic Data)") +
  xlab("Year") + ylab("Daily Crashes")

p %>% summary()

##Different attempt at forecasting for monthly (THIS WORKS)
#install.packages("Mcomp")
#install.packages("smooth")
library(Mcomp)
library(smooth)

#Working with entire time series monthly (THIS Works) linear model 
crashes_mts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash))) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "/") %>%
  group_by(Year, Month) %>%
  summarise(mcount = sum(count)) %>%
  tidyr::spread(key=Month, value=mcount)

crashes_mts = as.data.frame(crashes_mts)

rownames(crashes_mts) = seq(2015, 2021)

crashes_mts3 = crashes_mts %>%
  select(02:13)

crashes_mts2 <- ts(c(t(crashes_mts3)), frequency=12)

crashes_mts4 <- window(crashes_mts2, start=c(1,01), end=c(7,05), frequency=12)

fit.crashes <- tslm(crashes_mts4 ~ trend + season)

summary(fit.crashes)

fcast <- forecast::forecast(fit.crashes)

summary(fcast)

autoplot(fcast) +
  ggtitle("Forecasts of Monthly Car Crashes Using Linear Model (With Pandemic Data)") +
  xlab("Year") + ylab("Monthly Crashes")

#Working with non COVID time series monthly (THIS Works) linear model 
crashes_mts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/02/29") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash))) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "/") %>%
  group_by(Year, Month) %>%
  summarise(mcount = sum(count)) %>%
  tidyr::spread(key=Month, value=mcount)

crashes_mts.noncovid = as.data.frame(crashes_mts.noncovid)

rownames(crashes_mts.noncovid) = seq(2015, 2020)

crashes_mts3.noncovid = crashes_mts.noncovid %>%
  select(02:13)

crashes_mts2.noncovid <- ts(c(t(crashes_mts3.noncovid)), frequency=12)

crashes_mts4.noncovid <- window(crashes_mts2.noncovid, start=c(1,01), 
                                end=c(6,02), frequency=12)
fit.crashes.noncovid <- tslm(crashes_mts4.noncovid ~ trend + season)

summary(fit.crashes.noncovid)

fcast.noncovid <- forecast::forecast(fit.crashes.noncovid)

summary(fcast.noncovid)

autoplot(fcast.noncovid) +
  ggtitle("Forecasts of Monthly Car Crashes Using Linear Model (Without Pandemic Data)") +
  xlab("Year") + ylab("Monthly Crashes")

##Statistical Analyses - ANOVA

#Is there a significant difference between the amount of crashes on each DOW
crashes_dow = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date, Crash_Date_DOW) %>%
  summarize(count = length(unique(key_crash)))

anova_dow = aov(count ~ Crash_Date_DOW, data = crashes_dow)
summary(anova_dow)

TukeyHSD(anova_dow)

#Is there a significant difference between the amount of crashes per each race 
crashes_race = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(Race != "Unknown", Race != "NA") %>%
  group_by(Date, Race) %>%
  summarize(count = length(key_crash))

anova_race = aov(count ~ Race, data = crashes_race)
summary(anova_race)

TukeyHSD(anova_race)

#Is there a significant difference between the amount of crashes per each gender
crashes_gender= crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(Gender != "Unknown", Gender != "NA") %>%
  group_by(Date, Gender) %>%
  summarize(count = length(key_crash))

anova_gender = aov(count ~ Gender, data = crashes_gender)
summary(anova_gender)

TukeyHSD(anova_gender)

#Is there a significant difference between the amount of crashes per each Vision
#Obstruction 
crashes_vision= crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(VisionObstruction != "Unknown", VisionObstruction != "NA") %>%
  group_by(Date, VisionObstruction) %>%
  summarize(count = length(key_crash))

anova_vision = aov(count ~ VisionObstruction, data = crashes_vision)
summary(anova_vision)

TukeyHSD(anova_vision)

#Is there a significant difference between the amount of crashes per each 
#vehicle type
crashes_vehicle= crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(VehicleType != "Unknown", VehicleType != "NA", VehicleType != "") %>%
  group_by(Date, VehicleType) %>%
  summarize(count = length(key_crash))

anova_vehicle = aov(count ~ VehicleType, data = crashes_vehicle)
summary(anova_vehicle)

TukeyHSD(anova_vehicle)

#Is there a significant difference between the amount of crashes per each 
#Weather Condition
crashes_weather= crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(WeatherCondition1 != "Unknown", WeatherCondition1 != "NA") %>%
  group_by(Date, WeatherCondition1) %>%
  summarize(count = length(key_crash))

anova_weather = aov(count ~ WeatherCondition1, data = crashes_weather)
summary(anova_weather)

TukeyHSD(anova_weather)

#Is there a significant difference between the amount of crashes per each 
#traffic control type
crashes_traffic= crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(TrafficControlType != "Unknown", TrafficControlType != "NA") %>%
  group_by(Date, TrafficControlType) %>%
  summarize(count = length(key_crash))

anova_traffic = aov(count ~ TrafficControlType, data = crashes_traffic)
summary(anova_traffic)

TukeyHSD(anova_traffic)

#Is there a significant difference between the amount of crashes per each 
#road feature
crashes_roadfeat = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(RoadFeature != "Unknown", RoadFeature != "NA", RoadFeature != "NaN") %>%
  group_by(Date, RoadFeature) %>%
  summarize(count = length(key_crash))

anova_roadfeat = aov(count ~ RoadFeature, data = crashes_roadfeat)
summary(anova_roadfeat)

TukeyHSD(anova_roadfeat)

#Is there a significant difference between the amount of crashes per each 
#alcohol result type
crashes_alcohol= crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(AlcoholResultType != "Unknown", AlcoholResultType != "NA") %>%
  group_by(Date, AlcoholResultType) %>%
  summarize(count = length(key_crash))

anova_alcohol = aov(count ~ AlcoholResultType, data = crashes_alcohol)
summary(anova_alcohol)

TukeyHSD(anova_alcohol)

#Is there a significant difference between the amount of crashes per each 
#month
crashes_month = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(Crash_Date_Month != "Unknown", Crash_Date_Month != "NA") %>%
  group_by(Date, Crash_Date_Month) %>%
  summarize(count = length(key_crash))

anova_month = aov(count ~ Crash_Date_Month, data = crashes_month)
summary(anova_month)

TukeyHSD(anova_month)

#Is there a significant difference between the amount of crashes per each 
#airbag deploy
crashes_airbag = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(AirbagDeployed != "Unknown", AirbagDeployed != "NA") %>%
  group_by(Date, AirbagDeployed) %>%
  summarize(count = length(key_crash))

anova_airbag = aov(count ~ AirbagDeployed, data = crashes_airbag)
summary(anova_airbag)

TukeyHSD(anova_airbag)

#Is there a significant difference between the amount of crashes per each 
#protection
crashes_protection = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(Protection != "Unknown", Protection != "NA", 
         Protection != "Unable to determine") %>%
  group_by(Date, Protection) %>%
  summarize(count = length(key_crash))

anova_protection = aov(count ~ Protection, data = crashes_protection)
summary(anova_protection)

TukeyHSD(anova_protection)

#Is there a significant difference between the amount of crashes per each 
#contributing circumstance
crashes_contributing = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(ContributingCircumstance1 != "Unknown", 
         ContributingCircumstance1 != "NA") %>%
  group_by(Date, ContributingCircumstance1) %>%
  summarize(count = length(key_crash))

anova_contributing = aov(count ~ ContributingCircumstance1, 
                         data = crashes_contributing)
summary(anova_contributing)

TukeyHSD(anova_contributing)

#Is there a significant difference between the amount of crashes per each 
#first harmful event
crashes_first_harm = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(FirstHarmfulEvent != "Unknown", FirstHarmfulEvent != "NA") %>%
  group_by(Date, FirstHarmfulEvent) %>%
  summarize(count = length(key_crash))

anova_first_harm = aov(count ~ FirstHarmfulEvent, data = crashes_first_harm)
summary(anova_first_harm)

TukeyHSD(anova_first_harm)

#Is there a significant difference between the amount of crashes per each 
#most harmful event
crashes_most_harm = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  filter(MostHarmfulEvent != "Unknown", MostHarmfulEvent != "NA") %>%
  group_by(Date, MostHarmfulEvent) %>%
  summarize(count = length(key_crash))

anova_most_harm = aov(count ~ MostHarmfulEvent, data = crashes_most_harm)
summary(anova_most_harm)

TukeyHSD(anova_most_harm)

##Statistical Analyses - t test

#Is there a significant difference between the amount of crashes in rain v. 
#no rain
crashes_rain = crashes %>%
  filter(WeatherCondition1 != "Unknown", WeatherCondition1 != "NA", 
         WeatherCondition1 != "") %>%
  mutate(WeatherCondition1 = case_when(
    WeatherCondition1 == "Rain" ~ "Rain", 
    TRUE ~ "Not Rain")) %>%
  group_by(key_crash,WeatherCondition1) %>%
  summarize(count = length(key_crash))

crashes_rain <- mutate(crashes_rain, rain = WeatherCondition1 == "Rain")

t.test(count ~ rain, data = crashes_rain)

crashes_rain %>%
  ggplot(aes(fill=WeatherCondition1, x=WeatherCondition1)) + 
  geom_bar(show.legend = FALSE) + 
  geom_text(stat="count", aes(x=WeatherCondition1, label=..count..), vjust=-0.25)
  