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
  labs(title = "Crash Frequency by Alcohol Result Type and Crash Date DOW",
       x = "Crash Date DOW",
       y = "Count")

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
  labs(title = "Crash Frequency by Injury and Contributing Circumstance",
       x = "Contributing Circumstance",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", PersonType != "NA", 
        PersonType != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=PersonType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Person Type",
       x = "Person Type",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Ejection != "NA", 
         Ejection != "NaN", Ejection != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=Ejection)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Ejection",
       x = "Ejection",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", RoadFeature != "NA", 
         RoadFeature != "NaN", RoadFeature != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=RoadFeature)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Road Feature",
       x = "Road Feature",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", RoadClassification != "NA", 
         RoadClassification != "NaN", RoadClassification != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=RoadClassification)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Road Classification",
       x = "Road Classification",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", WeatherCondition1 != "NA", 
         WeatherCondition1 != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=WeatherCondition1)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Weather Condition",
       x = "Weather Condition",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", AlcoholResultType != "NA", 
         AlcoholResultType != "NaN", AlcoholResultType != "Unknown",
         AlcoholResultType != "Contaminated sample/unusable", 
         AlcoholResultType != "Pending") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=AlcoholResultType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Alcohol Result Type",
       x = "Alcohol Result Type",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", TrafficControlType != "NA", 
         TrafficControlType != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=TrafficControlType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() +
  labs(title = "Crash Frequency by Injury and Traffic Control Type",
       x = "Traffic Control Type",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", VehicleType != "NA", 
         VehicleType != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=VehicleType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Vehicle Type",
       x = "Vehicle Type",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Protection != "NA", 
         Protection != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=Protection)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Protection",
       x = "Protection",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", VisionObstruction != "NA", 
         VisionObstruction != "NaN") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=VisionObstruction)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Vision Obstruction",
       x = "Vision Obstruction",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", AirbagDeployed != "NA", 
         AirbagDeployed != "NaN", AirbagDeployed != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=AirbagDeployed)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Airbag Deployed",
       x = "Airbag Deployed",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", FirstHarmfulEvent != "NA", 
         FirstHarmfulEvent != "NaN", FirstHarmfulEvent != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=FirstHarmfulEvent)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and First Harmful Event",
       x = "First Harmful Event",
       y = "Count")

crashes %>%
  filter(Injury != "NA", Injury != "Unknown", MostHarmfulEvent != "NA", 
         MostHarmfulEvent != "NaN", MostHarmfulEvent != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=MostHarmfulEvent)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Crash Frequency by Injury and Most Harmful Event",
       x = "Most Harmful Event",
       y = "Count")

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
  geom_point(data = crashes_filter, aes(x=LocationLongitude, y= LocationLatitude, size = 0.2))
wake_map

#Heat map (option 1) - need to zoom in 
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
    Crash_Date_Hour >= 6 & Crash_Date_Hour < 14 ~ "Shift 1",
    Crash_Date_Hour >= 14 & Crash_Date_Hour < 22 ~ "Shift 2",
    TRUE ~ "Shift 3")) %>%
  ggplot() +
  geom_bar(aes(x=shift, fill = shift)) +
  labs(title="Frequency of Crashes by Shift", x="Shift", y="Count")
