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
       y = "Percentage") + 
  scale_x_discrete(labels = c("Alcohol use", 
                              "Going wrong way",
                              "Disregarded other traffic signs",
                              "Disregarded road markings",
                              "Disregarded stop sign",
                              "Disregarded traffic signals",
                              "Disregarded yield sign", 
                              "Driver distracted",
                              "Driver distracted by communication device",
                              "Driver distracted by external distraction", 
                              "Driver distracted by electronic",
                              "Driver distracted by passenger", 
                              "Drug use", 
                              "Exceeded speed limit",
                              "Exceeded safe speed", 
                              "Failed to yield", 
                              "Failure to reduce speed",
                              "Followed too closely",
                              "Improper backing",
                              "Improper lane change", 
                              "Improper or no signal",
                              "Improper parking",
                              "Improper turn",
                              "Inattention",
                              "No contributing circumstance",
                              "Operated defective equipment", 
                              "Erratic, reckless, negligent", 
                              "Other improper passing", 
                              "Other*",
                              "Overcorrected/oversteered",
                              "Passed on curve",  
                              "Passed on hill",
                              "Passed stopped school bus", 
                              "Right turn on red",
                              "Swerved or avoided",  
                              "Use of improper lane",
                              "Visibility obstructed"))

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
  filter(ContributingCircumstance1 != "NA", ContributingCircumstance1 != "Unknown", 
         ContributingCircumstance1 != "Unable to determine") %>%
  count(ContributingCircumstance1) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         ContributingCircumstance1 = reorder(ContributingCircumstance1,logtrans)) %>% 
  ggplot(aes(x=ContributingCircumstance1, y=logtrans, fill=ContributingCircumstance1)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Contributing Circumstance 1",
       x = "Contributing Circumstance 1",
       y = "Count (log10 Scale)") + 
  scale_x_discrete(labels = c("Passed on hill", 
                              "Passed on curve",  
                              "Passed stopped school bus",
                              "Driver distracted by electronic", 
                              "Improper or no signal",
                              "Driver distracted by external distraction",
                              "Right turn on red",
                              "Drug use",
                              "Disregarded yield sign", 
                              "Driver distracted by communication device", 
                              "Driver distracted by passenger", 
                              "Visibility obstructed",
                              "Disregarded other traffic signs", 
                              "Driver distracted", 
                              "Disregarded road markings",
                              "Operated defective equipment",
                              "Exceeded speed limit",
                              "Disregarded stop sign",
                              "Going wrong way", 
                              "Use of improper lane",
                              "Alcohol use", 
                              "Other improper passing", 
                              "Followed too closely", 
                              "Swerved or avoided",
                              "Overcorrected/oversteered",
                              "Erratic, reckless, negligent", 
                              "Other*",
                              "Disregarded traffic signals",
                              "Exceeded safe speed",
                              "Improper parking",
                              "Improper turn",
                              "Improper lane change",
                              "Improper backing",
                              "Failed to yield", 
                              "Inattention", 
                              "Failure to reduce speed",
                              "No contributing circumstance"))

crashes %>% 
  filter(ContributingCircumstance2 != "NA", 
         ContributingCircumstance2 != "Unknown", 
         ContributingCircumstance2 != "Unable to determine") %>%
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
       y = "Count (log10 Scale)") + 
  scale_x_discrete(labels = c("Passed on curve",  
                              "Passed on hill", 
                              "Passed stopped school bus",
                              "Disregarded yield sign", 
                              "Improper or no signal",
                              "Right turn on red",
                              "Disregarded other traffic signs", 
                              "Disregarded stop sign",
                              "Driver distracted by external distraction",
                              "Driver distracted by electronic", 
                              "Disregarded road markings",
                              "Drug use",
                              "Visibility obstructed",
                              "Swerved or avoided",
                              "Other improper passing",
                              "Exceeded speed limit",
                              "Use of improper lane",
                              "Driver distracted by communication device", 
                              "Driver distracted by passenger", 
                              "Operated defective equipment",
                              "Improper parking",
                              "No contributing circumstance", 
                              "Disregarded traffic signals",
                              "Other*",
                              "Driver distracted", 
                              "Going wrong way",
                              "Exceeded safe speed",
                              "Followed too closely", 
                              "Overcorrected/oversteered",
                              "Improper turn",
                              "Alcohol use", 
                              "Improper lane change",
                              "Erratic, reckless, negligent", 
                              "Improper backing",
                              "Failed to yield",
                              "Failure to reduce speed",
                              "Inattention"))

crashes %>% 
  filter(ContributingCircumstance3 != "NA", 
         ContributingCircumstance3 != "Unknown", 
         ContributingCircumstance3 != "Unable to determine") %>%
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
       y = "Count (log10 Scale)") + 
  scale_x_discrete(labels = c("Passed on hill", 
                              "Right turn on red",
                              "Passed on curve", 
                              "Disregarded yield sign", 
                              "Improper or no signal",
                              "Disregarded other traffic signs", 
                              "Disregarded stop sign",
                              "Improper parking",
                              "No contributing circumstance", 
                              "Disregarded road markings",
                              "Visibility obstructed",
                              "Driver distracted by electronic",
                              "Swerved or avoided",
                              "Exceeded speed limit",
                              "Driver distracted by external distraction",
                              "Operated defective equipment",
                              "Other improper passing",
                              "Use of improper lane",
                              "Disregarded traffic signals",
                              "Exceeded safe speed",
                              "Improper backing",
                              "Driver distracted by communication device",
                              "Going wrong way",
                              "Driver distracted by passenger", 
                              "Other*",
                              "Improper lane change",
                              "Drug use",
                              "Improper turn",
                              "Overcorrected/oversteered",
                              "Driver distracted", 
                              "Followed too closely", 
                              "Failure to reduce speed",
                              "Failed to yield",
                              "Alcohol use", 
                              "Erratic, reckless, negligent",
                              "Inattention"))

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
  ggtitle("Time Series Plot for Frequency of Daily Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Count") + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90))

#Adding 7 day moving average to daily time series
library(zoo)

crashes_ts %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  mutate(seven_avg= rollmean(count, 7,
                             align="left", 
                             fill=0)) %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() + 
  geom_line(aes(y = seven_avg), 
            color = "red", 
            size = .75) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90))

crashes_ts %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() +
  geom_line(aes(y=rollmean(count, 7, na.pad = TRUE)), color = "red") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes With 7 Day Moving Average (With Pandemic Data)")

#30 day moving average 
crashes_ts %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() +
  geom_line(aes(y=rollmean(count, 30, na.pad = TRUE)), color = "red") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes With 30 Day Moving Average (With Pandemic Data)")

#100 day moving average 
crashes_ts %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() +
  geom_line(aes(y=rollmean(count, 100, na.pad = TRUE)), color = "red") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes With 100 Day Moving Average (With Pandemic Data)")

# Annual crashes time series
crashes_annual %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  ggtitle("Time Series Plots for Frequency of Daily Crashes Organized by Year") + 
  geom_line() + 
  geom_line(aes(y=rollmean(count, 7, na.pad = TRUE)), color = "red") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") + 
  xlab("Date") + ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Year, scales = "free_x")

#Different attempt at forecasting daily (THIS WORKS) 
#Loading necessary packages 
library(forecast)
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
  xlab("Date") + ylab("Count") +
  ggtitle("Number of Daily Car Crashes") +
  guides(colour=guide_legend(title=" ")) + 
  scale_x_continuous(breaks = c(0, 365, 731, 1096, 1461, 1826, 2192), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90))

#Prediction with HoltWinters() and plotting forecast
covid <- HoltWinters(crashests2)

summary(covid)

plot(fitted(covid), main = "Box Jenkins Decomposition of Daily Crashes (With Pandemic Data)")

fcast <- forecast::forecast(covid, h=214)

daily_forecast_values_covid_HW <- summary(fcast)

rownames(daily_forecast_values_covid_HW) <- seq(as.Date("2021/06/01"), 
                                                as.Date("2021/12/31"), "day")

daily_forecast_values_covid_HW

autoplot(fcast) +
  ggtitle("Forecasts of Daily Car Crashes Using Holt Winters (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes")

#STLF forecasting
crashests2 %>% mstl() %>%
  autoplot()

m <- crashests2 %>%
  stlf(lambda = 0, h = 214) 

m %>%
  autoplot() + 
  ggtitle("Seasonal and Trend Decomposition Using Loess Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes")

daily_forecast_values_covid_STLF <- summary(m)

rownames(daily_forecast_values_covid_STLF) <- seq(as.Date("2021/06/01"), 
                                                as.Date("2021/12/31"), "day")

#TBATS forecasting 
y <- msts(crashes_ts$count, seasonal.periods=c(7,365.25))
y2 <- msts(crashests2, seasonal.periods=c(7,365.25))

fit_tbats_covid <- tbats(y)
fit_tbats_covid2 <- tbats(y2)

fc_tbats_covid <- forecast::forecast(fit_tbats_covid, h=214)
fc_tbats_covid2 <- forecast::forecast(fit_tbats_covid2, h=214)

daily_forecast_values_covid_TBATS <- summary(fc_tbats_covid)
daily_forecast_values_covid_TBATS2 <- summary(fc_tbats_covid2)

rownames(daily_forecast_values_covid_TBATS) <- seq(as.Date("2021/06/01"), 
                                                  as.Date("2021/12/31"), "day")
rownames(daily_forecast_values_covid_TBATS2) <- seq(as.Date("2021/06/01"), 
                                                  as.Date("2021/12/31"), "day")

head(daily_forecast_values_covid_TBATS)
head(daily_forecast_values_covid_TBATS2)

autoplot(fc_tbats_covid) + 
  ggtitle("TBATS Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle=90))
autoplot(fc_tbats_covid2) + 
  ggtitle("TBATS Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes")

####Working with non COVID time series daily (THIS WORKS)
#Regular non COVID time series daily 
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/02/29") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))

crashes_ts.noncovid %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes (Without Pandemic Data") + 
  xlab("Date") + ylab("Count") + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90))

#Converting crashes_ts to a time series object 
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), end = c(2020,60),
                 frequency = 365)

#Converting crashes_ts to a time series object 
crashests.noncovid2 <- as.ts(crashes_ts.noncovid, start = c(2015,1), end = c(2020,60), 
                            frequency = 365)

#Least squares estimation
fit.crashes.noncovid2 <- tslm(count ~ Date, data=crashests.noncovid2)
summary(fit.crashes.noncovid2)

#Fitted values (linear plot through ts)
autoplot(crashests.noncovid2[,'count'], series="Data") +
  autolayer(fitted(fit.crashes.noncovid2), series="Fitted") +
  xlab("Date") + ylab("Count") +
  ggtitle("Number of Daily Car Crashes (Without Pandemic Data)") +
  guides(colour=guide_legend(title=" ")) + 
  scale_x_continuous(breaks = c(0, 365, 731, 1096, 1461, 1826, 2192), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90))

#Prediction with HoltWinters() and plotting forecast
noncovid <- HoltWinters(crashests.noncovid)

summary(noncovid)

plot(fitted(noncovid), main = "Box Jenkins Decomposition of Daily Crashes (Without Pandemic Data)") 

fcast.noncovid <- forecast(noncovid, 671)

daily_forecast_values_noncovid_HW <- summary(fcast.noncovid)

rownames(daily_forecast_values_noncovid_HW) <- seq(as.Date("2020/03/01"), 
                                                  as.Date("2021/12/31"), "day")

daily_forecast_values_noncovid_HW

true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2020/03/01") & Date <= as.Date("2021/05/31"))

true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                               start = c(2020,3), end = c(2021,153), 
                               frequency = 365)

autoplot(fcast.noncovid) +
  autolayer(true_noncovid_crashes, alpha=0.8) + 
  ggtitle("Forecasts of Daily Car Crashes Using HoltWinters (Without Pandemic Data)") +
  xlab("Year") + ylab("Crashes") + 
  theme(legend.position = "bottom")

#STLF forecasting 
crashests.noncovid %>% mstl() %>%
  autoplot()

p <- crashests.noncovid %>%
  stlf(lambda = 0, h = 671) 

true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2020/03/01") & Date <= as.Date("2021/05/31"))

true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                               start = c(2020,3), end = c(2021,153), 
                               frequency = 365)

p %>%
  autoplot() + 
  autolayer(true_noncovid_crashes, alpha=0.8) + 
  ggtitle("Seasonal and Trend Decomposition Using Loess Forecasting Model for Daily Car Crashes (Without Pandemic Data)") +
  xlab("Year") + ylab("Daily Crashes") + 
  theme(legend.position = "bottom")

daily_forecast_values_noncovid_STLF <- summary(p)

rownames(daily_forecast_values_noncovid_STLF) <- seq(as.Date("2020/03/01"), 
                                                   as.Date("2021/12/31"), "day")

#TBATS forecasting noncovid 
y.noncovid <- msts(crashes_ts.noncovid$count, seasonal.periods=c(7,365.25))
y2.noncovid <- msts(crashests.noncovid, seasonal.periods=c(7,365.25))

fit_tbats_noncovid <- tbats(y.noncovid)
fit_tbats_noncovid2 <- tbats(y2.noncovid)

fc_tbats_noncovid <- forecast::forecast(fit_tbats_noncovid, h=671)
fc_tbats_noncovid2 <- forecast::forecast(fit_tbats_noncovid2, h=671)

daily_forecast_values_noncovid_TBATS <- summary(fc_tbats_noncovid)
daily_forecast_values_noncovid_TBATS2 <- summary(fc_tbats_noncovid2)

rownames(daily_forecast_values_noncovid_TBATS) <- seq(as.Date("2020/03/01"), 
                                                      as.Date("2021/12/31"), "day")
rownames(daily_forecast_values_noncovid_TBATS2) <- seq(as.Date("2020/03/01"), 
                                                       as.Date("2021/12/31"), "day")

head(daily_forecast_values_noncovid_TBATS)
head(daily_forecast_values_noncovid_TBATS2)

true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2020/03/01") & Date <= as.Date("2021/05/31"))

true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2020,3), end = c(2021,153), 
                            frequency = 365)

autoplot(fc_tbats_noncovid) + 
  #autolayer(true_noncovid_crashes, alpha=0.8) + 
  ggtitle("TBATS Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom")
autoplot(fc_tbats_noncovid2) + 
  autolayer(true_noncovid_crashes, alpha=0.8) + 
  ggtitle("TBATS Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes") + 
  theme(legend.position = "bottom")

##Multi-step forecasts on training data (daily)
#HW (with covid)
training_HW <- subset(crashests2, end=length(crashests2)-214)
test_HW <- subset(crashests2, start=length(crashests2)-213)
crashes.train_HW <- HoltWinters(training_HW)
crashes.train_HW %>%
  forecast(h=214) %>%
  autoplot() + autolayer(test_HW, alpha=0.7)

autoplot(training_HW, series="Training data") +
  autolayer(fitted(crashes.train_HW, h=12),
            series="12-step fitted values")

#crashes.test_HW <- HoltWinters(test_HW)
#accuracy(crashes.test_HW)

#STLF (with covid)
training_STLF <- subset(crashests2, end=length(crashests2)-214)
test_STLF <- subset(crashests2, start=length(crashests2)-213)
crashes.train_STLF <- stlf(training_STLF, lambda = 0, h = 214)
crashes.train_STLF %>%
  forecast(h=214) %>%
  autoplot() + autolayer(test_STLF, alpha = 0.6)

autoplot(training_STLF, series="Training data") +
  autolayer(fitted(crashes.train_STLF, h=12),
            series="12-step fitted values")

#crashes.test_STLF <- stlf(test_STLF)
#accuracy(crashes.test_STLF)

#TBATS (with covid)
y <- msts(crashes_ts$count, seasonal.periods=c(7,365.25))
y2 <- msts(crashests2, seasonal.periods=c(7,365.25))

training_TBATS <- subset(y, end=length(y)-151)
training_TBATS2 <- subset(y2, end=length(y2)-151)

test_TBATS <- subset(y, start=length(y)-150)
test_TBATS2 <- subset(y2, start=length(y2)-150)

crashes.train_TBATS <- tbats(training_TBATS)
crashes.train_TBATS2 <- tbats(training_TBATS2)

crashes.train_TBATS %>%
  forecast(h=151) %>%
  autoplot(alpha=0.2) + 
  autolayer(test_TBATS, alpha=0.75) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom") + 
  ggtitle("Multi-Step TBATS Daily Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") 
crashes.train_TBATS2 %>%
  forecast(h=151) %>%
  autoplot(alpha=0.2) + 
  ggtitle("Multi-Step TBATS Daily Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  autolayer(test_TBATS2, alpha=0.75) + 
  theme(legend.position = "bottom")

autoplot(training_TBATS, series="Training data") +
  autolayer(fitted(crashes.train_TBATS, h=12),
            series="12-step fitted values")
autoplot(training_TBATS2, series="Training data") +
  autolayer(fitted(crashes.train_TBATS2, h=12),
            series="12-step fitted values")

crashes.test_TBATS <- tbats(test_TBATS)
crashes.test_TBATS2 <- tbats(test_TBATS2)

accuracy(crashes.test_TBATS)
accuracy(crashes.test_TBATS2)

#HW (noncovid)
training_HW_noncovid <- subset(crashests.noncovid , end=length(crashests.noncovid)-671)
test_HW_noncovid <- subset(crashests.noncovid, start=length(crashests.noncovid)-670)
crashes.train_HW_noncovid <- HoltWinters(training_HW_noncovid)
crashes.train_HW_noncovid %>%
  forecast(h=671) %>%
  autoplot() + 
  autolayer(test_HW_noncovid)

autoplot(training_HW_noncovid, series="Training data") +
  autolayer(fitted(crashes.train_HW_noncovid, h=12),
            series="12-step fitted values")

#crashes.test_HW_noncovid <- HoltWinters(test_HW_noncovid)
#accuracy(crashes.test_HW_noncovid)

#STLF (non covid)
training_STLF_noncovid <- subset(crashests.noncovid, end=length(crashests.noncovid)-671)
test_STLF_noncovid <- subset(crashests.noncovid, start=length(crashests.noncovid)-670)
crashes.train_STLF_noncovid <- stlf(training_STLF_noncovid, lambda = 0, h = 671)
crashes.train_STLF_noncovid %>%
  forecast(h=671) %>%
  autoplot() + autolayer(test_STLF_noncovid, alpha = 0.6)

autoplot(training_STLF_noncovid, series="Training data") +
  autolayer(fitted(crashes.train_STLF_noncovid, h=12),
            series="12-step fitted values")

#crashes.test_STLF_noncovid <- stlf(test_STLF_noncovid)
#accuracy(crashes.test_STLF_noncovid)

#TBATS (noncovid)
y.noncovid <- msts(crashes_ts.noncovid$count, seasonal.periods=c(7,365.25))
y2.noncovid <- msts(crashests.noncovid, seasonal.periods=c(7,365.25))

training_TBATS.noncovid <- subset(y.noncovid, end=length(y.noncovid)-671)
training_TBATS2.noncovid <- subset(y2.noncovid, end=length(y2.noncovid)-671)

test_TBATS.noncovid <- subset(y.noncovid, start=length(y.noncovid)-670)
test_TBATS2.noncovid <- subset(y2.noncovid, start=length(y2.noncovid)-670)

crashes.train_TBATS.noncovid <- tbats(training_TBATS.noncovid)
crashes.train_TBATS2.noncovid <- tbats(training_TBATS2.noncovid)

crashes.train_TBATS.noncovid %>%
  forecast(h=671) %>%
  autoplot(alpha=0.2) + 
  autolayer(test_TBATS.noncovid, alpha=0.75) +
  theme(legend.position = "bottom") + 
  ggtitle("Multi-Step TBATS Daily Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") 
crashes.train_TBATS2.noncovid %>%
  forecast(h=671) %>%
  autoplot(alpha=0.2) + 
  ggtitle("Multi-Step TBATS Daily Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  autolayer(test_TBATS2.noncovid, alpha=0.75) + 
  theme(legend.position = "bottom")

autoplot(training_TBATS.noncovid, series="Training data") +
  autolayer(fitted(crashes.train_TBATS.noncovid, h=12),
            series="12-step fitted values")
autoplot(training_TBATS2.noncovid, series="Training data") +
  autolayer(fitted(crashes.train_TBATS2.noncovid, h=12),
            series="12-step fitted values")

crashes.test_TBATS.noncovid <- tbats(test_TBATS.noncovid)
crashes.test_TBATS2.noncovid <- tbats(test_TBATS2.noncovid)

accuracy(crashes.test_TBATS.noncovid)
accuracy(crashes.test_TBATS2.noncovid)

##Forecast Combinations (daily covid) (WORKS)
library(forecast)
train <- window(crashests2, end=c(2019,12))
h <- length(crashests2) - length(train)
#ETS <- forecast(ets(train), h=h)
ARIMA <- forecast::forecast(auto.arima(train, lambda=0, biasadj=TRUE), h=h)
STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)
NNAR <- forecast::forecast(nnetar(train), h=h)
TBATS <- forecast::forecast(tbats(train, biasadj=TRUE), h=h)

Combination <- (ARIMA[["mean"]] + TBATS[["mean"]] + STL[["mean"]] + 
                  NNAR[["mean"]])/4

true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2021/05/31"))

true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2019,1), end = c(2021,153), 
                            frequency = 365)

library(RColorBrewer)

autoplot(train) +
  autolayer(true_noncovid_crashes) +
  autolayer(NNAR, series="NNAR", alpha=0.8) +
  autolayer(STL, series="STL", PI=F, alpha=0.8) +
  autolayer(Combination, series="Combination", alpha=0.8) +
  autolayer(ARIMA, series="ARIMA", PI=F) +
  autolayer(TBATS, series="TBATS", PI=F) +
  scale_color_brewer(palette="RdYlBu") + 
  xlab("Year") + ylab("Crashes") +
  ggtitle("Time Series Combination (With Pandemic Data)")

c(ARIMA = accuracy(ARIMA, crashests2)["Test set", "RMSE"],
  TBATS = accuracy(TBATS, crashests2)["Test set", "RMSE"],
  NNAR = accuracy(NNAR, crashests2)["Test set", "RMSE"],
  STL = accuracy(STL, crashests2)["Test set", "RMSE"],
  Combination =
    accuracy(Combination, crashests2)["Test set", "RMSE"])

##Forecast Combinations (daily noncovid) (WORKS)
library(forecast)
train.noncovid <- window(crashests2, end=c(2019,12))
h.noncovid <- 426
ARIMA.noncovid <- forecast::forecast(auto.arima(train.noncovid, lambda=0, 
                                                biasadj=TRUE), h=h.noncovid)
STL.noncovid <- stlf(train.noncovid, lambda=0, h=h.noncovid, biasadj=TRUE)
NNAR.noncovid <- forecast::forecast(nnetar(train.noncovid), h=h.noncovid)
TBATS.noncovid <- forecast::forecast(tbats(train.noncovid, biasadj=TRUE), 
                                     h=h.noncovid)

Combination.noncovid <- (ARIMA.noncovid[["mean"]] + TBATS.noncovid[["mean"]] + STL.noncovid[["mean"]] + 
                  NNAR.noncovid[["mean"]])/4

true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2020/03/01"))

true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2019,1), end = c(2020,60), 
                            frequency = 365)

library(RColorBrewer)

autoplot(train.noncovid) +
  autolayer(true_noncovid_crashes) +
  autolayer(NNAR.noncovid, series="NNAR", alpha=0.8) +
  autolayer(STL.noncovid, series="STL", PI=F, alpha=0.8) +
  autolayer(Combination.noncovid, series="Combination", alpha=0.8) +
  autolayer(ARIMA.noncovid, series="ARIMA", PI=F) +
  autolayer(TBATS.noncovid, series="TBATS", PI=F) +
  scale_color_brewer(palette="RdYlBu") + 
  xlab("Year") + ylab("Crashes") +
  ggtitle("Time Series Combination (Without Pandemic Data)") + 
  theme(legend.position = "bottom")

c(ARIMA = accuracy(ARIMA.noncovid, crashests2)["Test set", "RMSE"],
  TBATS = accuracy(TBATS.noncovid, crashests2)["Test set", "RMSE"],
  NNAR = accuracy(NNAR.noncovid, crashests2)["Test set", "RMSE"],
  STL = accuracy(STL.noncovid, crashests2)["Test set", "RMSE"],
  Combination =
    accuracy(Combination.noncovid, crashests2)["Test set", "RMSE"])


##Working with ets() daily (doesn't work)
#(THIS GETS RID OF SEASONALITY DUE TO TOO HIGH OF FREQUENCY)
fit.ets.daily.covid <- ets(y=crashests2, model="MAM")

summary(fit.ets.daily.covid)

autoplot(fit.ets.daily.covid)

cbind('Residuals' = residuals(fit.ets.daily.covid),
      'Forecast errors' = residuals(fit.ets.daily.covid,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

fit.ets.daily.covid %>% forecast(h=214) %>%
  autoplot()

##Looking at seasonality of data 
#Working with TBATS daily 
#Do I use crashes_ts (like I did with the ts() func or should I use crashests2?)
y <- msts(crashes_ts$count, seasonal.periods=c(7,365.25))
y2 <- msts(crashests2, seasonal.periods=c(7,365.25))
fit <- tbats(y)
fc <- forecast(fit, h=214)
summary(fc) 
plot(fc)

#Working with ARIMA (doesn't work)
y <- ts(crashes_ts, frequency=7)
z <- fourier(ts(crashes_ts, frequency=365.25), K=5)
zf <- fourier(ts(crashes_ts, frequency=365.25), K=5, h=100)
fit <- auto.arima(y, xreg=cbind(z,crashes_rain), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf,holidayf), h=100) #we need future covariate values


#Time Series with Covariates 
#below does not work 
d <- crashes_ts$Injury
fit <- auto.arima(crashests2, xreg=d) # It finds a ARMA(1,0,2) is best.
checkresiduals(fit)

fourier.crashes <- tslm(crashests2 ~ trend + fourier(crashests2, K=2))
summary(fourier.crashes)

p <- crashes$Injury

fit.crashes <- tslm(crashests2 ~ p)

#testing ARIMA - doesnt work 
fit <- auto.arima(crashes[,"Injury"], seasonal=FALSE)
fit %>% forecast(h=10) %>% autoplot(include=80)

#tslm 
fit.crashes <- tslm(crashes_ts~crashes_rain)

#possible covariates?
crashes_rain = crashes %>%
  filter(WeatherCondition1 != "Unknown", WeatherCondition1 != "NA", 
         WeatherCondition1 != "") %>%
  mutate(WeatherCondition1 = case_when(
    WeatherCondition1 == "Rain" ~ "Rain", 
    TRUE ~ "Not Rain")) %>%
  group_by(key_crash,Crash_Date_DOW) %>%
  summarize(count = length(key_crash))

crashes_Friday = crashes %>%
  filter(Crash_Date_DOW != "Unknown", Crash_Date_DOW != "NA", 
         Crash_Date_DOW != "") %>%
  mutate(Crash_Date_DOW = case_when(
    Crash_Date_DOW == "Friday" ~ "Friday", 
    TRUE ~ "Not Friday")) %>%
  group_by(key_crash,Crash_Date_DOW) %>%
  summarize(count = length(key_crash))

##Adding dummy variables to dataset 
#snow
crashes_snow = crashes %>%
  mutate(Snow = case_when(
    WeatherCondition1 == "Snow" ~ 1, 
    TRUE ~ 0)) 

crashes_snow = crashes_snow %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date,Snow) %>% #am I grouping by the right things? 
  summarize(count = length(unique(key_crash)))

#Converting crashes_snow to a time series object 
crashes_snow_ts2 <- ts(crashes_snow$count, start = c(2015,1), end = c(2021,153),
                 frequency = 365)

#Converting crashes_snow to a time series object (not the method to use)
crashes_snow_ts <- as.ts(crashes_snow)

##attempting blogpost code (works)
y <- ts(crashes_snow$count, frequency=7)
z <- fourier(ts(crashes_snow$count, frequency=365.25), K=5)
zf <- fourier(ts(crashes_snow$count, frequency=365.25), K=5, h=100)
fit <- auto.arima(y, xreg=cbind(z), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf), h=100)

autoplot(fc)

##attempting textbook (Works)
#crashes_snow_ts3 <- window(crashes_snow)
crashes_snow_ts2 <- ts(crashes_snow$count, start = c(2015,1), end = c(2021,153),
                       frequency = 365)
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(crashes_snow_ts2, xreg = fourier(crashes_snow_ts2, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(crashes_snow_ts2, K=i, h=214))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("")
}

#loading necessary packages
#install.packages("gridExtra")
library(gridExtra)

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)

#diff attempt (TAKES A LONG TIME (because K=1-25) BUT IT DOES WORK)
bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(crashes_snow_ts2, xreg=fourier(crashes_snow_ts2, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}

fc <- forecast(bestfit,
               xreg=fourier(crashes_snow_ts2, K=bestK, h=214))
autoplot(fc)

##friday dummy variable
crashes_friday = crashes %>%
  mutate(Friday = case_when(
    Crash_Date_DOW == "Friday" ~ 1, 
    TRUE ~ 0)) 

crashes_friday = crashes_friday %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date,Friday) %>% #am I grouping by the right things? 
  summarize(count = length(unique(key_crash)))

#Converting crashes_ts to a time series object 
crashes_friday_ts2 <- ts(crashes_friday$count, start = c(2015,1), end = c(2021,153),
                         frequency = 365)

#Converting crashes_ts to a time series object 
crashes_friday_ts <- as.ts(crashes_friday)

##attempting blogpost 
y <- ts(crashes_friday$count, frequency=7)
z <- fourier(ts(crashes_friday$count, frequency=365.25), K=5)
zf <- fourier(ts(crashes_friday$count, frequency=365.25), K=5, h=100)
fit <- auto.arima(y, xreg=cbind(z), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf), h=100)
autoplot(fc)

##attempting textbook 
#crashes_friday_ts3 <- window(crashes_friday)
crashes_friday_ts2 <- ts(crashes_friday$count, start = c(2015,1), end = c(2021,153),
                       frequency = 365)
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(crashes_friday_ts2, xreg = fourier(crashes_friday_ts2, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(crashes_friday_ts2, K=i, h=214))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("")
}

#loading necessary packages
#install.packages("gridExtra")
library(gridExtra)

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)

#diff attempt (TAKES A LONG TIME BUT WORKS)
bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(crashes_friday_ts2, xreg=fourier(crashes_friday_ts2, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}

fc <- forecast(bestfit,
               xreg=fourier(crashes_friday_ts2, K=bestK, h=214))
autoplot(fc)

##friday and snow together 
crashes_snow_friday = crashes %>%
  mutate(Friday = case_when(
    Crash_Date_DOW == "Friday" ~ 1, 
    TRUE ~ 0)) %>%
  mutate(Snow = case_when(
    WeatherCondition1 == "Snow" ~ 1, 
    TRUE ~ 0))

crashes_snow_friday = crashes_snow_friday %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date,Friday,Snow) %>% #am I grouping by the right things? 
  summarize(count = length(unique(key_crash)))

#Converting crashes_ts to a time series object 
crashes_snow_friday_ts2 <- ts(crashes_snow_friday$count, start = c(2015,1), end = c(2021,153),
                         frequency = 365)

#Converting crashes_ts to a time series object 
crashes_snow_friday_ts <- as.ts(crashes_snow_friday)

##attempting blogpost 
y <- ts(crashes_snow_friday$count, frequency=7)
z <- fourier(ts(crashes_snow_friday$count, frequency=365.25), K=5)
zf <- fourier(ts(crashes_snow_friday$count, frequency=365.25), K=5, h=100)
fit <- auto.arima(y, xreg=cbind(z), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf), h=100)
autoplot(fc)

##attempting textbook 
#crashes_friday_ts3 <- window(crashes_friday)
crashes_snow_friday_ts2 <- ts(crashes_snow_friday$count, start = c(2015,1), end = c(2021,153),
                         frequency = 365)
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(crashes_snow_friday_ts2, xreg = fourier(crashes_snow_friday_ts2, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(crashes_snow_friday_ts2, K=i, h=214))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("")
}

#loading necessary packages
#install.packages("gridExtra")
library(gridExtra)

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)

#diff attempt (TAKES A LONG TIME BUT WORKS)
bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(crashes_snow_friday_ts2, xreg=fourier(crashes_snow_friday_ts2, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}

fc <- forecast(bestfit,
               xreg=fourier(crashes_snow_friday_ts2, K=bestK, h=214))
autoplot(fc)

##Different attempt at forecasting for monthly (THIS WORKS)
#install.packages("Mcomp")
#install.packages("smooth")
library(Mcomp)
library(smooth)

#Working with entire time series monthly (THIS Works)
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

crashes_mts_test = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash))) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "/") %>%
  group_by(Year, Month) %>%
  summarise(mcount = sum(count))

crashes_mts_test %>%
  mutate(date = seq(as.Date(2015/01/01), as.Date(2021/05/01), Month))

crashes_mts_test %>%
  ggplot(aes(x = Month, y = mcount)) + 
  geom_line() +
  geom_line(aes(y=rollmean(mcount, 2, na.pad = TRUE)), color = "red") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes With 7 Day Moving Average (With Pandemic Data)")


crashes_mts = as.data.frame(crashes_mts)

plot(crashes_mts)

rownames(crashes_mts) = seq(2015, 2021)

crashes_mts3 = crashes_mts %>%
  select(02:13)

crashes_mts2 <- ts(c(t(crashes_mts3)), frequency=12)

crashes_mts4 <- window(crashes_mts2, start=c(1,01), end=c(7,05), frequency=12)

fit.crashes <- tslm(crashes_mts4 ~ trend + season)

summary(fit.crashes)

fcast2 <- forecast::forecast(fit.crashes, h=8)

monthly_forecast_values_covid_lin <- summary(fcast2)

rownames(monthly_forecast_values_covid_lin) <- seq(as.Date("2021/05/31"), 
                                                     as.Date("2021/12/31"), 
                                                   "month")

autoplot(fcast2) +
  ggtitle("Forecasts of Monthly Car Crashes Using Linear Model (With Pandemic Data)") +
  xlab("Year") + ylab("Monthly Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90)) 

#Linear model with non COVID time series monthly (THIS Works)
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

fcast.noncovid <- forecast::forecast(fit.crashes.noncovid, h=23)

monthly_forecast_values_noncovid_lin <- summary(fcast.noncovid)

rownames(monthly_forecast_values_noncovid_lin) <- seq(as.Date("2020/02/29"), 
                                                   as.Date("2021/12/31"), "month")

autoplot(fcast.noncovid) +
  ggtitle("Forecasts of Monthly Car Crashes Using Linear Model (Without Pandemic Data)") +
  xlab("Year") + ylab("Monthly Crashes") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))

#Holtwinters with COVID time series monthly (THIS Works)
covid.monthly <- HoltWinters(crashes_mts4)

summary(covid.monthly)

plot(fitted(covid.monthly), main = "Box Jenkins Decomposition of Monthly Crashes (With Pandemic Data)") 

fcast.covid.monthly <- forecast::forecast(covid.monthly, 8)

monthly_forecast_values_covid_HW <- summary(fcast.covid.monthly)

rownames(monthly_forecast_values_covid_HW) <- seq(as.Date("2021/05/31"), 
                                                      as.Date("2021/12/31"), "month")

autoplot(fcast.covid.monthly) +
  ggtitle("Forecasts of Monthly Car Crashes Using HoltWinters (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))

#Holtwinters with COVID time series monthly with parameters 
covid.monthly.p <- HoltWinters(crashes_mts4, alpha = 0.8161, beta = 1e-04, 
                             gamma = 1e-04)

summary(covid.monthly.p)

plot(fitted(covid.monthly.p), main = "Box Jenkins Decomposition of Monthly Crashes (With Pandemic Data)") 

fcast.covid.monthly.p <- forecast::forecast(covid.monthly.p, 8)

monthly_forecast_values_covid_HW.p <- summary(fcast.covid.monthly.p)

rownames(monthly_forecast_values_covid_HW.p) <- seq(as.Date("2021/05/31"), 
                                                  as.Date("2021/12/31"), "month")

autoplot(fcast.covid.monthly.p) +
  ggtitle("Forecasts of Monthly Car Crashes Using HoltWinters (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))

#Holtwinters with nonCOVID time series monthly (THIS Works)
noncovid.monthly <- HoltWinters(crashes_mts4.noncovid)

summary(noncovid.monthly)

plot(fitted(noncovid.monthly), main = "Box Jenkins Decomposition of Monthly Crashes (Without Pandemic Data)") 

fcast.noncovid.monthly <- forecast::forecast(noncovid.monthly, 23)

monthly_forecast_values_noncovid_HW <- summary(fcast.noncovid.monthly)

rownames(monthly_forecast_values_noncovid_HW) <- seq(as.Date("2020/02/29"), 
                                                      as.Date("2021/12/31"), "month")

autoplot(fcast.noncovid.monthly) +
  ggtitle("Forecasts of Monthly Car Crashes Using HoltWinters (Without Pandemic Data)") +
  xlab("Year") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))

#Holtwinters with nonCOVID time series monthly with parameters
noncovid.monthly.p <- HoltWinters(crashes_mts4.noncovid, alpha = 0.2493, 
                                beta = 0.0056, gamma = 0.0038)

summary(noncovid.monthly.p)

plot(fitted(noncovid.monthly.p), main = "Box Jenkins Decomposition of Monthly Crashes (Without Pandemic Data)") 

fcast.noncovid.monthly.p <- forecast::forecast(noncovid.monthly.p, 23)

monthly_forecast_values_noncovid_HW.p <- summary(fcast.noncovid.monthly.p)

rownames(monthly_forecast_values_noncovid_HW.p) <- seq(as.Date("2020/02/29"), 
                                                     as.Date("2021/12/31"), "month")

autoplot(fcast.noncovid.monthly.p) +
  ggtitle("Forecasts of Monthly Car Crashes Using HoltWinters (Without Pandemic Data)") +
  xlab("Year") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))

#Stlf Forecasting Model of Monthly Crashes (With Pandemic Data)
crashes_mts4 %>% mstl() %>%
  autoplot()

t <- crashes_mts4 %>%
  stlf(lambda = 0, h = 8) 

t %>%
  autoplot() + 
  ggtitle("Seasonal and Trend Decomposition Using Loess Forecasting Model for Monthly Car Crashes (With Pandemic Data)") +
  xlab("Year") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8, 9), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))

monthly_forecast_values_covid_STLF <- summary(t)

rownames(monthly_forecast_values_covid_STLF) <- seq(as.Date("2021/05/31"), 
                                                     as.Date("2021/12/31"), "month")

#Stlf Forecasting Model of Monthly Crashes (Without Pandemic Data)
crashes_mts4.noncovid %>% mstl() %>%
  autoplot()

q <- crashes_mts4.noncovid %>%
  stlf(lambda = 0, h = 23) 

q %>%
  autoplot() + 
  ggtitle("Seasonal and Trend Decomposition Using Loess Forecasting Model for Monthly Car Crashes (Without Pandemic Data)") +
  xlab("Year") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))

monthly_forecast_values_noncovid_STLF <- summary(q)

rownames(monthly_forecast_values_noncovid_STLF) <- seq(as.Date("2020/02/29"), 
                                                     as.Date("2021/12/31"), "month")

##Multi-step forecasts on training data (monthly)
#HW (with covid)
training_HW_monthly <- subset(crashes_mts4, end=length(crashes_mts4)-8)
test_HW_monthly <- subset(crashes_mts4, start=length(crashes_mts4)-7)
crashes.train_HW_monthly <- HoltWinters(training_HW_monthly)
crashes.train_HW_monthly %>%
  forecast(h=8) %>%
  autoplot() + autolayer(test_HW_monthly)

autoplot(training_HW_monthly, series="Training data") +
  autolayer(fitted(crashes.train_HW_monthly, h=12),
            series="12-step fitted values")

#crashes.test_HW_monthly <- HoltWinters(test_HW_monthly)
#accuracy(crashes.test_HW_monthly)

#STLF (with covid)
training_STLF_monthly <- subset(crashes_mts4, end=length(crashes_mts4)-8)
test_STLF_monthly <- subset(crashes_mts4, start=length(crashes_mts4)-7)
crashes.train_STLF_monthly <- stlf(training_STLF_monthly, lambda = 0, h = 8)
crashes.train_STLF_monthly %>%
  forecast(h=8) %>%
  autoplot() + autolayer(test_STLF_monthly)

autoplot(training_STLF_monthly, series="Training data") +
  autolayer(fitted(crashes.train_STLF_monthly, h=12),
            series="12-step fitted values")

#crashes.test_STLF_monthly <- stlf(test_STLF_monthly)
#accuracy(crashes.test_STLF_monthly)

#HW (noncovid)
training_HW_noncovid_monthly <- subset(crashes_mts4.noncovid, end=length(crashes_mts4.noncovid)-23)
test_HW_noncovid_monthly <- subset(crashes_mts4.noncovid, start=length(crashes_mts4.noncovid)-22)
crashes.train_HW_noncovid_monthly <- HoltWinters(training_HW_noncovid_monthly)
crashes.train_HW_noncovid_monthly %>%
  forecast(h=23) %>%
  autoplot() + autolayer(test_HW_noncovid_monthly)

autoplot(training_HW_noncovid_monthly, series="Training data") +
  autolayer(fitted(crashes.train_HW_noncovid_monthly, h=12),
            series="12-step fitted values")

#crashes.test_HW_noncovid_monthly <- HoltWinters(test_HW_noncovid_monthly)
#accuracy(crashes.test_HW_noncovid_monthly)

#STLF (non covid)
training_STLF_noncovid_monthly <- subset(crashes_mts4.noncovid, end=length(crashes_mts4.noncovid)-23)
test_STLF_noncovid_monthly <- subset(crashes_mts4.noncovid, start=length(crashes_mts4.noncovid)-22)
crashes.train_STLF_noncovid_monthly <- stlf(training_STLF_noncovid_monthly, lambda = 0, h = 23)
crashes.train_STLF_noncovid_monthly %>%
  forecast(h=23) %>%
  autoplot() + autolayer(test_STLF_noncovid_monthly)

autoplot(training_STLF_noncovid_monthly, series="Training data") +
  autolayer(fitted(crashes.train_STLF_noncovid_monthly, h=12),
            series="12-step fitted values")

#crashes.test_STLF_noncovid_monthly <- stlf(test_STLF_noncovid_monthly)
#accuracy(crashes.test_STLF_noncovid_monthly)

## Forecast combination (monthly covid) (DOES NOT WORK)
train_monthly <- window(crashes_mts4, end=c(2018,12))
h_monthly <- length(crashes_mts4) - length(train_monthly)
#ETS <- forecast(ets(train), h=h)
ARIMA_monthly <- forecast(auto.arima(train_monthly, lambda=0, biasadj=TRUE), h=h_monthly)
STL_monthly <- stlf(train_monthly, lambda=0, h=h_monthly, biasadj=TRUE)
NNAR_monthly <- forecast(nnetar(train_monthly), h=h_monthly)
TBATS_monthly <- forecast(tbats(train_monthly, biasadj=TRUE), h=h_monthly)

Combination_monthly <- (ARIMA_monthly[["mean"]] + TBATS_monthly[["mean"]] + 
                          STL_monthly[["mean"]] + NNAR_monthly[["mean"]])/4

autoplot(train_monthly) +
  autolayer(NNAR_monthly, series="NNAR") +
  autolayer(STL_monthly, series="STL", PI=F) +
  autolayer(Combination_monthly, series="Combination") +
  autolayer(ARIMA_monthly, series="ARIMA", PI=F) +
  autolayer(TBATS_monthly, series="TBATS", PI=F) +
  scale_color_brewer(palette="RdYlBu") + 
  xlab("Year") + ylab("Crashes") +
  ggtitle("Crashes")

c(ARIMA_monthly = accuracy(ARIMA_monthly, crashes_mts4)["Test set", "RMSE"],
  TBATS_monthly = accuracy(TBATS_monthly, crashes_mts4)["Test set", "RMSE"],
  NNAR_monthly = accuracy(NNAR_monthly, crashes_mts4)["Test set", "RMSE"],
  STL_monthly = accuracy(STL_monthly, crashes_mts4)["Test set", "RMSE"],
  Combination_monthly =
    accuracy(Combination_monthly, crashes_mts4)["Test set", "RMSE"])

##Working with ets() monthly covid MAM model 
fit.ets.monthly.covid <- ets(y=crashes_mts4, model="MAM")

summary(fit.ets.monthly.covid)

autoplot(fit.ets.monthly.covid)

cbind('Residuals' = residuals(fit.ets.monthly.covid),
      'Forecast errors' = residuals(fit.ets.monthly.covid,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

fit.ets.monthly.covid %>% forecast(h=8) %>%
  autoplot()

##Working with ets() monthly noncovid MAM model 
fit.ets.monthly.noncovid <- ets(y=crashes_mts4.noncovid, model="MAM")

summary(fit.ets.monthly.noncovid)

autoplot(fit.ets.monthly.noncovid)

cbind('Residuals' = residuals(fit.ets.monthly.noncovid),
      'Forecast errors' = residuals(fit.ets.monthly.noncovid,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

fit.ets.monthly.noncovid %>% forecast(h=8) %>%
  autoplot()

##Working with ets() monthly MNM covid model 

fit.ets.monthly.covid.MNM <-ets(y=crashes_mts4, model="ZZM")
#automatically selects MNM

summary(fit.ets.monthly.covid.MNM)
#alpha = 0.3386, gamma = 2e-04

autoplot(fit.ets.monthly.covid.MNM)

cbind('Residuals' = residuals(fit.ets.monthly.covid.MNM),
      'Forecast errors' = residuals(fit.ets.monthly.covid.MNM,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

fit.ets.monthly.covid.MNM %>% forecast(h=8) %>%
  autoplot()

##Working with ets() monthly MMM covid model 

fit.ets.monthly.covid.MMM <-ets(y=crashes_mts4, model="MMM")

summary(fit.ets.monthly.covid.MMM)

autoplot(fit.ets.monthly.covid.MMM)

cbind('Residuals' = residuals(fit.ets.monthly.covid.MMM),
      'Forecast errors' = residuals(fit.ets.monthly.covid.MMM,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

fit.ets.monthly.covid.MMM %>% forecast(h=8) %>%
  autoplot()

##Working with ets() monthly MNA covid model 

fit.ets.monthly.covid.MNA <-ets(y=crashes_mts4, model="MNA")
summary(fit.ets.monthly.covid.MNA)

autoplot(fit.ets.monthly.covid.MNA)

cbind('Residuals' = residuals(fit.ets.monthly.covid.MNA),
      'Forecast errors' = residuals(fit.ets.monthly.covid.MNA,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

fit.ets.monthly.covid.MNA %>% forecast(h=8) %>%
  autoplot()

##Advanced Forecasting Methods

#NNAR method forecasting daily crashes (with pandemic data)
fit.nnetar <- nnetar(crashests2, lambda=0)

summary(fit.nnetar)

daily_forecast_values_covid_NNAR <- summary(forecast(fit.nnetar, h=214))

rownames(daily_forecast_values_covid_NNAR) <- seq(as.Date("2021/06/01"), 
                                                       as.Date("2021/12/31"), "day")

autoplot(forecast(fit.nnetar,h=214,lambda=0)) + 
  ggtitle("Neural Network Autoregression Forecasting Model of Daily Crashes (With Pandemic Data)")

#NNAR method forecasting daily crashes (without pandemic data)
fit.nnetar.noncovid <- nnetar(crashests.noncovid, lambda=0)

summary(fit.nnetar.noncovid)

daily_forecast_values_noncovid_NNAR <- summary(forecast(fit.nnetar.noncovid, h=671))

rownames(daily_forecast_values_noncovid_NNAR) <- seq(as.Date("2020/03/01"), 
                                                  as.Date("2021/12/31"), "day")

autoplot(forecast(fit.nnetar.noncovid,h=671,lambda=0)) + 
  ggtitle("Neural Network Autoregression Forecasting Model of Daily Crashes (Without Pandemic Data)")

#NNAR method forecasting monthly crashes (with pandemic data)
fit.nnetar.monthly <- nnetar(crashes_mts4, lambda=0)

summary(fit.nnetar.monthly)

forecast(fit.nnetar.monthly, h=8)

autoplot(forecast(fit.nnetar.monthly,h=20,lambda=0)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8, 9), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022", 
                                "Jan 2023")) + 
  theme(axis.text.x = element_text(angle = 90))

#NNAR method forecasting monthly crashes (without pandemic data)
fit.nnetar.monthly.noncovid <- nnetar(crashes_mts4.noncovid, lambda=0)

summary(fit.nnetar.monthly.noncovid)

forecast(fit.nnetar.monthly.noncovid, h=23)

autoplot(forecast(fit.nnetar.monthly.noncovid,h=23,lambda=0)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))

##Comparing Models 

#Basic 2015-2019 daily ts 
crashes_ts.2019 = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2019/12/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))

crashes_ts.2019 %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90))

#Linear model 2015-2019 daily ts 
crashests.2019 <- ts(crashes_ts.2019$count, start = c(2015,1), end = c(2019,365),
                     frequency = 365)

crashests.2019.2 <- as.ts(crashes_ts.2019, start = c(2015,1), end = c(2019,365), 
                          frequency = 365)

fit.crashes.2019.2 <- tslm(count ~ Date, data=crashests.2019.2)
summary(fit.crashes.2019.2)

autoplot(crashests.2019.2[,'count'], series="Data") +
  autolayer(fitted(fit.crashes.2019.2), series="Fitted") +
  xlab("Date") + ylab("") +
  ggtitle("2015-2019 Number of Daily Car Crashes") +
  guides(colour=guide_legend(title=" ")) + 
  scale_x_continuous(breaks = c(0, 365, 731, 1096, 1461, 1826), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020")) + 
  theme(axis.text.x = element_text(angle = 90))

#Holtwinters 2015-2019 daily ts 
twentynineteen <- HoltWinters(crashests.2019)

summary(twentynineteen)

plot(fitted(twentynineteen), main = "Box Jenkins Decomposition of Daily Crashes") 

fcast.2019 <- forecast(twentynineteen, 61)

twenty_nineteen_daily_forecast_values_HW <- summary(fcast.2019)

rownames(twenty_nineteen_daily_forecast_values_HW) <- seq(as.Date("2020/01/01"), 
                                                           as.Date("2020/03/01"), 
                                                          "day")
true_value = crashes_ts %>%
  filter(Date >= as.Date("2020/01/01") & Date <= as.Date("2020/03/01"))

twenty_nineteen_daily_forecast_values_HW <- twenty_nineteen_daily_forecast_values_HW %>%
  mutate(true_count = true_value$count)
                                                        
autoplot(fcast.2019) +
  ggtitle("2015-2019 Forecasts of Daily Car Crashes Using HoltWinters") +
  xlab("Year") + ylab("Crashes")

#Stlf model 2015-2019 daily ts 
crashests.2019 %>% mstl() %>%
  autoplot()

p <- crashests.2019 %>%
  stlf(lambda = 0, h = 61) 

p %>%
  autoplot() + 
  ggtitle("2015-2019 Seasonal and Trend Decomposition Using Loess Forecasting Model for Daily Car Crashes") +
  xlab("Year") + ylab("Daily Crashes")

twenty_nineteen_daily_forecast_values_STLF <- summary(p)

rownames(twenty_nineteen_daily_forecast_values_STLF) <- seq(as.Date("2020/01/01"), 
                                                           as.Date("2020/03/01"), "day")

true_value = crashes_ts %>%
  filter(Date >= as.Date("2020/01/01") & Date <= as.Date("2020/03/01"))

twenty_nineteen_daily_forecast_values_STLF <- twenty_nineteen_daily_forecast_values_STLF %>%
  mutate(true_count = true_value$count)

#2015-2016 monthly ts 
crashes_mts.2019 = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2019/12/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash))) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "/") %>%
  group_by(Year, Month) %>%
  summarise(mcount = sum(count)) %>%
  tidyr::spread(key=Month, value=mcount)

crashes_mts.2019 = as.data.frame(crashes_mts.2019)

rownames(crashes_mts.2019) = seq(2015, 2019)

crashes_mts3.2019 = crashes_mts.2019 %>%
  select(02:13)

crashes_mts2.2019 <- ts(c(t(crashes_mts3.2019)), frequency=12)

crashes_mts4.2019 <- window(crashes_mts2.2019, start=c(1,01), end=c(5,12), 
                            frequency=12)

#Linear model 2015-2019 monthly ts 
fit.crashes.2019 <- tslm(crashes_mts4.2019 ~ trend + season)

summary(fit.crashes.2019)

fcast.2019 <- forecast::forecast(fit.crashes.2019, h=3)

twenty_nineteen_monthly_forecast_values_lin <- summary(fcast.2019)

rownames(twenty_nineteen_monthly_forecast_values_lin) <- seq(as.Date("2020/01/01"), 
                                                           as.Date("2020/03/01"), "month")

autoplot(fcast.2019) +
  ggtitle("2015-2019 Forecasts of Monthly Car Crashes Using Linear Model") +
  xlab("Year") + ylab("Monthly Crashes")

#Holtwinters model 2015-2019 monthly ts 
covid.monthly.2019 <- HoltWinters(crashes_mts4.2019)

summary(covid.monthly.2019)

plot(fitted(covid.monthly.2019), main = "2015-2019 Box Jenkins Decomposition of Monthly Crashes") 

fcast.covid.monthly.2019 <- forecast(covid.monthly.2019, 3)

twenty_nineteen_monthly_forecast_values_HW <- summary(fcast.covid.monthly.2019)

rownames(twenty_nineteen_monthly_forecast_values_HW) <- seq(as.Date("2020/01/01"), 
                                                           as.Date("2020/03/01"), "month")

autoplot(fcast.covid.monthly.2019) +
  ggtitle("Forecasts of Monthly Car Crashes Using HoltWinters") +
  xlab("Year") + ylab("Crashes")

#STLF model 2015-2019 monthly ts
crashes_mts4.2019 %>% mstl() %>%
  autoplot()

t <- crashes_mts4.2019 %>%
  stlf(lambda = 0, h = 3) 

t %>%
  autoplot() + 
  ggtitle("2015-2019 Seasonal and Trend Decomposition Using Loess Forecasting Model for Monthly Car Crashes") +
  xlab("Year") + ylab("Daily Crashes")

twenty_nineteen_monthly_forecast_values_STLF <- summary(t)

rownames(twenty_nineteen_monthly_forecast_values_STLF) <- seq(as.Date("2020/01/01"), 
                                                           as.Date("2020/03/01"), "month")

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

### Modeling Injury Outcome

crashes_pm = crashes %>%
  filter(Injury != "", Injury != "Unknown", VehicleType != "Unknown", 
         ContributingCircumstance1 != "Unknown", ContributingCircumstance1 != "", 
         Protection != "Unknown", Protection != "", WeatherCondition1 != "Unknown",
         WeatherCondition1 != "", MostHarmfulEvent != "Unknown",
         MostHarmfulEvent != "", RoadFeature != "Unknown", RoadFeature != "",
         TrafficControlType != "Unknown", TrafficControlType != "",
         RoadClassification != "Unkown", RoadClassification != "",
         PersonType != "", PersonType != "Unknown", AlcoholResultType != "",
         AlcoholResultType != "Unknown", VisionObstruction != "", 
         VisionObstruction != "Unknown") %>%
  mutate(Injury = as.factor(Injury)) %>%
  mutate(Injury2 = if_else(Injury == "No injury", "No injury", "Injury"), 
         Injury2 = as.factor(Injury2)) %>%
  mutate(VehicleType = as.factor(VehicleType)) %>%
  mutate(ContributingCircumstance1 = as.factor(ContributingCircumstance1)) %>%
  mutate(Protection = as.factor(Protection)) %>%
  mutate(WeatherCondition1 = as.factor(WeatherCondition1)) %>%
  mutate(MostHarmfulEvent = as.factor(MostHarmfulEvent)) %>%
  mutate(RoadFeature = as.factor(RoadFeature)) %>%
  mutate(TrafficControlType = as.factor(TrafficControlType)) %>%
  mutate(RoadClassification = as.factor(RoadClassification)) %>%
  mutate(PersonType = as.factor(PersonType)) %>%
  mutate(AlcoholResultType = as.factor(AlcoholResultType)) %>%
  mutate(VisionObstruction = as.factor(VisionObstruction))
  

  
summary(crashes_pm$Injury)
summary(crashes_pm$Injury2)

Injury2.fit <- glm(Injury2~Age+VehicleType+ContributingCircumstance1+Protection, 
                   data=crashes_pm, family = "binomial")
summary(Injury2.fit)

#Model coefficients
coef(Injury2.fit) #extract coefficients
round(coef(Injury2.fit), digits = 4) #get rounded coefficients
round(confint(Injury2.fit), digits = 4) #get confidence intervals (CI) for coefs

#Odds ratios
round(exp(coef(Injury2.fit)), digits = 4) #OR=exp(coef)
(round(exp(coef(Injury2.fit)), digits = 4)-1)*100 #percent change in odds
round(exp(confint(Injury2.fit)), digits = 4) #confidence intervals for ORs
round(data.frame(OR=exp(coef(Injury2.fit)), exp(confint(Injury2.fit))), digits = 4) #ORs & their CIs

#Model selection based on AIC
#install.packages("MASS") #run once: installing package needed for the `stepAIC` function
library(MASS) 
Injury2.select <- stepAIC(Injury2.fit)
summary(Injury2.select)

#Get a pseudo R^2 
install.packages("pscl") #package for computing the McFadden R^2
library(pscl)
pR2(Injury2.select)

#Prediction accuracy on test data
set.seed(101) #for reproducibility of results
sample <- sample(c(TRUE, FALSE), nrow(crashes_pm), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
crashes_pm.train <- crashes_pm[sample, ]
crashes_pm.test <- crashes_pm[!sample, ]
crashes_pm.fit.train <- glm(Injury2~Age+VehicleType+ContributingCircumstance1+Protection, 
                            data=crashes_pm.train, family="binomial") #fitting model on training set
crashes_pm.pred.prob <- predict(crashes_pm.fit.train, newdata=crashes_pm.test, 
                                type="response") #predicting prob. of default=1 for test set
crashes_pm.pred <- ifelse(crashes_pm.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
(tab <- table(pred=crashes_pm.pred, actual=crashes_pm.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy=mean(crashes_pm.pred==crashes_pm.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data

#Balancing data
df_crashes_pm_Injury_ind <- which(crashes_pm$Injury2 == "Injury")
df_crashes_pm_NoInjury_ind <- which(crashes_pm$Injury2 == "No injury")

oversample_df1 <- crashes_pm[c(df_crashes_pm_Injury_ind,
                               df_crashes_pm_NoInjury_ind,
                               df_crashes_pm_Injury_ind), ]

oversample_df2 <- crashes_pm[c(df_crashes_pm_NoInjury_ind,
                               df_crashes_pm_Injury_ind,
                               df_crashes_pm_NoInjury_ind), ]

#install.packages("ROSE")
library(ROSE)
over <- ovun.sample(Injury2~., data = crashes_pm.train, method = "both")$crashes_pm

rose <- ROSE(Injury2~., data = crashes_pm.train, N = 500000, seed=2021)$crashes_pm

#install.packages("Amelia")
library(Amelia)
missmap(crashes_pm)

#best method so far for oversampling
df_crashes_pm_Injury_ind <- which(crashes_pm$Injury2 == "Injury")
df_crashes_pm_NoInjury_ind <- which(crashes_pm$Injury2 == "No injury")

oversample_df1 <- crashes_pm[c(df_crashes_pm_NoInjury_ind, 
                               rep(df_crashes_pm_Injury_ind, 5)), ]
 
table(oversample_df1$Injury2)

#Undersampling model
crashes_pm_no_injury = crashes_pm %>%
  filter(Injury2 == "No injury")

crashes_pm_injury = crashes_pm %>%
  filter(Injury2 == "Injury")

s = sample(1:nrow(crashes_pm_no_injury), 35000, replace = F)

undersample_df1 = crashes_pm_injury %>%
  bind_rows(crashes_pm_no_injury[s,])

### Modeling Injury Outcome Using Oversampled Data

Injury2.fit2 <- glm(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                      WeatherCondition1+MostHarmfulEvent+RoadFeature+
                      TrafficControlType+RoadClassification+PersonType+
                      AlcoholResultType+VisionObstruction,
                   data=oversample_df1, family = "binomial")
summary(Injury2.fit2)

ci = cbind(coef(Injury2.fit2)-1.96*(summary(Injury2.fit2)$coefficients[,"Std. Error"]),
       coef(Injury2.fit2)+1.96*(summary(Injury2.fit2)$coefficients[,"Std. Error"]))

round(ci, 3)

#Model coefficients
coef(Injury2.fit2) #extract coefficients
round(coef(Injury2.fit2), digits = 4) #get rounded coefficients
round(confint(Injury2.fit2), digits = 4) #TAKES REALLY LONG get confidence intervals (CI) for coefs

#Odds ratios
round(exp(coef(Injury2.fit2)), digits = 4) #OR=exp(coef)
(round(exp(coef(Injury2.fit2)), digits = 4)-1)*100 #percent change in odds
round(exp(confint(Injury2.fit2)), digits = 4) #TAKES REALLY LONG confidence intervals for ORs
round(data.frame(OR=exp(coef(Injury2.fit2)), exp(confint(Injury2.fit2))), digits = 4) #TAKES REALLY LONG ORs & their CIs

#Model selection based on AIC
#install.packages("MASS") #run once: installing package needed for the `stepAIC` function
library(MASS) 
Injury2.select2 <- stepAIC(Injury2.fit2)
summary(Injury2.select2)

#Get a pseudo R^2 
#install.packages("pscl") #package for computing the McFadden R^2
library(pscl)
pR2(Injury2.select2)

#Prediction accuracy on test data
set.seed(101) #for reproducibility of results
sample2 <- sample(c(TRUE, FALSE), nrow(oversample_df1), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
oversample_df1.train2 <- oversample_df1[sample, ]
oversample_df1.test2 <- oversample_df1[!sample, ]
oversample_df1.fit.train2 <- glm(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                   WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                   TrafficControlType+RoadClassification+PersonType+Ejection+
                                   AlcoholResultType+VisionObstruction, 
                            data=oversample_df1.train2, family="binomial") #fitting model on training set
oversample_df1.pred.prob2 <- predict(oversample_df1.fit.train2, newdata=oversample_df1.test2, 
                                type="response") #predicting prob. of default=1 for test set
oversample_df1.pred2 <- ifelse(oversample_df1.pred.prob2>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
(tab2 <- table(pred=oversample_df1.pred2, actual=oversample_df1.test2$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy2=mean(oversample_df1.pred2==oversample_df1.test2$Injury2, na.rm=T)*100) #percent of correct predictions in test data




  