library(reshape2)
library(tidyverse)
setwd("/Users/cmorris/Desktop/dsreu2021/rstudiodirectory/ResearchProject")

persons <- read.csv(file="Persons_Involved_in_Crashes.csv")
glimpse(persons)

locations <- read.csv(file="Reported_Crash_Locations.csv")
glimpse(locations)

crashes <- persons %>% left_join(locations, by="key_crash")

library(ggplot2)
library(dplyr)
library(dslabs)
library(ggrepel)
library(ggthemes)

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
       y = "Count (log10 Sclae)")

crashes %>%
  filter(PersonType == "Driver", Age != "NA", Age != "NaN", Age != "") %>%
  ggplot() +
  geom_histogram(aes(x=Age), binwidth = 5, col="red", fill="darkgrey") +
  labs(title="Histogram for Drivers' Ages", x="Age", y="Frequency")

crashes %>% 
  filter(AlcoholResultType != "NA", AlcoholResultType != "", 
         AlcoholResultType != "NaN") %>%
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
# Question: Remove pending/unknown?

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
  filter(PersonType == "Driver" | PersonType == "Passenger") %>% 
  ggplot() +
  geom_histogram(aes(x=Age), binwidth = 5, col="red", fill="darkgrey") +
  labs(x="Age", y="Frequency", title="Drivers' vs. Passengers' Age") +
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
  filter(Gender != "NA", Gender != "Unknown") %>%
  count(Gender) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         Gender = reorder(Gender,logtrans)) %>% 
  ggplot(aes(x=Gender, y=logtrans, fill=Gender)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Gender",
       x = "Gender",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(Race != "NA", Race != "Unknown") %>%
  count(Race) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         Race = reorder(Race,logtrans)) %>% 
  ggplot(aes(x=Race, y=logtrans, fill=Race)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Race",
       x = "Race",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(Injury != "NA", Injury != "Unknown") %>%
  count(Injury) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         Injury = reorder(Injury,logtrans)) %>% 
  ggplot(aes(x=Injury, y=logtrans, fill=Injury)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Frequency of Crashes by Injury",
       x = "Injury",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(Protection == "Shoulder and lap belt" | Protection == "None used", Injury != "NA", Injury != "Unknown") %>%
  count(Injury) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         Injury = reorder(Injury,logtrans)) %>% 
  ggplot(aes(x=Injury, y=logtrans, fill=Injury)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  facet_wrap(~Protection) +
  labs(title = "Frequency of Crashes by Injury of Shoulder and Lap Belt Protection",
       x = "Injury",
       y = "Count (log10 Scale)")

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
