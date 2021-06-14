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
