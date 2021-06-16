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
  filter(PersonType == "Driver" | PersonType == "Passenger", Age != "", Age != "NA") %>% 
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
  filter(LocationFeetFromRoad != "NA", LocationFeetFromRoad != "Unknown") %>%
  count(LocationFeetFromRoad) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         LocationFeetFromRoad= reorder(LocationFeetFromRoad,logtrans)) %>% 
  ggplot(aes(x=LocationFeetFromRoad, y=logtrans, fill=LocationFeetFromRoad)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Crash Distance from Road",
       x = "Location Feet From Road",
       y = "Count (log10 Scale)")



crashes %>% 
  filter(LocationDirectionFromRoad != "NA", LocationDirectionFromRoad != "Unknown") %>%
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
  filter(RoadClassification != "NA", RoadClassification != "Unknown") %>%
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
  count(Crash_Date_Year) %>% 
  mutate(logtrans = round(log10(n), digits = 4)) %>% 
  ggplot(aes(x=Crash_Date_Year, y=logtrans, fill=Crash_Date_Year)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label=logtrans),nudge_y=0.5) + 
  geom_abline(intercept = log10(387995/6.5), slope = 0, lty="dashed", color="darkgrey") +
  labs(title = "Frequency of Crashes by Year", 
       x = "Year", 
       y = "Count (log10 Scale)")

crashes %>% 
  filter(drivers != "NA", drivers != "") %>%
  count(drivers) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         drivers= reorder(drivers,logtrans)) %>% 
  ggplot(aes(x=drivers, y=logtrans, fill=drivers)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Drivers per Crash",
       x = "Number of Drivers",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(passengers != "NA", passengers != "") %>%
  count(passengers) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         passengers= reorder(passengers,logtrans)) %>% 
  ggplot(aes(x=passengers, y=logtrans, fill=passengers)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Passengers per Crash",
       x = "Number of Passengers",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(pedestrians != "NA", pedestrians != "") %>%
  count(pedestrians) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         pedestrians= reorder(pedestrians,logtrans)) %>% 
  ggplot(aes(x=pedestrians, y=logtrans, fill=pedestrians)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Pedestrians per Crash",
       x = "Number of Pedestrians",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(pedalcyclists != "NA", pedalcyclists != "") %>%
  count(pedalcyclists) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         pedalcyclists= reorder(pedalcyclists,logtrans)) %>% 
  ggplot(aes(x=pedalcyclists, y=logtrans, fill=pedalcyclists)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Pedalcyclists per Crash",
       x = "Number of Pedalcyclists",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(other_person_type != "NA", other_person_type != "") %>%
  count(other_person_type) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         other_person_type= reorder(other_person_type,logtrans)) %>% 
  ggplot(aes(x=other_person_type, y=logtrans, fill=other_person_type)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Others per Crash",
       x = "Number of Others",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(unknown_person_type != "NA", unknown_person_type != "") %>%
  count(unknown_person_type) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         unknown_person_type= reorder(unknown_person_type,logtrans)) %>% 
  ggplot(aes(x=unknown_person_type, y=logtrans, fill=unknown_person_type)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Unknown per Crash",
       x = "Number of Unknown",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(killed != "NA", killed != "") %>%
  count(killed) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         killed= reorder(killed,logtrans)) %>% 
  ggplot(aes(x=killed, y=logtrans, fill=killed)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number Killed per Crash",
       x = "Number Killed",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(type_a_injury != "NA", type_a_injury != "") %>%
  count(type_a_injury) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         type_a_injury= reorder(type_a_injury,logtrans)) %>% 
  ggplot(aes(x=type_a_injury, y=logtrans, fill=type_a_injury)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Type A Injuries per Crash",
       x = "Number of Type A Injuries",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(type_b_injury != "NA", type_b_injury != "") %>%
  count(type_b_injury) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         type_b_injury= reorder(type_b_injury,logtrans)) %>% 
  ggplot(aes(x=type_b_injury, y=logtrans, fill=type_b_injury)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Type B Injuries per Crash",
       x = "Number of Type B Injuries",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(type_c_injury != "NA", type_c_injury != "") %>%
  count(type_c_injury) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         type_c_injury= reorder(type_c_injury,logtrans)) %>% 
  ggplot(aes(x=type_c_injury, y=logtrans, fill=type_c_injury)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Type C Injuries per Crash",
       x = "Number of Type C Injuries",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(no_injury != "NA", no_injury != "") %>%
  count(no_injury) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         no_injury= reorder(no_injury,logtrans)) %>% 
  ggplot(aes(x=no_injury, y=logtrans, fill=no_injury)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Non-Injured per Crash",
       x = "Number of Non-Injured",
       y = "Count (log10 Scale)")

crashes %>% 
  filter(injury_unknown != "NA", injury_unknown != "") %>%
  count(injury_unknown) %>% 
  mutate(logtrans = round(log10(n), digits = 2), 
         injury_unknown= reorder(injury_unknown,logtrans)) %>% 
  ggplot(aes(x=injury_unknown, y=logtrans, fill=injury_unknown)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label=logtrans),nudge_y=0.2) +
  coord_flip() +
  labs(title = "Number of Unknown Injuries per Crash",
       x = "Number of Unknown Injuries",
       y = "Count (log10 Scale)")

library(maps)
crashes %>% 
  filter(LocationLatitude != "0", LocationLongitude != "0",
         LocationLatitude != "NA", LocationLatitude != "",
         LocationLongitude != "NA", LocationLongitude != "") %>%
  ggplot(aes(LocationLongitude, LocationLatitude)) +
  borders('county', 'south carolina', fill = "light blue") +
  geom_point(size=0.5, aes(col="k"), show.legend = F) +
  coord_quickmap()


<<<<<<< HEAD

=======
###Visualizing text data using word clouds 

#Installing and loading necessary packages 
install.packages('wordcloud2')
library(wordcloud2)

#Word cloud for location road name on  
location_road_name_on_freq <- crashes %>%
  count(LocationRoadNameOn)

set.seed(101)
location_road_name_on_freq %>%
  wordcloud2(shape = 'circle', backgroundColor = "black", minSize = 5)

#Word cloud for location road name at 
location_road_name_at_freq <- crashes %>%
  count(LocationRoadNameAt)

set.seed(102)
location_road_name_at_freq %>%
  wordcloud2(shape = 'circle', backgroundColor = "black", minSize = 5)
>>>>>>> 7094983f34a80478cd7f488b36a8390435ee5f17
