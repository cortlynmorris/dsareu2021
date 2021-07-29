---
title: "Visualizing, Modeling, and Forecasting Crashes in Wake County, North Carolina"
author: "Kelly Wentzlof, Cortlyn Morris, Ayan Gulzar"
date: "7/30/2021"
output:
  html_document:
    toc: true
    toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Abstract

Studies have shown that road traffic crashes are one of the leading causes of non-natural death for U.S citizens. There are many different ways that road crashes occur, and several various factors lead to it. It is essential to analyze the most common elements of road crashes. In this report, our goal is to model and forecast crashes in Wake County, North Carolina. To accomplish this goal, a public dataset  called  North Carolina Wake County crashes was available to use. The crash dataset used in this research was from 2015 to March 2021. Using the dataset to our advantage our intent is to  observe which factors have a higher cause of crashes in Wake County. The objective of this research is to address the above research question by using bar plots of common factors leading to crashes and Time Series Forecasting crashes in Wake County.

# Introduction 

Car crashes are something that occurs nearly every day. On average, there are 16,438 car crashes per day in the U.S. When it comes to car accidents, several factors correlate. However, it is crucial to understand some of the most frequent causes of crashes. Analyzing the most common reasons can help notify other drivers on the road so that there can be a decrease in crashes. 

When it comes to identifying car crashes, different states in the U.S experience crashes at different rates. Road accidents do not only harm the driver of the vehicle. It also affects the passenger and the pedestrians.  Most people assume that the driver gets injured the most in a road accident. Passengers and pedestrians are still dying at high rates.

From the beginning of 2015, information on car crashes has been listed in the Wake County car accident database. This data includes the driver's age, gender, vehicle type, and many more critical car accident factors. Our database is based on what police officers have recorded at the scene of the accident. Most of the data found in this database are true, but there is a small percentage of error with some of the data. Our goal is to sort out essential factors from the database to help forecast crashes in Wake County.

In this report, we will be visualizing, modeling and forecasting car crashes in Wake County. Our research questions are 1) What are the main factors that contribute to a crash? 2) Did the COVID pandemic impact crash frequency? 3) What are the conditions that lead to worse outcomes in terms of crash injuries?4) Can we forecast the daily number of crashes in Wake County? 5) Can we predict traffic hotspots in Wake County? We assembled a dataset from the Wake County crashes database from 2015 to March 2021. Although some of the input in the database is missing, this data will help provide some information about crashes in Wake County. This will also help us forecast car crashes after 2021. 

# Background 

When it comes to car crashes, people assume that the driver was either speeding or not paying attention. It is essential to understand that there are other factors to consider when a car accident occurs. Since everything has slowly started to open after the COVID-19 pandemic, there has been an increase in car crashes. Studying the different factors of impacts can help decrease the number of accidents.

## Hotspots

When looking at car crashes it is essential to study hotspots and notice roads where crashes are common. Some roads might not be known for accidents and others might be known for having the most accidents or even most life-threatening crashes. There has not been a lot of research done on the types of roads that crashes occur on. This is something to consider when looking at road crashes because it gives us a better understanding of roads where crashes are more common than others.  

## Weather Condition

Weather conditions are a prominent factor in car crashes. When studying car crashes it is crucial to study how the weather affects a crash. Sometimes crashes tend to increase when there is bad weather. Researchers have found that rain was one of the top contributors to weather-related auto crashes.

## Outro 

With all these factors of crashes done in previous research, some have not studied in-depth about this topic. Our goal is to expand this topic as we found there is no research on the main factors that cause car accidents. The condition that leads to the worst outcome to injuries. Lastly, the most common road that crashes happen. We will forecast  Crashes in Wake County, North Carolina from 2015 to March 2021. 

# Data, Method, and Analysis 

## Data

For this study, we utilized public data from the Wake County crashes dataset. We combined datasets from each year so that we could forecast car crashes in Wake County. This database gets updated every month and has all the specifications for the car accident. 

```{r loading-libraries, message=FALSE, warning=FALSE, echo=FALSE, include=FALSE}
library(reshape2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(dslabs)
library(ggrepel)
library(ggthemes)
library(maps)
library(scales)
library(zoo)
#devtools::install_github("m-clark/confusionMatrix")
library(confusionMatrix)
library(ROSE)
library(ranger)
library(Rcpp)
```

```{r read-data, message=FALSE, echo=FALSE, include=FALSE}
#Code for Cortlyns computer (Mac)
 # setwd("/Users/cmorris/Desktop/dsreu2021/rstudiodirectory/ResearchProject")
 # persons <- read.csv(file="Persons_Involved_in_Crashes.csv")
 # glimpse(persons)
 # #    
 # locations <- read.csv(file="Reported_Crash_Locations.csv")
 # glimpse(locations)
 # #    
 # crashes <- persons %>% left_join(locations, by="key_crash")
#Code for Kelly's computer (Windows)
#library(readr)
persons <- read_csv("~/NCAT REU/Mostafa/Data/Persons_Involved_in_Crashes.csv")
locations <- read_csv("~/NCAT REU/Mostafa/Data/Reported_Crash_Locations.csv")
crashes <- persons %>% left_join(locations, by="key_crash")
# Code for Ayan's computer (Window)
#library(readr)
# persons <- read_csv("C:/Users/ayang/Desktop/Project FIles REU A&T/REU Research Project1/Persons_Involved_in_Crashes.csv")
# 
# locations <- read_csv("C:/Users/ayang/Desktop/Project FIles REU A&T/REU Research Project1/Reported_Crash_Locations.csv")
# 
# crashes <- persons %>% left_join(locations, by="key_crash")
```

## Method 

### Exploratory Data Analysis 

Exploratory data analysis was the first step in the process of understanding and finding patterns within the crash data set. We conducted univariate and bivariate explorations. 

#### Univariate Explorations 

Below are the most important univariate visualizations that we discovered. 

```{r driver-passenger-age, echo=FALSE, fig.width = 10}
crashes %>% 
  filter(PersonType == "Driver" | PersonType == "Passenger", Age != "", Age != "NA") %>% 
  ggplot() +
  geom_histogram(aes(x=Age), binwidth = 5, col="red", fill="darkgrey") +
  labs(x="Age", y="Frequency", title="Drivers' vs. Passengers' Age") +
  theme(legend.position = "top") +
  facet_wrap(~PersonType, dir = "v")
```

For the graph above we created a histogram to compare the drivers' and passengers' age. This helped us understand what age is common for drivers to get injured in a car accident. The range of drivers' ages is from about 16 to 100. The range of passengers' ages is from about 0 to 100. The range of drivers' ages is slightly larger due to the age restrictions for getting a license.

```{r crash-year, echo=FALSE, fig.width = 10}
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
```

For this bar plot, we had data for the years 2015 to March 2021. In the visual above you can see that there was a high rate of car accidents from the beginning of 2015 to the end of 2019.We included a horizontal dashed line to indicate where the yearly average lies and we can see that only the years 2020 and 2021 fall below this yearly average. The year with the most crashes in 2019 but not by many. The full year with the fewest crashes in 2020 which was to be expected due to the lockdowns that took place to try and mitigate the spread of Covid-19. 2021 also looks to be on track to be another low crash year but we do not have a perfect explanation for that seeing as things are beginning to open and "return to normal" again.

```{r weather-condition, echo=FALSE, fig.width = 10}
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
```

For this visual, we also created a bar plot to determine which weather condition tends to be the most common for car crashes. After constructing the bar plot you can recognize that most crashes tend to happen when the weather is clear. This is not very surprising because in North Carolina, the majority of the time the weather is clear. An interesting note is that snow still has a relatively high frequency despite it snowing minimally in North Carolina. 

```{r vehicle-type, echo=FALSE, fig.width = 10}
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
```

For the type of vehicle in a crash we had data for the years 2015 to March 2021. This bar plot helps us understand the most common vehicles and the least common vehicles in an accident in Wake County. The highest frequency of crashes occur in a passenger car. However, this is not very surprising because passenger cars are relatively common. An interesting note is that sport utility vehicles have the second highest frequency of crashes, however, sport utility vehicles are not nearly as common of a car.

#### Bivariate Explorations

This section is dedicated to visualizing factors that may lead to worse outcomes of crashes in terms of injury. Using percent stacked bar charts, we are able to determine which variables lead to a more or less serious injuries which we can later use to help predict the seriousness of injury through predictive modeling. Below are some of the most important bivariate explorations. 

```{r protection-injury, echo=FALSE, fig.width = 10}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Protection != "NA", 
         Protection != "NaN", Protection != "Unable to determine", 
         Protection != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=Protection)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Protection",
       x = "Protection",
       y = "Percentage")
```

The most injuries occurred when people were using reflective clothing, lighting, and a helmet as protection. This makes sense because people using this would most likely be pedestrians or bikers, both of which are more exposed to danger and to having more significant injuries. Shoulder belts, lap belts, and the combination between the two appear to help decrease the amount of injuries that occur.

```{r most-harm-injury, echo=FALSE, fig.width = 10}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", MostHarmfulEvent != "NA", 
         MostHarmfulEvent != "NaN", MostHarmfulEvent != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=MostHarmfulEvent)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Most Harmful Event",
       x = "Most Harmful Event",
       y = "Percentage")
```

The most harmful event that led to most people killed and disabling injuries was with pedestrians. This would make sense because pedestrians often do not have protection from cars. Overturn/rollover harmful events also had a significant amount of injuries. Backing up had the least amount of injuries which would make sense because when backing a car up, people tend to be going a slower speed than if they were accelerating forward. 

```{r traffic-injury, echo=FALSE, fig.width = 10}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", TrafficControlType != "NA", 
         TrafficControlType != "NaN", TrafficControlType != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=TrafficControlType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() +
  labs(title = "Injury Frequency by Traffic Control Type",
       x = "Traffic Control Type",
       y = "Percentage")
```

Places where warning signs are post led to the post injuries including people killed. This may be because the signs are not taken as seriously and are less common to see, therefore, more people may disregard the warnings. Not many injuries occur near Railroad crossings. 

```{r road-class-injury, echo=FALSE, fig.width = 10}
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
```

Public vehicular areas and private roads/driveways lead to the least amount of injuries. This may be because these areas are minimally trafficked and are usually at slower speeds than other roads. 

```{r person-injury, echo=FALSE, fig.width = 10}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType != "NA", 
        PersonType != "NaN", PersonType != "Unknown", PersonType != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=PersonType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Person Type",
       x = "Person Type",
       y = "Percentage")
```

Pedestrians are most likely to die, followed by pedalcyclists most likely due to lack of protection and high exposure to the environment. Driver's are slightly less likely to be injured than passengers. 

```{r alcohol-filter-injury, echo=FALSE, fig.width = 10}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", AlcoholResultType != "NA", 
         AlcoholResultType != "NaN", AlcoholResultType != "Unknown",
         AlcoholResultType != "Contaminated sample/unusable", 
         AlcoholResultType != "Pending", AlcoholResultType != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=AlcoholResultType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Alcohol Result Type",
       x = "Alcohol Result Type",
       y = "Percentage")
```

The most people are killed when there are no alcohol or other drugs. This is slightly surprising, however, it may be because when people are driving with drugs or alcohol in their system, they may attempt to be more cautious so that they do not get pulled over by police. 

```{r vision-obstruct-injury, echo=FALSE, fig.width = 10}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", VisionObstruction != "NA", 
         VisionObstruction != "NaN", VisionObstruction != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=VisionObstruction)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Vision Obstruction",
       x = "Vision Obstruction",
       y = "Percentage")
```

Signs and embankment led to the most people being killed. This may be because these are both stationary opaque objects that will continuously block people's views of the road and conditions. Also being blinded by other lights led to the most disabling injuries. 

### Identifying Hot Spots of Crashes

This section is dedicated to identifying hot spots of crashes in Wake County, North Carolina. 

#### Word Clouds 

Below are two word clouds created to visualize the frequency of crashes on streets in Wake County, North Carolina. 

```{r tool, message=FALSE, echo=FALSE}
devtools::install_github("gaospecial/wordcloud2")
```

```{r load-library-word-cloud, message=FALSE, echo=FALSE}
#install.packages('wordcloud2')
library(wordcloud2)
```

```{r location-on-word-cloud, echo=FALSE, fig.width=10, fig.align='center'}
location_road_name_on_freq <- crashes %>%
  count(LocationRoadNameOn)
set.seed(101)
location_road_name_on_freq %>%
  wordcloud2(shape = 'circle', backgroundColor = "black", minSize = 5) 
```

From the word cloud, we can see many crashes occur on 440, 40, and Capital Blvd. This makes sense because 440 and 40 are major interstates that connect to several major junctions and Capital Blvd is a main road in North Carolina as well. Due to these streets being heavily trafficked, more crashes are likely to occur. 

```{r location-at-word-cloud, echo=FALSE, fig.width=10, fig.align='center'}
location_road_name_at_freq <- crashes %>%
  count(LocationRoadNameAt)
set.seed(102)
location_road_name_at_freq %>%
  wordcloud2(shape = 'circle', backgroundColor = "black", minSize = 5)
```

From the word cloud, we can see that 440, Capital Blvd, 40, and Blue Ridge Rd frequently have crashes that are near by. This would make sense because most of these streets are meet near the center of Raleigh, the capital of North Carolina, therefore, they are highly trafficked and populated. With many cars merging and intersecting onto these common and high speed streets, it is not surprising that many crashes occur at these locations. 

#### Interactive Heat Maps 

Below are two interactive heat maps dedicated to organizing the Wake County, North Carolina crash data in a spatial manner. 

```{r load-library-interactive-map, message=FALSE, echo=FALSE}
library(ggplot2)
#install.packages("ggmap")
library(ggmap)
library(RColorBrewer)
library(plotly)
```

```{r nc-interactive-map, message=FALSE, warning=FALSE, echo=FALSE}
map_bounds <- c(-78.98, 35.51, -78.24, 36.06)
crashes_filter <- crashes %>%
  filter(LocationLatitude != "0", LocationLongitude != "0",
         LocationLatitude != "NA", LocationLatitude != "",
         LocationLongitude != "NA", LocationLongitude != "",
         LocationLatitude > 35.4, LocationLatitude < 36.2,
         LocationLongitude < -78, LocationLongitude > -79)
coords.map.nc <- get_stamenmap(map_bounds, zoom = 10, maptype = "toner-lite")
coords.map.nc <- ggmap(coords.map.nc, extent="panel", legend="none")
coords.map.nc <- coords.map.nc + stat_density2d(data=crashes_filter,  
                                          aes(x=LocationLongitude, 
                                              y=LocationLatitude, 
                                              fill=..level..),
                                          alpha = 0.3, 
                                          geom="polygon")
coords.map.nc <- coords.map.nc +   scale_fill_gradientn(colours=rev(brewer.pal(7, "RdBu")))
coords.map.nc <- coords.map.nc + theme_bw() + 
  ggtitle("Heat Map of Crashes in North Carolina") + 
  xlab("Longitude") + ylab("Latitude")
ggplotly(coords.map.nc) %>%
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  )
```

In the visual above we created a zoomed-out heat map of common places where road accidents occur in North Carolina. As we can see from the legend on the right, the more red the area is, the more crashes are likely to occur. The capital of North Carolina, Raleigh is heavily colored in dark red, indicating that many crashes occur here. As we move further from the capital city, the level of crashes tends to decrease. 

```{r wake-interactive-map, message=FALSE, warning=FALSE, echo=FALSE}
crashes_filter <- crashes %>%
  filter(LocationLatitude != "0", LocationLongitude != "0",
         LocationLatitude != "NA", LocationLatitude != "",
         LocationLongitude != "NA", LocationLongitude != "",
         LocationLatitude > 35.4, LocationLatitude < 36.2,
         LocationLongitude < -78, LocationLongitude > -79)
map_bounds <- c(-78.8, 35.72, -78.5, 35.9) #coordinates of wake county
coords.map.wake <- get_stamenmap(map_bounds, zoom = 13, maptype = "toner-lite")
coords.map.wake <- ggmap(coords.map.wake, extent="panel")
coords.map.wake <- coords.map.wake + stat_density2d(data=crashes_filter,  
                                          aes(x=LocationLongitude, 
                                              y=LocationLatitude, 
                                              fill=..level..),
                                          alpha=0.3, 
                                          geom="polygon")
coords.map.wake <- coords.map.wake + scale_fill_gradientn(colours=rev(brewer.pal(7, "RdYlGn")))
coords.map.wake <- coords.map.wake + theme_bw() + 
  ggtitle("Heat Map of Crashes in Wake County") + 
  xlab("Latitude") + ylab("Longitude")
ggplotly(coords.map.wake) %>%
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  )
```

In addition to the previous map, above we created a zoomed in heat map for crashes in Wake County, North Carolina. This heat map shows streets that appeared in the word clouds in the previous section such as Capital Boulevard. As we can see, Capital Boulevard runs through the heart of the city and intersects with the dark red color indicating a large level of crashes in the area. This corresponds with the world cloud because Capital Boulevard was one of the largest street names which also indicated a higher count of crash occurrences. In general, from the Wake County heat map, we can see that crashes are much more likely to occur in the center of the city. We see that as more streets populate an area, more crashes occur. 

### Time Series Visualization 

This section is dedicated to visualizing the entire data set of car crashes in Wake County, North Carolina from January 1, 2015 to May 31, 2021. 

#### Daily Time Series 

```{r load-library-ts, echo=FALSE, message=FALSE}
library(zoo)
```

```{r data-i, echo=FALSE, message=FALSE}
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), 
                         end = c(2020,59), frequency = 365)
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
```

```{r crashes-ts, echo=FALSE}
crashes_ts = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
```

```{r 7-day-ma-ts, echo=FALSE, warning=FALSE, fig.width = 10, fig.align='center'}
crashes_ts %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() +
  geom_line(aes(y=rollmean(count, 7, na.pad = TRUE)), color = "red") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=8)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes (With Pandemic Data)")
```

Above we have a daily pandemic time series plot with a seven day moving average of the number of crashes from January 1, 2015 to May 31, 2021. We see that there are major spikes around February 2015, late October 2015, and December 2016. The two latter spikes may be related to the popular holidays, Halloween and Christmas where many people are traveling or out on the town preparing for celebrations. 

Additionally, from the time series plot, we can see that there is a significant drop in car crashes in March 2020. This can be explained by the stay-at-home-orders due to the COVID-19 pandemic. After the stay at home orders, the frequency of crashes seems to be slightly lower throughout May 2021. There will likely be an increase in the next few month due to stay-at-home-orders and COVID-19 regulations being lifted now that the vaccination is available. 

```{r noncovid-daily-ts, echo=FALSE, message=FALSE, fig.width = 10}
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashes_ts.noncovid %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes (Without Pandemic Data)") + xlab("Date") + ylab("Count") + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size = 10))
```

In the time series plot above, we have taken out the pandemic data and only plotted the daily data from January 1, 2015 to March 1, 2020. We wanted to visualize the overall pattern and trend of the data when the large pandemic dip was taken out. Removing the pandemic data, shows us that the trend is very linear and does not include large or significant changes throughout time. 

```{r annual-ts, echo=FALSE, fig.width = 10}
crashes_annual = crashes_ts %>%
  separate(Date, 
           into = c("Year", "Month"), sep = 4, remove = FALSE) %>%
  select(-Month)
crashes_annual %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  ggtitle("Time Series Plots for Frequency of Daily Crashes Organized by Year") + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") + 
  xlab("Date") + ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Year, scales = "free_x")
```

Above, we can see the daily time series plot broken into annual plots. This helps better visualize the spikes and dips throughout the specific years. For example, we can now determine that the first large spike was near the end of February 2015. Inspecting our data set, we were able to find that the large spike actually occured on February 24, 2015. On this day, a huge snow and ice storm hit the Wake County area created dangerous road conditions for drivers (Armstrong 2015). Since North Carolina is a southern state, the drivers there are not accustomed to the slippery and icy road conditions that result from snowfalls, therefore, are more likely to get into more crashes and result in more serious injuries. 

#### Monthly Time Series 

```{r monthly-ts, echo=FALSE, message=FALSE, fig.width=10, fig.align='center'}
forecast::autoplot(crashes_mts4) +
  ggtitle("Time Series Plot for Frequency of Monthly Crashes (With Pandemic Data)") + xlab("Date") + ylab("Monthly Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", 
                                "Jan 2018", "Jan 2019", "Jan 2020", 
                                "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), 
        title = element_text(size=10))
```

Seen above is a monthly pandemic time series plot. We decided to visualize the daily data as well as the monthly data in order to inspect the different seasonality and trends between monthly and data data. As the plot shows, there seems to be a spike near the fall to winter months of each year. This may be due to the colder months resulting in more rain, snow, and poorer weather overall. We can also see the large dip in March 2020 where the pandemic first hit, as well as the overall lower average of crashes during the pandemic when the stay-at-home orders were in effect. 

```{r monthly-ts-noncovid, echo=FALSE, message=FALSE, fig.width = 10}
forecast::autoplot(crashes_mts4.noncovid) +
  ggtitle("Time Series Plot for Frequency of Monthly Crashes (Without Pandemic Data)") + xlab("Date") + ylab("Monthly Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", 
                                "Jan 2018", "Jan 2019", "Jan 2020", 
                                "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), 
        title = element_text(size=10))
```

Similar to the daily time series, we also removed the pandemic data using data only from January 1, 2015 to March 1, 2020 to visualize the monthly data without the COVID-19 dip. Without the pandemic data, the spikes near the end of each year are more prominent and easier to recognize. We also can see that near the beginning of each year there is a large dip right before or right after New Years. This may be due to the fact that people have left the state for vacations, therefore, there are less drivers in Wake County. Or this could be due to the fact that many people are settled in one location by New Years because they probably drove to their relatives or friends before Christmas Day. 

### Time Series Forecasting 

```{r loading-libraries-2, message=FALSE, warning=FALSE, echo=FALSE, include=FALSE}
library(reshape2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(dslabs)
library(ggrepel)
library(ggthemes)
library(maps)
library(scales)
library(zoo)
library(forecast)
library(urca)
library(sf)
library(TTR)
library(fpp2)
```

```{r data, echo=FALSE, message=FALSE}
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), 
                         end = c(2020,59), frequency = 365)
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
```

#### Daily with Pandemic Data 

First, we had to ensure that the data we working with was stationary. Stationarity is when there is no observed trend or seasonality within a dataset. Therefore, if you pick out a section of points at a specific time, you will not be able pick out a that same pattern within the data set again at a similar time. Stationarity is a large assumption that must be met for many models when forecasting. 

```{r stationarity-trans-daily-covid, echo=FALSE, eval=FALSE}
##Looking at stationarity of crashests2 daily time series 
library(urca)
acf(crashests2)
summary(ur.kpss(crashests2))
```

```{r stationarity-trans-daily-ii}
library(urca)
ndiffs(crashests2) #1
nsdiffs(crashests2) #0
#Attempt at differencing 
dif_crashests2 <- diff(crashests2)
#Looking at stationarity of first difference 
acf(dif_crashests2)
```

```{r stationarity-trans-daily-iii, echo=FALSE, eval=FALSE}
summary(ur.kpss(dif_crashests2))
ndiffs(dif_crashests2) #0
nsdiffs(dif_crashests2) #0
```

The ndiffs() function checks if the data set needs any first difference calculations. The nsdiffs() function checks if the data set needs any seasonal difference calculations.

Here our daily pandemic data needs one first difference and we do this with the diff() function. Using the diff() function, we take the first difference and then we check the acf of the data to make sure that it does not take too many lags to hit 0 indicating that the first difference transformed our data to be stationary. 

```{r stat-trans-daily-covid-plot, fig.width = 10, echo=FALSE}
cbind("Crashes" = crashests2,
      "First differenced" = diff(crashests2)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformations of Daily Pandemic Data")
```

The plot above, shows the data before the stationary transformation and after the first difference was taken.

Now that our daily pandemic data has been transformed to be stationary, our assumptions for our forecasting models are met and we can begin to forecast. 

```{r tbats-stationary, echo=FALSE, message=FALSE, include=FALSE}
#Stationary
#reload libraries 
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)
dif_crashests2 <- diff(crashests2)
dif_y <- msts(dif_crashests2, seasonal.periods=c(7,365.25))
dif_fit_tbats_covid2 <- tbats(dif_y)
dif_fc_tbats_covid2 <- forecast::forecast(dif_fit_tbats_covid2, h=214)
dYhat <- dif_fc_tbats_covid2$mean #point forecast values 
dYhat_ci_upper <- dif_fc_tbats_covid2$upper
dYhat_ci_lower <- dif_fc_tbats_covid2$lower
#crashests2
#dif_crashests2<-diff(crashests2)
#z0<-cumsum(c(crashests2[1], dif_crashests2))
#all(crashests2==z0) #TRUE
#dif_crashests2 <- diff(crashests2) #already ran earlier
#dYhat <- dif_fc_tbats_covid2$mean #already ran earlier 
Yhat <- cumsum(c(crashests2[length(crashests2)],dYhat))
Yhat <- ts(Yhat, start = c(2021, 152), frequency=365)
Yhat_ci_upper <- cumsum(c(crashests2[length(crashests2)],dYhat_ci_upper))
Yhat_ci_upper <- ts(Yhat_ci_upper, start = c(2021, 152), end = c(2022,1), frequency=365)
Yhat_ci_lower <- cumsum(c(crashests2[length(crashests2)],dYhat_ci_lower))
Yhat_ci_lower <- ts(Yhat_ci_lower, start = c(2021, 152), end = c(2022,1), frequency=365)
```

```{r tbats-stationary-plot, echo=FALSE, message=FALSE, fig.width = 10}
#Stationary
autoplot(crashests2) + 
  autolayer(Yhat, series="Point Forecasts") + 
  #autolayer(Yhat_ci_upper) + 
  #autolayer(Yhat_ci_lower) + 
  ggtitle("TBATS Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  theme(title = element_text(size = 10), legend.position = "bottom")
```

The Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components (TBATS model) was chosen because it is known to perform well at forecasting with long daily time series. In order to use this model, we had to transform our data to be stationary, as shown in the steps previously. 

In the plot above, we used our crash data from January 1, 2015 to May 31, 2021 to forecast crashes from June 1, 2021 until January 1, 2022. As we can see from the plot, the TBATS model does well in following the slight spikes and dips throughout the months. However, due to the large dip because of the pandemic data, the forecasts appear to be below the average line of the data. This indicates that the pandemic data affects the forecasts of future crashes. 

```{r arima, echo=FALSE, fig.width = 10}
fit_arima <- auto.arima(crashests2)
fc_arima <- forecast(fit_arima, h=214)
autoplot(fc_arima) + 
  ggtitle("ARIMA Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  theme(title = element_text(size = 10))
```

The autoregressive integrated moving average (ARIMA) model was also chosen because it is known to perform well at forecasting with long daily time series. Unlike the TBATS model (shown previously), the ARIMA model automatically takes the necessary differences on the original data. Therefore, no manual differencing needs to occur. 

From the plot above, we can see that the forecasts create some slight spikes and dips at the beginning and then flatten out to a horizontal line. This is most likely due to the fact that the trend is linear for a majority of the time series. As we can see, the horizontal line is closer to the average line of the data after the pandemic rather than before the pandemic. We can see that the pandemic data has a large impact on teh future forecasts of crashes. 

#### Daily without Pandemic Data 

First we check the daily nonpandemic data for stationarity as we did for the daily pandemic data. 

```{r stationarity-trans-daily-noncovid, echo=FALSE, eval=FALSE}
##Looking at stationarity of crashests.noncovid daily time series 
library(urca)
acf(crashests.noncovid)
summary(ur.kpss(crashests.noncovid))
```

```{r stationarity-trans-daily-noncovid-ii}
ndiffs(crashests.noncovid) #1
nsdiffs(crashests.noncovid) #0
#Attempt at differencing (noncovid)
dif_crashests.noncovid <- diff(crashests.noncovid)
#Looking at stationarity of first difference (noncovid)
acf(dif_crashests.noncovid)
```

```{r stationarity-trans-daily-noncovid-iii, echo=FALSE, eval=FALSE}
summary(ur.kpss(dif_crashests.noncovid))
ndiffs(dif_crashests.noncovid) #0
nsdiffs(dif_crashests.noncovid) #0
```

Similar to the daily pandemic data, the daily nonpandemic data needs one first difference and we do this with the diff() function. Using the diff() function, we take the first difference and then we check the acf of the data to make sure that it does not take too many lags to hit 0 indicating that the first difference transformed our data to be stationary.

```{r stat-trans-daily-noncovid-plot, fig.width = 10, echo=FALSE}
cbind("Crashes" = crashests.noncovid,
      "First differenced" = diff(crashests.noncovid)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformation of Daily NonPandemic Data")
```

The plot above, shows the data before the stationary transformation and after the first difference was taken. 

Now that the daily nonpandemic data has been transformed to be stationary, the assumptions of the forecasting models have been met and we can begin to forecast. 

```{r tbats-noncovid-stationary, echo=FALSE, message=FALSE, include=FALSE}
#Stationary
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), 
                         end = c(2020,59), frequency = 365)
dif_crashests.noncovid <- diff(crashests.noncovid)
dif_y_noncovid <- msts(dif_crashests.noncovid, seasonal.periods=c(7,365.25))
dif_fit_tbats_noncovid <- tbats(dif_y_noncovid)
dif_fc_tbats_noncovid <- forecast::forecast(dif_fit_tbats_noncovid, h=671)
dYhat_noncovid <- dif_fc_tbats_noncovid$mean #point forecast values 
#crashests.noncovid
#dif_crashests.noncovid<-diff(crashests.noncovid)
#z0<-cumsum(c(crashests.noncovid[1], dif_crashests.noncovid))
#all(crashests.noncovid==z0) #TRUE
#dif_crashests2 <- diff(crashests2) #already ran earlier
#dYhat <- dif_fc_tbats_covid2$mean #already ran earlier 
Yhat_noncovid <- cumsum(c(crashests.noncovid[length(crashests.noncovid)],dYhat_noncovid))
Yhat_noncovid <- ts(Yhat, start = c(2020, 60), frequency=365)
```

```{r tbats-noncovid-stationary-plot, echo=FALSE, message=FALSE, fig.width = 10}
#Stationary
true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2020/03/01") & Date <= as.Date("2021/05/31"))
true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2020,60), end = c(2021,153), 
                            frequency = 365)
autoplot(crashests.noncovid) + 
  autolayer(true_noncovid_crashes, alpha=0.7, series="True Count") +
  autolayer(Yhat_noncovid, alpha=0.6, series="Point Forecasts") + 
  ggtitle("TBATS Forecasting Model for Daily Car Crashes (Without Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  theme(title = element_text(size = 10), legend.position = "bottom")
```

In the plot above, we used the TBATS model for the same reasons that we used it with the daily pandemic data. 

In the pink, we can see that the forecasts produced by the TBATS model follow the true crash count, in blue, very well. Despite not having the pandemic data, the TBATS model used data from January 1, 2015 to March 1, 2020 to help predict a slight dip that ended up following the pandemic dip very well. 

```{r arima-noncovid, echo=FALSE, fig.width = 10}
fit_arima.noncovid <- auto.arima(crashests.noncovid)
fc_arima.noncovid <- forecast(fit_arima.noncovid, h=671)
autoplot(fc_arima.noncovid) + 
  ggtitle("ARIMA Forecasting Model for Daily Car Crashes (Without Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  theme(title = element_text(size = 10))
```

The plot above uses the ARIMA model for the same reasons that it was used for the daily pandemic data. 

From the plot we can see that the forecast produced slight spikes and dips near the beginning of the forecast and then quickly transitions into a horizontal line. Using the nonpandemic data, the forecasts produce what appears to be a horizontal line with a slight negative slope. This model does not do well at predicting the sharp dip due to the pandemic. However, the model does do well at maintaining the almost horizontal trend line throughout time. 

#### Monthly with pandemic Data 

First we check the monthly pandemic data for stationarity as we did for the daily data. 

```{r stationarity-trans-monthly-covid, echo=FALSE, eval=FALSE}
##Looking at stationarity of crashes_mts4 monthly time series 
library(urca)
acf(crashes_mts4)
summary(ur.kpss(crashes_mts4))
```

```{r stationarity-trans-monthly-covid-ii}
ndiffs(crashes_mts4) #1
nsdiffs(crashes_mts4) #0
#Attempt at differencing 
dif_crashes_mts4 <- diff(crashes_mts4)
#Looking at stationarity of first difference 
acf(dif_crashes_mts4)
```

```{r stationarity-trans-monthly-covid-iii, echo=FALSE, eval=FALSE}
summary(ur.kpss(dif_crashes_mts4))
ndiffs(dif_crashes_mts4) #0
nsdiffs(dif_crashes_mts4) #0
```

Like both sets of daily data, our monthly pandemic data needs one first difference and we do this with the diff() function. Using the diff() function, we take the first difference and then we check the acf of the data to make sure that it does not take too many lags to hit 0 indicating that the first difference transformed our data to be stationary.

```{r stat-trans-monthly-covid-plot, fig.width = 10, echo=FALSE}
cbind("Crashes" = crashes_mts4,
      "First differenced" = diff(crashes_mts4)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformation of Monthly Pandemic Data") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90)) 
```

The plot above, shows the data before the stationary transformation and after the first difference was taken. As we can see the large dip with the pandemic is mainly neutralized and the spikes near the end of each year become less trendy and more constant. 

Now that the monthly pandemic data has been transformed to be stationary, the assumptions for the forecasting models are met and we can begin forecasting. 

```{r tbats-stationary-monthly, echo=FALSE, message=FALSE, include=FALSE}
#Stationary
#reload libraries 
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
dif_crashes_mts4 <- diff(crashes_mts4)
dif_y_monthly <- msts(dif_crashes_mts4, seasonal.periods=12)
dif_fit_tbats_covid2_monthly <- tbats(dif_y_monthly)
dif_fc_tbats_covid2_monthly <- forecast::forecast(dif_fit_tbats_covid2, h=8)
dYhat_monthly <- dif_fc_tbats_covid2_monthly$mean #point forecast values 
#crashests2
#dif_crashests2<-diff(crashests2)
#z0<-cumsum(c(crashests2[1], dif_crashests2))
#all(crashests2==z0) #TRUE
#dif_crashests2 <- diff(crashests2) #already ran earlier
#dYhat <- dif_fc_tbats_covid2$mean #already ran earlier 
Yhat_monthly <- cumsum(c(crashes_mts4[length(crashes_mts4)],dYhat_monthly))
Yhat_monthly <- ts(Yhat_monthly, start = c(7,6), frequency=12)
```

```{r tbats-stationary-plot-monthly, echo=FALSE, message=FALSE, fig.width = 10}
#Stationary
autoplot(crashes_mts4) + 
  autolayer(Yhat_monthly, series="Point Forecasts") + 
  ggtitle("TBATS Forecasting Model for Monthly Car Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=10), 
        legend.position = "bottom") 
```

The Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components (TBATS model) was chosen because it is known to perform well at forecasting with monthly time series. In order to use this model, we had to transform our data to be stationary, as shown in the steps previously.

From the time series plot above, we can see that the TBATS model produces a somewhat horizontal line from June 1, 2021 to January 1, 2022. The line appears to be between what would be the average line of the data before the pandemic and the average line after the pandemic. This shows that the pandemic has an impact on not only the daily forecasts but the monthly ones as well. 

```{r arima-monthly, echo=FALSE, message=FALSE, fig.width = 10}
fit_arima_monthly <- auto.arima(crashes_mts4)
fc_arima_monthly <- forecast(fit_arima_monthly, h=8)
autoplot(fc_arima_monthly) + 
  ggtitle("ARIMA Forecasting Model for Monthly Car Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9))
  
```

The autoregressive integrated moving average (ARIMA) model was also chosen because it is known to perform well at forecasting with long daily time series. Unlike the TBATS model (shown previously), the ARIMA model automatically takes the necessary differences on the original data. Therefore, no manual differencing needs to occur.

In the monthly time series plot above, we can see that the ARIMA model does a good job of producing similar spikes and dips in the forecasts compared to the original data. However, similar to the TBATS forecsts we can see that the spikes and dips are lower than the average line of crashes before the pandemic. 

```{r monthly-HW-fcast-decomp-stationary, echo=FALSE}
#Stationary
dif.covid.monthly <- HoltWinters(dif_crashes_mts4)
#summary(dif.covid.monthly)
#plot(fitted(dif.covid.monthly), main = "Box Jenkins Decomposition of Monthly Crashes (With Pandemic Data)", cex.main=1)
dif.fcast.covid.monthly <- forecast::forecast(dif.covid.monthly, h=8, level=c(80,95))
dYhat.HW <- dif.fcast.covid.monthly$mean
#crashes_mts4
#dif_crashes_mts4<-diff(crashes_mts4)
#z0<-cumsum(c(crashes_mts4[1], dif_crashes_mts4))
#all(crashes_mts4==z0) #TRUE
Yhat.HW <- cumsum(c(crashes_mts4[length(crashes_mts4)],dYhat.HW))
Yhat.HW <- ts(Yhat.HW, start = c(7, 5), frequency=12)
```

```{r monthly-HW-fcast-filter-forecast-ts-stationary, echo=FALSE, message=FALSE, fig.width = 10}
#Stationary 
autoplot(crashes_mts4) +
  autolayer(Yhat.HW, series = "Point Forecasts") +
  ggtitle("Forecasts of Monthly Car Crashes Using HoltWinters (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9), 
        legend.position = "bottom")
```

The HoltWinters model was chosen to forecast the monthly pandemic data because it does well at handling monthly data that needs to be first differenced. Similar to the TBATS model, the HoltWinters model must have data inputted that is already first differenced, it does not automatically difference the data. Therefore, we had to manually take the first difference of the monthly data and then input it into the HoltWinters() function. 

Above, we can see that the HoltWinters model produces much sharper dips and spikes than the ARIMA or TBATS models. This helps reproduce a similar pattern in the forecasts, that the other two models lack in. However, like the other models, the HoltWinters model is heavily affected by the pandemic dip in March 2020, creating forecasts that are well above the average prepandemic crash line. 

#### Monthly without Pandemic Data 

First we check the monthly pandemic data for stationarity as we did for the other data. 

```{r stationarity-trans-monthly-noncovid, echo=FALSE, eval=FALSE}
##Looking at stationarity of crashes_mts4.noncovid monthly time series 
library(urca)
acf(crashes_mts4.noncovid)
summary(ur.kpss(crashes_mts4.noncovid))
```

```{r stationarity-trans-monthly-noncovid-ii}
ndiffs(crashes_mts4.noncovid) #1
nsdiffs(crashes_mts4.noncovid) #1
#Attempt at seasonal differencing (noncovid)
season_dif_crashes_mts4.noncovid <- diff(crashes_mts4.noncovid,12)
#Looking at stationarity of seasonal difference (noncovid)
acf(season_dif_crashes_mts4.noncovid)
```

```{r stationairy-trans-monthly-noncovid-iii, echo=FALSE, eval=FALSE}
summary(ur.kpss(season_dif_crashes_mts4.noncovid))
ndiffs(season_dif_crashes_mts4.noncovid) #0
nsdiffs(season_dif_crashes_mts4.noncovid) #0
```

Unlike the other data sets, our monthly nonpandemic data needs one seasonal difference rather than a first difference. We take a seasonal difference with the diff() function and also add the argument "lag=12" to indicate a seasonal difference. Using the diff() function, we take the seasonal difference and then we check the acf of the data to make sure that it does not take too many lags to hit 0 indicating that the first difference transformed our data to be stationary.

```{r stat-trans-monthly-noncovid-plot, fig.width = 10, echo=FALSE}
cbind("Crashes" = crashes_mts4.noncovid,
      "Seasonally\n differenced" = diff(crashes_mts4.noncovid, 12)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformation of Monthly NonPandemic Data")
```

The plot above, shows the monthly nonpandemic data before the stationary transformation and after the first difference was taken. 

Now that the monthly nonpandemic data has been transformed to be stationary, the assumptions of forecasting models are met.

```{r sarima-library, echo=FALSE, message=FALSE}
#install.packages("astsa")
library(astsa)
```

```{r sarima, echo=FALSE, message=FALSE, include=FALSE}
fc_sarima <- sarima.for(crashes_mts4.noncovid, n.ahead=23, 1,0,0,0,1,1,12) 
```

```{r sarima-plot, echo=FALSE, message=FALSE, fig.width = 10}
fc_sarima <- ts(fc_sarima$pred, start=c(6,3),frequency=12)
autoplot(crashes_mts4.noncovid) + 
  autolayer(fc_sarima, series="Point Forecasts") + 
  ggtitle("SARIMA Forecasting Model for Monthly Car Crashes (Without Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9), 
        legend.position = "bottom")
```

The Seasonal Autoregressive Integrated Moving Average (SARIMA) model was chosen because it is able to automatically handle our monthly nonpandemic data that needs to be seasonally differenced. Therefore, we do not have to manually difference the data, the sarima() function will do the differencing fore us. Additionally, the SARIMA model was also chosen because it has been shown to create accurate forecasts for seasonally adjusted data. 

From the plot, we can see that the SARIMA model does a great job at matching the pattern of spikes and dips from the original data. While, the major dip in crashes due to the COVID-19 pandemic was not forecasted here, this plot shows us a good portrayal of what the crash counts may have looked if the pandemic had not occurred. 

```{r stlf, echo=FALSE, message=FALSE, fig.width = 10}
q <- crashes_mts4.noncovid %>%
  stlf(lambda = 0, h = 23, level=c(80,95)) 
q %>%
  autoplot() + 
  ggtitle("STLF Model for Monthly Car Crashes (Without Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))
```

The Seasonal and Trend Decomposition using Loess Forecasting Model (STLF) was chosen because it is able to automatically handle our monthly nonpandemic data that needs to be seasonally differenced. Therefore, like the SARIMA model, we do not need to manually difference the data, the stlf() function we automatically do it for us. It was also chosen because it has been shown to create accurate forecasts for seasonally adjusted data.

From the plot, we can see that the STLF model also does a great job at matching the pattern of spikes and dips from the original data. While, the major dip in crashes due to the COVID-19 pandemic was not forecasted here, this STLF forecasts show us a good portrayal of what the crash counts may have looked if the pandemic had not occurred. 

### Modeling Crash Injuries 

```{r, echo=FALSE, include=FALSE}
crashes_pm = crashes %>%
  filter(Injury != "", Injury != "Unknown",  WeatherCondition1 != "Unknown",
         WeatherCondition1 != "", MostHarmfulEvent != "Unknown",
         MostHarmfulEvent != "", RoadFeature != "Unknown", RoadFeature != "", 
         RoadFeature != "NaN", TrafficControlType != "NaN", RoadClassification != "NaN",
         TrafficControlType != "Unknown", TrafficControlType != "",
         RoadClassification != "Unknown", RoadClassification != "",
         PersonType != "", PersonType != "Unknown", 
         VehicleType != "All terrain vehicle (ATV)",
         VehicleType != "Farm equipment", VehicleType != "Farm tractor",
         VehicleType != "Military", VehicleType != "Motor scooter or motor bike",
         VehicleType != "Motor home/recreational vehicle", 
         VehicleType != "Tractor/doubles", VehicleType != "Truck/tractor (i.e., bobtail)",
         ContributingCircumstance1 != "Passed stopped school bus", 
         ContributingCircumstance1 != "Passed on hill", 
         ContributingCircumstance1 != "Passed on curve",
         ContributingCircumstance1 != "Driver distracted by other electronic 
         device (navigation device, DVD player, etc.) ",
         Protection != "Lighting", Protection != "Protective pads",
         Protection != "Reflective clothing", WeatherCondition1 != "Blowing sand, dirt, snow",
         WeatherCondition1 != "Severe crosswinds", MostHarmfulEvent != "Jackknife",
         MostHarmfulEvent != "RR train, engine", TrafficControlType != "RR cross bucks only",
         TrafficControlType != "RR flasher", TrafficControlType != "RR gate and flasher",
         TrafficControlType != "School zone signs", VisionObstruction != "Blinded, headlights",
         VisionObstruction != "Blinded, other lights", VisionObstruction != "Building(s)",
         VisionObstruction != "Embankment", VisionObstruction != "Sign(s)") %>%
  mutate(Injury = as.factor(Injury)) %>%
  mutate(Injury2 = if_else(Injury == "No injury", "No injury", "Injury"), 
         Injury2 = as.factor(Injury2)) %>%
  mutate(VehicleType = if_else(VehicleType %in% c("Unknown",""), 
                               "Unknown/Blank", VehicleType),
         VehicleType = as.factor(VehicleType)) %>%
  mutate(ContributingCircumstance1 = if_else(ContributingCircumstance1 %in% c("Unknown",""), 
                                             "Unknown/Blank", ContributingCircumstance1),
         ContributingCircumstance1 = as.factor(ContributingCircumstance1)) %>%
  mutate(Protection = if_else(Protection %in% c("Unable to determine",""), 
                              "Unknown/Blank", Protection),
         Protection = as.factor(Protection)) %>%
  mutate(WeatherCondition1 = as.factor(WeatherCondition1)) %>%
  mutate(MostHarmfulEvent = as.factor(MostHarmfulEvent)) %>%
  mutate(RoadFeature = as.factor(RoadFeature)) %>%
  mutate(TrafficControlType = as.factor(TrafficControlType)) %>%
  mutate(RoadClassification = as.factor(RoadClassification)) %>%
  mutate(PersonType = as.factor(PersonType)) %>%
  mutate(VisionObstruction = if_else(VisionObstruction %in% c("Unknown",""), 
                                     "Unknown/Blank", VisionObstruction),
         VisionObstruction = as.factor(VisionObstruction))
```

The original data that we used for this project included a variable called “Injury” which was made up of the following levels: no injury, A type injury (disabling), B type injury (evident), C type injury (possible), killed, and unknown. In order to look at this from a binary standpoint, we created a new variable called “Injury2” where any of the responses with A type injury, B type injury, C type injury, or killed were assigned the value of injury, the responses with no injury were assigned to no injury, and unknown and blank responses were removed. After doing so, we were left with extremely unbalanced data which can be seen in the barplot below. When you work with balanced data, parameter variability decreases and the ratio between sensitivity and specificity has been shown to be much better than when working with unbalanced data.

```{r, echo=FALSE}
crashes_pm %>% 
  ggplot() +
  geom_bar(aes(Injury2, fill=Injury2), show.legend = FALSE) +
  geom_text(stat="count", aes(x=Injury2, label=..count..), vjust=-0.25) +
  labs(title = "Frequency of Injury2",
       x = "Injury2",
       y = "Count")
```

#### Logistic Regression 

Due to this imbalance, we decided to test a variety of balancing methods to compare the accuracy of each of these using the logistic regression model to predict the value of Injury2. We used the unbalanced data as our base to compare oversampled, undersampled, and a combination of both over- and undersampled balanced datasets that we created. We decided to create two combination samples of different sizes to see if there was any difference in accuracy where one is the same size as the unbalanced data and the other is a subsample. We then fitted a logistic regression model for each of these datasets using 11 predictors. These predictors were variables that we took from our original crashes dataset that we found to be factors that impacted crash frequency when doing our exploratory data analysis early on in the project. The 11 variables that we selected were Age, VehicleType, ContributingCircumstance1, Protection, WeatherCondition1, MostHarmfulEvent, RoadFeature, TrafficControlType, RoadClassification, PersonType, and VisionObstruction. Before we could use all of these variables for our models, we had to clean the data to remove unknown, blank, and NaN responses as well as levels of each variable that did not have enough responses to be considered important (fewer than 100 responses). However, we could not remove all of the unknown and blank responses from all of the variables because if we were to do so, we would be removing too much information. To solve this problem, we mutated the data so that there was a level for the variables VehicleType, ContributingCircumstance1, Protection, and VisionObstruction titled Unknown/Blank. 

#### Random Forest 

```{r random-forest-dataset, echo=FALSE, include=FALSE}
crashes_pm.clean = crashes_pm %>%
  dplyr::select(Injury2,Age,VehicleType,ContributingCircumstance1,Protection,
           WeatherCondition1,MostHarmfulEvent,RoadFeature,
           TrafficControlType,RoadClassification,PersonType,
           VisionObstruction) %>%
  na.omit()
```

In addition to using logistic regression models to predict whether a person will sustain an injury in a crash, we decided to also fit random forest models to each of the same types of balanced datasets. Random forest is an alternative prediction model that works by “growing” many decision trees to build a forest. Before we could go about fitting the model, we had to clean the dataset we built for our logistic regression models more by omitting all NAs that were automatically removed before. Then, we were able to create the same kind of oversampled, undersampled, and combination samples to fit random forest models to. We used the same 11 predictors that we chose for the logistic regression model and then modified the same code to reach our results. 

# Results 

## Forecasting Daily and Monthly Crashes in Wake County Account for the COVID-19 Pandemic Effect

These multi-step forecasts and comparison of models help us determine which model best forecasts for daily pandemic data, daily nonpandemic data, monthly pandemic data, and monthly nonpandemic data. Additionally, we have determined that the pandemic data not only effected the crash counts in Wake County, NC, but they also affect the future accuracy of forecasting crashes. 

### Daily with Pandemic Data 

```{r tbats-ms-stationary, echo=FALSE, message=FALSE, include=FALSE}
#Stationary
#getting differenced training and test sets 
dif_y2 <- msts(dif_crashests2, seasonal.periods=c(7,365.25))
dif_training_TBATS2 <- subset(dif_y2, end=length(dif_y2)-151)
dif_test_TBATS2 <- subset(dif_y2, start=length(dif_y2)-150)
dif_crashes.train_TBATS2 <- tbats(dif_training_TBATS2)
dif_fc_train_TBATS2 <- forecast(dif_crashes.train_TBATS2, h=151)
#getting point forecasts 
dYhat_ms_TBATS <- dif_fc_train_TBATS2$mean
#setting up normal ts training and test sets 
y2 <- msts(crashests2, seasonal.periods=c(7,365.25))
training_TBATS2 <- subset(y2, end=length(y2)-151)
test_TBATS2 <- subset(y2, start=length(y2)-150)
#reverting back to original points   
Yhat_ms_TBATS <- cumsum(c(training_TBATS2[length(training_TBATS2)], dYhat_ms_TBATS))
Yhat_ms_TBATS <- ts(Yhat_ms_TBATS, start = c(2021, 1), frequency = 365)
```

```{r tbats-ms-stationary-plot, echo=FALSE, fig.width = 10, fig.align='center'}
#Stationary 
autoplot(training_TBATS2) + 
  autolayer(Yhat_ms_TBATS, series="Point Forecasts") + 
  autolayer(test_TBATS2, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step TBATS Daily Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom")
dif_crashes.test_TBATS2 <- tbats(dif_test_TBATS2)
accuracy(dif_crashes.test_TBATS2)
```

For the plot above, we split the original data into a training set and a test set. The training set was data from January 1, 2015 to December 31, 2020. The test set was data from January 1, 2021 to May 31, 2021. Using the training set, the TBATS model forecasted car crash counts from January 1, 2021 to May 31, 2021. In the plot, we can see that the TBATS forecasts follow the test set very well. However, the TBATS model could have been better at predicting sharper dips and spikes. 

As we can see from the accuracy() function output, the overall errors of the TBATS model are relatively small. We will specifically look at the root mean square error which is 12.39. 

```{r ms-arima, echo=FALSE, fig.width = 10}
#Stationary
training_arima <- subset(crashests2, end=length(crashests2)-151)
test_arima <- subset(crashests2, start=length(crashests2)-150)
crashes.train_arima <- auto.arima(training_arima)
fc_train_arima <- forecast(crashes.train_arima, h=151)
autoplot(fc_train_arima) + 
  autolayer(test_arima, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step ARIMA Daily Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom")
crashes.test_arima <- arima(test_arima)
accuracy(crashes.test_arima)
```

For the plot above, we split the original data into a training set and a test set. The training set was data from January 1, 2015 to December 31, 2020. The test set was data from January 1, 2021 to May 31, 2021. Using the training set, the ARIMA model forecasted car crash counts from January 1, 2021 to May 31, 2021. In the plot, we can see that the ARIMA forecasts follow the test set somewhat well. However, the ARIMA model could have done much better at predicting sharper dips and spikes. 

As we can see from the accuracy() function output, the overall errors of the ARIMA model are relatively small. We will specifically look at the root mean square error which is 15.44 which is about 3 points higher than the TBATS RMSE. 

```{r combination, echo=FALSE, message=FALSE, fig.width = 10}
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)
train <- window(crashests2, end=c(2019,12))
h <- length(crashests2) - length(train)
#ARIMA model 
ARIMA <- forecast::forecast(auto.arima(train, lambda=0, biasadj=TRUE), h=h)
#TBATS model 
train_tbats <- msts(dif_crashests2, end=c(2019,12), seasonal.periods = c(7,365.25))
TBATS <- forecast::forecast(tbats(train_tbats, biasadj = TRUE), h=h)
dYhat_combo <- TBATS$mean
  
Yhat_combo <- cumsum(c(train[length(train)], dYhat_combo))
Yhat_combo <- ts(Yhat_combo, start = c(2019, 12), frequency = 365)
Combination <- (ARIMA[["mean"]] + Yhat_combo)/2
true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2021/05/31"))
true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2019,1), end = c(2021,153), 
                            frequency = 365)
autoplot(train) +
  autolayer(true_noncovid_crashes) +
  autolayer(Combination, series="Combination", alpha=0.8) +
  autolayer(ARIMA, series="ARIMA", PI=F) +
  autolayer(Yhat_combo, series="TBATS", alpha=0.7) +
  xlab("Date") + ylab("Crashes") +
  ggtitle("Time Series Combination (With Pandemic Data)") + 
  theme(legend.position = "bottom")
c(ARIMA = accuracy(ARIMA, crashests2)["Test set", "RMSE"],
  TBATS = accuracy(Yhat_combo, crashests2)["Test set", "RMSE"],
  Combination =
    accuracy(Combination, crashests2)["Test set", "RMSE"])
```

This model shows the TBATS model, the ARIMA model, as well as the combination model. The combination model takes the average of the TBATS and the ARIMA forecasts to create a combination forecast. 

The model with the smallest root mean square error (smallest error in general), is said to perform best. From the root mean square error shown below the plot, we can see that the TBATS model performs much better than the ARIMA and the Combination model.

In conclusion, out of the TBATS, ARIMA, and Combination models, the TBATS model creates the most accurate forecasts of daily crashes based on daily pandemic data. 

### Daily without Pandemic Data 

```{r ms-arima-noncovid, echo=FALSE, fig.width = 10}
#Stationary
training_arima_noncovid <- subset(crashests.noncovid, end=length(crashests.noncovid)-426)
test_arima_noncovid <- subset(crashests.noncovid, start=length(crashests.noncovid)-425)
crashes.train_arima_noncovid <- auto.arima(training_arima_noncovid)
fc_train_arima_noncovid <- forecast(crashes.train_arima_noncovid, h=426)
autoplot(fc_train_arima_noncovid) + 
  autolayer(test_arima_noncovid, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step ARIMA Daily Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom", title = element_text(size=9))
crashes.test_arima_noncovid <- arima(test_arima_noncovid)
accuracy(crashes.test_arima_noncovid)
```

For the plot above, we split the original data into a training set and a test set. The training set was data from January 1, 2015 to December 31, 2019. The test set was data from January 1, 2020 to March 1, 2020. We purposefully excluded the pandemic data in this multi-step time series to help determine how well each model forecasts crashes without pandemic data. Using the training set, the ARIMA model forecasted car crash counts from January 1, 2020 to March 1, 2020. In the plot, we can see that the ARIMA forecasts create a horizontal line which sits in the middle of the test set throughout the time series. However, the ARIMA model could have done much better at predicting sharper dips and spikes. 

As we can see from the accuracy() function output, the overall errors of the ARIMA model are somewhat larger than the pandemic daily time series forecasts. We will specifically look at the root mean square error which is 17.41 which is higher than both the TBATS model and the ARIMA model for daily pandemic data. 

```{r combination-noncovid, echo=FALSE, message=FALSE, fig.width = 10}
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), 
                         end = c(2020,59), frequency = 365)
train.noncovid <- window(crashests2, end=c(2019,12))
h.noncovid <- 426
#ARIMA model 
ARIMA.noncovid <- forecast::forecast(auto.arima(train.noncovid, lambda=0, biasadj=TRUE), h=h)
#TBATS model 
train_tbats.noncovid <- msts(dif_crashests.noncovid, end=c(2019,12), seasonal.periods = c(7,365.25))
TBATS.noncovid <- forecast::forecast(tbats(train_tbats.noncovid, biasadj = TRUE), h=h)
dYhat_combo.noncovid <- TBATS.noncovid$mean
  
Yhat_combo.noncovid <- cumsum(c(train.noncovid[length(train.noncovid)], dYhat_combo.noncovid))
Yhat_combo.noncovid <- ts(Yhat_combo.noncovid, start = c(2019, 12), frequency = 365)
Combination.noncovid <- (ARIMA.noncovid[["mean"]] + Yhat_combo.noncovid)/2
true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2021/05/31"))
true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2019,1), end = c(2021,153), 
                            frequency = 365)
autoplot(train.noncovid) +
  autolayer(true_noncovid_crashes, series = "True Count") +
  autolayer(Combination.noncovid, series="Combination", alpha=0.8) +
  autolayer(ARIMA.noncovid, series="ARIMA", PI=F) +
  autolayer(Yhat_combo.noncovid, series="TBATS", alpha=0.7) +
  xlab("Date") + ylab("Crashes") +
  ggtitle("Time Series Combination (Without Pandemic Data)") + 
  theme(legend.position = "bottom")
c(ARIMA = accuracy(ARIMA.noncovid, crashests2)["Test set", "RMSE"],
  TBATS = accuracy(Yhat_combo.noncovid, crashests2)["Test set", "RMSE"],
  Combination =
    accuracy(Combination.noncovid, crashests2)["Test set", "RMSE"])
```

This model shows the TBATS model, the ARIMA model, as well as the combination model. The combination model takes the average of the TBATS and the ARIMA forecasts to create a combination forecast for daily nonpandemic data. 

The model with the smallest root mean square error (smallest error in general), is said to perform best. From the root mean square error shown below the plot, we can see that the ARIMA model performs slightly better than the TBATS and the Combination model.

In conclusion, out of the TBATS, ARIMA, and Combination models, the ARIMA model creates the most accurate forecasts of daily crashes based on daily nonpandemic data. 

### Monthly with Pandemic Data 

```{r tbats-ms-stationary-monthly, echo=FALSE, message=FALSE, include=FALSE}
#Stationary
dif_y2_monthly <- msts(dif_crashes_mts4, seasonal.periods=12)
dif_training_TBATS2_monthly <- subset(dif_y2_monthly, end=length(dif_y2_monthly)-5)
dif_test_TBATS2_monthly <- subset(dif_y2_monthly, start=length(dif_y2_monthly)-4)
dif_crashes.train_TBATS2_monthly <- tbats(dif_training_TBATS2_monthly)
dif_fc_train_TBATS2_monthly <- forecast(dif_crashes.train_TBATS2_monthly, h=5)
dYhat_ms_TBATS_monthly <- dif_fc_train_TBATS2_monthly$mean
y2 <- msts(crashes_mts4, seasonal.periods=12)
training_TBATS2 <- subset(y2, end=length(y2)-5)
test_TBATS2 <- subset(y2, start=length(y2)-4)
  
#Yhat_ms_TBATS <- cumsum(c(crashests2[length(crashests2)], dYhat_ms_TBATS))
Yhat_ms_TBATS_monthly <- cumsum(c(training_TBATS2[length(training_TBATS2)], dYhat_ms_TBATS_monthly))
Yhat_ms_TBATS_monthly <- ts(Yhat_ms_TBATS_monthly, start = c(6,12), frequency=12)
```

```{r tbats-ms-stationary-plot-monthly, echo=FALSE, fig.width = 10, message=FALSE}
#Stationary 
autoplot(training_TBATS2) + 
  autolayer(Yhat_ms_TBATS_monthly, series="Point Forecasts") + 
  autolayer(test_TBATS2, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step TBATS Monthly Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom", title=element_text(size=10)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))
dif_crashes.test_TBATS2_monthly <- tbats(dif_test_TBATS2_monthly)
accuracy(dif_crashes.test_TBATS2_monthly)
```

For the plot above, we split the original monthly data into a training set and a test set. The training set was data from January 1, 2015 to December 31, 2020. The test set was data from January 1, 2021 to May 31, 2021. Using the training set, the TBATS model forecasted car crash counts from January 1, 2021 to May 31, 2021. In the plot, we can see that the TBATS forecasts create more dips and spikes than necessary. However, the forecasts are still in the same general count.  

As we can see from the accuracy() function output, the overall errors of the TBATS model are larger. We will specifically look at the root mean square error which is 62.13. This is a significantly larger error than the daily data forecasting methods produce. This may be due to the fact that there is smaller data frequencies, therefore, giving the model less time to adjust and make accurate predictions. Additionally, the large dip due to the pandemic has a larger effect on monthly data due to the lower frequencies in data, most likely making forecasting more difficult. 

```{r ms-arima-monthly, echo=FALSE, message=FALSE, fig.width = 10}
#Stationary
training_arima_monthly <- subset(crashes_mts4, end=length(crashes_mts4)-5)
test_arima_monthly <- subset(crashes_mts4, start=length(crashes_mts4)-4)
crashes.train_arima_monthly <- auto.arima(training_arima_monthly)
fc_train_arima_monthly <- forecast(crashes.train_arima_monthly, h=5)
autoplot(fc_train_arima_monthly) + 
  autolayer(test_arima_monthly, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step ARIMA Monthly Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9), 
        legend.position = "bottom")
crashes.test_arima_monthly <- arima(test_arima_monthly)
accuracy(crashes.test_arima_monthly)
```

For the plot above, we split the original monthly data into a training set and a test set. The training set was data from January 1, 2015 to December 31, 2020. The test set was data from January 1, 2021 to May 31, 2021. Using the training set, the ARIMA model forecasted car crash counts from January 1, 2021 to May 31, 2021. In the plot, we can see that the ARIMA forecasts creates a large dip in the data, while the true data shown by the pink test set stays at a consistent level around 1500 crashes. 

As we can see from the accuracy() function output, the overall errors of the ARIMA model are very large. We will specifically look at the root mean square error which is 119.68. This is a significantly larger error than the daily data forecasting methods produce as well as much larger than the TBATS model. This may be due to the fact that there is smaller data frequencies, therefore, giving the model less time to adjust and make accurate predictions. Additionally, the large dip due to the pandemic seems to have a large effect on the forecasts most likely resulting in a similar large dip made by the ARIMA forecast. 

```{r ms-HW-monthly-forecast-ts-stationary, echo=FALSE, message=FALSE, fig.width = 10}
#Stationary  
dif_training_HW_monthly <- subset(dif_crashes_mts4, end=length(dif_crashes_mts4)-5)
dif_test_HW_monthly <- subset(dif_crashes_mts4, start=length(dif_crashes_mts4)-4)
dif_crashes.train_HW_monthly <- HoltWinters(dif_training_HW_monthly)
dif_fc_HW <- forecast::forecast(dif_crashes.train_HW_monthly, h=5)
dYhat_ms_HW <- dif_fc_HW$mean
Yhat_ms_HW <- cumsum(c(crashes_mts4[length(crashes_mts4)],dYhat_ms_HW))
Yhat_ms_HW <- ts(Yhat_ms_HW, start = c(7,1), frequency=12)
training_HW_monthly <- subset(crashes_mts4, end=length(crashes_mts4)-5)
test_HW_monthly <- subset(crashes_mts4, start=length(crashes_mts4)-4)
true_count_monthly <- ts(crashes_mts4, start=c(7,1), end=c(7,5), frequency=12)
autoplot(training_HW_monthly) + 
  autolayer(Yhat_ms_HW, series = "Point Forecasts") + 
  autolayer(test_HW_monthly, series = "Test Set") + 
  ggtitle("Multi-Step HoltWinters Monthly Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 9))
``` 

For the plot above, we split the original monthly data into a training set and a test set. The training set was data from January 1, 2015 to December 31, 2020. The test set was data from January 1, 2021 to May 31, 2021. Using the training set, the HoltWinters model forecasted car crash counts from January 1, 2021 to May 31, 2021. In the plot, we can see that the HoltWinters forecasts creates a large dip in the data, while the true data shown by the pink test set stays at a consistent level around 1500 crashes.   

```{r combination-monthly, echo=FALSE, message=FALSE, fig.width = 10}
train_monthly <- window(crashes_mts4, end=c(5,12))
h_monthly <- length(crashes_mts4) - length(train_monthly)
#ARIMA model 
ARIMA_monthly <- forecast::forecast(auto.arima(train_monthly, lambda=0, biasadj=TRUE), h=h_monthly)
#TBATS model 
train_tbats_monthly <- msts(dif_crashes_mts4, end=c(5,12), seasonal.periods = 12)
TBATS_monthly <- forecast::forecast(tbats(train_tbats_monthly, biasadj = TRUE), h=h_monthly)
dYhat_combo_monthly <- TBATS_monthly$mean
  
Yhat_combo_monthly <- cumsum(c(train_monthly[length(train_monthly)], dYhat_combo_monthly))
Yhat_combo_monthly <- ts(Yhat_combo_monthly, start = c(5, 12), frequency = 12)
#HoltWinters Model 
HW_monthly <- forecast::forecast(HoltWinters(dif_crashes_mts4), h=h_monthly)
dYhat_HW_monthly <- HW_monthly$mean
Yhat_HW_monthly <- cumsum(c(train_monthly[length(train_monthly)],dYhat_HW_monthly))
Yhat_HW_monthly <- ts(Yhat_HW_monthly, start = c(5,12), frequency=12)
Combination_monthly <- (ARIMA_monthly[["mean"]] + Yhat_combo_monthly + Yhat_HW_monthly)/3
true_count_monthly <- ts(crashes_mts4, start=c(6,1), end=c(7,5), frequency=12)
autoplot(train_monthly) +
  autolayer(true_count_monthly, series="True Count") + 
  autolayer(Combination_monthly, series="Combination", alpha=0.8) +
  autolayer(ARIMA_monthly, series="ARIMA", PI=F) +
  autolayer(Yhat_combo_monthly, series="TBATS", alpha=0.7) +
  autolayer(Yhat_HW_monthly, series="HoltWinters", alpha=0.7) + 
  xlab("Date") + ylab("Crashes") +
  ggtitle("Time Series Combination (With Pandemic Data)") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 9)) 
  
c(ARIMA = accuracy(ARIMA_monthly, crashes_mts4)["Test set", "RMSE"],
  TBATS = accuracy(Yhat_combo_monthly, crashes_mts4)["Test set", "RMSE"],
  HW = accuracy(Yhat_HW_monthly, crashes_mts4)["Test set", "RMSE"],
  Combination =
    accuracy(Combination_monthly, crashes_mts4)["Test set", "RMSE"])
```

This model shows the TBATS model, the ARIMA model, the HoltWinters model as well as the combination model. The combination model takes the average of the TBATS, the ARIMA, and the HoltWinters forecasts to create a combination forecast for monthly pandemic data. 

From the root mean square error, we can see that all of the models did not perform very well, most likely due to the unforeseen large dip in car crashes during the pandemic that very much effects future forecasts. 

In conclusion, out of the four models, the HoltWinters model creates the most accurate forecasts of monthly crashes based on monthly pandemic data.

### Monthly without Pandemic Data 

```{r ms-sarima, echo=FALSE, message=FALSE, include=FALSE}
training_sarima <- subset(crashes_mts4.noncovid, end=length(crashes_mts4.noncovid)-14)
test_sarima <- subset(crashes_mts4.noncovid, start=length(crashes_mts4.noncovid)-13)
crashes.train_sarima <- sarima.for(training_sarima, n.ahead=14, 1,0,0,0,1,1,12)
crashes.train_sarima <- ts(crashes.train_sarima$pred, start=c(5,1),frequency=12)
```

```{r ms-sarima-plot, echo=FALSE, message=FALSE, fig.width = 10}
autoplot(training_sarima) + 
  autolayer(crashes.train_sarima, series="Point Forecasts") + 
  autolayer(test_sarima, series = "Test Set") + 
  ggtitle("Multi-Step SARIMA Monthly Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size =10))
crashes.test_sarima_monthly <- arima(test_sarima)
accuracy(crashes.test_sarima_monthly)
```

For the plot above, we split the original monthly data into a training set and a test set. The training set was data from January 1, 2015 to December 31, 2019. The test set was data from January 1, 2020 to March 1, 2020. Using the training set, the SARIMA model forecasted car crash counts from January 1, 2020 to March 1, 2021. In the plot, we can see that the SARIMA forecasts follows the same patter of the test set of true crashes very well. However, the forecasts barely stay in the confidence interval, therefore, the pattern is good, but the prediction values are not very accurate.

As we can see from the accuracy() function output, the overall errors of the SARIMA model are very large. We will specifically look at the root mean square error which is 164.20. This is a significantly larger error than the daily data forecasting methods. This may be due to the fact that there is smaller data frequencies, therefore, giving the model less time to adjust and make accurate predictions.

```{r noncovid-ms-STLF-forecast-ts, echo=FALSE, message=FALSE, fig.width = 10}
training_STLF_noncovid_monthly <- subset(crashes_mts4.noncovid, end=length(crashes_mts4.noncovid)-14)
test_STLF_noncovid_monthly <- subset(crashes_mts4.noncovid, start=length(crashes_mts4.noncovid)-13)
crashes.train_STLF_noncovid_monthly <- stlf(training_STLF_noncovid_monthly, lambda = 0, h = 14, level=c(80,95))
crashes.train_STLF_noncovid_monthly %>%
  forecast::forecast(h=14) %>%
  autoplot() + 
  autolayer(test_STLF_noncovid_monthly, series="Test Set") + 
  ggtitle("Multi-Step STLF Monthly Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 10))
crashes.test_stlf_monthly <- arima(test_STLF_noncovid_monthly)
accuracy(crashes.test_stlf_monthly)
```

For the plot above, we split the original monthly data into a training set and a test set. The training set was data from January 1, 2015 to December 31, 2019. The test set was data from January 1, 2020 to March 1, 2020. Using the training set, the STLF model forecasted car crash counts from January 1, 2020 to March 1, 2021. In the plot, we can see that the STLF forecasts follows the same patter of the test set of true crashes very well. However, the forecasts barely stay in the confidence interval, therefore, the pattern is good, but the prediction values are not very accurate.

As we can see from the accuracy() function output, the overall errors of the STLF model are very large. We will specifically look at the root mean square error which is 164.20. This is a significantly larger error than the daily data forecasting methods. This may be due to the fact that there is smaller data frequencies, therefore, giving the model less time to adjust and make accurate predictions.

```{r combination-monthly-noncovid, echo=FALSE, message=FALSE, include=FALSE}
train_monthly.noncovid <- window(crashes_mts4.noncovid, end=c(5,12))
h_monthly.noncovid <- length(crashes_mts4.noncovid) - length(train_monthly.noncovid)
#SARIMA Model 
SARIMA <- sarima.for(train_monthly.noncovid, n.ahead=14, 1,0,0,0,1,1,12) 
SARIMA <- ts(SARIMA$pred, start=c(6,1),frequency=12)
```

```{r combination-monthly-noncovid-plot, echo=FALSE, message=FALSE, fig.width = 10}
#STLF Model 
STLF <- stlf(train_monthly.noncovid, lambda = 0, h = 14) 
Combination_monthly.noncovid <- (SARIMA + STLF[["mean"]])/2
true_count_monthly <- ts(crashes_mts4, start=c(6,1), end=c(7,1), frequency=12)
autoplot(train_monthly.noncovid) +
  autolayer(true_count_monthly, series="True Count") + 
  autolayer(SARIMA, series="SARIMA") +
  autolayer(STLF, series="STLF", PI=F) +
  autolayer(Combination_monthly.noncovid, series="Combination") +
  xlab("Date") + ylab("Crashes") +
  ggtitle("Time Series Combination (Without Pandemic Data)") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 10)) 
  
c(SARIMA = accuracy(SARIMA, crashes_mts4.noncovid)["Test set", "RMSE"],
  STLF = accuracy(STLF, crashes_mts4.noncovid)["Test set", "RMSE"],
  Combination =
    accuracy(Combination_monthly.noncovid, crashes_mts4.noncovid)["Test set", "RMSE"])
```

This model shows the SARIMA model and the STLF model as well as the combination model. The combination model takes the average of the SARIMA and STLF forecasts to create a combination forecast for monthly pandemic data. As seen in the plot, the three models perform very similar and pretty well. However, due to the covid pandemic in 2020, the models were unable to predict the dramatic decrease in crash counts during the pandemic.

In conclusion, out of the three models, the Combination model creates the most accurate forecasts of monthly crashes based on monthly nonpandemic data.

## Modeling Crash Injuries 

This section is dedicated to showing the results of the predictive modeling of the outcomes of crashes in terms of crashes. 

From the logistic regression and the random forest model, we can determine the main conditions that lead to worse outcomes in terms of injury. Hopefully resulting in these models helping policy makers to make the roads safer by adding necessary precautions and transportation development. 

### Logistic Regression 

```{r unbalanced-data-pred-acc, echo=FALSE, include=FALSE}
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
(stats = calc_stats(tab, prevalence = NULL, positive = "Injury"))
```

```{r oversampled-dataset, echo=FALSE, include=FALSE}
df_crashes_pm_Injury_ind <- which(crashes_pm$Injury2 == "Injury")
df_crashes_pm_NoInjury_ind <- which(crashes_pm$Injury2 == "No injury")
oversample_df1 <- crashes_pm[c(df_crashes_pm_NoInjury_ind, 
                               rep(df_crashes_pm_Injury_ind, 5)), ]
```

```{r oversample-data-pred-acc, echo=FALSE, include=FALSE}
set.seed(101) #for reproducibility of results
sample2 <- sample(c(TRUE, FALSE), nrow(oversample_df1), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
oversample_df1.train <- oversample_df1[sample2, ]
oversample_df1.test <- oversample_df1[!sample2, ]
oversample_df1.fit.train <- glm(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                   WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                   TrafficControlType+RoadClassification+PersonType+
                                   AlcoholResultType+VisionObstruction, 
                                 data=oversample_df1.train, family="binomial") #fitting model on training set
oversample_df1.pred.prob <- predict(oversample_df1.fit.train, newdata=oversample_df1.test, 
                                     type="response") #predicting prob. of default=1 for test set
oversample_df1.pred <- ifelse(oversample_df1.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
(tab2 <- table(pred=oversample_df1.pred, actual=oversample_df1.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy2=mean(oversample_df1.pred==oversample_df1.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats2 = calc_stats(tab2, prevalence = NULL, positive = "Injury"))
```

```{r undersampled-dataset, echo=FALSE, include=FALSE}
crashes_pm_no_injury = crashes_pm %>%
  filter(Injury2 == "No injury")
crashes_pm_injury = crashes_pm %>%
  filter(Injury2 == "Injury")
s = sample(1:nrow(crashes_pm_no_injury), 35000, replace = F)
undersample_df1 = crashes_pm_injury %>%
  bind_rows(crashes_pm_no_injury[s,])
```

```{r undersample-data-pred-acc, echo=FALSE, include=FALSE}
set.seed(101) #for reproducibility of results
sample3 <- sample(c(TRUE, FALSE), nrow(undersample_df1), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
undersample_df1.train <- undersample_df1[sample3, ]
undersample_df1.test <- undersample_df1[!sample3, ]
undersample_df1.fit.train <- glm(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                    WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                    TrafficControlType+RoadClassification+PersonType+
                                    VisionObstruction, 
                                  data=undersample_df1.train, family="binomial") #fitting model on training set
undersample_df1.pred.prob <- predict(undersample_df1.fit.train, newdata=undersample_df1.test, 
                                      type="response") #predicting prob. of default=1 for test set
undersample_df1.pred <- ifelse(undersample_df1.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
(tab3 <- table(pred=undersample_df1.pred, actual=undersample_df1.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy3=mean(undersample_df1.pred==undersample_df1.test$Injury2, na.rm=T)*100) #percent of correct predictions in test
(stats3 = calc_stats(tab3, prevalence = NULL, positive = "Injury"))
```

```{r combo-datasets, echo=FALSE, include=FALSE}
overunder <- ROSE::ovun.sample(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                 WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                 TrafficControlType+RoadClassification+PersonType+VisionObstruction, 
                               data = crashes_pm.train, method = "both", N = 245633)$data
overunder2 <- ROSE::ovun.sample(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                 WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                 TrafficControlType+RoadClassification+PersonType+VisionObstruction, 
                               data = crashes_pm.train, method = "both", N = 100000)$data
```

```{r combo-data-pred-acc, echo=FALSE, include=FALSE}
set.seed(101) #for reproducibility of results
sample4 <- sample(c(TRUE, FALSE), nrow(overunder), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
overunder.train <- overunder[sample4, ]
overunder.test <- overunder[!sample4, ]
overunder.fit.train <- glm(Injury2~., data=overunder.train, family="binomial") #fitting model on training set
overunder.pred.prob <- predict(overunder.fit.train, newdata=overunder.test, 
                                type="response") #predicting prob. of default=1 for test set
overunder.pred <- ifelse(overunder.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
overunder.pred <- factor(overunder.pred, levels = c("No injury", "Injury")) #predicting `default` based on prob estimates
(tab4 <- table(pred=overunder.pred, actual=overunder.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy4=mean(overunder.pred==overunder.test$Injury2, na.rm=T)*100) #percent of correct predictions in test
(stats4 = calc_stats(tab4, prevalence = NULL, positive = "Injury"))
set.seed(101) #for reproducibility of results
sample5 <- sample(c(TRUE, FALSE), nrow(overunder2), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
overunder.train2 <- overunder[sample5, ]
overunder.test2 <- overunder[!sample5, ]
overunder.fit.train2 <- glm(Injury2~., data=overunder.train2, family="binomial") #fitting model on training set
overunder.pred.prob2 <- predict(overunder.fit.train2, newdata=overunder.test2, 
                                type="response") #predicting prob. of default=1 for test set
overunder.pred2 <- ifelse(overunder.pred.prob2>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
overunder.pred2 <- factor(overunder.pred2, levels = c("No injury", "Injury")) #predicting `default` based on prob estimates
(tab5 <- table(pred=overunder.pred2, actual=overunder.test2$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy5=mean(overunder.pred2==overunder.test2$Injury2, na.rm=T)*100) #percent of correct predictions in test
(stats5 = calc_stats(tab5, prevalence = NULL, positive = "Injury"))
```

After running the logistic regression models on each of the datasets, we were able to compile a table where the statistics from each of the balanced datasets and the original unbalanced dataset to compare the accuracy of the predictions. The balanced accuracy and F1 score are the two statistics that will tell us the most about the accuracy of each model. Balanced accuracy takes the mean of sensitivity and specificity while the F1 score is the harmonic mean of precision and recall. As shown in the table below, we can see large differences in both of these statistics when comparing the unbalanced, oversampled, and undersampled data with the combination data. The combination full dataset and the combination subsample both have an F1 score of about 0.7 and a balanced accuracy of about 0.69 which is much better than the other samples with results ranging from 0.22 to 0.47.

```{r StatisticsComparison-Table, echo=FALSE}
(StatisticsComparison = cbind(SampleType = c("Unbalanced", "Oversample", "Undersample", "Combination Full Sample", 
        "Combination Sub-Sample"),round(rbind(stats, stats2, stats3, stats4, stats5), 4)))
```

The full combination dataset proved to be the most accurate of the datasets for our logistic regression model so we chose to analyze its fitted model. We ran the stepAIC() command in R to make sure that all of the variables were significant to the model and because none of them were removed, we will be looking at the fitted model itself. In addition to the regular summary, we added the odds ratio to the final data we analyzed. When looking at this table and the various results, we can highlight some important variables. If the MostHarmfulEvent of an accident is Overturn/rollover, then you are 5.0065 times more likely to be injured. If the VehicleType is a Motorcycle, then you are 31.7351 more likely to be injured in a crash. These are some of the more expected results but we can also look at a few that defied our initial perception of the variable's effect on the injury outcome of a crash. For example, if the WeatherCondition1 at the time of the crash is Snow, a condition that is typically known to heighten the number of crashes, you are only 0.6251 more likely to be injured in a crash.

```{r Injury2.fit4-table, echo=FALSE}
Injury2.fit4 <- glm(Injury2~., data=overunder, family = "binomial")
require(broom)
out <- tidy(Injury2.fit4)
Injury2.fit4_Summary = cbind(out, odds.ratio = round(exp(coef(Injury2.fit4)), digits = 4))
Injury2.fit4.summary = dplyr::select(Injury2.fit4_Summary, -term)
require(knitr)
kable(Injury2.fit4.summary)
```

### Random Forest 

```{r unbalanced-data-pred-acc-rf, echo=FALSE, include=FALSE}
library(ranger)
set.seed(101) #for reproducibility of results
sample.rf <- sample(c(TRUE, FALSE), nrow(crashes_pm.clean), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
crashes_pm.clean.train <- crashes_pm.clean[sample.rf, ]
crashes_pm.clean.test <- crashes_pm.clean[!sample.rf, ]
crashes_pm.clean.fit.train<-ranger::ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                 WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                 TrafficControlType+RoadClassification+PersonType+
                                 VisionObstruction, data=crashes_pm.clean, 
                               num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
crashes_pm.clean.pred <- predict(crashes_pm.clean.fit.train, data=crashes_pm.clean.test, type="response")
(tab.rf <- table(pred=crashes_pm.clean.pred$predictions, actual=crashes_pm.clean.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf=mean(crashes_pm.clean.pred$predictions==crashes_pm.clean.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf = calc_stats(tab.rf, prevalence = NULL, positive = "Injury"))
```

```{r oversampled-dataset-rf, echo=FALSE, include=FALSE}
df_crashes_pm.clean_Injury_ind <- which(crashes_pm.clean$Injury2 == "Injury")
df_crashes_pm.clean_NoInjury_ind <- which(crashes_pm.clean$Injury2 == "No injury")
oversample_df2 <- crashes_pm.clean[c(df_crashes_pm.clean_NoInjury_ind, 
                               rep(df_crashes_pm.clean_Injury_ind, 5)), ]
```

```{r oversample-data-pred-acc-rf, echo=FALSE, include=FALSE}
set.seed(101) #for reproducibility of results
sample.rf2 <- sample(c(TRUE, FALSE), nrow(oversample_df2), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
oversample_df2.train <- oversample_df2[sample.rf2, ]
oversample_df2.test <- oversample_df2[!sample.rf2, ]
oversample_df2.fit.train <- ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                     WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                     TrafficControlType+RoadClassification+PersonType+
                                     VisionObstruction, data=oversample_df2.train, 
                                   num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
oversample_df2.pred <-predict(oversample_df2.fit.train, data=oversample_df2.test, type="response")
(tab.rf2 <- table(pred=oversample_df2.pred$predictions, actual=oversample_df2.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf2=mean(oversample_df2.pred$predictions==oversample_df2.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf2 = calc_stats(tab.rf2, prevalence = NULL, positive = "Injury"))
```

```{r undersampled-data-rf, echo=FALSE, include=FALSE}
crashes_pm.clean_no_injury = crashes_pm.clean %>%
  filter(Injury2 == "No injury")
crashes_pm.clean_injury = crashes_pm.clean %>%
  filter(Injury2 == "Injury")
s2 = sample(1:nrow(crashes_pm.clean_no_injury), 35000, replace = F)
undersample_df2 = crashes_pm.clean_injury %>%
  bind_rows(crashes_pm.clean_no_injury[s2,])
```

```{r undersample-data-pred-acc-rf, echo=FALSE, include=FALSE}
set.seed(101) #for reproducibility of results
sample.rf3 <- sample(c(TRUE, FALSE), nrow(undersample_df2), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
undersample_df2.train <- undersample_df2[sample.rf3, ]
undersample_df2.test <- undersample_df2[!sample.rf3, ]
undersample_df2.fit.train <- ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                     WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                     TrafficControlType+RoadClassification+PersonType+
                                     VisionObstruction, data=undersample_df2.train, 
                                   num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
undersample_df2.pred <-predict(undersample_df2.fit.train, data=undersample_df2.test, type="response")
(tab.rf3 <- table(pred=undersample_df2.pred$predictions, actual=undersample_df2.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf3=mean(undersample_df2.pred$predictions==undersample_df2.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf3 = calc_stats(tab.rf3, prevalence = NULL, positive = "Injury"))
```

```{r combo-datasets-rf, echo=FALSE, include=FALSE}
overunder.rf <- ROSE::ovun.sample(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                 WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                 TrafficControlType+RoadClassification+PersonType+VisionObstruction, 
                               data = crashes_pm.clean.train, method = "both", N = 245633)$data
overunder2.rf <- ROSE::ovun.sample(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                     WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                     TrafficControlType+RoadClassification+PersonType+VisionObstruction, 
                                   data = crashes_pm.clean.train, method = "both", N = 100000)$data
```

```{r combo-data-pred-acc-rf, echo=FALSE, include=FALSE}
set.seed(101) #for reproducibility of results
sample.rf4 <- sample(c(TRUE, FALSE), nrow(overunder.rf), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
overunder.rf.train <- overunder.rf[sample.rf4, ]
overunder.rf.test <- overunder.rf[!sample.rf4, ]
overunder.rf.fit.train <- ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                      WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                      TrafficControlType+RoadClassification+PersonType+
                                      VisionObstruction, data=overunder.rf.train, 
                                    num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
overunder.rf.pred <-predict(overunder.rf.fit.train, data=overunder.rf.test, type="response")
(tab.rf4 <- table(pred=overunder.rf.pred$predictions, actual=overunder.rf.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf4=mean(overunder.rf.pred$predictions==overunder.rf.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf4 = calc_stats(tab.rf4, prevalence = NULL, positive = "Injury"))
set.seed(101) #for reproducibility of results
sample.rf5 <- sample(c(TRUE, FALSE), nrow(overunder2.rf), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
overunder2.rf.train <- overunder2.rf[sample.rf5, ]
overunder2.rf.test <- overunder2.rf[!sample.rf5, ]
overunder2.rf.fit.train <- ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                   WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                   TrafficControlType+RoadClassification+PersonType+
                                   VisionObstruction, data=overunder2.rf.train, 
                                 num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
overunder2.rf.pred <-predict(overunder2.rf.fit.train, data=overunder2.rf.test, type="response")
#overunder2.rf.pred <- ifelse(overunder2.rf.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
(tab.rf5 <- table(pred=overunder2.rf.pred$predictions, actual=overunder2.rf.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf5=mean(overunder2.rf.pred$predictions==overunder2.rf.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf5 = calc_stats(tab.rf5, prevalence = NULL, positive = "Injury"))
```

Just as we were able to do after running the logistic regression models, we were able to put together a table with the statistics that meausre the accuracy of each of the random forest models. By looking at the balanced accuracy and F1 scores, we can see substantially higher values in both for all 5 datasets. However, the model that has the highest accuracy is the combination sub sample with a balanced accuracy of 0.8519 and an F1 score of 0.8529. These statistics make this model the most accurate out of all of the logistic regression and random forest models.

```{r StatisticsComparison-table-rf, echo=FALSE}
(StatisticsComparison.rf = cbind(SampleType = c("Unbalanced", "Oversample", "Undersample", "Combination Full Sample", 
                                            "Combination Sub-Sample"),round(rbind(stats.rf, stats.rf2, stats.rf3, stats.rf4, stats.rf5), 4)))
```

# Conclusion 

Although there have been a couple of research projects done on this topic. Most of the research has not been in-depth about car accidents. When looking at a car crash there are many different factors that come into play. There are also a lot of factors that take place when forecasting car crashes. There has not been a lot of research done on what conditions lead to the outcome of car crashes. Within our exploratory data analysis, we are able to visually see what common factors contribute to car crashes in Wake County. From this stage, we found out that it is essential to understand factors of car crashes so that we can learn how to forecast them in the future. 

The variables that significantly increase a car crash are the drivers age, vehicle type, and hotspots. Each of these variables play an important role when identifying a car crash. Comparing these models to each other we see that on average the drivers under 18 years old and drivers over 65 years old tend to get in the most accidents. This is because drivers under the age of 18 are considered as new drivers and have very little experience on the road. These drivers tend to get excited and not follow the rules. Drivers over the age of 65 tend to get in an accident because as they get older they have a slower reaction time. When they are on the road sometimes they are not able to comprehend what is going on.  

After comparing these variables we realized that young drivers and older drivers both tend to drive sports cars. This is surprising because sports cars are unknown to be the most unsafe vehicle to drive. While performing our research we inferred that young drivers and older drivers would drive sedans, SUVs, and minivans since those vehicles are safer to drive. Today more drivers are getting their licenses without a road test due to the pandemic. As observed in the forecasting models above, crashes in Wake County will slowly increase with everything opening back up ever since the pandemic occurred. 

We made use of multiple different methods to present factors that cause car crashes in Wake County. We were able to create a forecasting model for our time series data. Both TBATS and ARIMA models produced solid predictions which had high accuracy. Both of these time series models follow a similar pattern. It was clearly shown that from March of 2020 to December of 2020 there was a decrease in car accidents. This was because there was a pandemic and no one was on the road for a long period. After everything started to open up there was a rise in car accidents since more people were driving. Based on the research we performed various factors play a role in car crashes, with some factors having a greater effect than others. 

# Discussion 

This report highlights the many different factors of car crashes. In this report our goal was to analyze car crashes from 2015 to March 2021 and forecast crashes after 2021. Though our research was thorough to our best ability, data from the Wake County crashes data set had some human error. This was because the police report was filled out by officers and they did not fill in the document correctly. If it was possible to work with a dataset that was filled out correctly we would be able to give a better analysis on car crashes in Wake County. 

We have taken advantage of the Wake County Crashes database. We have also contributed by making a Time Series Forecasting. Many researchers don't have access to a similar database as we did. Therefore, the database we accessed helped us get a better idea of car crashes in Wake County, NC from 2015 to March 2021. 

Based on the research we have performed, we can make a few implications of what may cause the most accidents in Wake County. The word cloud shown in the section (Identifying Hot Spots of Crashes) we can see that most crashes occur on Capital Blvd. 440, and 40. This makes sense because these roads are some of the major interstate highways in North Carolina. Although more research is needed to compare the major interstates that connect to Wake County, it seems that the most liveliest roads tend to have the most road accidents. 

Looking at an overview of crashes in Wake County North Carolina, one way to reduce car crashes is to not drive on the highway if possible. This is because when people drive on highways they tend to speed which causes them to lose control of their vehicle. People tend to use the highways because it gets them to their destination faster. Hence, the highway becomes busy which causes car accidents to occur. 

# References

Armstrong, Tim. “Snow and Ice: February 24, 2015.” National Weather Service, NOAA's National Weather Service, 14 Mar. 2015, 
www.weather.gov/ilm/SnowIce2015Feb24#:~:text=Freezing%20rain%20increased%20in%
20intensity,at%20the%20Wilmington%20International%20Airport. 

Hodler, Axel. “Creating a Heat Map from Coordinates Using R.” Medium, Medium, 20 Dec. 2018, axelhodler.medium.com/creating-a-heat-map-from-coordinates-using-r-780db4901075. 

Hyndman, Rob. “Forecasting: Principles and Practice (2nd Ed).” 2.1 Ts Objects, otexts.com/fpp2/ts-objects.html. 

Interactive Maps in R, 2019, bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html. 

Pierre LPierre L 26.8k55 gold badges3939 silver badges5959 bronze badges, and hrbrmstrhrbrmstr 71.8k1111 gold badges120120 silver badges180180 bronze badges. “Heatmap Plot by Value Using Ggmap.” Stack Overflow, 1 Mar. 1964, stackoverflow.com/questions/32148564/heatmap-plot-by-value-using-ggmap. 

Salas-Eljatib, Christian, et al. “A Study on the Effects of Unbalanced Data When Fitting Logistic Regression Models in Ecology.” Ecological Indicators, Elsevier, 7 Nov. 2017,
www.sciencedirect.com/science/article/abs/pii/S1470160X17306659. 

Sievert, Carson. “Interactive Web-Based Data Visualization with R, Plotly, and Shiny.” 4 Maps, 19 Dec. 2019, plotly-r.com/maps.html. 

Viet, Truc. Spatial Heat Map Plotting Using R, 2021, trucvietle.me/r/tutorial/2017/01/18/spatial-heat-map-plotting-using-r.html.

# Acknowledgements 

The authors acknowledge financial support from the NSF HBCU-UP ACE: NSF HRD-1719498 and Research Experiences for Undergraduate Students in Data Science Analytics, National Security Agency H98230-18-1-0097.

# Appendix 

Here is our code of the original and tidied datasets. 

Below is a list of the libraries that we used through the report 

```{r loading-libraries-code, eval=FALSE}
library(reshape2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(dslabs)
library(ggrepel)
library(ggthemes)
library(maps)
library(scales)
library(zoo)
#devtools::install_github("m-clark/confusionMatrix")
library(confusionMatrix)
library(ROSE)
library(ranger)
library(Rcpp)
```

## Data Wrangling, Cleaning and Processing 

Here is how we read the two crash data sets into R and how we combined the two data sets into one large data set named "crashes". 

```{r read-data-code, eval=FALSE}
#Code for Cortlyns computer (Mac)
 # setwd("/Users/cmorris/Desktop/dsreu2021/rstudiodirectory/ResearchProject")
 # persons <- read.csv(file="Persons_Involved_in_Crashes.csv")
 # glimpse(persons)
 # #    
 # locations <- read.csv(file="Reported_Crash_Locations.csv")
 # glimpse(locations)
 # #    
 # crashes <- persons %>% left_join(locations, by="key_crash")
#Code for Kelly's computer (Windows)
#library(readr)
persons <- read_csv("~/NCAT REU/Mostafa/Data/Persons_Involved_in_Crashes.csv")
locations <- read_csv("~/NCAT REU/Mostafa/Data/Reported_Crash_Locations.csv")
crashes <- persons %>% left_join(locations, by="key_crash")
# Code for Ayan's computer (Window)
#library(readr)
# persons <- read_csv("C:/Users/ayang/Desktop/Project FIles REU A&T/REU Research Project1/Persons_Involved_in_Crashes.csv")
# 
# locations <- read_csv("C:/Users/ayang/Desktop/Project FIles REU A&T/REU Research Project1/Reported_Crash_Locations.csv")
# 
# crashes <- persons %>% left_join(locations, by="key_crash")
```

## Data Visuals 

Using the 'crashes' data we explored univariate and bivariate explorations. We created histograms, bar plots, and percent stacked bar plots.

Below is the code for the univariate explorations. 

```{r driver-passenger-age-code, eval=FALSE}
crashes %>% 
  filter(PersonType == "Driver" | PersonType == "Passenger", Age != "", Age != "NA") %>% 
  ggplot() +
  geom_histogram(aes(x=Age), binwidth = 5, col="red", fill="darkgrey") +
  labs(x="Age", y="Frequency", title="Drivers' vs. Passengers' Age") +
  theme(legend.position = "top") +
  facet_wrap(~PersonType, dir = "v")
```

```{r crash-year-code, eval=FALSE}
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
```

```{r weather-condition-code, eval=FALSE}
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
```

```{r vehicle-type-code, eval=FALSE}
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
```

Below is the code for the bivariate explorations. 

```{r protection-injury-code, eval=FALSE}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Protection != "NA", 
         Protection != "NaN", Protection != "Unable to determine", 
         Protection != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=Protection)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Protection",
       x = "Protection",
       y = "Percentage")
```

```{r most-harm-injury-code, eval=FALSE}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", MostHarmfulEvent != "NA", 
         MostHarmfulEvent != "NaN", MostHarmfulEvent != "Unknown") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=MostHarmfulEvent)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Most Harmful Event",
       x = "Most Harmful Event",
       y = "Percentage")
```

```{r traffic-injury-code, eval=FALSE}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", TrafficControlType != "NA", 
         TrafficControlType != "NaN", TrafficControlType != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=TrafficControlType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() +
  labs(title = "Injury Frequency by Traffic Control Type",
       x = "Traffic Control Type",
       y = "Percentage")
```

```{r road-class-injury-code, eval=FALSE}
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
```

```{r person-injury-code, eval=FALSE}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", Injury != "", PersonType != "NA", 
        PersonType != "NaN", PersonType != "Unknown", PersonType != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=PersonType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Person Type",
       x = "Person Type",
       y = "Percentage")
```

```{r alcohol-filter-injury-code, eval=FALSE}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", AlcoholResultType != "NA", 
         AlcoholResultType != "NaN", AlcoholResultType != "Unknown",
         AlcoholResultType != "Contaminated sample/unusable", 
         AlcoholResultType != "Pending", AlcoholResultType != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=AlcoholResultType)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Alcohol Result Type",
       x = "Alcohol Result Type",
       y = "Percentage")
```

```{r vision-obstruct-injury-code, eval=FALSE}
crashes %>%
  filter(Injury != "NA", Injury != "Unknown", VisionObstruction != "NA", 
         VisionObstruction != "NaN", VisionObstruction != "") %>%
  group_by(Injury) %>%
  ggplot(aes(fill=Injury, x=VisionObstruction)) + 
  geom_bar(position="fill", stat="count") + 
  coord_flip() + 
  labs(title = "Injury Frequency by Vision Obstruction",
       x = "Vision Obstruction",
       y = "Percentage")
```

We used the code below to create word clouds of crash locations. 

```{r tool-code, eval=FALSE}
devtools::install_github("gaospecial/wordcloud2")
```

```{r location-on-word-cloud-code, eval=FALSE}
location_road_name_on_freq <- crashes %>%
  count(LocationRoadNameOn)
set.seed(101)
location_road_name_on_freq %>%
  wordcloud2(shape = 'circle', backgroundColor = "black", minSize = 5) 
```

```{r location-at-word-cloud-code, eval=FALSE}
location_road_name_at_freq <- crashes %>%
  count(LocationRoadNameAt)
set.seed(102)
location_road_name_at_freq %>%
  wordcloud2(shape = 'circle', backgroundColor = "black", minSize = 5)
```

Then we further filterd the crashes data to create maps of the crashes in North Carolina and a zoomed-in map of Wake County. 

```{r nc-interactive-map-code, eval=FALSE}
map_bounds <- c(-78.98, 35.51, -78.24, 36.06)
crashes_filter <- crashes %>%
  filter(LocationLatitude != "0", LocationLongitude != "0",
         LocationLatitude != "NA", LocationLatitude != "",
         LocationLongitude != "NA", LocationLongitude != "",
         LocationLatitude > 35.4, LocationLatitude < 36.2,
         LocationLongitude < -78, LocationLongitude > -79)
coords.map.nc <- get_stamenmap(map_bounds, zoom = 10, maptype = "toner-lite")
coords.map.nc <- ggmap(coords.map.nc, extent="panel", legend="none")
coords.map.nc <- coords.map.nc + stat_density2d(data=crashes_filter,  
                                          aes(x=LocationLongitude, 
                                              y=LocationLatitude, 
                                              fill=..level..),
                                          alpha = 0.3, 
                                          geom="polygon")
coords.map.nc <- coords.map.nc +   scale_fill_gradientn(colours=rev(brewer.pal(7, "RdBu")))
coords.map.nc <- coords.map.nc + theme_bw() + 
  ggtitle("Heat Map of Crashes in North Carolina") + 
  xlab("Longitude") + ylab("Latitude")
ggplotly(coords.map.nc) %>%
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  )
```

```{r wake-interactive-map-code, eval=FALSE}
crashes_filter <- crashes %>%
  filter(LocationLatitude != "0", LocationLongitude != "0",
         LocationLatitude != "NA", LocationLatitude != "",
         LocationLongitude != "NA", LocationLongitude != "",
         LocationLatitude > 35.4, LocationLatitude < 36.2,
         LocationLongitude < -78, LocationLongitude > -79)
map_bounds <- c(-78.8, 35.72, -78.5, 35.9) #coordinates of wake county
coords.map.wake <- get_stamenmap(map_bounds, zoom = 13, maptype = "toner-lite")
coords.map.wake <- ggmap(coords.map.wake, extent="panel")
coords.map.wake <- coords.map.wake + stat_density2d(data=crashes_filter,  
                                          aes(x=LocationLongitude, 
                                              y=LocationLatitude, 
                                              fill=..level..),
                                          alpha=0.3, 
                                          geom="polygon")
coords.map.wake <- coords.map.wake + scale_fill_gradientn(colours=rev(brewer.pal(7, "RdYlGn")))
coords.map.wake <- coords.map.wake + theme_bw() + 
  ggtitle("Heat Map of Crashes in Wake County") + 
  xlab("Latitude") + ylab("Longitude")
ggplotly(coords.map.wake) %>%
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  )
```

Here we organized the data into subsets for daily pandemic data, daily nonpandemic data, monthly pandemic data, and monthly nonpandemic data. These were used for forecasting. 

```{r data-i-code, eval=FALSE}
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), 
                         end = c(2020,59), frequency = 365)
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
```

Then we created the time series visualizations. 

```{r crashes-ts-code, eval=FALSE}
crashes_ts = crashes %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
```

```{r 7-day-ma-ts-code, eval=FALSE}
crashes_ts %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  geom_line() +
  geom_line(aes(y=rollmean(count, 7, na.pad = TRUE)), color = "red") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=8)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes (With Pandemic Data)")
```

```{r noncovid-daily-ts-code, eval=FALSE}
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashes_ts.noncovid %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  ggtitle("Time Series Plot for Frequency of Daily Crashes (Without Pandemic Data)") + xlab("Date") + ylab("Count") + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size = 10))
```

```{r annual-ts-code, eval=FALSE}
crashes_annual = crashes_ts %>%
  separate(Date, 
           into = c("Year", "Month"), sep = 4, remove = FALSE) %>%
  select(-Month)
crashes_annual %>%
  filter(as.Date(Date) >= "2015/01/01") %>%
  ggplot(aes(x = as.Date(Date), y = count)) + 
  ggtitle("Time Series Plots for Frequency of Daily Crashes Organized by Year") + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") + 
  xlab("Date") + ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Year, scales = "free_x")
```

```{r monthly-ts-code, eval=FALSE}
forecast::autoplot(crashes_mts4) +
  ggtitle("Time Series Plot for Frequency of Monthly Crashes (With Pandemic Data)") + xlab("Date") + ylab("Monthly Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", 
                                "Jan 2018", "Jan 2019", "Jan 2020", 
                                "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), 
        title = element_text(size=10))
```

```{r monthly-ts-noncovid-code, eval=FALSE}
forecast::autoplot(crashes_mts4.noncovid) +
  ggtitle("Time Series Plot for Frequency of Monthly Crashes (Without Pandemic Data)") + xlab("Date") + ylab("Monthly Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", 
                                "Jan 2018", "Jan 2019", "Jan 2020", 
                                "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), 
        title = element_text(size=10))
```

## Time Series Forecasting 

From here, we needed to make crashes into a time series object so that we could begin forecasting. 

```{r data-code, eval=FALSE}
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), 
                         end = c(2020,59), frequency = 365)
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
```

We checked for stationarity before every forecasting method was used and transformed the data if necessary . 

```{r stationarity-trans-daily-covid-code, eval=FALSE}
##Looking at stationarity of crashests2 daily time series 
library(urca)
acf(crashests2)
summary(ur.kpss(crashests2))
```

```{r stationarity-trans-daily-ii-code, eval=FALSE}
library(urca)
ndiffs(crashests2) #1
nsdiffs(crashests2) #0
#Attempt at differencing 
dif_crashests2 <- diff(crashests2)
#Looking at stationarity of first difference 
acf(dif_crashests2)
```

```{r stationarity-trans-daily-iii-code, eval=FALSE}
summary(ur.kpss(dif_crashests2))
ndiffs(dif_crashests2) #0
nsdiffs(dif_crashests2) #0
```

```{r stat-trans-daily-covid-plot-code, eval=FALSE}
cbind("Crashes" = crashests2,
      "First differenced" = diff(crashests2)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformations of Daily Pandemic Data")
```

```{r stationarity-trans-daily-noncovid-code, eval=FALSE}
##Looking at stationarity of crashests.noncovid daily time series 
library(urca)
acf(crashests.noncovid)
summary(ur.kpss(crashests.noncovid))
```

```{r stationarity-trans-daily-noncovid-ii-code, eval=FALSE}
ndiffs(crashests.noncovid) #1
nsdiffs(crashests.noncovid) #0
#Attempt at differencing (noncovid)
dif_crashests.noncovid <- diff(crashests.noncovid)
#Looking at stationarity of first difference (noncovid)
acf(dif_crashests.noncovid)
```

```{r stationarity-trans-daily-noncovid-iii-code, eval=FALSE}
summary(ur.kpss(dif_crashests.noncovid))
ndiffs(dif_crashests.noncovid) #0
nsdiffs(dif_crashests.noncovid) #0
```

```{r stat-trans-daily-noncovid-plot-code, eval=FALSE}
cbind("Crashes" = crashests.noncovid,
      "First differenced" = diff(crashests.noncovid)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformation of Daily NonPandemic Data")
```

```{r stationarity-trans-monthly-covid-code, eval=FALSE}
##Looking at stationarity of crashes_mts4 monthly time series 
library(urca)
acf(crashes_mts4)
summary(ur.kpss(crashes_mts4))
```

```{r stationarity-trans-monthly-covid-ii-code, eval=FALSE}
ndiffs(crashes_mts4) #1
nsdiffs(crashes_mts4) #0
#Attempt at differencing 
dif_crashes_mts4 <- diff(crashes_mts4)
#Looking at stationarity of first difference 
acf(dif_crashes_mts4)
```

```{r stationarity-trans-monthly-covid-iii-code, eval=FALSE}
summary(ur.kpss(dif_crashes_mts4))
ndiffs(dif_crashes_mts4) #0
nsdiffs(dif_crashes_mts4) #0
```

```{r stat-trans-monthly-covid-plot-code, eval=FALSE}
cbind("Crashes" = crashes_mts4,
      "First differenced" = diff(crashes_mts4)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformation of Monthly Pandemic Data") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90)) 
```

```{r stationarity-trans-monthly-noncovid-code, eval=FALSE}
##Looking at stationarity of crashes_mts4.noncovid monthly time series 
library(urca)
acf(crashes_mts4.noncovid)
summary(ur.kpss(crashes_mts4.noncovid))
```

```{r stationarity-trans-monthly-noncovid-ii-code, eval=FALSE}
ndiffs(crashes_mts4.noncovid) #1
nsdiffs(crashes_mts4.noncovid) #1
#Attempt at seasonal differencing (noncovid)
season_dif_crashes_mts4.noncovid <- diff(crashes_mts4.noncovid,12)
#Looking at stationarity of seasonal difference (noncovid)
acf(season_dif_crashes_mts4.noncovid)
```

```{r stationairy-trans-monthly-noncovid-iii-code, eval=FALSE}
summary(ur.kpss(season_dif_crashes_mts4.noncovid))
ndiffs(season_dif_crashes_mts4.noncovid) #0
nsdiffs(season_dif_crashes_mts4.noncovid) #0
```

```{r stat-trans-monthly-noncovid-plot-code, eval=FALSE}
cbind("Crashes" = crashes_mts4.noncovid,
      "Seasonally\n differenced" = diff(crashes_mts4.noncovid, 12)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformation of Monthly NonPandemic Data")
```

After checking all the data sets for stationarity, we began forecasting. 

Below are forecasts for daily pandemic data. 

```{r tbats-stationary-code, eval=FALSE}
#Stationary
#reload libraries 
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)
dif_crashests2 <- diff(crashests2)
dif_y <- msts(dif_crashests2, seasonal.periods=c(7,365.25))
dif_fit_tbats_covid2 <- tbats(dif_y)
dif_fc_tbats_covid2 <- forecast::forecast(dif_fit_tbats_covid2, h=214)
dYhat <- dif_fc_tbats_covid2$mean #point forecast values 
dYhat_ci_upper <- dif_fc_tbats_covid2$upper
dYhat_ci_lower <- dif_fc_tbats_covid2$lower
#crashests2
#dif_crashests2<-diff(crashests2)
#z0<-cumsum(c(crashests2[1], dif_crashests2))
#all(crashests2==z0) #TRUE
#dif_crashests2 <- diff(crashests2) #already ran earlier
#dYhat <- dif_fc_tbats_covid2$mean #already ran earlier 
Yhat <- cumsum(c(crashests2[length(crashests2)],dYhat))
Yhat <- ts(Yhat, start = c(2021, 152), frequency=365)
Yhat_ci_upper <- cumsum(c(crashests2[length(crashests2)],dYhat_ci_upper))
Yhat_ci_upper <- ts(Yhat_ci_upper, start = c(2021, 152), end = c(2022,1), frequency=365)
Yhat_ci_lower <- cumsum(c(crashests2[length(crashests2)],dYhat_ci_lower))
Yhat_ci_lower <- ts(Yhat_ci_lower, start = c(2021, 152), end = c(2022,1), frequency=365)
```

```{r tbats-stationary-plot-code, eval=FALSE}
#Stationary
autoplot(crashests2) + 
  autolayer(Yhat, series="Point Forecasts") + 
  #autolayer(Yhat_ci_upper) + 
  #autolayer(Yhat_ci_lower) + 
  ggtitle("TBATS Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  theme(title = element_text(size = 10), legend.position = "bottom")
```

```{r arima-code, eval=FALSE}
fit_arima <- auto.arima(crashests2)
fc_arima <- forecast(fit_arima, h=214)
autoplot(fc_arima) + 
  ggtitle("ARIMA Forecasting Model for Daily Car Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  theme(title = element_text(size = 10))
```

```{r tbats-ms-stationary-code, eval=FALSE}
#Stationary
#getting differenced training and test sets 
dif_y2 <- msts(dif_crashests2, seasonal.periods=c(7,365.25))
dif_training_TBATS2 <- subset(dif_y2, end=length(dif_y2)-151)
dif_test_TBATS2 <- subset(dif_y2, start=length(dif_y2)-150)
dif_crashes.train_TBATS2 <- tbats(dif_training_TBATS2)
dif_fc_train_TBATS2 <- forecast(dif_crashes.train_TBATS2, h=151)
#getting point forecasts 
dYhat_ms_TBATS <- dif_fc_train_TBATS2$mean
#setting up normal ts training and test sets 
y2 <- msts(crashests2, seasonal.periods=c(7,365.25))
training_TBATS2 <- subset(y2, end=length(y2)-151)
test_TBATS2 <- subset(y2, start=length(y2)-150)
#reverting back to original points   
Yhat_ms_TBATS <- cumsum(c(training_TBATS2[length(training_TBATS2)], dYhat_ms_TBATS))
Yhat_ms_TBATS <- ts(Yhat_ms_TBATS, start = c(2021, 1), frequency = 365)
```

```{r tbats-ms-stationary-plot-code, eval=FALSE}
#Stationary 
autoplot(training_TBATS2) + 
  autolayer(Yhat_ms_TBATS, series="Point Forecasts") + 
  autolayer(test_TBATS2, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step TBATS Daily Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom")
dif_crashes.test_TBATS2 <- tbats(dif_test_TBATS2)
accuracy(dif_crashes.test_TBATS2)
```

```{r ms-arima-code, eval=FALSE}
#Stationary
training_arima <- subset(crashests2, end=length(crashests2)-151)
test_arima <- subset(crashests2, start=length(crashests2)-150)
crashes.train_arima <- auto.arima(training_arima)
fc_train_arima <- forecast(crashes.train_arima, h=151)
autoplot(fc_train_arima) + 
  autolayer(test_arima, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step ARIMA Daily Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom")
crashes.test_arima <- arima(test_arima)
accuracy(crashes.test_arima)
```

```{r combination-code, eval=FALSE}
crashes_ts = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2021/05/31") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests2 <- ts(crashes_ts$count, start = c(2015,1), end = c(2021,153),
                frequency = 365)
train <- window(crashests2, end=c(2019,12))
h <- length(crashests2) - length(train)
#ARIMA model 
ARIMA <- forecast::forecast(auto.arima(train, lambda=0, biasadj=TRUE), h=h)
#TBATS model 
train_tbats <- msts(dif_crashests2, end=c(2019,12), seasonal.periods = c(7,365.25))
TBATS <- forecast::forecast(tbats(train_tbats, biasadj = TRUE), h=h)
dYhat_combo <- TBATS$mean
  
Yhat_combo <- cumsum(c(train[length(train)], dYhat_combo))
Yhat_combo <- ts(Yhat_combo, start = c(2019, 12), frequency = 365)
Combination <- (ARIMA[["mean"]] + Yhat_combo)/2
true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2021/05/31"))
true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2019,1), end = c(2021,153), 
                            frequency = 365)
autoplot(train) +
  autolayer(true_noncovid_crashes) +
  autolayer(Combination, series="Combination", alpha=0.8) +
  autolayer(ARIMA, series="ARIMA", PI=F) +
  autolayer(Yhat_combo, series="TBATS", alpha=0.7) +
  xlab("Date") + ylab("Crashes") +
  ggtitle("Time Series Combination (With Pandemic Data)") + 
  theme(legend.position = "bottom")
c(ARIMA = accuracy(ARIMA, crashests2)["Test set", "RMSE"],
  TBATS = accuracy(Yhat_combo, crashests2)["Test set", "RMSE"],
  Combination =
    accuracy(Combination, crashests2)["Test set", "RMSE"])
```

Below are forecasts for daily nonpandemic data. 

```{r tbats-noncovid-stationary-code, eval=FALSE}
#Stationary
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), 
                         end = c(2020,59), frequency = 365)
dif_crashests.noncovid <- diff(crashests.noncovid)
dif_y_noncovid <- msts(dif_crashests.noncovid, seasonal.periods=c(7,365.25))
dif_fit_tbats_noncovid <- tbats(dif_y_noncovid)
dif_fc_tbats_noncovid <- forecast::forecast(dif_fit_tbats_noncovid, h=671)
dYhat_noncovid <- dif_fc_tbats_noncovid$mean #point forecast values 
#crashests.noncovid
#dif_crashests.noncovid<-diff(crashests.noncovid)
#z0<-cumsum(c(crashests.noncovid[1], dif_crashests.noncovid))
#all(crashests.noncovid==z0) #TRUE
#dif_crashests2 <- diff(crashests2) #already ran earlier
#dYhat <- dif_fc_tbats_covid2$mean #already ran earlier 
Yhat_noncovid <- cumsum(c(crashests.noncovid[length(crashests.noncovid)],dYhat_noncovid))
Yhat_noncovid <- ts(Yhat, start = c(2020, 60), frequency=365)
```

```{r tbats-noncovid-stationary-plot-code, eval=FALSE}
#Stationary
true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2020/03/01") & Date <= as.Date("2021/05/31"))
true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2020,60), end = c(2021,153), 
                            frequency = 365)
autoplot(crashests.noncovid) + 
  autolayer(true_noncovid_crashes, alpha=0.7, series="True Count") +
  autolayer(Yhat_noncovid, alpha=0.6, series="Point Forecasts") + 
  ggtitle("TBATS Forecasting Model for Daily Car Crashes (Without Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  theme(title = element_text(size = 10), legend.position = "bottom")
```

```{r arima-noncovid-code, eval=FALSE}
fit_arima.noncovid <- auto.arima(crashests.noncovid)
fc_arima.noncovid <- forecast(fit_arima.noncovid, h=671)
autoplot(fc_arima.noncovid) + 
  ggtitle("ARIMA Forecasting Model for Daily Car Crashes (Without Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  theme(title = element_text(size = 10))
```

```{r ms-arima-noncovid-code, eval=FALSE}
#Stationary
training_arima_noncovid <- subset(crashests.noncovid, end=length(crashests.noncovid)-426)
test_arima_noncovid <- subset(crashests.noncovid, start=length(crashests.noncovid)-425)
crashes.train_arima_noncovid <- auto.arima(training_arima_noncovid)
fc_train_arima_noncovid <- forecast(crashes.train_arima_noncovid, h=426)
autoplot(fc_train_arima_noncovid) + 
  autolayer(test_arima_noncovid, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step ARIMA Daily Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom", title = element_text(size=9))
crashes.test_arima_noncovid <- arima(test_arima_noncovid)
accuracy(crashes.test_arima_noncovid)
```

```{r combination-noncovid-code, eval=FALSE}
crashes_ts.noncovid = crashes %>%
  filter(as.Date(crash_date) >= "2015/01/01" & as.Date(crash_date) <= "2020/03/01") %>%
  separate(crash_date, 
           into = c("Date", "Hour"), sep = 11) %>%
  group_by(Date) %>%
  summarize(count = length(unique(key_crash)))
crashests.noncovid <- ts(crashes_ts.noncovid$count, start = c(2015,1), 
                         end = c(2020,59), frequency = 365)
train.noncovid <- window(crashests2, end=c(2019,12))
h.noncovid <- 426
#ARIMA model 
ARIMA.noncovid <- forecast::forecast(auto.arima(train.noncovid, lambda=0, biasadj=TRUE), h=h)
#TBATS model 
train_tbats.noncovid <- msts(dif_crashests.noncovid, end=c(2019,12), seasonal.periods = c(7,365.25))
TBATS.noncovid <- forecast::forecast(tbats(train_tbats.noncovid, biasadj = TRUE), h=h)
dYhat_combo.noncovid <- TBATS.noncovid$mean
  
Yhat_combo.noncovid <- cumsum(c(train.noncovid[length(train.noncovid)], dYhat_combo.noncovid))
Yhat_combo.noncovid <- ts(Yhat_combo.noncovid, start = c(2019, 12), frequency = 365)
Combination.noncovid <- (ARIMA.noncovid[["mean"]] + Yhat_combo.noncovid)/2
true_noncovid_crashes <- crashes_ts %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2021/05/31"))
true_noncovid_crashes <- ts(true_noncovid_crashes$count, 
                            start = c(2019,1), end = c(2021,153), 
                            frequency = 365)
autoplot(train.noncovid) +
  autolayer(true_noncovid_crashes, series = "True Count") +
  autolayer(Combination.noncovid, series="Combination", alpha=0.8) +
  autolayer(ARIMA.noncovid, series="ARIMA", PI=F) +
  autolayer(Yhat_combo.noncovid, series="TBATS", alpha=0.7) +
  xlab("Date") + ylab("Crashes") +
  ggtitle("Time Series Combination (Without Pandemic Data)") + 
  theme(legend.position = "bottom")
c(ARIMA = accuracy(ARIMA.noncovid, crashests2)["Test set", "RMSE"],
  TBATS = accuracy(Yhat_combo.noncovid, crashests2)["Test set", "RMSE"],
  Combination =
    accuracy(Combination.noncovid, crashests2)["Test set", "RMSE"])
```

Below are forecasts for monthly pandemic data. 

```{r tbats-stationary-monthly-code, eval=FALSE}
#Stationary
#reload libraries 
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
dif_crashes_mts4 <- diff(crashes_mts4)
dif_y_monthly <- msts(dif_crashes_mts4, seasonal.periods=12)
dif_fit_tbats_covid2_monthly <- tbats(dif_y_monthly)
dif_fc_tbats_covid2_monthly <- forecast::forecast(dif_fit_tbats_covid2, h=8)
dYhat_monthly <- dif_fc_tbats_covid2_monthly$mean #point forecast values 
#crashests2
#dif_crashests2<-diff(crashests2)
#z0<-cumsum(c(crashests2[1], dif_crashests2))
#all(crashests2==z0) #TRUE
#dif_crashests2 <- diff(crashests2) #already ran earlier
#dYhat <- dif_fc_tbats_covid2$mean #already ran earlier 
Yhat_monthly <- cumsum(c(crashes_mts4[length(crashes_mts4)],dYhat_monthly))
Yhat_monthly <- ts(Yhat_monthly, start = c(7,6), frequency=12)
```

```{r tbats-stationary-plot-monthly-code, eval=FALSE}
#Stationary
autoplot(crashes_mts4) + 
  autolayer(Yhat_monthly, series="Point Forecasts") + 
  ggtitle("TBATS Forecasting Model for Monthly Car Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=10), 
        legend.position = "bottom") 
```

```{r arima-monthly-code, eval=FALSE}
fit_arima_monthly <- auto.arima(crashes_mts4)
fc_arima_monthly <- forecast(fit_arima_monthly, h=8)
autoplot(fc_arima_monthly) + 
  ggtitle("ARIMA Forecasting Model for Monthly Car Crashes (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9))
  
```

```{r monthly-HW-fcast-decomp-stationary-code, eval=FALSE}
#Stationary
dif.covid.monthly <- HoltWinters(dif_crashes_mts4)
#summary(dif.covid.monthly)
#plot(fitted(dif.covid.monthly), main = "Box Jenkins Decomposition of Monthly Crashes (With Pandemic Data)", cex.main=1)
dif.fcast.covid.monthly <- forecast::forecast(dif.covid.monthly, h=8, level=c(80,95))
dYhat.HW <- dif.fcast.covid.monthly$mean
#crashes_mts4
#dif_crashes_mts4<-diff(crashes_mts4)
#z0<-cumsum(c(crashes_mts4[1], dif_crashes_mts4))
#all(crashes_mts4==z0) #TRUE
Yhat.HW <- cumsum(c(crashes_mts4[length(crashes_mts4)],dYhat.HW))
Yhat.HW <- ts(Yhat.HW, start = c(7, 5), frequency=12)
```

```{r monthly-HW-fcast-filter-forecast-ts-stationary-code, eval=FALSE}
#Stationary 
autoplot(crashes_mts4) +
  autolayer(Yhat.HW, series = "Point Forecasts") +
  ggtitle("Forecasts of Monthly Car Crashes Using HoltWinters (With Pandemic Data)") +
  xlab("Date") + ylab("Crashes") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9), 
        legend.position = "bottom")
```

```{r tbats-ms-stationary-monthly-code, eval=FALSE}
#Stationary
dif_y2_monthly <- msts(dif_crashes_mts4, seasonal.periods=12)
dif_training_TBATS2_monthly <- subset(dif_y2_monthly, end=length(dif_y2_monthly)-5)
dif_test_TBATS2_monthly <- subset(dif_y2_monthly, start=length(dif_y2_monthly)-4)
dif_crashes.train_TBATS2_monthly <- tbats(dif_training_TBATS2_monthly)
dif_fc_train_TBATS2_monthly <- forecast(dif_crashes.train_TBATS2_monthly, h=5)
dYhat_ms_TBATS_monthly <- dif_fc_train_TBATS2_monthly$mean
y2 <- msts(crashes_mts4, seasonal.periods=12)
training_TBATS2 <- subset(y2, end=length(y2)-5)
test_TBATS2 <- subset(y2, start=length(y2)-4)
  
#Yhat_ms_TBATS <- cumsum(c(crashests2[length(crashests2)], dYhat_ms_TBATS))
Yhat_ms_TBATS_monthly <- cumsum(c(training_TBATS2[length(training_TBATS2)], dYhat_ms_TBATS_monthly))
Yhat_ms_TBATS_monthly <- ts(Yhat_ms_TBATS_monthly, start = c(6,12), frequency=12)
```

```{r tbats-ms-stationary-plot-monthly-code, eval=FALSE}
#Stationary 
autoplot(training_TBATS2) + 
  autolayer(Yhat_ms_TBATS_monthly, series="Point Forecasts") + 
  autolayer(test_TBATS2, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step TBATS Monthly Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom", title=element_text(size=10)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))
dif_crashes.test_TBATS2_monthly <- tbats(dif_test_TBATS2_monthly)
accuracy(dif_crashes.test_TBATS2_monthly)
```

```{r ms-arima-monthly-code, eval=FALSE}
#Stationary
training_arima_monthly <- subset(crashes_mts4, end=length(crashes_mts4)-5)
test_arima_monthly <- subset(crashes_mts4, start=length(crashes_mts4)-4)
crashes.train_arima_monthly <- auto.arima(training_arima_monthly)
fc_train_arima_monthly <- forecast(crashes.train_arima_monthly, h=5)
autoplot(fc_train_arima_monthly) + 
  autolayer(test_arima_monthly, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step ARIMA Monthly Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9), 
        legend.position = "bottom")
crashes.test_arima_monthly <- arima(test_arima_monthly)
accuracy(crashes.test_arima_monthly)
```

```{r ms-HW-monthly-forecast-ts-stationary-code, eval=FALSE}
#Stationary  
dif_training_HW_monthly <- subset(dif_crashes_mts4, end=length(dif_crashes_mts4)-5)
dif_test_HW_monthly <- subset(dif_crashes_mts4, start=length(dif_crashes_mts4)-4)
dif_crashes.train_HW_monthly <- HoltWinters(dif_training_HW_monthly)
dif_fc_HW <- forecast::forecast(dif_crashes.train_HW_monthly, h=5)
dYhat_ms_HW <- dif_fc_HW$mean
Yhat_ms_HW <- cumsum(c(crashes_mts4[length(crashes_mts4)],dYhat_ms_HW))
Yhat_ms_HW <- ts(Yhat_ms_HW, start = c(7,1), frequency=12)
training_HW_monthly <- subset(crashes_mts4, end=length(crashes_mts4)-5)
test_HW_monthly <- subset(crashes_mts4, start=length(crashes_mts4)-4)
true_count_monthly <- ts(crashes_mts4, start=c(7,1), end=c(7,5), frequency=12)
autoplot(training_HW_monthly) + 
  autolayer(Yhat_ms_HW, series = "Point Forecasts") + 
  autolayer(test_HW_monthly, series = "Test Set") + 
  ggtitle("Multi-Step HoltWinters Monthly Forecasts of Crashes (With Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 9))
``` 

```{r combination-monthly-code, eval=FALSE}
train_monthly <- window(crashes_mts4, end=c(5,12))
h_monthly <- length(crashes_mts4) - length(train_monthly)
#ARIMA model 
ARIMA_monthly <- forecast::forecast(auto.arima(train_monthly, lambda=0, biasadj=TRUE), h=h_monthly)
#TBATS model 
train_tbats_monthly <- msts(dif_crashes_mts4, end=c(5,12), seasonal.periods = 12)
TBATS_monthly <- forecast::forecast(tbats(train_tbats_monthly, biasadj = TRUE), h=h_monthly)
dYhat_combo_monthly <- TBATS_monthly$mean
  
Yhat_combo_monthly <- cumsum(c(train_monthly[length(train_monthly)], dYhat_combo_monthly))
Yhat_combo_monthly <- ts(Yhat_combo_monthly, start = c(5, 12), frequency = 12)
#HoltWinters Model 
HW_monthly <- forecast::forecast(HoltWinters(dif_crashes_mts4), h=h_monthly)
dYhat_HW_monthly <- HW_monthly$mean
Yhat_HW_monthly <- cumsum(c(train_monthly[length(train_monthly)],dYhat_HW_monthly))
Yhat_HW_monthly <- ts(Yhat_HW_monthly, start = c(5,12), frequency=12)
Combination_monthly <- (ARIMA_monthly[["mean"]] + Yhat_combo_monthly + Yhat_HW_monthly)/3
true_count_monthly <- ts(crashes_mts4, start=c(6,1), end=c(7,5), frequency=12)
autoplot(train_monthly) +
  autolayer(true_count_monthly, series="True Count") + 
  autolayer(Combination_monthly, series="Combination", alpha=0.8) +
  autolayer(ARIMA_monthly, series="ARIMA", PI=F) +
  autolayer(Yhat_combo_monthly, series="TBATS", alpha=0.7) +
  autolayer(Yhat_HW_monthly, series="HoltWinters", alpha=0.7) + 
  xlab("Date") + ylab("Crashes") +
  ggtitle("Time Series Combination (With Pandemic Data)") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 9)) 
  
c(ARIMA = accuracy(ARIMA_monthly, crashes_mts4)["Test set", "RMSE"],
  TBATS = accuracy(Yhat_combo_monthly, crashes_mts4)["Test set", "RMSE"],
  HW = accuracy(Yhat_HW_monthly, crashes_mts4)["Test set", "RMSE"],
  Combination =
    accuracy(Combination_monthly, crashes_mts4)["Test set", "RMSE"])
```

Below are forecasts for monthly nonpandemic data. 

```{r sarima-code, eval=FALSE}
fc_sarima <- sarima.for(crashes_mts4.noncovid, n.ahead=23, 1,0,0,0,1,1,12) 
```

```{r sarima-plot-code, eval=FALSE}
fc_sarima <- ts(fc_sarima$pred, start=c(6,3),frequency=12)
autoplot(crashes_mts4.noncovid) + 
  autolayer(fc_sarima, series="Point Forecasts") + 
  ggtitle("SARIMA Forecasting Model for Monthly Car Crashes (Without Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7, 8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9), 
        legend.position = "bottom")
```

```{r stlf-code, eval=FALSE}
q <- crashes_mts4.noncovid %>%
  stlf(lambda = 0, h = 23, level=c(80,95)) 
q %>%
  autoplot() + 
  ggtitle("STLF Model for Monthly Car Crashes (Without Pandemic Data)") +
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022")) + 
  theme(axis.text.x = element_text(angle = 90))
```

```{r ms-sarima-code, eval=FALSE}
training_sarima <- subset(crashes_mts4.noncovid, end=length(crashes_mts4.noncovid)-14)
test_sarima <- subset(crashes_mts4.noncovid, start=length(crashes_mts4.noncovid)-13)
crashes.train_sarima <- sarima.for(training_sarima, n.ahead=14, 1,0,0,0,1,1,12)
crashes.train_sarima <- ts(crashes.train_sarima$pred, start=c(5,1),frequency=12)
```

```{r ms-sarima-plot-code, eval=FALSE}
autoplot(training_sarima) + 
  autolayer(crashes.train_sarima, series="Point Forecasts") + 
  autolayer(test_sarima, series = "Test Set") + 
  ggtitle("Multi-Step SARIMA Monthly Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size =10))
crashes.test_sarima_monthly <- arima(test_sarima)
accuracy(crashes.test_sarima_monthly)
```

```{r noncovid-ms-STLF-forecast-ts-code, eval=FALSE}
training_STLF_noncovid_monthly <- subset(crashes_mts4.noncovid, end=length(crashes_mts4.noncovid)-14)
test_STLF_noncovid_monthly <- subset(crashes_mts4.noncovid, start=length(crashes_mts4.noncovid)-13)
crashes.train_STLF_noncovid_monthly <- stlf(training_STLF_noncovid_monthly, lambda = 0, h = 14, level=c(80,95))
crashes.train_STLF_noncovid_monthly %>%
  forecast::forecast(h=14) %>%
  autoplot() + 
  autolayer(test_STLF_noncovid_monthly, series="Test Set") + 
  ggtitle("Multi-Step STLF Monthly Forecasts of Crashes (Without Pandemic Data)") + 
  xlab("Date") + ylab("Crashes") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 10))
crashes.test_stlf_monthly <- arima(test_STLF_noncovid_monthly)
accuracy(crashes.test_stlf_monthly)
```

```{r combination-monthly-noncovid-code, eval=FALSE}
train_monthly.noncovid <- window(crashes_mts4.noncovid, end=c(5,12))
h_monthly.noncovid <- length(crashes_mts4.noncovid) - length(train_monthly.noncovid)
#SARIMA Model 
SARIMA <- sarima.for(train_monthly.noncovid, n.ahead=14, 1,0,0,0,1,1,12) 
SARIMA <- ts(SARIMA$pred, start=c(6,1),frequency=12)
```

```{r combination-monthly-noncovid-plot-code, eval=FALSE}
#STLF Model 
STLF <- stlf(train_monthly.noncovid, lambda = 0, h = 14) 
Combination_monthly.noncovid <- (SARIMA + STLF[["mean"]])/2
true_count_monthly <- ts(crashes_mts4, start=c(6,1), end=c(7,1), frequency=12)
autoplot(train_monthly.noncovid) +
  autolayer(true_count_monthly, series="True Count") + 
  autolayer(SARIMA, series="SARIMA") +
  autolayer(STLF, series="STLF", PI=F) +
  autolayer(Combination_monthly.noncovid, series="Combination") +
  xlab("Date") + ylab("Crashes") +
  ggtitle("Time Series Combination (Without Pandemic Data)") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), 
                     labels = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018", 
                                "Jan 2019", "Jan 2020", "Jan 2021")) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 10)) 
  
c(SARIMA = accuracy(SARIMA, crashes_mts4.noncovid)["Test set", "RMSE"],
  STLF = accuracy(STLF, crashes_mts4.noncovid)["Test set", "RMSE"],
  Combination =
    accuracy(Combination_monthly.noncovid, crashes_mts4.noncovid)["Test set", "RMSE"])
```

## Data Modeling 

Here we separated the 'crashes' data set into factors that played a role into varying levels of injury for the logistic regression model. 

```{r, eval=FALSE}
crashes_pm = crashes %>%
  filter(Injury != "", Injury != "Unknown",  WeatherCondition1 != "Unknown",
         WeatherCondition1 != "", MostHarmfulEvent != "Unknown",
         MostHarmfulEvent != "", RoadFeature != "Unknown", RoadFeature != "", 
         RoadFeature != "NaN", TrafficControlType != "NaN", RoadClassification != "NaN",
         TrafficControlType != "Unknown", TrafficControlType != "",
         RoadClassification != "Unknown", RoadClassification != "",
         PersonType != "", PersonType != "Unknown", 
         VehicleType != "All terrain vehicle (ATV)",
         VehicleType != "Farm equipment", VehicleType != "Farm tractor",
         VehicleType != "Military", VehicleType != "Motor scooter or motor bike",
         VehicleType != "Motor home/recreational vehicle", 
         VehicleType != "Tractor/doubles", VehicleType != "Truck/tractor (i.e., bobtail)",
         ContributingCircumstance1 != "Passed stopped school bus", 
         ContributingCircumstance1 != "Passed on hill", 
         ContributingCircumstance1 != "Passed on curve",
         ContributingCircumstance1 != "Driver distracted by other electronic 
         device (navigation device, DVD player, etc.) ",
         Protection != "Lighting", Protection != "Protective pads",
         Protection != "Reflective clothing", WeatherCondition1 != "Blowing sand, dirt, snow",
         WeatherCondition1 != "Severe crosswinds", MostHarmfulEvent != "Jackknife",
         MostHarmfulEvent != "RR train, engine", TrafficControlType != "RR cross bucks only",
         TrafficControlType != "RR flasher", TrafficControlType != "RR gate and flasher",
         TrafficControlType != "School zone signs", VisionObstruction != "Blinded, headlights",
         VisionObstruction != "Blinded, other lights", VisionObstruction != "Building(s)",
         VisionObstruction != "Embankment", VisionObstruction != "Sign(s)") %>%
  mutate(Injury = as.factor(Injury)) %>%
  mutate(Injury2 = if_else(Injury == "No injury", "No injury", "Injury"), 
         Injury2 = as.factor(Injury2)) %>%
  mutate(VehicleType = if_else(VehicleType %in% c("Unknown",""), 
                               "Unknown/Blank", VehicleType),
         VehicleType = as.factor(VehicleType)) %>%
  mutate(ContributingCircumstance1 = if_else(ContributingCircumstance1 %in% c("Unknown",""), 
                                             "Unknown/Blank", ContributingCircumstance1),
         ContributingCircumstance1 = as.factor(ContributingCircumstance1)) %>%
  mutate(Protection = if_else(Protection %in% c("Unable to determine",""), 
                              "Unknown/Blank", Protection),
         Protection = as.factor(Protection)) %>%
  mutate(WeatherCondition1 = as.factor(WeatherCondition1)) %>%
  mutate(MostHarmfulEvent = as.factor(MostHarmfulEvent)) %>%
  mutate(RoadFeature = as.factor(RoadFeature)) %>%
  mutate(TrafficControlType = as.factor(TrafficControlType)) %>%
  mutate(RoadClassification = as.factor(RoadClassification)) %>%
  mutate(PersonType = as.factor(PersonType)) %>%
  mutate(VisionObstruction = if_else(VisionObstruction %in% c("Unknown",""), 
                                     "Unknown/Blank", VisionObstruction),
         VisionObstruction = as.factor(VisionObstruction))
```

Then we visualized the differences in injury to show the imbalance in the data. 

```{r, eval=FALSE}
crashes_pm %>% 
  ggplot() +
  geom_bar(aes(Injury2, fill=Injury2), show.legend = FALSE) +
  geom_text(stat="count", aes(x=Injury2, label=..count..), vjust=-0.25) +
  labs(title = "Frequency of Injury2",
       x = "Injury2",
       y = "Count")
```

We also needed to clean that data further for the Random Forest model. 

```{r random-forest-dataset-code, eval=FALSE}
crashes_pm.clean = crashes_pm %>%
  dplyr::select(Injury2,Age,VehicleType,ContributingCircumstance1,Protection,
           WeatherCondition1,MostHarmfulEvent,RoadFeature,
           TrafficControlType,RoadClassification,PersonType,
           VisionObstruction) %>%
  na.omit()
```

Then we began modeling injury with the logistic regression model. 

```{r unbalanced-data-pred-acc-code, eval=FALSE}
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
(stats = calc_stats(tab, prevalence = NULL, positive = "Injury"))
```

```{r oversampled-dataset-code, eval=FALSE}
df_crashes_pm_Injury_ind <- which(crashes_pm$Injury2 == "Injury")
df_crashes_pm_NoInjury_ind <- which(crashes_pm$Injury2 == "No injury")
oversample_df1 <- crashes_pm[c(df_crashes_pm_NoInjury_ind, 
                               rep(df_crashes_pm_Injury_ind, 5)), ]
```

```{r oversample-data-pred-acc-code, eval=FALSE}
set.seed(101) #for reproducibility of results
sample2 <- sample(c(TRUE, FALSE), nrow(oversample_df1), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
oversample_df1.train <- oversample_df1[sample2, ]
oversample_df1.test <- oversample_df1[!sample2, ]
oversample_df1.fit.train <- glm(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                   WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                   TrafficControlType+RoadClassification+PersonType+
                                   AlcoholResultType+VisionObstruction, 
                                 data=oversample_df1.train, family="binomial") #fitting model on training set
oversample_df1.pred.prob <- predict(oversample_df1.fit.train, newdata=oversample_df1.test, 
                                     type="response") #predicting prob. of default=1 for test set
oversample_df1.pred <- ifelse(oversample_df1.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
(tab2 <- table(pred=oversample_df1.pred, actual=oversample_df1.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy2=mean(oversample_df1.pred==oversample_df1.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats2 = calc_stats(tab2, prevalence = NULL, positive = "Injury"))
```

```{r undersampled-dataset-code, eval=FALSE}
crashes_pm_no_injury = crashes_pm %>%
  filter(Injury2 == "No injury")
crashes_pm_injury = crashes_pm %>%
  filter(Injury2 == "Injury")
s = sample(1:nrow(crashes_pm_no_injury), 35000, replace = F)
undersample_df1 = crashes_pm_injury %>%
  bind_rows(crashes_pm_no_injury[s,])
```

```{r undersample-data-pred-acc-code, eval=FALSE}
set.seed(101) #for reproducibility of results
sample3 <- sample(c(TRUE, FALSE), nrow(undersample_df1), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
undersample_df1.train <- undersample_df1[sample3, ]
undersample_df1.test <- undersample_df1[!sample3, ]
undersample_df1.fit.train <- glm(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                    WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                    TrafficControlType+RoadClassification+PersonType+
                                    VisionObstruction, 
                                  data=undersample_df1.train, family="binomial") #fitting model on training set
undersample_df1.pred.prob <- predict(undersample_df1.fit.train, newdata=undersample_df1.test, 
                                      type="response") #predicting prob. of default=1 for test set
undersample_df1.pred <- ifelse(undersample_df1.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
(tab3 <- table(pred=undersample_df1.pred, actual=undersample_df1.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy3=mean(undersample_df1.pred==undersample_df1.test$Injury2, na.rm=T)*100) #percent of correct predictions in test
(stats3 = calc_stats(tab3, prevalence = NULL, positive = "Injury"))
```

```{r combo-datasets-code, eval=FALSE}
overunder <- ROSE::ovun.sample(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                 WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                 TrafficControlType+RoadClassification+PersonType+VisionObstruction, 
                               data = crashes_pm.train, method = "both", N = 245633)$data
overunder2 <- ROSE::ovun.sample(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                 WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                 TrafficControlType+RoadClassification+PersonType+VisionObstruction, 
                               data = crashes_pm.train, method = "both", N = 100000)$data
```

```{r combo-data-pred-acc-code, eval=FALSE}
set.seed(101) #for reproducibility of results
sample4 <- sample(c(TRUE, FALSE), nrow(overunder), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
overunder.train <- overunder[sample4, ]
overunder.test <- overunder[!sample4, ]
overunder.fit.train <- glm(Injury2~., data=overunder.train, family="binomial") #fitting model on training set
overunder.pred.prob <- predict(overunder.fit.train, newdata=overunder.test, 
                                type="response") #predicting prob. of default=1 for test set
overunder.pred <- ifelse(overunder.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
overunder.pred <- factor(overunder.pred, levels = c("No injury", "Injury")) #predicting `default` based on prob estimates
(tab4 <- table(pred=overunder.pred, actual=overunder.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy4=mean(overunder.pred==overunder.test$Injury2, na.rm=T)*100) #percent of correct predictions in test
(stats4 = calc_stats(tab4, prevalence = NULL, positive = "Injury"))
set.seed(101) #for reproducibility of results
sample5 <- sample(c(TRUE, FALSE), nrow(overunder2), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
overunder.train2 <- overunder[sample5, ]
overunder.test2 <- overunder[!sample5, ]
overunder.fit.train2 <- glm(Injury2~., data=overunder.train2, family="binomial") #fitting model on training set
overunder.pred.prob2 <- predict(overunder.fit.train2, newdata=overunder.test2, 
                                type="response") #predicting prob. of default=1 for test set
overunder.pred2 <- ifelse(overunder.pred.prob2>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
overunder.pred2 <- factor(overunder.pred2, levels = c("No injury", "Injury")) #predicting `default` based on prob estimates
(tab5 <- table(pred=overunder.pred2, actual=overunder.test2$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy5=mean(overunder.pred2==overunder.test2$Injury2, na.rm=T)*100) #percent of correct predictions in test
(stats5 = calc_stats(tab5, prevalence = NULL, positive = "Injury"))
```

Here we look at the accuracy of this model with the undersampled, oversampled, combination full sample, combination sub-sample, and the original balanced data. 

```{r StatisticsComparison-Table-code, eval=FALSE}
(StatisticsComparison = cbind(SampleType = c("Unbalanced", "Oversample", "Undersample", "Combination Full Sample", 
        "Combination Sub-Sample"),round(rbind(stats, stats2, stats3, stats4, stats5), 4)))
```

```{r Injury2.fit4-table-code, eval=FALSE}
Injury2.fit4 <- glm(Injury2~., data=overunder, family = "binomial")
require(broom)
out <- tidy(Injury2.fit4)
Injury2.fit4_Summary = cbind(out, odds.ratio = round(exp(coef(Injury2.fit4)), digits = 4))
Injury2.fit4.summary = dplyr::select(Injury2.fit4_Summary, -term)
require(knitr)
kable(Injury2.fit4.summary)
```

Then we began modeling injury with the Random Forest model. 

```{r unbalanced-data-pred-acc-rf-code, eval=FALSE}
library(ranger)
set.seed(101) #for reproducibility of results
sample.rf <- sample(c(TRUE, FALSE), nrow(crashes_pm.clean), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
crashes_pm.clean.train <- crashes_pm.clean[sample.rf, ]
crashes_pm.clean.test <- crashes_pm.clean[!sample.rf, ]
crashes_pm.clean.fit.train <- ranger::ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                 WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                 TrafficControlType+RoadClassification+PersonType+
                                 VisionObstruction, data=crashes_pm.clean, 
                               num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
crashes_pm.clean.pred <- predict(crashes_pm.clean.fit.train, data=crashes_pm.clean.test, type="response")
(tab.rf <- table(pred=crashes_pm.clean.pred$predictions, actual=crashes_pm.clean.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf=mean(crashes_pm.clean.pred$predictions==crashes_pm.clean.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf = calc_stats(tab.rf, prevalence = NULL, positive = "Injury"))
```

```{r oversampled-dataset-rf-code, eval=FALSE}
df_crashes_pm.clean_Injury_ind <- which(crashes_pm.clean$Injury2 == "Injury")
df_crashes_pm.clean_NoInjury_ind <- which(crashes_pm.clean$Injury2 == "No injury")
oversample_df2 <- crashes_pm.clean[c(df_crashes_pm.clean_NoInjury_ind, 
                               rep(df_crashes_pm.clean_Injury_ind, 5)), ]
```

```{r oversample-data-pred-acc-rf-code, eval=FALSE}
set.seed(101) #for reproducibility of results
sample.rf2 <- sample(c(TRUE, FALSE), nrow(oversample_df2), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
oversample_df2.train <- oversample_df2[sample.rf2, ]
oversample_df2.test <- oversample_df2[!sample.rf2, ]
oversample_df2.fit.train <- ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                     WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                     TrafficControlType+RoadClassification+PersonType+
                                     VisionObstruction, data=oversample_df2.train, 
                                   num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
oversample_df2.pred <-predict(oversample_df2.fit.train, data=oversample_df2.test, type="response")
(tab.rf2 <- table(pred=oversample_df2.pred$predictions, actual=oversample_df2.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf2=mean(oversample_df2.pred$predictions==oversample_df2.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf2 = calc_stats(tab.rf2, prevalence = NULL, positive = "Injury"))
```

```{r undersampled-data-rf-code, eval=FALSE}
crashes_pm.clean_no_injury = crashes_pm.clean %>%
  filter(Injury2 == "No injury")
crashes_pm.clean_injury = crashes_pm.clean %>%
  filter(Injury2 == "Injury")
s2 = sample(1:nrow(crashes_pm.clean_no_injury), 35000, replace = F)
undersample_df2 = crashes_pm.clean_injury %>%
  bind_rows(crashes_pm.clean_no_injury[s2,])
```

```{r undersample-data-pred-acc-rf-code, eval=FALSE}
set.seed(101) #for reproducibility of results
sample.rf3 <- sample(c(TRUE, FALSE), nrow(undersample_df2), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
undersample_df2.train <- undersample_df2[sample.rf3, ]
undersample_df2.test <- undersample_df2[!sample.rf3, ]
undersample_df2.fit.train <- ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                     WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                     TrafficControlType+RoadClassification+PersonType+
                                     VisionObstruction, data=undersample_df2.train, 
                                   num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
undersample_df2.pred <-predict(undersample_df2.fit.train, data=undersample_df2.test, type="response")
(tab.rf3 <- table(pred=undersample_df2.pred$predictions, actual=undersample_df2.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf3=mean(undersample_df2.pred$predictions==undersample_df2.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf3 = calc_stats(tab.rf3, prevalence = NULL, positive = "Injury"))
```

```{rcombo-datasets-rf-code, eval=FALSE}
overunder.rf <- ROSE::ovun.sample(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                 WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                 TrafficControlType+RoadClassification+PersonType+VisionObstruction, 
                               data = crashes_pm.clean.train, method = "both", N = 245633)$data
overunder2.rf <- ROSE::ovun.sample(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                     WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                     TrafficControlType+RoadClassification+PersonType+VisionObstruction, 
                                   data = crashes_pm.clean.train, method = "both", N = 100000)$data
```

```{r combo-data-pred-acc-rf-code, eval=FALSE}
set.seed(101) #for reproducibility of results
sample.rf4 <- sample(c(TRUE, FALSE), nrow(overunder.rf), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
overunder.rf.train <- overunder.rf[sample.rf4, ]
overunder.rf.test <- overunder.rf[!sample.rf4, ]
overunder.rf.fit.train <- ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                      WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                      TrafficControlType+RoadClassification+PersonType+
                                      VisionObstruction, data=overunder.rf.train, 
                                    num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
overunder.rf.pred <-predict(overunder.rf.fit.train, data=overunder.rf.test, type="response")
(tab.rf4 <- table(pred=overunder.rf.pred$predictions, actual=overunder.rf.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf4=mean(overunder.rf.pred$predictions==overunder.rf.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf4 = calc_stats(tab.rf4, prevalence = NULL, positive = "Injury"))
set.seed(101) #for reproducibility of results
sample.rf5 <- sample(c(TRUE, FALSE), nrow(overunder2.rf), replace = T, prob = c(0.7,0.3)) #70/30% training/test sets
overunder2.rf.train <- overunder2.rf[sample.rf5, ]
overunder2.rf.test <- overunder2.rf[!sample.rf5, ]
overunder2.rf.fit.train <- ranger(Injury2~Age+VehicleType+ContributingCircumstance1+Protection+
                                   WeatherCondition1+MostHarmfulEvent+RoadFeature+
                                   TrafficControlType+RoadClassification+PersonType+
                                   VisionObstruction, data=overunder2.rf.train, 
                                 num.trees = 500, mtry = round(11/2), importance = "impurity", classification = T) #fitting model on training set
overunder2.rf.pred <-predict(overunder2.rf.fit.train, data=overunder2.rf.test, type="response")
#overunder2.rf.pred <- ifelse(overunder2.rf.pred.prob>0.5, "Injury", "No injury") #predicting `default` based on prob estimates
(tab.rf5 <- table(pred=overunder2.rf.pred$predictions, actual=overunder2.rf.test$Injury2)) #confusion matrix: cross-tab of predictions vs actual class
(accuracy.rf5=mean(overunder2.rf.pred$predictions==overunder2.rf.test$Injury2, na.rm=T)*100) #percent of correct predictions in test data
(stats.rf5 = calc_stats(tab.rf5, prevalence = NULL, positive = "Injury"))
```

```{r StatisticsComparison-table-rf-code, eval=FALSE}
(StatisticsComparison.rf = cbind(SampleType = c("Unbalanced", "Oversample", "Undersample", "Combination Full Sample", 
                                            "Combination Sub-Sample"),round(rbind(stats.rf, stats.rf2, stats.rf3, stats.rf4, stats.rf5), 4)))
```
