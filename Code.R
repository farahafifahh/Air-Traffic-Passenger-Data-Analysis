# Set the working directory
setwd("/Users/farahafifah/R Programming/Air Traffic Passenger Data/Air Traffic Passenger Data Analysis")

# Read a CSV files using read.csv() function
data <- read.csv("Air_Traffic_Passenger_Data.csv")

# Libraries
library(ggplot2)
library(dplyr)
library(plotrix)
library(e1071)
library(magrittr)
library(caret)
library(caTools)
install.packages("plotly")
library(tidyverse)

# Print the summary of the imported data
summary(data)

# Duplicate the data as a backup
data_new <- data

# Data Cleaning ====

# Converting data type of variables
data_new$Activity.Period <- as.numeric(data_new$Activity.Period)
data_new$Operating.Airline <- as.factor(data_new$Operating.Airline)
data_new$Operating.Airline.IATA.Code <- as.factor(data_new$Operating.Airline.IATA.Code)
data_new$Published.Airline <- as.factor(data_new$Published.Airline)
data_new$Published.Airline.IATA.Code <- as.factor(data_new$Published.Airline.IATA.Code)
data_new$GEO.Summary <- as.factor(data_new$GEO.Summary)
data_new$GEO.Region <- as.factor(data_new$GEO.Region)
data_new$Activity.Type.Code <- as.factor(data_new$Activity.Type.Code)
data_new$Price.Category.Code <- as.factor(data_new$Price.Category.Code)
data_new$Terminal <- as.factor(data_new$Terminal)
data_new$Boarding.Area <- as.factor(data_new$Boarding.Area)
data_new$Passenger.Count <- as.numeric(data_new$Passenger.Count)
data_new$Adjusted.Activity.Type.Code <- as.factor(data_new$Adjusted.Activity.Type.Code)
data_new$Adjusted.Passenger.Count <- as.numeric(data_new$Adjusted.Passenger.Count)
data_new$Year <- as.factor(data_new$Year)
data_new$Month <- as.factor(data_new$Month)

# Convert to logical
data_new$GEO.Summary<-ifelse(data_new$GEO.Summary=="Domestic",1,0)
data_new$GEO.Summary <- as.logical(data_new$GEO.Summary)

# Changing names of column
names(data_new) [1] <- "activity period"
names(data_new) [2] <- "operating airline"
names(data_new) [3] <- "operating code"
names(data_new) [4] <- "airline"
names(data_new) [5] <- "code"
names(data_new) [6] <- "isDomestic"
names(data_new) [7] <- "region"
names(data_new) [8] <- "type"
names(data_new) [9] <- "category"
names(data_new) [10] <- "terminal"
names(data_new) [11] <- "area"
names(data_new) [12] <- "pax"
names(data_new) [13] <- "adjusted type"
names(data_new) [14] <- "adjusted pax"
names(data_new) [15] <- "year"
names(data_new) [16] <- "month"

# Remove unnecessary column
data_new$`activity period` <- NULL
data_new$`adjusted type` <- NULL
data_new$`adjusted pax` <- NULL
data_new$`operating airline` <- NULL
data_new$`operating code` <- NULL


# Standardize United Airlines
data_new$airline[data_new$airline =="United Airlines - Pre 07/01/2013"] <- "United Airlines"

# Summary of the data after cleaning
summary(data_new)

# 1. Average Passenger Traffic between 2005 and 2016 ====

# Group the passenger traffic by mean of pax
passengerTraffic = data_new %>% group_by(isDomestic, month) %>%
  summarise(averagePax = round(mean(pax), digit = 0),
            .groups = 'drop')

# Plot the stacked bar graph
plot <- ggplot(passengerTraffic, aes(x = factor(month, labels = month.abb), 
                             y = averagePax, fill = isDomestic)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  theme_minimal() +
  scale_fill_discrete(name = "Destination", label = c("International", "Domestic")) +
  labs (x = "Month", y = "Passengers") +
  ggtitle("Monthly Average Passengers Count") +
  geom_text(aes(label = format(averagePax, big.mark = ",")), size = 2.75,
            position = position_stack(vjust = 0.5), colour = "white")
plot + theme(
  plot.title = element_text(hjust = 0.5, color="black", size=18, face="bold"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold")
)

# 2. Domestic Carriers Overview

# Domestic Airline
domesticAirline = data_new %>% group_by(isDomestic) %>%
  summarise()

# Domestic Passenger Count
domesticPassenger = data_new %>% 
  filter(isDomestic) %>%
  group_by(airline) %>%
  summarise(countPassenger = sum(pax),
            .groups = 'drop')

# Sort descending by countPassenger
domesticPassenger <- domesticPassenger[order(-domesticPassenger$countPassenger),]

# Only show top 5 or airline that has the most passengers
domesticPassenger <- domesticPassenger[1:5,]

plot <- ggplot(domesticPassenger, aes(x = reorder(airline, -countPassenger), y = countPassenger)) +
  theme_minimal() +
  geom_bar(stat = "identity") +
  geom_col(fill = "#E59393", color = "#D62D2D") +
  geom_text(aes(label = format(countPassenger, big.mark = ",")), size = 2.75,
            position = position_stack(vjust = 0.5), colour = "white")+
  labs(y = "Passengers", x = "Airline") +
  ggtitle("Domestic Passengers Count by Airline")
plot + theme(
  plot.title = element_text(hjust = 0.5, color="black", size=18, face="bold"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold")
)



