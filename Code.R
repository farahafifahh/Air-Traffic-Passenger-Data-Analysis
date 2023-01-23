# Set the working directory
setwd("/Users/farahafifah/R Programming/Air Traffic Passenger Data/Air Traffic Passenger Data Analysis")

# Read a CSV files using read.csv() function
data <- read.csv("Air_Traffic_Passenger_Data.csv")

# Libraries
library(ggplot2)
library(plotrix)
library(e1071)
library(caret)
library(caTools)
library(tidyverse)

# Print the summary of the imported data
summary(data)

# Duplicate the data as a backup
data_new <- data

# Data Cleaning ====

# Converting data type of variables
data_new$Published.Airline <- as.factor(data_new$Published.Airline)
data_new$Published.Airline.IATA.Code <- as.factor(data_new$Published.Airline.IATA.Code)
data_new$GEO.Summary <- as.factor(data_new$GEO.Summary)
data_new$GEO.Region <- as.factor(data_new$GEO.Region)
data_new$Activity.Type.Code <- as.factor(data_new$Activity.Type.Code)
data_new$Price.Category.Code <- as.factor(data_new$Price.Category.Code)
data_new$Terminal <- as.factor(data_new$Terminal)
data_new$Boarding.Area <- as.factor(data_new$Boarding.Area)
data_new$Passenger.Count <- as.numeric(data_new$Passenger.Count)
data_new$Year <- as.factor(data_new$Year)
data_new$Month <- as.factor(data_new$Month)

# Convert to logical
data_new$GEO.Summary<-ifelse(data_new$GEO.Summary=="Domestic",1,0)
data_new$GEO.Summary <- as.logical(data_new$GEO.Summary)

# Remove unnecessary column
data_new$Activity.Period <- NULL
data_new$Adjusted.Activity.Type.Code <- NULL
data_new$Adjusted.Passenger.Count <- NULL
data_new$Operating.Airline <- NULL
data_new$Operating.Airline.IATA.Code <- NULL

# Changing names of column
names(data_new) [1] <- "airline"
names(data_new) [2] <- "code"
names(data_new) [3] <- "isDomestic"
names(data_new) [4] <- "region"
names(data_new) [5] <- "type"
names(data_new) [6] <- "category"
names(data_new) [7] <- "terminal"
names(data_new) [8] <- "area"
names(data_new) [9] <- "pax"
names(data_new) [10] <- "year"
names(data_new) [11] <- "month"

# Standardize United Airlines
data_new$airline[data_new$airline =="United Airlines - Pre 07/01/2013"] <- "United Airlines"

# Summary of the data after cleaning
summary(data_new)
