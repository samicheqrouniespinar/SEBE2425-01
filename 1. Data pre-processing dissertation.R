# Data pre-processing of Electrification of Heat (EoH) data-set
# Author: Sami Cheqrouni-Espinar
# Date: 30/05/2025

# Install required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
install.packages("readr")                                         
install.packages("fs")
install.packages("purrr") 
install.packages("ggplot2")
install.packages("rlang")
install.packages("units")
install.packages("chron")
install.packages("healthyR")
install.packages("zoo")
install.packages("bigmemory")
install.packages("ff")
install.packages("DBI")
install.packages("duckdb")
install.packages("fst")
install.packages("pryr")
install.packages("patchwork")
install.packages("gtable")
install.packages("magrittr")
install.packages("ggcorrplot")
install.packages("plm")
install.packages("fastDummies")
install.packages("car")
install.packages("ggpubr")
install.packages("lm.beta")
install.packages("lmtest")
install.packages("AICcmodavg")
install.packages("ggResidpanel")
install.packages("gvlma")
install.packages("gplots")
install.packages("foreign")
install.packages("plotly")
install.packages("performance")
install.packages("see")

# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(caret)
library(MASS)
library(e1071)
library(caTools)
library(corrplot)
library(pROC)
library(ROCR)
library(readr)
library(fs)
library(data.table)
library(ggplot2)
library(rlang)
library(units)
library(chron)
library(healthyR)
library(zoo)
library(bigmemory)
library(ff)
library(arrow)
library(DBI)
library(duckdb)
library(fst)
library(pryr)
library(patchwork)
library(magrittr)
library(ggcorrplot)
library(plm)
library(fastDummies)
library(car)
library(ggpubr)
library(lm.beta)
library(lmtest)
library(dplyr)
library(AICcmodavg)
library(ggResidpanel)
library(gvlma)
library(gplots)
library(foreign)
library(plotly)
library(performance)
library(see)

## Data integration 
# Check to see that working directory exists
dir.exists("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/Cleansed data")    # It does in this case

# Setting folder path to files and working directory
folder_path <- normalizePath("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/Cleansed data")

# Setting working directory 
setwd("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/Cleansed data")

# Names of all files within cleansed data folder
list.files("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/Cleansed data")

# Assigning an object to the list of file names 
Property_ID <- list.files("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/Cleansed data")

# Creating an object of the folder paths to each CSV file 
csv_list <- list.files(path = folder_path,          # Assigns folder path above as the path for the files
                       pattern = "\\.csv$",         # Looks only for CSV files
                       full.names = TRUE,           #sorts in alphabetical order 
                       recursive = TRUE,            #Lists files only with the working directory
                       include.dirs = FALSE,        #Excludes subdirectory names in the recursive listings. 
                       ) 

# See list of property files
print(csv_list)

# Importing EoH metadata
EoH_meta_data <- read.csv("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/Metadata/BEIS Electrification of Heat Project - Property, Design and Installation Information.csv")

#Saving file
#write.csv(EoH_meta_data,"~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/HPlist.csv", row.names = FALSE)

# Creating subset of the data for property IDs with air-source heat pumps (ASHPs) in them
EoH_meta_data <- filter(EoH_meta_data,
                        HP_Recommend == c("ASHP","HT_ASHP"))

gc()
# Filter data set for the column with the Property ID so meta-data is a list of properties with ASHP's 
EoH_meta_data <- subset(EoH_meta_data, select = "Property_ID")

# Extracting unique IDs
meta_ids <- unique(EoH_meta_data$Property_ID)

# Filter CSVs whose filename contains a matching Property_ID
filtered_csv_list <- csv_list[
  sapply(csv_list, function(f) {
    prop_id <- gsub("Property_ID=|.csv", "", basename(f))
    prop_id %in% meta_ids
  })
]

# Start timing
start.time <- Sys.time()

# Load only relevant CSVs
all_csv <- lapply(filtered_csv_list, function(f) {
  message("Reading file: ", f)
  df <- read.csv(f, skipNul = FALSE)
  df$Property_ID <- gsub("Property_ID=|.csv", "", basename(f))
  return(df)
})

# Stop timing
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
print(time.taken)

# Combine into single dataframe
combined_data <- do.call(rbind, all_csv)

str(EoH_raw_data)
# Creating a nested list of EoH data for each household  
# Calculating time to import
start.time <- Sys.time() 
all_csv <- lapply(csv_list, function(f) {         #f represents the folder path for each CSV within the nested list
  message("Reading file: ", f)
  df <- read.csv(f, skipNul = FALSE)                       #Reads NULLs within the csv files
  df$Property_ID <-basename(f)                            #This creates a column for the property ID
  df$Property_ID<-gsub("Property_ID=","",as.character(df$Property_ID))  #These two lines remove any unecessary text from the ID.
  df$Property_ID<-gsub(".csv","",as.character(df$Property_ID))
  return(df)                                               #This returns the dataframe with the new columns and values 
})

# Calculating end time for script
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

# Keeping the original file names within the nested list 
all_csv <- setNames(all_csv ,basename(filtered_csv_list))

## Binding sample data 
#EoH_raw_data <- bind_rows(all_csv)

# Check memory used
mem_used() 

# Free some space on R 
gc()

# Binding full data set
start.time <- Sys.time()

EoH_raw_data <- rbindlist(all_csv, use.names = TRUE, fill = TRUE)
str(EoH_raw_data)
# EoH_raw_data <- as.data.frame(EoH_raw_data)
 
end.time <- Sys.time()
print(round(end.time - start.time, 2))

# Creating output path for file 
output_path_raw <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/EoH_raw_data.csv"

# Saving file 
fwrite(EoH_raw_data, output_path) 

# Loading EoH_raw_data back into R to save time 
EoH_raw_data <- fread(output_path)

# Importing a summary of EoH trial data  
EoH_summary_data <- read.csv("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/eoh_summary_for_publication.csv")
                      
# Building in gc() at regular intervals to free-up disc space in R 
gc()

# Filtering main data set so only properties with ASHPs remain. 
EoH_raw_data <- filter(EoH_raw_data, Property_ID %in% EoH_meta_data$Property_ID)

## Data cleansing 
#Checking to see if there Timestamps that have NA value's in data set
n_distinct(which(is.na(EoH_raw_data$Timestamp))) # There are no NULL values in the Timestamp column

# Creating a cleansed data set where there are no null values for the columns being used for analysis 
# EoH_data_clean <- EoH_raw_data[!is.na(EoH_raw_data$Timestamp),]
EoH_data_clean <- EoH_raw_data[!is.na(EoH_raw_data$External_Air_Temperature),]
EoH_data_clean <- EoH_data_clean[!is.na(EoH_data_clean$Internal_Air_Temperature),]
EoH_data_clean <- EoH_data_clean[!is.na(EoH_data_clean$Whole_System_Energy_Consumed),]
EoH_data_clean <- EoH_data_clean[!is.na(EoH_data_clean$Heat_Pump_Energy_Output),]

# Creating output path for file 
output_path_clean <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/EoH_data_clean.csv"

# Saving file 
fwrite(EoH_data_clean, output_path_clean) 

# Loading EoH_raw_data back into R to save time 
EoH_data_clean <- fread(output_path_clean)

# Seeing number of distinct properties in data set.
n_distinct(EoH_data_clean$Property_ID)

# Building in gc() at regular intervals to free-up disc space in R 
gc()

# View cleaned data
head(EoH_data_clean)

# Check of the format of the reduced data set
str(EoH_data_clean)        

# Changing format of 'Timestamp' to datetime format using lubridate
EoH_data_clean$Timestamp <- ymd_hms(EoH_data_clean$Timestamp, tz = "GMT")  

 # View  of the newly created variable 
head(EoH_data_clean)

# Cleaning data-set again to get rid of NULL values introduced from converting into standard
# date-time format 
EoH_data_clean<- EoH_data_clean[!is.na(EoH_data_clean$Timestamp),]

# Check to see if there are NULLs still in the cleaned dataframe 
n_distinct(which(is.na(EoH_data_clean$Timestamp)))   #No null values in cleaned dataframe

# Seeing format of data set 
str(EoH_data_clean)  #Timestamp now in the right format
gc()

## Data transformation

## Creating different time aggregations of data set 
EoH_data_clean$Year <- year(EoH_data_clean$Timestamp)      # Year variable
EoH_data_clean$Month <- month(EoH_data_clean$Timestamp)      # Month  variable
EoH_data_clean$Hour <- hour(EoH_data_clean$Timestamp)      # Hour variable
EoH_data_clean$Minute <- minute(EoH_data_clean$Timestamp)        # Minute variable
EoH_data_clean$Hour_Minute <-format(EoH_data_clean$Timestamp, "%H:%M:%S")     # Hour-minute variable
EoH_data_clean$Hour_Minute <- chron(times = EoH_data_clean$Hour_Minute)  # Running chron to ensure it formats in chronological order
EoH_data_clean$Monthday <- day(EoH_data_clean$Timestamp)       # Monthday variable
EoH_data_clean$date <- make_date(year = EoH_data_clean$Year, month = EoH_data_clean$Month, day = EoH_data_clean$Monthday)  #date variable
EoH_data_clean$day <- wday(EoH_data_clean$Timestamp) # wday translates a Saturday to 7 and Sunday to 1

# Quick check of data to see that it has formatted correctly
head(EoH_data_clean) 
str(EoH_data_clean) #Need to change Hour_Minute into correct date-time format 

# Creating more aggregations
EoH_data_clean$wday <- ifelse(EoH_data_clean$day == 1, "weekend", ifelse(EoH_data_clean$day==7,"weekend", "weekday"))
EoH_data_clean$wday <- as.factor(EoH_data_clean$wday)
EoH_data_clean$day <- as.integer(EoH_data_clean$day)
EoH_data_clean$Month <- as.integer(EoH_data_clean$Month)
EoH_data_clean$Year <- as.integer(EoH_data_clean$Year)
 
# Finding the difference in energy consumption and  heat output in each 2 minute interval within the data for each heat pump 
# Creating a variable also for power output per 2 minute interval
EoH_data_clean <- EoH_data_clean %>% 
  mutate(Interval_consumption_kWh = c(NA,abs(diff(Whole_System_Energy_Consumed)))) %>%
  mutate(Interval_heat_output_kW = (c(NA,abs(diff(Heat_Pump_Energy_Output)))*30)) %>%
  mutate(Power_output_2mins_kW = (Interval_consumption_kWh * 30)) %>%
  mutate(temp_decay_degrees_C = diff(c(NA,Internal_Air_Temperature)))

# Quick view of the data set with new variables
head(EoH_data_reduced)

## Data reduction 

# Removing unneccessary columns from the data set
EoH_data_reduced <- EoH_data_clean[, -c(2,5,6,7,8,12,13,14)]

# Removing any nulls from the data set that may have been introduced 
EoH_data_reduced <- EoH_data_reduced[!is.na(EoH_data_reduced$Interval_consumption_kWh),]
EoH_data_reduced <- EoH_data_reduced[!is.na(EoH_data_reduced$Interval_heat_output_kW),]
EoH_data_reduced <- EoH_data_reduced[!is.na(EoH_data_reduced$temp_decay_degrees_C),]

# Changing format of dates
EoH_data_reduced$date <- ymd(EoH_data_reduced$date, tz = "GMT") 

# Creating output path for file 
output_path_reduced <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/EoH_data_reduced.csv"

# Saving file 
fwrite(EoH_data_reduced, output_path_reduced) 

# Loading EoH reduced data back into R to save time 
EoH_data_reduced <- fread(output_path_reduced)

# Building in gc() at regular intervals to free-up disc space in R 
gc()

# Check to see if there are any remaining nulls
sum(is.na(EoH_data_reduced)) # still some present in the data set  
 
# Doing whole clean of the data set to remove any remaining nulls
#EoH_data_reduced <- na.omit(EoH_data_reduced)

# Check to see I've kept the right columns 
head(EoH_data_reduced) # Fine

# Check formatting of variables 
str(EoH_data_reduced)   
 
 # Creating a new data frame with average daily temperature
daily_avg <- EoH_data_reduced %>%
  mutate(date = as.Date(Timestamp)) %>% 
  group_by(Month, Monthday, Year, date) %>%
  summarize(avg_temp = mean(External_Air_Temperature, na.rm = TRUE))

# Check of format for newly created data frame
str(daily_avg)   

# Format date column in date format to be able to join after to reduced dataframe 
daily_avg$date <- ymd(daily_avg$date, tz = "GMT")  

# Format year into a number to be able to plot
daily_avg$Year <- as.numeric(daily_avg$Year)

# Only keep unique values 
daily_avg <- unique(daily_avg)

# Seeing the number of distinct dates
n_distinct(daily_avg$date)

# Make date a factor here
daily_avg$date <- as.factor(daily_avg$date)

# Check to see if avg_temp is normally distributed to be able 
# to see if mean is a good measure of the average.

qqnorm(daily_avg$avg_temp)
qqline(daily_avg$avg_temp)



# Using histograms to compare difference in means
Plot_1 <- ggplot(daily_avg) + 
  geom_histogram(bins = 3490, aes(x = avg_temp, color = "#000000" ,fill = "#0099F8"))+
  #geom_vline(xintercept = 0.115, linetype = 2, colour = "red") +
  #annotate("label", x = 0.115, y = 3300, label = "'On' point", colour = "red", size = 3) +
  #xlim(0, 0.15) + 
  labs(x = "Energy consumption per 2 minute interval (kWh)" , y = "Frequency") +
  ggtitle("Heat pump energy consumption per 2 minute interval (kWh/2mins) for sample of 40") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_1 <- Plot_1 + guides(fill=guide_legend(title="Heat pump ID ")) 
Plot_1

# Seeing filter for each year of trial
daily_avg_2021 <- daily_avg %>%
filter(Year == '2021')

daily_avg_2022 <- daily_avg %>%
  filter(Year == '2022')

daily_avg_2023 <- daily_avg %>%
  filter(Year == '2023')

# Using boxplots to compare difference
Plot_2 <- ggplot(daily_avg) +
  geom_boxplot(aes(x = "", y = avg_temp, colour = "black", fill = "red"), show.legend = FALSE) +
  labs(x = "",y = "Temperature (degrees Celcius)") +
  ggtitle("Distribution of Internal air temperatures") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
  plot.caption = element_text(face = "italic")) +
  #Plot_2 <- Plot_2 + guides(fill=guide_legend(title="Temperatures"))
Plot_2

# Converting date back to ymd format
daily_avg$date <- ymd(daily_avg$date)

# Remove any columns which may be duplicated when rejoining to main data set
daily_avg <- daily_avg[,-c(1:3)]

# Joining the average daily temperature column to the main dataframe by date
EoH_data_reduced <- left_join(EoH_data_reduced,daily_avg, by = "date")

# Check that no duplicate columns have been introduced
head(EoH_data_reduced)

# Creating a variable 'cold day flag' for when the average daily temperature is below 2C (making it a 'cold day')
EoH_data_reduced <-  EoH_data_reduced %>%
                   mutate(Cold_day_flag = ifelse(avg_temp < 2,"Cold day","Non-cold day"))

# Check formatting here
str(EoH_data_reduced)

# Make date a factor before visualising 
daily_avg$date <- as.Date(daily_avg$date)
str(daily_avg$date)

# Visualising average external temperature for all dates
highlighted_regions <- data.frame(
  xmin = as.Date(c('2020-10-26', '2021-01-01','2022-01-01','2023-01-01')),
  xmax = as.Date(c('2020-12-31', '2021-12-31','2022-12-31','2023-09-28' )),
  ymin = c(0,0,0,0),
  ymax = c(Inf,Inf, Inf,Inf),
  fill = factor(c("lightgreen","white","lightgreen","white"), levels = c("lightgreen","white"))
)

Plot_3 <- ggplot(daily_avg, aes(x = date, y= avg_temp)) +
  geom_rect(data = highlighted_regions, aes(xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax, fill = fill), 
            alpha = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("lightgreen","white","lightgreen","white")) +
  geom_bar(stat = "identity", na.rm=TRUE) +
  scale_colour_gradient(low='navy', high='red') +
  geom_hline(yintercept = 2, colour = "red",  size = 1) + 
  annotate("label", x = as.Date("2020-11-28"), y = 23, label = "2020", colour = "black", size = 3) +
  annotate("label", x = as.Date("2021-07-02"), y = 23, label = "2021", colour = "black", size = 3) +
  annotate("label", x = as.Date("2022-06-02"), y = 23, label = "2022", colour = "black", size = 3) +
  annotate("label", x = as.Date("2023-05-07"), y = 23, label = "2023", colour = "black", size = 3) +
  annotate("label", x = as.Date("2022-01-01"), y = 3, label = "Cold day temperature threshold", colour = "red", size = 3) +
  labs(title="Daily average external temperature during trial", x = "Date", y="
  Average daily external temperature (degrees °C)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  )  +
  theme(legend.position="none")
Plot_3

# Using a heatmap to show variation in temperature for example year (here using 2021 for instance)
Plot_4 <- ggplot(daily_avg_2021, aes(x = Month, y = Monthday, fill = avg_temp)) +
  geom_tile() +
  scale_fill_gradient(low = "navyblue", high = "red") +
  labs(x = "Month", y = "Monthday", title = "Temperature variation in 2023")+
   theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    #axis.text.x = element_text(angle =90)
  )  +
  scale_x_continuous(breaks = seq(0,12, by = 1)) +
  scale_y_continuous(breaks = seq(0,30, by = 5)) +
 guides(fill=guide_legend(title="Average external temperature (degrees °C)"))
Plot_4

## Applying assumptions to the data
# Assumption 1: Removing periods from the data-set where temperature variation exceeds 5°C within 2mins (from 2.7.2 of paper from Halloran et al.)
EoH_data_reduced <- filter(EoH_data_reduced, abs(temp_decay_degrees_C) < 5)

# Assumption 2: filtering data set for cold-days (where average external temperature less than 2°C) 
EoH_data_cold_days <- filter(EoH_data_reduced,
                             Cold_day_flag == "Cold day")

# Let's see the number of distinct dates
n_distinct(EoH_data_cold_days$date) #71 dates to now look at 

# Creating output path for file 
output_path_cold_days <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/EoH_data_cold_days.csv"

# Saving file 
fwrite(EoH_data_cold_days, output_path_cold_days) 

# Loading EoH cold data back into R to save time pre-processing 
EoH_data_cold_days <- fread(output_path_cold_days)

# Building in gc() at regular intervals to free-up disc space in R 
gc()

# Quick check of the data frame and formatting of the columns
head(EoH_data_cold_days) 
str(EoH_data_cold_days) #Looks fine 

# Make date a factor here in each data frame where the temperature range has been included
EoH_data_cold_days$date <- as.factor(EoH_data_cold_days$date)

# Check for null/NA values in data set
sum(is.na(EoH_data_cold_days)) # No nulls in the new data frame

# Print of dates where there are cold days in the data set 
unique(EoH_data_cold_days$date)  # dates of 'cold days' in the data set

# Creating an object of the cold dates to be able to refer to 
cold_dates <- unique(EoH_data_cold_days$date)

# Viewing the data set to see if the values for power output are sensible
view(EoH_data_cold_days)

# One outlier value with a consumption above 100kW which is physically 
# impossible and so removing this value from the data set in addition to 
# other values which are likely to be outliers (due to being too large or negative)

# Importing EoH metadata though this time not filtering down for Property ID
EoH_meta_data <- read.csv("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/Metadata/BEIS Electrification of Heat Project - Property, Design and Installation Information.csv")

# Creating subset of the meta-data for property IDs with ASHPs in them
EoH_meta_data_ASHP <- filter(EoH_meta_data,
                        HP_Recommend == c("ASHP","HT_ASHP"))


# Convert rated heat pump size in meta data to numerical value 
EoH_meta_data_ASHP$HP_Size_kW <- as.numeric(EoH_meta_data_ASHP$HP_Size_kW)

# Getting rid of null values in the data set
EoH_meta_data_ASHP <- na.omit(EoH_meta_data_ASHP)

# Checking the formatting
str(EoH_meta_data_ASHP$HP_Size_kW)

# Finding the maximum size heat pump
max(EoH_meta_data_ASHP$HP_Size_kW)
max_HP <- max(EoH_meta_data_ASHP$HP_Size_kW)

Plot_5 <- ggplot(EoH_meta_data) +
  geom_boxplot(aes(x = "", y = HP_Size_kW, fill = "red"), show.legend = FALSE) +
  labs(x = "",y = "Heat pump size (kW)") +
  ggtitle("Distribution of heat pump sizes in metadata") +
  theme(plot.title = element_text(hjust = 0.5, size = 15), 
  plot.caption = element_text(face = "italic")) 
  Plot_5 <- Plot_5 + guides(fill=guide_legend(title="Temperatures"))
Plot_5

# Seeing summary statistics in heat pump size 
summary(EoH_meta_data_ASHP$HP_Size_kW)
sd(EoH_meta_data_ASHP$HP_Size_kW)

# Assumption: filtering for periods where the heat pump heat output is less than the maximum rated power output
# in the data 
EoH_data_cold_days <- EoH_data_cold_days %>% 
  filter(Interval_heat_output_kW <= max_HP) 

# Filtering out negative values for the heat output as they're outliers
EoH_data_cold_days <- EoH_data_cold_days %>% 
  filter(Interval_heat_output_kW >= 0)
  
# Filtering out negative values for the power output as they're also outliers
EoH_data_cold_days <- EoH_data_cold_days %>% 
  filter(Power_output_2mins_kW >= 0) 

## Approach for finding optimal number of bins to be able to eventually define 
# 'On', 'Off' mode of heat pump accurately

# Seeing if Power output is normally distributed within the data set
qqnorm(EoH_data_cold_days$Power_output_2mins_kW)
qqline(EoH_data_cold_days$Power_output_2mins_kW) # Most likely not

# Statistical summary of Power_output_2mins
summary(EoH_data_cold_days$Power_output_2mins)

# Histogram showing variation in 2-mins power_output with display
# of both peaks of where compressor is on
Plot_6 <- ggplot(EoH_data_cold_days, aes(x = Power_output_2mins_kW)) + 
  geom_histogram(bins = 299, color = "#000000" ,fill = "#0099F8") +
  labs(x = "Power output per 2 minute interval (kW)" , y = "Frequency") +
  ggtitle("Heat pump power output per 2 minute interval") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_6 <- Plot_6 + guides(fill=guide_legend(title="Heat pump ID ")) 
Plot_6

# Histogram of internal air temperature
Plot_7 <- ggplot(EoH_data_cold_days, aes(x = Internal_Air_Temperature)) + 
  geom_histogram(bins = 299, color = "#000000" ) +
  labs(x = "Internal air temperature (degrees Celcius) ", y = "Frequency" ) +
  ggtitle("Internal air temperature per 2 minute interval for sample of 40") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_7 <- Plot_7 + guides(fill=guide_legend(title="Heat pump ID ")) 
Plot_7

# Firstly get rid of any duplicates which may exist in the data 
EoH_data_cold_days <- unique(EoH_data_cold_days)

# Finding value of n for size of data
n <- n_distinct(EoH_data_cold_days)
print(n)  #3,322,522 data size

# Statistical methods for calculating optimal number of bins 
# Pearson's rule 
Pearsons_bins <- sqrt(n)
Pearsons_bins <- round(Pearsons_bins,0)
Pearsons_bins  #1823 bins using Pearson's rule

#  Rice's rule 
Rice_bins <- 2*n^(1/3) 
Rice_bins <- round(Rice_bins,0)
print(Rice_bins) #298 bins using Rice's rule

# Sturges' rule 
Sturges_bin <- 1 + log2(n)
Sturges_bin <- round(Sturges_bin,0)
print(Sturges_bin)  #23 bins using Sturges' rule

# Freedman-Diaconis rule 
summary(EoH_data_cold_days$Power_output_2mins_kW)

# Finding range in normalized data
range <- max(EoH_data_cold_days$Power_output_2mins_kW) - min(EoH_data_cold_days$Power_output_2mins_kW)

Q3 <- quantile(EoH_data_cold_days$Power_output_2mins_kW, 0.75) 
Q1 <- quantile(EoH_data_cold_days$Power_output_2mins_kW, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin <- range/Bin_width
Freedman_Diaconic_bin <- round(Freedman_Diaconic_bin,0)
print(Freedman_Diaconic_bin) #3948 bins using this method

# Comparing the number of bins to one another
# Histogram using Sturges' rule
Plot_Sturges <- ggplot(EoH_data_cold_days, aes(x = Power_output_2mins_kW)) +
  geom_histogram(bins = Sturges_bin,color = "#000000" ,fill = "#0099F8") +
  ggtitle("Sturges' Rule") + 
  labs(x = "Power consumption (kW) per 2-minute interval (kW)" , y = "frequency") +
  ggtitle("Histogram plotted using Sturges' rule") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )

# Histogram using Rice's rule
Plot_Rice <- ggplot(EoH_data_cold_days, aes(x = Power_output_2mins_kW)) +
  geom_histogram(bins = Rice_bins,color = "#000000" ,fill = "#0099F8") +
  ggtitle("Sturges' Rule") + 
  labs(x = "Power consumption (kW) per 2-minute interval (kW)" , y = "frequency") +
  ggtitle("Histogram plotted using Rice's rule") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )

# Histogram using Pearson's rule
Plot_Pearson <- ggplot(EoH_data_cold_days, aes(x = Power_output_2mins_kW)) +
  geom_histogram(bins = Pearsons_bins,color = "#000000" ,fill = "#0099F8") +
  ylim(0,50000)+
  xlim(0,16)+
  ggtitle("Pearson's Rule") + 
  labs(x = "Power consumption (kW) per 2-minute interval" , y = "Frequency") +
  ggtitle("Histogram plotted using Pearson's rule") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )

# Histogram using Freedman-Diaconic's rule
Plot_Fd <- ggplot(EoH_data_cold_days, aes(x = Power_output_2mins_kW)) +
  ylim(0,28000)+
  xlim(0,16)+
  geom_histogram(bins = Freedman_Diaconic_bin,color = "#000000" ,fill = "#0099F8") +
  ggtitle("Freedman-Diaconis Rule") + 
  labs(x = "Power consumption (kW) per 2-minute interval" , y = "Frequency") +
  ggtitle("Histogram plotted using Freedman-Diaconis' rule") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
 
# View of histograms in canvas         
Plot_8 <- Plot_Sturges + Plot_Rice + Plot_Pearson + Plot_Fd
Plot_8

gc()
# View of histograms individually
Plot_Sturges
Plot_Rice
Plot_Pearson
Plot_Fd

# Visualising power output for where the heat pump is considered o
Plot_9 <- ggplot(EoH_data_cold_days, aes(x = Power_output_2mins_kW)) + 
  coord_cartesian(ylim = c(0,30000),
  xlim = c(0,10))+
  geom_histogram(bins = Freedman_Diaconic_bin, color = "#000000" ,fill = "#0099F8") +
  geom_freqpoly(bins = Freedman_Diaconic_bin)+
  labs(x = "Power output (kW) per 2 minute interval" , y = "Frequency") +
  ggtitle("Variation in power output (kW) per 2 mniute interval") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_9

# Creating histogram data for power output
Plot_9_data <- ggplot_build(Plot_9)$data[[1]]
head(Plot_9_data)

## Finding optimal number of bins for plotting internal air temperature
# Finding range in normalized data
range <- max(EoH_data_cold_days$Internal_Air_Temperature) - min(EoH_data_cold_days$Internal_Air_Temperature)

Q3 <- quantile(EoH_data_cold_days$Internal_Air_Temperature, 0.75) 
Q1 <- quantile(EoH_data_cold_days$Internal_Air_Temperature, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin1 <- range/Bin_width
Freedman_Diaconic_bin1 <- round(Freedman_Diaconic_bin1,0)
print(Freedman_Diaconic_bin1) #3948 bins using this method

# Histogram of internal air temperature 
Plot_10 <- ggplot(EoH_data_cold_days, aes(x = Internal_Air_Temperature)) + 
  geom_histogram(bins = Freedman_Diaconic_bin1, color = "#000000" ,fill = "#0099F8") +
  labs(x = "Lagged energy consumption (kWh) per 2 minute interval" , y = "Frequency") +
  ggtitle("Variation in heat pump energy consumption (kWh)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
plot.caption = element_text(face = "italic")
Plot_10 <- Plot_10 + guides(fill=guide_legend(title="Heat pump ID ")) 
Plot_10

## Identifying heat pumps with suitable load profiles for analysis 

# How many heat pumps in cold days data frame
n_distinct(EoH_data_cold_days$Property_ID)

# Which are those heat pumps 
unique(EoH_data_cold_days$Property_ID)

# Create a summary of the histogram data from Plot 10
Plot_10_data <- ggplot_build(Plot_10)$data[[1]]
head(Plot_10_data)

view(Plot_10_data)

# Looking at correlation between heat output and electricity consumption 
cor(EoH_data_cold_days$Interval_consumption_kWh, EoH_data_cold_days$Interval_heat_output_kW)

# High value of 0.877 heat output may also be a suitable measure
# for defining heat output

# Calculating the temperature decay for each time interval
EoH_data_cold_days <- EoH_data_cold_days %>%
  mutate(temp_decay = diff(c(NA,Internal_Air_Temperature)))

# Calculating the cumulative decay to work out the total temperature change for
# a given period of time 
EoH_data_cold_days <- EoH_data_cold_days[!is.na(EoH_data_cold_days$temp_decay),]

# Creating a cumulative decay column 
EoH_data_cold_days <- EoH_data_cold_days %>%
  mutate(cum_decay = cumsum(temp_decay_degrees_C)) %>%
  group_by(Property_ID)

# Checking format of data
str(EoH_data_cold_days)

# Make cumulative decay start from 0 (this isn't necessary to run if starting value in cumulative )
# decay column is 0
# EoH_data_cold_days$cum_decay <- EoH_data_cold_days$cum_decay - EoH_data_cold_days[1,21]

# Finding mean internal temperature to use it as the starting point of finding out
# temperature decay will mark threshold
mean_internal_temp <- mean(EoH_data_cold_days$Internal_Air_Temperature)
mean_external_temp <- mean(EoH_data_cold_days$avg_temp)
mean_internal_temp <- round(mean_internal_temp , digits = 2)

# Median thermal time constant of EoH data set from (Lizana, Halloran and McClulloh, 2024)
tau_EoH <- 44.4 * 60 * 60 

# Defining Newton's law of cooling to work out temperature decay per 2 minute period
# which most likely be due to the heat pump being turned off, starting from the mean
# internal temperature

temp_decay_off <- mean_internal_temp-(mean_internal_temp)*exp(2*60/tau_EoH)
print(temp_decay_off) # -0.01381149°C is threshold temperature decay for defining off periods

# Define parasitic load value as 50W 
parasatic_load <- 0.05

# Defining the inflection point in the histogram as the point when the heat pumps beginning to be turned on.

# Creating a categorical variable based on lagged consumption of when heat pump is either 'On', 'Off' or 'On' 
# but the compressor is off (which is considered still as being on)
# Taking the upper limit of the second bin and the lower interval of the 17th bin as where the 'On' and 'Off' periods are (this is also the inflection point in the histogram)
# Assuming anything less than 50W is a parasitic load 
# Temperature decay is the temperature decay that would occur for starting T0 at the mean internal temperature
# for a 2 minute interval based on thermal time constant of 44.4 hours 
EoH_data_cold_days <-mutate(EoH_data_cold_days, 
                            On_Off_flag = ifelse(Power_output_2mins_kW <= parasatic_load | Interval_heat_output_kW == 0 | temp_decay_off >= temp_decay_degrees_C , "Off",
                                                 ifelse(Power_output_2mins_kW >= 0.328, "On","On - Compressor Off")))

# Define periods to highlight
highlight_regions <- data.frame(
  xmin = c(0, 0.05, 0.214),
  xmax = c(0.05,0.214, 10),
  ymin = c(0,0,0),
  ymax = c(Inf,Inf, Inf),
  fill = factor(c("pink","lightblue","lightgreen"), levels = c("pink","lightblue","lightgreen"))
)

# Visualising when the heat pump is 'On' or 'Off' within the data for power output 
Plot_11 <- ggplot(EoH_data_cold_days, aes(x = Power_output_2mins_kW)) + 
  coord_cartesian(ylim = c(0,30000),
                  xlim = c(0,10))+
  geom_histogram(bins = Freedman_Diaconic_bin, color = "#000000") +
  #geom_vline(xintercept = 0.005, linetype = 2, colour = "red") + 
  #annotate("label", x = 0, y = 20000, label = "'On' region", colour = "red", size = 3) +
  #geom_vline(xintercept = 0.112, linetype = 2, colour = "red") +
  #annotate("label", x = 5, y = 20000, label = "'Off' region", colour = "darkgreen", size = 3) +
  labs(x = "Power output (kW) per 2 minute interval" , y = "Frequency") +
  ggtitle("Variation in power output (kW) with 'On'/'Off' regions") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  ) +  # Add time series line
  geom_rect(data = highlight_regions, aes(xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax, fill = fill), 
            alpha = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("pink","lightblue","lightgreen"))+
theme(legend.position="none")
Plot_11

# Define periods to highlight
highlight_regions <- data.frame(
  xmin = c(temp_decay_off),
  xmax = c(-Inf),
  ymin = c(0),
  ymax = c(Inf),
  fill = factor(c("pink"), levels = c("pink"))
)

# Finding value of n for size of data
n <- n_distinct(flexibility_periods_full)
print(n)  

# Freedman-Diaconis rule 
summary(flexibility_periods_full$temp_decay)

# Finding range in normalized data
range <- max(flexibility_periods_full$temp_decay) - min(flexibility_periods_full$temp_decay)

Q3 <- quantile(flexibility_periods_full$temp_decay, 0.75) 
Q1 <- quantile(flexibility_periods_full$temp_decay, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin2 <- range/Bin_width
Freedman_Diaconic_bin2 <- round(Freedman_Diaconic_bin2,0)
print(Freedman_Diaconic_bin2) 

# Histogram of recorded temperature change in the data set for temperature decay
Plot_12 <- ggplot(EoH_data_cold_days, aes(x = temp_decay)) + 
  coord_cartesian(ylim= c(0,3000),
  xlim = c(-2.5,2.5))+
  geom_histogram(bins = Freedman_Diaconic_bin2, color = "#000000") +
  annotate("label", x = -1.5, y = 2000, label = "'Off' region", colour = "red", size = 3) +
  labs(x = "Temperature decay (degrees °C) per 2-minute interval" , y = "Frequency") +
  ggtitle("Histogram of temperature decay defining 'off' region") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  ) +  # Add time series line
  geom_rect(data = highlight_regions, aes(xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax, fill = fill), 
            alpha = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("pink")) +
  theme(legend.position="none")
Plot_12

# Filtering data set for load profiles where there is 60 minutes of 'on' and 90 minutes of 'off' 
# Calculates the length of each of the run in the On_Off_flag for continuous values   
rle_flag <- rle(EoH_data_cold_days$On_Off_flag) 

# First converting cold days data set to data table 
EoH_data_cold_days <- setDT(EoH_data_cold_days)
EoH_data_cold_days[, run_id := rleid(Property_ID,On_Off_flag)] 
EoH_data_cold_days[, run_length_mins := .N, by = .(Property_ID, run_id)] 
EoH_data_cold_days <- as.data.frame(EoH_data_cold_days) 

# Calculating the maximum and minimum internal and external temperatures per period of 'On' and 'Off'
EoH_data_cold_days <- EoH_data_cold_days %>%
  group_by(run_id) %>%
  mutate(max_temp_internal = max(Internal_Air_Temperature)) %>%
  mutate(min_temp_internal = min(Internal_Air_Temperature)) %>%
  mutate(min_temp_external = min(External_Air_Temperature)) %>%
  mutate(max_temp_external = max(External_Air_Temperature)) %>%
  ungroup

# Calculating the total temperature change in externally and internally per run
EoH_data_cold_days <- EoH_data_cold_days %>%
  group_by(run_id) %>%
  mutate(Internal_temp_range = max_temp_internal - min_temp_internal) %>%
  mutate(External_temp_range = max_temp_external - min_temp_external) %>%
  ungroup() 

# Make it into a decimal in case it formats weirdly 
EoH_data_cold_days$Internal_temp_range <- round(EoH_data_cold_days$Internal_temp_range , digits = 3)
EoH_data_cold_days$External_temp_range <- round(EoH_data_cold_days$Internal_temp_range , digits = 3)

# Calculating the average external temperature range in temperature for each 'On','Off' period
EoH_data_cold_days <- EoH_data_cold_days %>%
group_by(run_id) %>%
  mutate(average_outside_temperature = mean(External_Air_Temperature))

# Creating variables for the peak and standard deviation of consumption within  a given period
EoH_data_cold_days <- EoH_data_cold_days %>% 
  arrange(Property_ID, Year, Month, Monthday, Hour, Minute) %>%
  group_by(Property_ID, date) %>%
  mutate(Peak_power_output_kW = rollmax(Power_output_2mins_kW, 30, fill = NA, align = "right")) %>%
  mutate(Peak_heat_output_kW = rollmax(Interval_heat_output_kW, 30, fill = NA, align = "right")) 
  
  EoH_data_cold_days <- EoH_data_cold_days %>%
  group_by(run_id) %>%
  mutate(Sd_power_output_kW = sd(Power_output_2mins_kW)) %>%
  mutate(Sd_heat_output_kW = sd(Interval_heat_output_kW))

# Rounding these to an appropriate degree of accuracy 
EoH_data_cold_days$Peak_power_output_kW <- round(EoH_data_cold_days$Peak_power_output_kW, digits = 2)
EoH_data_cold_days$Peak_heat_output_kW <- round(EoH_data_cold_days$Peak_power_output_kW , digits = 2)
EoH_data_cold_days$Sd_power_output_kW <- round(EoH_data_cold_days$Sd_power_output_kW, digits = 2)
EoH_data_cold_days$Sd_heat_output_kW <- round(EoH_data_cold_days$Sd_heat_output_kW , digits = 2)

# Check class type of the variables
str(EoH_data_cold_days) # date is a factor here but doesn't affect anything so will leave as it is for now

# Filtering that data set for where heat pump is considered 'Off' to eliminate
# periods when the heat punp is on but the compressor is off 
filtered_data <- EoH_data_cold_days %>%
  filter(On_Off_flag == "On" | On_Off_flag == "Off" | On_Off_flag == "On - Compressor Off")

# Creating output path for file 
output_path_filtered <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/filtered_data.csv"

# Saving file 
fwrite(filtered_data, output_path_filtered) 

# Loading EoH cold data back into R to save time pre-processing 
filtered_data <- fread(output_path_filtered)

# Building in gc() at regular intervals to free-up disc space in R 
gc()

# Ensure that Hour_Minute is formatted as a time object 
filtered_data$Hour_Minute <- chron(times = filtered_data$Hour_Minute)

# Ensure that run_length reads as an integer, not as a character
filtered_data$run_length_mins <- as.numeric(filtered_data$run_length_mins)

# Ensure it's showing the actual time length in the data not just the number of 2 minute periods
filtered_data$run_length_mins <- filtered_data$run_length_mins*2

# Creating a column of the total consumption in each hour of a given run to be able to validate 
# the technical potential for demand response afterwards 
filtered_data <- filtered_data %>%
  group_by(run_id) %>%
  mutate(Total_run_consumption_kWh = sum(Interval_consumption_kWh)) %>%
  ungroup()

# Creating a column also for the average internal air temperature in each run 
filtered_data <- filtered_data %>%
  group_by(run_id) %>%
  mutate(Avg_internal_air_temperature = mean(Internal_Air_Temperature))

# Making sure that date is in proper format
filtered_data$date <- ymd(filtered_data$date)

# Creating averages for heat and power consumption based on the outputs an hour prior to turn off
# This is because heat pump turns off mid-hour and so averages do not reflect consumption values 
# an hour leading up to when the heat pump switches off.
filtered_data <- filtered_data %>% 
  arrange(Property_ID, Year, Month, Monthday, Hour, Minute) %>%
  group_by(Property_ID, date) %>%
  mutate(Average_consumption_kWh = rollmean(Interval_consumption_kWh, 30, fill = NA, align = "right")) %>%
  mutate(Average_power_output_kW = rollmean(Power_output_2mins_kW, 30, fill = NA, align = "right")) %>%
  mutate(Average_heat_output_kW = rollmean(Interval_heat_output_kW, 30, fill = NA, align = "right"))

# Rounding to an appropriate degree of accuracy (2.d.p)
filtered_data$Average_consumption_kWh <- round(filtered_data$Average_consumption_kWh, digits = 2)
filtered_data$Average_power_output_kW <- round(filtered_data$Average_power_output_kW , digits = 2)
filtered_data$Average_heat_output_kW <- round(filtered_data$Average_heat_output_kW, digits = 2)

# Finding the total consumption to the hour leading up to an off-period. This is to be able to validate
# the energy saved from demand response to see if the value calculated is accurate 
filtered_data <- filtered_data %>%
  arrange(Property_ID, Year, Month, Monthday, Hour, Minute) %>%
  group_by(Property_ID,date) %>%
  mutate(Total_hourly_consumption_kWh = rollsum(Interval_consumption_kWh, 30, fill = NA, align = "right")) %>%
  ungroup()

# Rounding also to 2.d.p
filtered_data$Total_hourly_consumption_kWh <- round(filtered_data$Total_hourly_consumption_kWh, digits = 2)

# Filtering data set for the assumptions below
# Assumption 3: Only keeping Off-periods greater than 90 minutes 
# Assumption 4: Keep periods that have temp range of 2C in data set (based on section 2.7.2 of Halloran, Lizana and McCulloch)             
# Assumption 5: and the internal temperature is greater than 18 degreesC as that is for now the threshold
# Assumption 6: Periods start after 17:00pm (focusing on evening peaks for now)
# Assumption 7: Period must have at least on period for 60 minutes. 
filtered_data <- filtered_data %>%
  filter(On_Off_flag == "On" & Hour_Minute >= "17:00:00" & run_length_mins >= 60 | 
           On_Off_flag == "On - Compressor Off" & Hour_Minute >= "17:00:00" | 
           On_Off_flag == "Off" & run_length_mins >= 90 & Hour_Minute >= "17:00:00" & Internal_Air_Temperature >= 18 & External_temp_range <= 2) 

# Issue with the above filter is that 'On' periods may not be necessarily on the same days as 'Off' periods or for the same heat pumps
# Creating a seperate data frame for 'On' periods
filtered_data_on <- filtered_data %>%
  filter(On_Off_flag == "On" & Hour_Minute >= "17:00:00" & run_length_mins >= 60 | On_Off_flag == "On - Compressor Off" & Hour_Minute >= "17:00:00") 

# Creating a seperate data frame for 'Off' periods
filtered_data_off <- filtered_data %>%
  filter(On_Off_flag == "Off" & run_length_mins >= 90 & Hour_Minute >= "17:00:00" & Internal_Air_Temperature >= 18 & External_temp_range <= 2)

# Filtering for when the dates in the 'On' an 'Off' periods match 
filtered_data_on <- filter(filtered_data_on, date %in% filtered_data_off$date & Property_ID %in% filtered_data_off$Property_ID)

# Filtering the main filtered data for dates where there are both 'On' and 'Off' periods for the same day
filtered_data <- filter(filtered_data, date %in% filtered_data_on$date & Property_ID %in% filtered_data_on$Property_ID)

# Finding the number of cold days of the filtered dataframe
n_distinct(filtered_data$date) # From sample there are only 3 periods with these patterns

#Finding out when those dates are 
unique(filtered_data$date) #these periods are 2021-12-22 2022-12-14 2022-12-15

#Finding out which day has the most activity on
HP_activity_table <- table(filtered_data$date)
HP_activity_table

# Finding max/min internal temperatures to for creating appropriate new axis 
max(filtered_data$Internal_Air_Temperature)
min(filtered_data$Internal_Air_Temperature)

# Min/max interval consumption
max(filtered_data$Interval_consumption_kWh)
min(filtered_data$Interval_consumption_kWh)

# Plotting temperature profiles for evening periods of temperature decay, showing by different heat pumps
Plot_13 <- ggplot(filtered_data, aes(x = Timestamp, y = Internal_Air_Temperature, colour = Property_ID))+
  xlim(as.POSIXct(c("2021-04-11 17:00:00","2021-04-12 00:00:00")))+
  coord_cartesian(ylim=c(15,25))+
  geom_line()+
  labs(x = "Time interval" ,
       y = "Temperature (degrees C)",
       title ="Internal air temperature variation on 2021-04-11 for different properties" )+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) +
theme(legend.position="none")
Plot_13

# Same plot as above but showing these by On-Off periods 
Plot_14 <- ggplot(filtered_data, aes(x = Timestamp, y = Internal_Air_Temperature))+
  xlim(as.POSIXct(c("2021-04-11 18:00:00","2021-04-12 00:00:00")))+
  coord_cartesian(ylim=c(15,25))+
  geom_point(aes(colour = On_Off_flag))+
  labs(x = "Time interval" ,
       y = "Temperature (degreesC)",
       title ="Internal air temperature variation on 2021-04-11 showing 'on'/'off' periods" )+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
#Plot_14 <- Plot_14 + guides(fill=guide_legend(title="Property ID")) 
Plot_14

# Plot showing power consumption
Plot_15 <- ggplot(filtered_data, aes(x = Timestamp, y = Power_output_2mins_kW, colour = On_Off_flag))+
  xlim(as.POSIXct(c("2021-01-30 17:00:00","2021-01-31 00:00:00")))+
  #ylim(0,0.2)+
  geom_point()+
  labs(x = "Time interval" ,
       y = "Power output per 2 minuter interval (kW) ",
       title ="Power output variation on 2021-01-30 showing 'on'/'off' " )+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
Plot_15 <- Plot_15 + guides(fill=guide_legend(title="Property ID")) 
Plot_15

## Date visualisation
#Lets see a summary of the data
summary(EoH_data_cold_days)

# Creating half hourly version of the filtered data set 
filtered_data_half_hourly <- filtered_data %>%
  mutate(Interval_30mins = ceiling_date(Timestamp, "30 minutes")) %>%
  group_by(Interval_30mins, Property_ID) %>%
  mutate(half_hourly_consumption = sum(Interval_consumption_kWh)) %>%
  ungroup()

#    mean_internal_temperature = mean(Internal_Air_Temperature, na.rm =TRUE),
#    mean_external_temperature = mean(External_Air_Temperature, na.rm =TRUE),
#    half_hourly_consumption = sum(Interval_consumption_kWh, na.rm = TRUE),
#    Property_ID = Property_ID,
#    date = date) %>%
#    group_by(Interval_30mins)

# Creating a column for the half-hourly interval in minutes for the column 
filtered_data_half_hourly$Interval_mins <- format(filtered_data_half_hourly$Interval_30mins, "%H:%M:%S")
filtered_data_half_hourly$Interval_mins <- chron(times = filtered_data_half_hourly$Interval_mins)  #Running chron to ensure it formats in chronological order

gc()
# Check format of newly created data set
str(filtered_data_half_hourly)

# Removing duplicate entries 
filtered_data_half_hourly <- unique(filtered_data_half_hourly)
filtered_data_half_hourly <- na.omit(filtered_data_half_hourly)

# Creating output path for file 
output_path_half_hourly <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/filtered_data_half_hourly.csv"

# Saving file 
fwrite(filtered_data_half_hourly, output_path_half_hourly) 

# Loading EoH cold data back into R to save time pre-processing 
output_path_half_hourly <- fread(output_path_half_hourly)

# Seeing where the most activity lies
table(filtered_data_half_hourly$date)

# View plot of consumption for one day 
Plot_16 <- ggplot(filtered_data_half_hourly, aes(x = Interval_30mins, y = half_hourly_consumption, )) +
  xlim(as.POSIXct(c("2021-12-02 17:00:00", "2021-12-03 00:00:00"))) +
  geom_point()+
  labs(title="Half hourly consumption profiles for heat pumps", x = "Interval", y="
Half hourly consumption (kWh)")+
theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  )  
Plot_16

# View of the new data set
view(EoH_data_half_hourly)  #Now have half hourly summary of data. 

# Distribution of lagged energy consumption of heat pump 
Plot_17 <- ggplot(filtered_data, aes(x = date, y= Power_output_2mins_kW)) +
  geom_boxplot(na.rm=TRUE) +
  #ylim(0,0.25) +
  labs(title="Distribution of lagged energy consumption by different heat pumps on whole days", x = "Heat pump ID ", y="
Energy consumed per 2 mins interval(kWh/2mins)")+
theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
Plot_17

# Finding relative frequency of "off-time" for each heat pump for suitable 
# Load profile analysis
On_off_proportion <- EoH_data_cold_days %>%
  group_by (Property_ID, On_Off_flag) %>%
  summarise (n = n()) %>%
  mutate (Proportion_On_time = (n / sum(n))*100) %>%
  filter(On_Off_flag == "On")

# view of the data set 
view(On_off_proportion)

# Bar chart of the proportion of off time of each heat pump to visualise this
Plot_18 <- ggplot(On_off_proportion, aes(x = Property_ID, y= Proportion_On_time)) +
  geom_bar(stat = "identity", na.rm=TRUE) +
  labs(title="Proportion of time heat pump switched on", x = "Property ID ", y="
Proportion of time switched on (%)")+
  geom_hline(yintercept = mean(On_off_proportion$Proportion_On_time), colour = "red", linetype = 'dotted', size = 3) +
   annotate("label", x = "EOH1281", y = 60, label = "Average proportion of time switched on", colour = "red", size = 5) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90, size = 6),
    axis.title = element_text(size = 14)
  ) 
Plot_18

# Showing proportion of time that different heat pumps are kept on
Plot_19 <- ggplot(filtered_data, aes(x = Property_ID, y= On_Off_flag , fill = On_Off_flag, colour = "black")) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Proportion of time heat pump switched on", x = "Property ID ", y="
Proportion of time switched on (%)")+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90, size = 6),
    axis.title = element_text(size = 14)
  ) 
Plot_19


#based on above data EOH0546 might have suitable load profiles
# for producing meaningful analysis

table(filtered_data$Property_ID)

# Creating a filtered data set where it seems heat pumps have been turned off for a while
EOH0745 <- filter(EoH_data_cold_days, Property_ID == 'EOH0745')
EOH0582 <- filter(EoH_data_cold_days, Property_ID == 'EOH0582')

table(EOH0745$date)

# Load profile for one of the heat pumps on one of the cold days
Plot_20 <- ggplot(EOH0745, aes(x = Timestamp, y= Internal_Air_Temperature)) +
  geom_line()+
  geom_point(aes(colour = On_Off_flag))+
  xlim(as.POSIXct(c("2020-12-27 17:00:00", "2020-12-28 00:00:00"))) +
  labs(x = "Time" , y = "Power output per 2-minute interval (kW)") +
  ggtitle("Load profile of Property ID EOH0745 on 2020-12-27") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"))+
theme(legend.position="none")
Plot_20

# Load profile for one of the heat pumps on one of the cold days
Plot_21 <- ggplot(EoH_data_cold_days, aes(x = Timestamp, y= Power_output_2mins_kW)) + 
  xlim(as.POSIXct(c("2020-12-03 00:00:00", "2020-12-04 00:00:00"))) +
  ylim(0,4)+
   geom_line()+
  labs(x = "Time period" , y = "Power output per 2-minute interval (kW)") +
  ggtitle("Load profile of heat pumps on 2020-12-03 showing 'On' & 'Off' periods") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
plot.caption = element_text(face = "italic"))+
theme(legend.position="none")
Plot_21

# Need to identify heat pumps that have been turned off for sufficiently long

# Plot of half hourly load profile for a day where all heat pumps were turned on
Plot_22 <- ggplot(filtered_data_half_hourly ,aes(x = Interval_30mins, y = half_hourly_consumption, show.legend = FALSE)) + 
  xlim(as.POSIXct(c("2022-01-07 00:00", "2022-01-08 00:00")))+
  #ylim(0,2.75)+
  geom_line() +
  labs(x = "Half-hourly interval" , y = "Whole system energy consumption(kWh)") +
  ggtitle("Variation in half-hourl energy consumption (kWh) on 29-11-2021") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15), 
    plot.caption = element_text(face = "italic"))+
theme(legend.position="none")
Plot_22

# Some homes in the data set have noticeably lower average internal 
# temperatures compared to others

# Visualising this
Plot_23<- ggplot(Internal_avg, aes(x = Property_ID, y= avg_int_temp)) +
  geom_bar(stat = "identity", na.rm=TRUE) +
  geom_hline(yintercept = mean(Internal_avg$avg_int_temp), colour = "red", linetype = 'dotted', size = 1) + 
  annotate("label", x = "EOH1000", y = 20.5, label = "Internval average threshold temperature (degrees °C)", colour = "red", size = 3) +
  labs(title="Average internal temperature of properties on cold days", x = "Heat pump ID ", y="
Average internal temperature (degrees °C)")+
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
    ) 
Plot_23

# QQplot of Internal temperature
qqnorm(EoH_data_cold_days$Internal_Air_Temperature)
qqline(EoH_data_cold_days$Internal_Air_Temperature)

# Internal temperature is not normally distributed 

# Joining average internal temperatures calculated to cold days data frame 
EoH_data_cold_days <- left_join(Internal_avg,EoH_data_cold_days, by = "Property_ID")

# Moving time stamp column before Property ID column
EoH_data_cold_days <- EoH_data_cold_days %>% relocate(Timestamp, .before = Property_ID)

# Filtering out heat pumps from the data set where the average internal temperature is less than 15C
#EoH_data_cold_days <- filter(EoH_data_cold_days, avg_int_temp > 14)

# Let's see new average internal temperature of properties 
Internal_average <- mean(EoH_data_cold_days$Internal_Air_Temperature)
Internal_average

# Converting date into factor in order to plot 
EoH_data_cold_days$date <- as.factor(EoH_data_cold_days$date)

# Creating a summarised data set in order to plot variation in average external air temperature
# overtime
EoH_temp_summary <- 
  subset(EoH_data_cold_days, select = c("date","avg_temp"))  %>%
  group_by(date) %>%
  summarise(avg_temp = mean(avg_temp))

# Boxplot of distribution of internal and external temperatures
Plot_24 <- ggplot(filtered_data) +
  geom_boxplot(aes(x = "Internal Air Temperature", y = Internal_Air_Temperature, fill = "red"), show.legend = FALSE) +
  geom_boxplot(aes(x = "External Air Temperature", y = External_Air_Temperature, fill = "lightblue"), show.legend = FALSE) +
  labs(x = "",y = "Temperature (degrees °C)") +
  ggtitle("Distribution of internal and external air temperatures") +
theme(plot.title = element_text(hjust = 0.5, size = 15), 
plot.caption = element_text(face = "italic")) +
theme(legend.position="none")
Plot_24

################################################################################################
## Non-core section of script 
## Visualising trend of energy consumption and temperature change for a single day
# and for a single heat pump

# Create a subset of the data for 2021-12-22 
filtered_data_one_day1 <- EOH0745 %>%
  filter(between(Timestamp, as.Date("2020-12-24 00:17:00"), as.Date("2020-12-25 00:00:00")))

# Filtering single day data but removing any unnecessary columns 
filtered_data_one_day1 <- subset(filtered_data_one_day1, select = c("Timestamp","Power_output_2mins_kW","Internal_Air_Temperature")) 

# Create a facet plot with both internal temperature and lagged consumption varying 
# for the same time period for the suitable day identified above
Plot_25 <- filtered_data_one_day1 %>% 
  pivot_longer(-Timestamp) %>%
  ggplot(aes(x = Timestamp, y = value)) +
  xlim(as.POSIXct(c("2020-12-24 17:00:00","2020-12-25 00:00:00")))+
  geom_line()+
  labs(x = "Hour",y = "") +
  ggtitle("Variation of internal air temperature with electrical power output (kW) ") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) +
  facet_wrap(~name, ncol = 1, scales = "free_y")
 # geom_smooth(se= FALSE)
Plot_25

## Additional (non-core script) - Plotting temperature rise/fall with 
# electricity consumption

#  maximum values for the lagged consumption
# to be able to plot with temperature properly.

scaled_filtered_data <- filtered_data_one_day1 %>% 
  mutate(Internal_Air_Temperature_tr = 
           (Internal_Air_Temperature - min(Internal_Air_Temperature))/ (max(Internal_Air_Temperature) - min(Internal_Air_Temperature)),
         Internal_Air_Temperature_min = min(Internal_Air_Temperature),
         Internal_Air_Temperature_max = max(Internal_Air_Temperature), 
         
         Power_output_2mins_kW_tr =
           (Power_output_2mins_kW - min(Power_output_2mins_kW))/ (max(Power_output_2mins_kW) - min(Power_output_2mins_kW)),
         Interval_consumption_min = min(Power_output_2mins_kW),
         Interval_consumption_max = max(Power_output_2mins_kW)
  )

# Finding max/min internal temperatures to for creating appropiate new axis 
max_int_temp <- max(scaled_filtered_data$Internal_Air_Temperature)
min_int_temp <- min(scaled_filtered_data$Internal_Air_Temperature)

# Min/max interval consumption
max_power <- max(scaled_filtered_data$Power_output_2mins_kW)
min_power <- min(scaled_filtered_data$Power_output_2mins_kW)


# Plot showing Internal air temperature and interval consumption for one of them 
Plot_26 <- scaled_filtered_data %>%
  ggplot(aes(x = Timestamp)) +
  xlim(as.POSIXct(c("2020-12-24 17:00:00","2020-12-25 00:00:00"))) +
  geom_line(aes(y = Internal_Air_Temperature_tr), colour = "red") +
  geom_line(aes(y = Power_output_2mins_kW_tr), colour = "blue") +
  
  scale_y_continuous(
    # Axis ticks and limits for normalized temperature
    labels = seq(15, 30, 0.5),
    breaks = ((seq(15, 30, 0.5) - min_int_temp) / (max_int_temp-min_int_temp)),
    limits = ((c(12, 40)-min_int_temp)/max_int_temp),
    name = "Internal air temperature (°C) ",
    
    # Add secondary axis for Interval Consumption
    sec.axis = sec_axis(
      trans = ~ .,  # identity transform
      breaks = (seq(0, max_power+3, 0.5) - min_power) / max_power- min_power,
      labels = seq(0, max_power+3, 0.5),
      name = "Electrical power output (kW)"
    )
  ) +
  labs(
    title = "Internal air temperature vs Electrical power output (kW)",
    x = "Timestamp"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 90)
  ) + 
  theme(axis.title.y.left = element_text(colour = "red"))+
  theme(axis.title.y.right = element_text(colour = "blue"))

# Show the plot
Plot_26
