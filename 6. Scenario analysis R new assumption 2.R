# Sensitivity Analysis for analysis section of dissertation
# Author - Sami Cheqrouni-Espinar 
# Date - 09/08/2025

# Description: Script looks at changing certain assumptions from original data pre-processing 

# Creating output path for file 
output_path_cold_days <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/EoH_data_cold_days.csv"

# Saving file 
fwrite(EoH_data_cold_days, output_path_cold_days) 

# Loading EoH cold data back into R to save time pre-processing 
EoH_data_cold_days <- fread(output_path_cold_days)

## Changes to assumptions  
# Filtering dataset for when there is a continuous period of 'On' or 'Off' for an hour. 
# New Assumption 2: Looking at days colder than 0C (freezing temperatures) 
# New Assumption 3: Keeping Off-periods greater than 30 minutes 
# New assumption 5.1: Internal temperature range between 21C - 18C 
# New Assumption 5.2: internal temperature is greater than 19 or 20 degreesC as that is for now the threshold
# New Assumption 6: Periods specifically between 16:00pm - 19:00 (focusing on evening peaks for now)
# New Assumption 7: Period must have at least 'On' period for 60 minutes. 

# Changing one assumption at a time 

#### New Assumption 2: Looking at days colder than 0C (freezing temperatures) 

# Creating output path for file 
output_path_filtered <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/filtered_data.csv"

# Saving file 
fwrite(filtered_data, output_path_filtered) 

# Loading EoH cold data back into R to save time pre-processing 
filtered_data <- fread(output_path_filtered)

# Building in gc() at regular intervals to free-up disc space in R 
gc()

# Ensure that Hour_Minute is formatted as a time variable 
filtered_data$Hour_Minute <- chron(times = filtered_data$Hour_Minute)

# Ensure that run_length reads as an integer, not as a character
filtered_data$run_length_mins <- as.numeric(filtered_data$run_length_mins)

# Ensure it's showing the time period in the data 
filtered_data$run_length_mins <- filtered_data$run_length_mins*2

# Creating a column of the total consumption in each hour of a given run to be able to validate 
# the technical demand response potential to afterwards 
filtered_data <- filtered_data %>%
  group_by(run_id) %>%
  mutate(total_run_consumption_kWh = sum(Interval_consumption_kWh)) %>%
  ungroup()

# Creating a column also for the average internal air temperature in each run 
filtered_data <- filtered_data %>%
  group_by(run_id) %>%
  mutate(avg_internal_air_temperature = mean(Internal_Air_Temperature))

# Making sure that date is in proper format
filtered_data$date <- ymd(filtered_data$date)

# Creating averages for heat and power consumption based on the outputs an hour prior to turn off
# This is because heat pump turns off mid-hour and so averages do not reflect consumption values 
# an hour leading up to when the heat pump switches off.
filtered_data <- filtered_data %>% 
  arrange(Property_ID, Year, Month, Monthday, Hour, Minute) %>%
  group_by(Property_ID, date) %>%
  mutate(average_consumption_kWh = rollmean(Interval_consumption_kWh, 30, fill = NA, align = "right")) %>%
  mutate(average_power_output_kW = rollmean(Power_output_2mins_kW, 30, fill = NA, align = "right")) %>%
  mutate(average_heat_output_kW = rollmean(Interval_heat_output_kW, 30, fill = NA, align = "right"))

# Rounding to an appropiate degree of accuracy (2.d.p)
filtered_data$average_consumption_kWh <- round(filtered_data$average_consumption_kWh, digits = 2)
filtered_data$average_power_output_kW <- round(filtered_data$average_power_output_kW , digits = 2)
filtered_data$average_heat_output_kW <- round(filtered_data$average_heat_output_kW, digits = 2)

# Finding the total consumption to the hour leading up to an off-period. This is to be able to validate
# the energy saved from demand response to see if the value calculated is accurate 
filtered_data <- filtered_data %>%
  arrange(Property_ID, Year, Month, Monthday, Hour, Minute) %>%
  group_by(Property_ID,date) %>%
  mutate(total_hourly_consumption_kWh = rollsum(Interval_consumption_kWh, 30, fill = NA, align = "right")) %>%
  ungroup()

# group_by(Year,Month,Monthday,day, - use if it doesn't work

# Rounding also to 2.d.p
filtered_data$total_hourly_consumption_kWh <- round(filtered_data$total_hourly_consumption_kWh, digits = 2)

## New assumptions 
# Filtering dataset for when there is a continuous period of 'On' or 'Off' for an hour. 
# Assumption 2: Looking at days below 0C  
# Assumption 3: Keeping Off-periods greater than 30 minutes 
# Assumption 4: Keep periods that have temp range of 2C in dataset (based on section 2.7.2 of Halloran, Lizana and McCulloch)             
# Assumption 5: and the internal temperature is greater than 18 degreesC as that is for now the threshold
# Assumption 6: Periods start after 17:00pm (focusing on evening peaks for now)
# Assumption 7: Period must have at least on period for 60 minutes. 
filtered_data_new2 <- filtered_data %>%
  filter(On_Off_flag == "On" & Hour_Minute >= "17:00:00" & run_length_mins >= 60 | 
           On_Off_flag == "On - Compressor Off" & Hour_Minute >= "17:00:00" | 
           On_Off_flag == "Off" & run_length_mins >= 30 & Hour_Minute >= "17:00:00" & Internal_Air_Temperature >= 18 & External_temp_range <= 0) 

# Issue with the above filter is that 'On' periods may not be necessarily on the same days as 'Off' periods or for the same heat pumps
# Creating a seperate data frame for 'On' periods
filtered_data_on_new2 <- filtered_data %>%
  filter(On_Off_flag == "On" & Hour_Minute >= "17:00:00" & run_length_mins >= 60 | On_Off_flag == "On - Compressor Off" & Hour_Minute >= "17:00:00") 

# Creating a seperate dataframe for 'Off' periods
filtered_data_off_new2 <- filtered_data %>%
  filter(On_Off_flag == "Off" & run_length_mins >= 30 & Hour_Minute >= "17:00:00" & Internal_Air_Temperature >= 18 & External_temp_range <= 0)

# Filtering for when the dates in the 'On' an 'Off' periods match 
filtered_data_on_new2 <- filter(filtered_data_on_new2, date %in% filtered_data_off_new2$date & Property_ID %in% filtered_data_off_new2$Property_ID)

# Importing EoH metadata again 
EoH_meta_data <- read.csv("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/Metadata/BEIS Electrification of Heat Project - Property, Design and Installation Information.csv")

#Saving file
#write.csv(EoH_meta_data,"~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/HPlist.csv", row.names = FALSE)

# Creating subset of the data for property IDs with air-source heat pumps in them
EoH_meta_data <- filter(EoH_meta_data,
                        HP_Recommend == c("ASHP","HT_ASHP"))

# Joining metadata to filtered_data to create new variable 
filtered_data_new2 <- left_join(filtered_data_new2,EoH_meta_data, by = "Property_ID")

# Ensuring HP size is formatted as a number 
filtered_data_new2$HP_Size_kW <- as.numeric(filtered_data_new2$HP_Size_kW)

# Calculating the average hourly consumption across a given period of on or off
filtered_data_new2 <- filtered_data_new2 %>%
  mutate(average_hourly_consumption_kWh = (total_run_consumption_kWh/ (run_length_mins))*60)

# Filter for just on and off periods only, no 'On - Compressor Off' periods 
filtered_data_new2 <- filtered_data_new2 %>%
  filter(On_Off_flag == 'On' | On_Off_flag == 'Off')

# Also creating a variable of the fraction that heat pump is working relative to it's rated size for the hour 
#leading up to the 'Off' period
filtered_data_new2 <- filtered_data_new2 %>% 
  mutate(percent_work_output = (average_heat_output_kW/HP_Size_kW)*100)

# Rounding variable created to 2.d.p
filtered_data_new2$percent_work_output <- round(filtered_data_new2$percent_work_output, digits = 1)

# Finding the average power output per run to be able to calculate the technical potential for demand response 
filtered_data_new2 <- filtered_data_new2 %>%
  group_by(Property_ID, run_id) %>%
  mutate(average_power_output_per_run_kW  = median(Power_output_2mins_kW))

# Removing duplicates from the dataset
filtered_data_new2 <- unique(filtered_data_new2) 

# Creating an numeric variable for where 'On' or 'Off' takes on binary
# value of either 1 or 0 
filtered_data_new2 <- filtered_data_new2 %>%
  arrange(Property_ID, Timestamp) %>%
  mutate(On_Off_flag_num = ifelse(On_Off_flag == "Off",0,1)) %>%
  ungroup()

# Formatting date as date
filtered_data_new2$date <- ymd(filtered_data_new2$date)

# Ensure the above variable is formatted as a number 
filtered_data_new2$On_Off_flag_num <- as.numeric(filtered_data_new2$On_Off_flag_num)

# Consecutive variable used to identify where for the same heat pump and on the same date 
# the heat pump is turning from 'On' to 'Off'
filtered_data_new2 <- filtered_data_new2 %>%
  group_by(Property_ID, date) %>%
  mutate(consecutive = shift(On_Off_flag_num, 1, type = "lag", fill = NA)) %>%
  mutate(consecutive_new = On_Off_flag_num - consecutive) %>%
  ungroup()

# Get rid of null values here 
filtered_data_new2 <- na.omit(filtered_data_new2)

# Moving columns before meta-data 
filtered_data_new2 <- filtered_data_new2  %>%  
  relocate(average_hourly_consumption_kWh, .before = Delivery_Contractor) %>%
  relocate(percent_work_output, .before = Delivery_Contractor) %>%
  relocate(average_power_output_per_run_kW, .before = Delivery_Contractor) %>%
  relocate(On_Off_flag, .before = Delivery_Contractor) %>%
  relocate(On_Off_flag_num, .before = Delivery_Contractor)%>%
  relocate(consecutive, .before = Delivery_Contractor) %>%
  relocate(consecutive_new, .before = Delivery_Contractor)

# Getting rid of NA's introduce
# filtered_data_new2 <- na.omit(filtered_data_new2)

#Fix consecutive column so that it shows lag between each row rather than
# A duplicate of the On_Off_flag_num
#filtered_data_new2$consecutive <- filtered_data_new2$consecutive - lag(filtered_data_new2$consecutive, default = 0)

# Creating a variable here for the average output for the 60 minutes previous as lag function not seeming to work 
# on it's own
filtered_data_new2 <- filtered_data_new2 %>%
  mutate(average_power_output_kW_lag = shift(average_power_output_kW, 1, type = "lag", fill = NA))

# Creating a column for calculating the technical potential for demand response 
# and the estimate energy savings as a result 
# Demand response potential = consumption if the heat pump were continuing running - power consumption in off periods 
filtered_data_new2 <- filtered_data_new2 %>%
  mutate(Technical_potential_demand_response_kW = average_power_output_kW_lag - average_power_output_per_run_kW) %>%
  mutate(Shiftable_potential_energy_kWh = (run_length_mins * (Technical_potential_demand_response_kW))/(30*2))

# Rounding variables to an appropriate degree of accuracy (2.d.p)
filtered_data_new2$Technical_potential_demand_response_kW <- round(filtered_data_new2$Technical_potential_demand_response_kW, digits = 2)
filtered_data_new2$Shiftable_potential_energy_kWh <- round(filtered_data_new2$Shiftable_potential_energy_kWh, digits = 2)

# Relocating variables so they are before meta data 
filtered_data_new2 <- filtered_data_new2  %>%  
  relocate(Technical_potential_demand_response_kW, .before = Delivery_Contractor) %>%
  relocate(Shiftable_potential_energy_kWh, .before = Delivery_Contractor)

# Finding calculating the saved enery in hourly and half hourly periods  
filtered_data_new2 <- filtered_data_new2 %>%
  mutate(hourly_flexibility_potential_kWh = (Shiftable_potential_energy_kWh / (run_length_mins))*60) %>%
  mutate(half_hourly_flexibility_potential_kWh = hourly_flexibility_potential_kWh/2)

# Rounding to an appropiate degree of accuracy and moving before meta data variables
filtered_data_new2$hourly_flexibility_potential_kWh <- round(filtered_data_new2$hourly_flexibility_potential_kWh, digits = 2)
filtered_data_new2$half_hourly_flexibility_potential_kWh <- round(filtered_data_new2$half_hourly_flexibility_potential_kWh, digits = 2)

# Relocating variables so they are before meta data 
filtered_data_new2 <- filtered_data_new2  %>%  
  relocate(hourly_flexibility_potential_kWh, .before = Delivery_Contractor) %>%
  relocate(half_hourly_flexibility_potential_kWh, .before = Delivery_Contractor)

### For the full set of results 
# Remove any nulls from the dataet 
filtered_data_new2 <- na.omit(filtered_data_new2)

# Creating a variable of the hourly consumption from the period previous forward so 
# that accuracy of the demand response can be calculated relative to it 
filtered_data_new2 <- filtered_data_new2 %>%
  mutate(total_hourly_consumption_kWh_lag = shift(total_hourly_consumption_kWh, 1, type = "lag", fill = NA))

# Move accuracy and error columns so that they are in view 
filtered_data_new2 <- filtered_data_new2  %>%  
  relocate(total_hourly_consumption_kWh_lag, .before = Delivery_Contractor)

# Ensuring these are all numeric class types
filtered_data_new2$hourly_flexibility_potential_kWh <- as.numeric(filtered_data_new2$hourly_flexibility_potential_kWh)
filtered_data_new2$total_hourly_consumption_kWh <- as.numeric(filtered_data_new2$total_hourly_consumption_kWh)
filtered_data_new2$total_hourly_consumption_kWh_lag <- as.numeric(filtered_data_new2$total_hourly_consumption_kWh_lag)

table(filtered_data_new2$date)

# Creating a variable for the error rate in results 
filtered_data_new2 <- filtered_data_new2 %>%
  arrange(Property_ID,date) %>%
  group_by(Property_ID,date) %>%
  mutate(error_rate = ((abs(hourly_flexibility_potential_kWh - (total_hourly_consumption_kWh_lag)))/(total_hourly_consumption_kWh_lag))*100) %>%
  mutate(accuracy = 100 - error_rate) %>%
  ungroup()

# Rounding percentage and error rate to 2.d.p
filtered_data_new2$error_rate <- round(filtered_data_new2$error_rate, digits = 1)
filtered_data_new2$accuracy <- round(filtered_data_new2$accuracy, digits = 1)

# Move accuracy and error columns so that 
filtered_data_new2 <- filtered_data_new2  %>%  
  relocate(accuracy, .before = Interval_consumption_kWh) %>%
  relocate(error_rate, .before = Interval_consumption_kWh)

# Move On/off period before accuracy for explanation of error 
filtered_data_new2 <- filtered_data_new2  %>%  
  relocate(On_Off_flag, .before = accuracy)

# Shifting variables forward from 'Off' period so that they can be eventually analysed during correlation analysis 
filtered_data_new2 <- filtered_data_new2 %>%
  mutate(percent_work_output = shift(percent_work_output, 1, type = "lead", fill = NA)) %>%
  mutate(average_power_output_kW = shift(average_power_output_kW, 1, type = "lead", fill = NA)) %>%
  mutate(average_heat_output_kW = shift(average_heat_output_kW,1, type = "lead", fill = NA)) %>%
  mutate(average_hourly_consumption_kWh = shift(average_hourly_consumption_kWh, type = "lead", fill = NA)) %>%
  mutate(total_hourly_consumption_kWh = shift(total_hourly_consumption_kWh, type = "lead", fill = NA)) %>%
  mutate(total_run_consumption_kWh = shift(total_run_consumption_kWh, 1, type = "lead", fill = NA))

# Creating a variable for the error rate in results - Old
#post_processing_full <- filtered_data_new2 %>%
#  arrange(Property_ID,date,day,Hour) %>%
#  group_by(Property_ID,date,day) %>%
#  mutate(error_rate = 
#           ((abs(hourly_flexibility_potential_kWh - lag(average_hourly_consumption_kWh)))/lag(average_hourly_consumption_kWh))*100) %>%
#  mutate(accuracy = 100 - error_rate) %>%
#  ungroup()

# Filtering for consecutive On-off periods
post_processing_full_new2 <- filtered_data_new2 %>%
  filter(consecutive_new == 1 | consecutive_new == -1)

# Creating a dataframe for flexibility periods within the dataset
flexibility_periods_new2 <- post_processing_full_new2 %>%
  filter(On_Off_flag == "Off")

# Converting run_id into a factor to be able to plot  
flexibility_periods_new2$run_id <- as.factor(flexibility_periods_new2$run_id)

# Filtering for just Off periods to be able to look at accuracy 
flexibility_periods_full_new2 <- flexibility_periods_new2 %>%
  filter(On_Off_flag == "Off")

# Demand response for each period 
Plot_72 <- ggplot(flexibility_periods_full_new2, aes(x = run_id , y = Technical_potential_demand_response_kW)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full_new2$Technical_potential_demand_response_kW), colour = "red", linetype = 'dotted', size = 1) +
  annotate("label", x = 300, y = mean(flexibility_periods_full_new2$Technical_potential_demand_response_kW) + 0.3, label = "Mean technical potential for demand response", colour = "red", size = 3) +
  labs(x = "'Off' period" ,
       y = "Technical potential for demand response  (kW) ",
       title ="Periods with technical potential for demand response that meet assumptions") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90, size = 1)
  ) 
Plot_72 <- Plot_72 + guides(fill=guide_legend(title="Property ID")) 
Plot_72

# Move accuracy and error columns so that 
flexibility_periods_full_new2 <- flexibility_periods_full_new2  %>%  
  relocate(accuracy, .before = Interval_consumption_kWh) %>%
  relocate(error_rate, .before = Interval_consumption_kWh)

# Getting rid of Null values
flexibility_periods_full_new2 <- na.omit(flexibility_periods_full_new2)

# Changing names of columns in dataset for clarity in correlation analysis
flexibility_periods_full_new2 <- flexibility_periods_full_new2 %>%
  rename(Average_daily_temperature = avg_temp) %>%
  rename(Internal_temperature_decay = Internal_temp_range) %>%
  rename(Average_outside_temperature = average_outside_temperature) %>%
  rename(Technical_potential_demand_response_kW = Technical_potential_demand_response_kW) %>%
  rename(Total_Floor_Area_m2 = Total_Floor_Area) %>%
  rename(Average_internal_air_temperature = avg_internal_air_temperature) %>%
  rename(Measured_accuracy_percent = accuracy) %>%
  rename(Work_capacity_percent = percent_work_output) %>%
  rename(Period_length_minutes = run_length_mins) 

# Changing spelling on some of columns
flexibility_periods_full_new2 <- flexibility_periods_full_new2 %>% 
  rename(Average_power_output_kW = average_power_output_kW) %>%
  rename(Average_heat_output_kW = average_heat_output_kW) %>%
  rename(Total_run_consumption_kWh = total_run_consumption_kWh) %>%
  rename(Shiftable_potential_energy_kWh = Shiftable_potential_energy_kWh)

# Removing period two periods from data which have been assumed
# to be outliers
flexibility_periods_full_new2 <- flexibility_periods_full_new2 %>%
  filter(Technical_potential_demand_response_kW >= 0)

# Summary statistics about the accuracy of calculated results
median(flexibility_periods_full_new2$Measured_accuracy_percent) # median accuracy of results i 98.3%
mean(flexibility_periods_full_new2$Measured_accuracy_percent) # mean accuract is 95%
summary(flexibility_periods_full_new2$Measured_accuracy_percent)

str(post_processing_full_new2)

# Converting run ID into factor to be able to plot 
post_processing_full_new2$run_id <- as.factor(post_processing_full_new2$run_id)
flexibility_periods_full_new2$run_id <- as.factor(flexibility_periods_full_new2$run_id)

# Visualising the accuracy
Plot_73 <- ggplot(flexibility_periods_full_new2, aes(x = run_id , y = Measured_accuracy_percent)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full_new2$Measured_accuracy_percent), colour = "red", linetype = 'dotted', size = 1) +
  annotate("label", x = 300, y = mean(flexibility_periods_full_new2$Measured_accuracy_percent) + 6, label = "Average accuracy of calculated period", colour = "red", size = 3) +
  labs(x = "run ID" ,
       y = "Accuracy (%) ",
       title ="Accuracy of calculated technical potential for demand response ") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
theme(legend.position="none")
Plot_73

## Answers to research question 

# Calculating new summary statistics of dataset 
summary(flexibility_periods_full_new2$Technical_potential_demand_response_kW)
summary(flexibility_periods_full_new2$Shiftable_potential_energy_kWh)
summary(flexibility_periods_full_new2$Period_length_minutes)
summary(flexibility_periods_full_new2$Internal_temperature_decay)
summary(flexibility_periods_full_new2$Work_capacity_percent)
summary(flexibility_periods_full_new2$Measured_accuracy_percent)


# How much flexibility
mean(flexibility_periods_full_new2$Technical_potential_demand_response_kW) # 2.06kW  per period
median(flexibility_periods_full_new2$half_hourly_flexibility_potential_kWh) # 0.97 kW per half hour of flexibility
summary(flexibility_periods_full_new2$Shiftable_potential_energy_kWh) # 5.958 kWh of technical potential per event  
sd(flexibility_periods_full_new2$Shiftable_potential_energy_kWh) # 3.79 (very large)

# Summary statistics of results 
summary(flexibility_periods_full_new2$Shiftable_potential_energy_kWh)
sd(flexibility_periods_full_new2$Shiftable_potential_energy_kWh)

# Calculating optimal number of bins for plotting histogram

# Finding value of n for size of data
n1 <- n_distinct(flexibility_periods_full_new2)
print(n1)  #610 data size

# Freedman-Diaconis rule 
summary(flexibility_periods_full_new2$Technical_potential_demand_response_kW)

# Finding range in normalized data
range <- max(flexibility_periods_full_new2$Technical_potential_demand_response_kW) - min(flexibility_periods_full_new2$Technical_potential_demand_response_kW)

Q3 <- quantile(flexibility_periods_full_new2$Technical_potential_demand_response_kW, 0.75) 
Q1 <- quantile(flexibility_periods_full_new2$Technical_potential_demand_response_kW, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n1^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin3 <- range/Bin_width
Freedman_Diaconic_bin3 <- round(Freedman_Diaconic_bin3,0)
print(Freedman_Diaconic_bin3) 

# Histogram of demand response potential 
Plot_74 <- ggplot(flexibility_periods_full_new2, aes(x = Technical_potential_demand_response_kW)) +
  geom_histogram(bins = Freedman_Diaconic_bin3, color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full_new2$Technical_potential_demand_response_kW), colour = "orange", linetype = 'dotted', size = 1)+
  #annotate("label", x =  median(flexibility_periods_full_new2$Technical_potential_demand_response_kW) , y = 60, label = "Median demand response per period (kW)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full_new2$Technical_potential_demand_response_kW), colour = "red", linetype = 'dotted', size = 1)+
  #annotate("label", x =  mean(flexibility_periods_full_new2$Technical_potential_demand_response_kW) , y = 50, label = "Mean demand response per period (kW)", colour = "red", size = 3) +
  labs(x = "Technical potential for demand response (kW)" ,
       y = "Frequency",
       title ="Histogram of technical potential for demand response") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_74 <- Plot_74 + guides(fill=guide_legend(title="Property ID")) 
Plot_74

# Boxplot of demand response potential 
Plot_75 <- ggplot(flexibility_periods_full_new2, aes(y = Technical_potential_demand_response_kW)) +
  geom_boxplot(color = "#000000", fill = "#FFFFFF") +
  labs(x = "" ,
       y = "Technical potential for demand response (kW)",
       title ="Boxplot of technical potential for demand response") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_75 <- Plot_75 + guides(fill=guide_legend(title="Property ID")) 
Plot_75

Panel_plot1 <- Plot_74 + Plot_75
Panel_plot1

# Length of flexibility periods 
summary(flexibility_periods_full_new2$Heat_free_hours) # 1.167 hours of flexibility per heat pump
sd(flexibility_periods_full_new2$Heat_free_hours)

# Bar graph of seeing the average run length of each flexibility period
Plot_76 <- ggplot(flexibility_periods_full_new2, aes(x = run_id , y = (Heat_free_hours))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full_new2$Heat_free_hours), colour = "red", linetype = 'dotted', size = 1)+
  annotate("label", x = 300, y = mean(flexibility_periods_full_new2$Heat_free_hours) + .7 , label = "Average length of period", colour = "red", size = 3) +
  labs(x = "Period" ,
       y = "Length of time of period (hours)",
       title ="Length of time of periods with technical potential for demand response") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    #axis.text.x = element_text(angle =90)
  ) 
Plot_76 <- Plot_76 + guides(fill=guide_legend(title="Property ID")) 
Plot_76

# Bar graph of seeing the saved energy of each period with demand response potential
Plot_77 <- ggplot(flexibility_periods_full_new2, aes(x = run_id , y = Shiftable_potential_energy_kWh)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full_new2$Shiftable_potential_energy_kWh), colour = "red", linetype = 'dotted', size = 1)+
  annotate("label", x = 300, y = mean(flexibility_periods_full_new2$Shiftable_potential_energy_kWh) + .7 , label = "Average saved energy per period", colour = "red", size = 3) +
  labs(x = "Period" ,
       y = "Saved potential energy (kWh)",
       title ="Saved potential energy use from periods with demand response potential") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
Plot_77 <- Plot_77 + guides(fill=guide_legend(title="Property ID")) 
Plot_77

# Freedman-Diaconis rule 
summary(flexibility_periods_full_new2$Heat_free_hours)

# Finding range in normalized data
range <- max(flexibility_periods_full_new2$Heat_free_hours) - min(flexibility_periods_full_new2$Heat_free_hours)

Q3 <- quantile(flexibility_periods_full_new2$Heat_free_hours, 0.75) 
Q1 <- quantile(flexibility_periods_full_new2$Heat_free_hours, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n1^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin4 <- range/Bin_width
Freedman_Diaconic_bin4 <- round(Freedman_Diaconic_bin4,0)
print(Freedman_Diaconic_bin4) 

# Histogram of length of times
Plot_78 <- ggplot(flexibility_periods_full_new2, aes(x = (Heat_free_hours))) +
  geom_histogram(bins = Freedman_Diaconic_bin4, color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full_new2$Heat_free_hours), colour = "orange", linetype = 'dotted', size = 1) +
  #annotate("label", x =  median(flexibility_periods_full_new2$Technical_potential_demand_response_kW) , y = 60, label = "Median length of time (hours)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full_new2$Heat_free_hours), colour = "red", linetype = 'dotted', size = 1) +
  #annotate("label", x =  mean(flexibility_periods_full_new2$Technical_potential_demand_response_kW) , y = 50, label = "Mean length of time (hours)", colour = "red", size = 3) +
  labs(x = "Length of time of periods (hours)" ,
       y = "Frequency",
       title ="Histogram of length of time of periods with demand response potential") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  ) 
Plot_78 <- Plot_78 + guides(fill=guide_legend(title="Property ID")) 
Plot_78

# Summary of internal temperature decay 
summary(flexibility_periods_full_new2$Internal_temperature_decay)
sd(flexibility_periods_full_new2$Internal_temperature_decay)

# Freedman-Diaconis rule 
summary(flexibility_periods_full_new2$Internal_temperature_decay)

# Finding range in normalized data
range <- max(flexibility_periods_full_new2$Internal_temperature_decay) - min(flexibility_periods_full_new2$Internal_temperature_decay)

Q3 <- quantile(flexibility_periods_full_new2$Internal_temperature_decay, 0.75) 
Q1 <- quantile(flexibility_periods_full_new2$Internal_temperature_decay, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n1^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin5 <- range/Bin_width
Freedman_Diaconic_bin5 <- round(Freedman_Diaconic_bin5,0)
print(Freedman_Diaconic_bin5) 

# Distribution of internal temperature range
Plot_79 <- ggplot(flexibility_periods_full_new2, aes(x = (Internal_temperature_decay))) +
  geom_histogram(bins = Freedman_Diaconic_bin5, color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full_new2$Internal_temperature_decay), colour = "orange", linetype = 'dotted', size = 1)+
  #annotate("label", x =  median(flexibility_periods_full_new2$Technical_potential_demand_response_kW) , y = 60, label = "Median length of time (hours)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full_new2$Internal_temperature_decay), colour = "red", linetype = 'dotted', size = 1)+
  #annotate("label", x =  mean(flexibility_periods_full_new2$Technical_potential_demand_response_kW) , y = 50, label = "Mean length of time (hours)", colour = "red", size = 3) +
  labs(x = "Internal temperature decay (degrees °C) " ,
       y = "Frequency",
       title ="Internal temperature decay during periods with technical potential for demand response") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_79 <- Plot_79 + guides(fill=guide_legend(title="Property ID")) 
Plot_79

