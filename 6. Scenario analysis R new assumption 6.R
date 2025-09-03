# Sensitivity Analysis for analysis section of dissertation
# Author - Sami Cheqrouni-Espinar 
# Date - 09/08/2025
# Description of new script: Implementing new assumption 6


## New assumptions 
# Filtering dataset for when there is a continuous period of 'On' or 'Off' for an hour. 
# Assumption 3: Keeping Off-periods greater than 30 minutes 
# Assumption 4: Keep periods that have temp range of 2C in dataset (based on section 2.7.2 of Halloran, Lizana and McCulloch)             
# Assumption 5: Internal temperature is greater than 18 degreesC 
# New assumption 6: Periods between 16:00pm - 19:00pm 
# Assumption 7: Period must have at least on period for 60 minutes. 
filtered_data_new6 <- filtered_data %>%
  filter(On_Off_flag == "On" & Hour_Minute >= "16:00:00" & Hour_Minute <= "19:00:00" & run_length_mins >= 60 | 
           On_Off_flag == "On - Compressor Off" & Hour_Minute >= "16:00:00" & Hour_Minute <= "19:00:00" | 
           On_Off_flag == "Off" & run_length_mins >= 90 & Hour_Minute >= "16:00:00" & Hour_Minute <= "19:00:00" & Internal_Air_Temperature >= 18 & External_temp_range <= 2) 

# Issue with the above filter is that 'On' periods may not be necessarily on the same days as 'Off' periods or for the same heat pumps
# Creating a seperate data frame for 'On' periods
filtered_data_on_new6 <- filtered_data %>%
  filter(On_Off_flag == "On" & Hour_Minute >= "16:00:00" & Hour_Minute <= "19:00:00" & run_length_mins >= 60 | On_Off_flag == "On - Compressor Off" & Hour_Minute >= "16:00:00" & Hour_Minute <= "19:00:00") 

# Creating a seperate dataframe for 'Off' periods
filtered_data_off_new6 <- filtered_data %>%
  filter(On_Off_flag == "Off" & run_length_mins >= 90 & Hour_Minute >= "16:00:00" & Hour_Minute <= "19:00:00" & Internal_Air_Temperature >= 18 & External_temp_range <= 2)

# Filtering for when the dates in the 'On' an 'Off' periods match 
filtered_data_on_new6 <- filter(filtered_data_on_new6, date %in% filtered_data_off_new6$date & Property_ID %in% filtered_data_off_new6$Property_ID)

# Importing EoH metadata again 
EoH_meta_data <- read.csv("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/Metadata/BEIS Electrification of Heat Project - Property, Design and Installation Information.csv")

#Saving file
#write.csv(EoH_meta_data,"~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/General/HPlist.csv", row.names = FALSE)

# Creating subset of the data for property IDs with air-source heat pumps in them
EoH_meta_data <- filter(EoH_meta_data,
                        HP_Recommend == c("ASHP","HT_ASHP"))

# Joining metadata to filtered_data to create new variable 
filtered_data_new6 <- left_join(filtered_data_new6,EoH_meta_data, by = "Property_ID")

# Ensuring HP size is formatted as a number 
filtered_data_new6$HP_Size_kW <- as.numeric(filtered_data_new6$HP_Size_kW)

# Calculating the average hourly consumption across a given period of on or off
filtered_data_new6 <- filtered_data_new6 %>%
  mutate(average_hourly_consumption_kWh = (total_run_consumption_kWh/ (run_length_mins))*60)

# Filter for just on and off periods only, no 'On - Compressor Off' periods 
filtered_data_new6 <- filtered_data_new6 %>%
  filter(On_Off_flag == 'On' | On_Off_flag == 'Off')

# Also creating a variable of the fraction that heat pump is working relative to it's rated size for the hour 
#leading up to the 'Off' period
filtered_data_new6 <- filtered_data_new6 %>% 
  mutate(percent_work_output = (average_heat_output_kW/HP_Size_kW)*100)

# Rounding variable created to 2.d.p
filtered_data_new6$percent_work_output <- round(filtered_data_new6$percent_work_output, digits = 1)

# Finding the average power output per run to be able to calculate the technical potential for demand response 
filtered_data_new6 <- filtered_data_new6 %>%
  group_by(Property_ID, run_id) %>%
  mutate(average_power_output_per_run_kW  = median(Power_output_2mins_kW))

# Removing duplicates from the dataset
filtered_data_new6 <- unique(filtered_data_new6) 

# Creating an numeric variable for where 'On' or 'Off' takes on binary
# value of either 1 or 0 
filtered_data_new6 <- filtered_data_new6 %>%
  arrange(Property_ID, Timestamp) %>%
  mutate(On_Off_flag_num = ifelse(On_Off_flag == "Off",0,1)) %>%
  ungroup()

# Formatting date as date
filtered_data_new6$date <- ymd(filtered_data_new6$date)

# Ensure the above variable is formatted as a number 
filtered_data_new6$On_Off_flag_num <- as.numeric(filtered_data_new6$On_Off_flag_num)

# Consecutive variable used to identify where for the same heat pump and on the same date 
# the heat pump is turning from 'On' to 'Off'
filtered_data_new6 <- filtered_data_new6 %>%
  group_by(Property_ID, date) %>%
  mutate(consecutive = shift(On_Off_flag_num, 1, type = "lag", fill = NA)) %>%
  mutate(consecutive_new = On_Off_flag_num - consecutive) %>%
  ungroup()

# Get rid of null values here 
filtered_data_new6 <- na.omit(filtered_data_new6)

# Moving columns before meta-data 
filtered_data_new6 <- filtered_data_new6  %>%  
  relocate(average_hourly_consumption_kWh, .before = Delivery_Contractor) %>%
  relocate(percent_work_output, .before = Delivery_Contractor) %>%
  relocate(average_power_output_per_run_kW, .before = Delivery_Contractor) %>%
  relocate(On_Off_flag, .before = Delivery_Contractor) %>%
  relocate(On_Off_flag_num, .before = Delivery_Contractor)%>%
  relocate(consecutive, .before = Delivery_Contractor) %>%
  relocate(consecutive_new, .before = Delivery_Contractor)

# Getting rid of NA's introduce
# filtered_data_new6 <- na.omit(filtered_data_new6)

#Fix consecutive column so that it shows lag between each row rather than
# A duplicate of the On_Off_flag_num
#filtered_data_new6$consecutive <- filtered_data_new6$consecutive - lag(filtered_data_new6$consecutive, default = 0)

# Creating a variable here for the average output for the 60 minutes previous as lag function not seeming to work 
# on it's own
filtered_data_new6 <- filtered_data_new6 %>%
  mutate(average_power_output_kW_lag = shift(average_power_output_kW, 1, type = "lag", fill = NA))

# Creating a column for calculating the technical potential for demand response 
# and the estimate energy savings as a result 
# Demand response potential = consumption if the heat pump were continuing running - power consumption in off periods 
filtered_data_new6 <- filtered_data_new6 %>%
  mutate(Technical_potential_demand_response_kW = average_power_output_kW_lag - average_power_output_per_run_kW) %>%
  mutate(Shiftable_energy_potential_kWh = (run_length_mins * (Technical_potential_demand_response_kW))/(30*2))

# Rounding variables to an appropriate degree of accuracy (2.d.p)
filtered_data_new6$Technical_potential_demand_response_kW <- round(filtered_data_new6$Technical_potential_demand_response_kW, digits = 2)
filtered_data_new6$Shiftable_energy_potential_kWh <- round(filtered_data_new6$Shiftable_energy_potential_kWh, digits = 2)

# Relocating variables so they are before meta data 
filtered_data_new6 <- filtered_data_new6  %>%  
  relocate(Technical_potential_demand_response_kW, .before = Delivery_Contractor) %>%
  relocate(Shiftable_energy_potential_kWh, .before = Delivery_Contractor)

# Finding calculating the saved enery in hourly and half hourly periods  
filtered_data_new6 <- filtered_data_new6 %>%
  mutate(hourly_flexibility_potential_kWh = (Shiftable_energy_potential_kWh / (run_length_mins))*60) %>%
  mutate(half_hourly_flexibility_potential_kWh = hourly_flexibility_potential_kWh/2)

# Rounding to an appropiate degree of accuracy and moving before meta data variables
filtered_data_new6$hourly_flexibility_potential_kWh <- round(filtered_data_new6$hourly_flexibility_potential_kWh, digits = 2)
filtered_data_new6$half_hourly_flexibility_potential_kWh <- round(filtered_data_new6$half_hourly_flexibility_potential_kWh, digits = 2)

# Relocating variables so they are before meta data 
filtered_data_new6 <- filtered_data_new6  %>%  
  relocate(hourly_flexibility_potential_kWh, .before = Delivery_Contractor) %>%
  relocate(half_hourly_flexibility_potential_kWh, .before = Delivery_Contractor)

### For the full set of results 
# Remove any nulls from the dataet 
filtered_data_new6 <- na.omit(filtered_data_new6)

# Creating a variable of the hourly consumption from the period previous forward so 
# that accuracy of the demand response can be calculated relative to it 
filtered_data_new6 <- filtered_data_new6 %>%
  mutate(total_hourly_consumption_kWh_lag = shift(total_hourly_consumption_kWh, 1, type = "lag", fill = NA))

# Move accuracy and error columns so that they are in view 
filtered_data_new6 <- filtered_data_new6  %>%  
  relocate(total_hourly_consumption_kWh_lag, .before = Delivery_Contractor)

# Ensuring these are all numeric class types
filtered_data_new6$hourly_flexibility_potential_kWh <- as.numeric(filtered_data_new6$hourly_flexibility_potential_kWh)
filtered_data_new6$total_hourly_consumption_kWh <- as.numeric(filtered_data_new6$total_hourly_consumption_kWh)
filtered_data_new6$total_hourly_consumption_kWh_lag <- as.numeric(filtered_data_new6$total_hourly_consumption_kWh_lag)

table(filtered_data_new6$date)

# Creating a variable for the error rate in results 
filtered_data_new6 <- filtered_data_new6 %>%
  arrange(Property_ID,date) %>%
  group_by(Property_ID,date) %>%
  mutate(error_rate = ((abs(hourly_flexibility_potential_kWh - (total_hourly_consumption_kWh_lag)))/(total_hourly_consumption_kWh_lag))*100) %>%
  mutate(accuracy = 100 - error_rate) %>%
  ungroup()

# Rounding percentage and error rate to 2.d.p
filtered_data_new6$error_rate <- round(filtered_data_new6$error_rate, digits = 1)
filtered_data_new6$accuracy <- round(filtered_data_new6$accuracy, digits = 1)

# Move accuracy and error columns so that 
filtered_data_new6 <- filtered_data_new6  %>%  
  relocate(accuracy, .before = Interval_consumption_kWh) %>%
  relocate(error_rate, .before = Interval_consumption_kWh)

# Move On/off period before accuracy for explanation of error 
filtered_data_new6 <- filtered_data_new6  %>%  
  relocate(On_Off_flag, .before = accuracy)

# Shifting variables forward from 'Off' period so that they can be eventually analysed during correlation analysis 
filtered_data_new6 <- filtered_data_new6 %>%
  mutate(percent_work_output = shift(percent_work_output, 1, type = "lead", fill = NA)) %>%
  mutate(average_power_output_kW = shift(average_power_output_kW, 1, type = "lead", fill = NA)) %>%
  mutate(average_heat_output_kW = shift(average_heat_output_kW,1, type = "lead", fill = NA)) %>%
  mutate(average_hourly_consumption_kWh = shift(average_hourly_consumption_kWh, type = "lead", fill = NA)) %>%
  mutate(total_hourly_consumption_kWh = shift(total_hourly_consumption_kWh, type = "lead", fill = NA)) %>%
  mutate(total_run_consumption_kWh = shift(total_run_consumption_kWh, 1, type = "lead", fill = NA))

# Creating a variable for the error rate in results - Old
#post_processing_full <- filtered_data_new6 %>%
#  arrange(Property_ID,date,day,Hour) %>%
#  group_by(Property_ID,date,day) %>%
#  mutate(error_rate = 
#           ((abs(hourly_flexibility_potential_kWh - lag(average_hourly_consumption_kWh)))/lag(average_hourly_consumption_kWh))*100) %>%
#  mutate(accuracy = 100 - error_rate) %>%
#  ungroup()

# Filtering for consecutive On-off periods
post_processing_full_new6 <- filtered_data_new6 %>%
  filter(consecutive_new == 1 | consecutive_new == -1)

# Creating a dataframe for flexibility periods within the dataset
flexibility_periods_new6 <- post_processing_full_new6 %>%
  filter(On_Off_flag == "Off")

# Converting run_id into a factor to be able to plot  
flexibility_periods_new6$run_id <- as.factor(flexibility_periods_new6$run_id)

# Filtering for just Off periods to be able to look at accuracy 
flexibility_periods_full_new6 <- flexibility_periods_new6 %>%
  filter(On_Off_flag == "Off")

# Technical potential for demand response for each period 
Plot_103 <- ggplot(flexibility_periods_full_new6, aes(x = run_id , y = Technical_potential_demand_response_kW)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full_new6$Technical_potential_demand_response_kW), colour = "red", linetype = 'dotted', size = 1) +
  annotate("label", x = 300, y = mean(flexibility_periods_full_new6$Technical_potential_demand_response_kW) + 0.3, label = "Mean technical potential for demand response", colour = "red", size = 3) +
  labs(x = "'Off' period" ,
       y = "Technical potential for demand response  (kW) ",
       title ="Periods with technical potential for demand response that meet assumptions") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90, size = 1)
  ) 
Plot_103 <- Plot_103 + guides(fill=guide_legend(title="Property ID")) 
Plot_103

# Move accuracy and error columns so that 
flexibility_periods_full_new6 <- flexibility_periods_full_new6  %>%  
  relocate(accuracy, .before = Interval_consumption_kWh) %>%
  relocate(error_rate, .before = Interval_consumption_kWh)

# Getting rid of Null values
flexibility_periods_full_new6 <- na.omit(flexibility_periods_full_new6)

# Changing names of columns in dataset for clarity in correlation analysis
flexibility_periods_full_new6 <- flexibility_periods_full_new6 %>%
  rename(Average_daily_temperature = avg_temp) %>%
  rename(Internal_temperature_decay = Internal_temp_range) %>%
  rename(Average_outside_temperature = average_outside_temperature) %>%
  rename(Shiftable_energy_potential_kWh = Technical_potential_demand_response_kW) %>%
  rename(Total_Floor_Area_m2 = Total_Floor_Area) %>%
  rename(Average_internal_air_temperature = avg_internal_air_temperature) %>%
  rename(Measured_accuracy_percent = accuracy) %>%
  rename(Work_capacity_percent = percent_work_output) %>%
  rename(Period_length_minutes = run_length_mins) 

# Changing spelling on some of columns
flexibility_periods_full_new6 <- flexibility_periods_full_new6 %>% 
  rename(Average_power_output_kW = average_power_output_kW) %>%
  rename(Average_heat_output_kW = average_heat_output_kW) %>%
  rename(Total_run_consumption_kWh = total_run_consumption_kWh) %>%
  rename(Shiftable_energy_potential_kWh = Shiftable_energy_potential_kWh)

# Removing period two periods from data which have been assumed
# to be outliers
flexibility_periods_full_new6 <- flexibility_periods_full_new6 %>%
  filter(Shiftable_energy_potential_kWh >= 0)

# Summary statistics about the accuracy of calculated results
median(flexibility_periods_full_new6$Measured_accuracy_percent) # median accuracy of results i 98.3%
mean(flexibility_periods_full_new6$Measured_accuracy_percent) # mean accuract is 95%
summary(flexibility_periods_full_new6$Measured_accuracy_percent)

str(post_processing_full_new6)

# Converting run ID into factor to be able to plot 
post_processing_full_new6$run_id <- as.factor(post_processing_full_new6$run_id)
flexibility_periods_full_new6$run_id <- as.factor(flexibility_periods_full_new6$run_id)

# Visualising the accuracy
Plot_114 <- ggplot(flexibility_periods_full_new6, aes(x = run_id , y = Measured_accuracy_percent)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full_new6$Measured_accuracy_percent), colour = "red", linetype = 'dotted', size = 1) +
  annotate("label", x = 300, y = mean(flexibility_periods_full_new6$Measured_accuracy_percent) + 6, label = "Average accuracy of calculated period", colour = "red", size = 3) +
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
Plot_114

## Answers to research question 

# How much demand response potential
mean(flexibility_periods_full_new6$Shiftable_energy_potential_kWh) # 2.06kW  per period
median(flexibility_periods_full_new6$half_hourly_flexibility_potential_kWh) # 0.97 kW per half hour of flexibility
summary(flexibility_periods_full_new6$Shiftable_energy_potential_kWh) # 5.958 kWh of technical potential per event  
sd(flexibility_periods_full_new6$Shiftable_energy_potential_kWh) # 3.79 (very large)

# Summary statistics of results 
summary(flexibility_periods_full_new6$Shiftable_energy_potential_kWh)
sd(flexibility_periods_full_new6$Shiftable_energy_potential_kWh)

# Calculating optimal number of bins for plotting histogram

# Finding value of n for size of data
n1 <- n_distinct(flexibility_periods_full_new6)
print(n1)  #610 data size

# Freedman-Diaconis rule 
summary(flexibility_periods_full_new6$Shiftable_energy_potential_kWh)

# Finding range in normalized data
range <- max(flexibility_periods_full_new6$Shiftable_energy_potential_kWh) - min(flexibility_periods_full_new6$Shiftable_energy_potential_kWh)

Q3 <- quantile(flexibility_periods_full_new6$Shiftable_energy_potential_kWh, 0.75) 
Q1 <- quantile(flexibility_periods_full_new6$Shiftable_energy_potential_kWh, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n1^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin3 <- range/Bin_width
Freedman_Diaconic_bin3 <- round(Freedman_Diaconic_bin3,0)
print(Freedman_Diaconic_bin3) 

# Histogram of technical potential demand response
Plot_115 <- ggplot(flexibility_periods_full_new6, aes(x = Shiftable_energy_potential_kWh)) +
  geom_histogram(bins = Freedman_Diaconic_bin3, color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full_new6$Shiftable_energy_potential_kWh), colour = "orange", linetype = 'dotted', size = 1)+
  #annotate("label", x =  median(flexibility_periods_full_new6$Technical_potential_demand_response_kW) , y = 60, label = "Median demand response per period (kW)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full_new6$Shiftable_energy_potential_kWh), colour = "red", linetype = 'dotted', size = 1)+
  #annotate("label", x =  mean(flexibility_periods_full_new6$Technical_potential_demand_response_kW) , y = 50, label = "Mean demand response per period (kW)", colour = "red", size = 3) +
  labs(x = "Technical potential for demand response (kW)" ,
       y = "Frequency",
       title ="Histogram of technical potential for demand response") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_115 <- Plot_115 + guides(fill=guide_legend(title="Property ID")) 
Plot_115

# Boxplot of demand response potential 
Plot_116 <- ggplot(flexibility_periods_full_new6, aes(y = Shiftable_energy_potential_kWh)) +
  geom_boxplot(color = "#000000", fill = "#FFFFFF") +
  labs(x = "" ,
       y = "Technical potential for demand response (kW)",
       title ="Boxplot of technical potential for demand response") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_116 <- Plot_116 + guides(fill=guide_legend(title="Property ID")) 
Plot_116

Panel_plot1 <- Plot_115 + Plot_116
Panel_plot1

# Length of flexibility periods 
summary(flexibility_periods_full_new6$Period_length_minutes)/60 # 1.167 hours of flexibility per heat pump
sd((flexibility_periods_full_new6$Period_length_minutes)/60)

# Bar graph of seeing the number of heat free hours per period 
Plot_117 <- ggplot(flexibility_periods_full_new6, aes(x = run_id , y = (Heat_free_hours))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full_new6$Heat_free_hours), colour = "red", linetype = 'dotted', size = 1)+
  annotate("label", x = 300, y = mean(flexibility_periods_full_new6$Heat_free_hours) + .7 , label = "Average length of period", colour = "red", size = 3) +
  labs(x = "Period" ,
       y = "Length of time of period (hours)",
       title ="Length of time of periods with technical potential for demand response") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    #axis.text.x = element_text(angle =90)
  ) 
Plot_117 <- Plot_117 + guides(fill=guide_legend(title="Property ID")) 
Plot_117

# Bar graph of seeing the saved energy of each period with demand response potential
Plot_118 <- ggplot(flexibility_periods_full_new6, aes(x = run_id , y = Shiftable_energy_potential_kWh)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full_new6$Shiftable_energy_potential_kWh), colour = "red", linetype = 'dotted', size = 1)+
  annotate("label", x = 300, y = mean(flexibility_periods_full_new6$Shiftable_energy_potential_kWh) + .7 , label = "Average saved energy per period", colour = "red", size = 3) +
  labs(x = "Period" ,
       y = "Saved potential energy (kWh)",
       title ="Saved potential energy use from periods with demand response potential") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
Plot_118 <- Plot_118 + guides(fill=guide_legend(title="Property ID")) 
Plot_118

# Freedman-Diaconis rule 
summary(flexibility_periods_full_new6$Heat_free_hours)

# Finding range in normalized data
range <- max(flexibility_periods_full_new6$Heat_free_hours) - min(flexibility_periods_full_new6$Heat_free_hours)

Q3 <- quantile(flexibility_periods_full_new6$Heat_free_hours, 0.75) 
Q1 <- quantile(flexibility_periods_full_new6$Heat_free_hours, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n1^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin4 <- range/Bin_width
Freedman_Diaconic_bin4 <- round(Freedman_Diaconic_bin4,0)
print(Freedman_Diaconic_bin4) 

# Histogram of length of times
Plot_119 <- ggplot(flexibility_periods_full_new6, aes(x = (Heat_free_hours))) +
  geom_histogram(bins = Freedman_Diaconic_bin4, color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full_new6$Heat_free_hours), colour = "orange", linetype = 'dotted', size = 1) +
  #annotate("label", x =  median(flexibility_periods_full_new6$Technical_potential_demand_response_kW) , y = 60, label = "Median length of time (hours)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full_new6$Heat_free_hours), colour = "red", linetype = 'dotted', size = 1) +
  #annotate("label", x =  mean(flexibility_periods_full_new6$Technical_potential_demand_response_kW) , y = 50, label = "Mean length of time (hours)", colour = "red", size = 3) +
  labs(x = "Length of time of periods (hours)" ,
       y = "Frequency",
       title ="Histogram of length of time of periods with demand response potential") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  ) 
Plot_119 <- Plot_119 + guides(fill=guide_legend(title="Property ID")) 
Plot_119

# Summary of internal temperature decay 
summary(flexibility_periods_full_new6$Internal_temperature_decay)
sd(flexibility_periods_full_new6$Internal_temperature_decay)

# Freedman-Diaconis rule 
summary(flexibility_periods_full_new6$Internal_temperature_decay)

# Finding range in normalized data
range <- max(flexibility_periods_full_new6$Internal_temperature_decay) - min(flexibility_periods_full_new6$Internal_temperature_decay)

Q3 <- quantile(flexibility_periods_full_new6$Internal_temperature_decay, 0.75) 
Q1 <- quantile(flexibility_periods_full_new6$Internal_temperature_decay, 0.25) 

# Finding IQR from the range in maximum and minimum values
IQR <- Q3 - Q1

# Finding bin width in the data 
Bin_width <- 2*(IQR)/(n1^(1/3))

# Finding number of bins using Freedman-Diaconic's rule
Freedman_Diaconic_bin5 <- range/Bin_width
Freedman_Diaconic_bin5 <- round(Freedman_Diaconic_bin5,0)
print(Freedman_Diaconic_bin5) 

# Distribution of internal temperature range
Plot_120 <- ggplot(flexibility_periods_full_new6, aes(x = (Internal_temperature_decay))) +
  geom_histogram(bins = Freedman_Diaconic_bin5, color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full_new6$Internal_temperature_decay), colour = "orange", linetype = 'dotted', size = 1)+
  #annotate("label", x =  median(flexibility_periods_full_new6$Technical_potential_demand_response_kW) , y = 60, label = "Median length of time (hours)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full_new6$Internal_temperature_decay), colour = "red", linetype = 'dotted', size = 1)+
  #annotate("label", x =  mean(flexibility_periods_full_new6$Technical_potential_demand_response_kW) , y = 50, label = "Mean length of time (hours)", colour = "red", size = 3) +
  labs(x = "Internal temperature decay (degrees °C) " ,
       y = "Frequency",
       title ="Internal temperature decay during periods with technical potential for demand response") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_120 <- Plot_120 + guides(fill=guide_legend(title="Property ID")) 
Plot_120

