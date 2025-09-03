# Calculating technical potential for demand response from dataset 
# Author - Sami Cheqrouni-Espinar
# Date - 15/06/2025
rm(filtered_data_sample)
rm(filtered_data)

## For whole dataset 

# Importing EoH metadata again 
EoH_meta_data <- read.csv("~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Data/EoH data folder/Metadata/BEIS Electrification of Heat Project - Property, Design and Installation Information.csv")

# Creating subset of the data for property IDs with air-source heat pumps in them
EoH_meta_data <- filter(EoH_meta_data,
                        HP_Recommend == c("ASHP","HT_ASHP"))

# Convert rated heat pump size in meta data to numerical value 
EoH_meta_data$HP_Size_kW <- as.numeric(EoH_meta_data$HP_Size_kW)

# Joining metadata to filtered_data to create new variable 
filtered_data <- left_join(filtered_data,EoH_meta_data, by = "Property_ID")

# Calculating the average hourly consumption across a given period of on or off
filtered_data <- filtered_data %>%
  mutate(Average_hourly_consumption_kWh = (Total_run_consumption_kWh/ (run_length_mins))*60)

# Filter for just on and off periods only, no 'On - Compressor Off' periods 
filtered_data <- filtered_data %>%
  filter(On_Off_flag == 'On' | On_Off_flag == 'Off')

# Also creating a variable of the fraction that heat pump is working relative to it's rated size for the hour 
#leading up to the 'Off' period
filtered_data <- filtered_data %>% 
 mutate(Percent_work_output = (Average_heat_output_kW/HP_Size_kW)*100)

# Rounding variable created to 2.d.p
filtered_data$Percent_work_output <- round(filtered_data$Percent_work_output, digits = 1)

# Finding the average power output per run to be able to calculate the technical potential for demand response (using the median) 
filtered_data <- filtered_data %>%
  group_by(Property_ID, run_id) %>%
  mutate(Average_power_output_per_run_kW  = median(Power_output_2mins_kW))

# Removing duplicates from the dataset
filtered_data <- unique(filtered_data) 

# Creating an numeric variable for where 'On' or 'Off' takes on binary
# value of either 1 or 0 
filtered_data <- filtered_data %>%
  arrange(Property_ID, Timestamp) %>%
  mutate(On_Off_flag_num = ifelse(On_Off_flag == "Off",0,1)) %>%
  ungroup()

# Formatting date as date class type
filtered_data$date <- ymd(filtered_data$date)

# Ensure the above variable is formatted as a number 
filtered_data$On_Off_flag_num <- as.numeric(filtered_data$On_Off_flag_num)

# Consecutive variable used to identify where for the same heat pump and on the same date 
# the heat pump is turning from 'On' to 'Off'
# Only run this and other shift functions once otherwise it moves periods too much and you need
# to process data frame from scratch 
filtered_data <- filtered_data %>%
  group_by(Property_ID, date) %>%
  mutate(consecutive = shift(On_Off_flag_num, 1, type = "lag", fill = NA)) %>%
  mutate(consecutive_new = On_Off_flag_num - consecutive) %>%
  ungroup()
 
# Get rid of null values here 
filtered_data <- na.omit(filtered_data)

# Moving columns before meta-data variables
filtered_data <- filtered_data  %>%  
  relocate(Average_hourly_consumption_kWh, .before = Delivery_Contractor) %>%
  relocate(Percent_work_output, .before = Delivery_Contractor) %>%
  relocate(Average_power_output_per_run_kW, .before = Delivery_Contractor) %>%
  relocate(On_Off_flag, .before = Delivery_Contractor) %>%
  relocate(On_Off_flag_num, .before = Delivery_Contractor)%>%
  relocate(consecutive, .before = Delivery_Contractor) %>%
  relocate(consecutive_new, .before = Delivery_Contractor)

#Fix consecutive column so that it shows lag between each row rather than
# A duplicate of the On_Off_flag_num
#filtered_data$consecutive <- filtered_data$consecutive - lag(filtered_data$consecutive, default = 0)

# Creating a variable here for the average output for the 60 minutes previous as lag function not seeming to work 
# on it's own
filtered_data <- filtered_data %>%
  mutate(Average_power_output_kW_lag = shift(Average_power_output_kW, 1, type = "lag", fill = NA))

# Creating a column for calculating the technical potential for demand response 
# and the estimate energy savings as a result 
# Demand response potential = consumption if the heat pump were continuing running - power consumption in off periods 
filtered_data <- filtered_data %>%
  mutate(Technical_potential_demand_response_kW = Average_power_output_kW_lag - Average_power_output_per_run_kW) %>%
  mutate(Shiftable_energy_potential_kWh = (run_length_mins * (Technical_potential_demand_response_kW))/(30*2))

# Rounding variables to an appropriate degree of accuracy (2.d.p)
filtered_data$Technical_potential_demand_response_kW <- round(filtered_data$Technical_potential_demand_response_kW, digits = 2)
filtered_data$Shiftable_energy_potential_kWh <- round(filtered_data$Shiftable_energy_potential_kWh, digits = 2)

# Relocating variables so they are before meta data 
filtered_data <- filtered_data  %>%  
  relocate(Technical_potential_demand_response_kW, .before = Delivery_Contractor) %>%
  relocate(Shiftable_energy_potential_kWh, .before = Delivery_Contractor)

# Finding calculating the saved enery in hourly and half hourly periods  
filtered_data <- filtered_data %>%
  mutate(Hourly_shiftable_energy_potential_kWh = (Shiftable_energy_potential_kWh / (run_length_mins))*60) %>%
  mutate(Half_Hourly_shiftable_energy_potential_kWh = Hourly_shiftable_energy_potential_kWh/2)

# Rounding to an appropiate degree of accuracy and moving before meta data variables
filtered_data$Hourly_shiftable_energy_potential_kWh <- round(filtered_data$Hourly_shiftable_energy_potential_kWh, digits = 2)
filtered_data$Half_Hourly_shiftable_energy_potential_kWh <- round(filtered_data$Half_Hourly_shiftable_energy_potential_kWh, digits = 2)

# Relocating variables so they are before meta data 
filtered_data <- filtered_data  %>%  
  relocate(Hourly_shiftable_energy_potential_kWh, .before = Delivery_Contractor) %>%
  relocate(Half_Hourly_shiftable_energy_potential_kWh, .before = Delivery_Contractor)


