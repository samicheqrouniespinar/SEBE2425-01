## Results and correlation analysis (post processing)
# Author: Sami Cheqrouni-Espinar

## Calculating error rate in results

### For the full set of results 
# Remove any nulls from the dataet 
filtered_data <- na.omit(filtered_data)

# Creating a variable of the hourly consumption from the period previous forward so 
# that accuracy of the demand response can be calculated relative to it 
filtered_data <- filtered_data %>%
  mutate(Total_hourly_consumption_kWh_lag = shift(Total_hourly_consumption_kWh, 1, type = "lag", fill = NA))

# Move accuracy and error columns so that they are in view 
filtered_data <- filtered_data  %>%  
  relocate(Total_hourly_consumption_kWh_lag, .before = Delivery_Contractor)

# Ensuring these are all numeric class types
filtered_data$Hourly_shiftable_energy_potential_kWh <- as.numeric(filtered_data$Hourly_shiftable_energy_potential_kWh)
filtered_data$Total_hourly_consumption_kWh <- as.numeric(filtered_data$Total_hourly_consumption_kWh)
filtered_data$Total_hourly_consumption_kWh_lag <- as.numeric(filtered_data$Total_hourly_consumption_kWh_lag)

# Creating a variable for the error rate in results 
filtered_data <- filtered_data %>%
  arrange(Property_ID,date) %>%
  group_by(Property_ID,date) %>%
  mutate(error_rate = ((abs(Hourly_shiftable_energy_potential_kWh - (Total_hourly_consumption_kWh_lag)))/(Total_hourly_consumption_kWh_lag))*100) %>%
  mutate(accuracy = 100 - error_rate) %>%
  ungroup()

# Rounding percentage and error rate to 2.d.p
filtered_data$error_rate <- round(filtered_data$error_rate, digits = 1)
filtered_data$accuracy <- round(filtered_data$accuracy, digits = 1)

# Move accuracy and error columns so that 
filtered_data <- filtered_data  %>%  
  relocate(accuracy, .before = Interval_consumption_kWh) %>%
  relocate(error_rate, .before = Interval_consumption_kWh)
  
# Move On/off period before accuracy for explanation of error 
filtered_data <- filtered_data  %>%  
  relocate(On_Off_flag, .before = accuracy)

# Changing names of columns in dataset for clarity in correlation analysis
filtered_data <- filtered_data %>%
  rename(Average_daily_temperature = avg_temp) %>%
  rename(Internal_temperature_decay = Internal_temp_range) %>%
  rename(Average_outside_temperature = average_outside_temperature) %>%
  rename(Total_Floor_Area_m2 = Total_Floor_Area) %>%
  rename(Average_internal_air_temperature = Avg_internal_air_temperature) %>%
  rename(Measured_accuracy_percent = accuracy) %>%
  rename(Work_capacity_percent = Percent_work_output) %>%
  rename(Period_length_minutes = run_length_mins) 

# Creating a set of variables from the previous 'On' period to shift forward 
# into 'Off' period so that they can be eventually analysed during correlation analysis 
filtered_data <- filtered_data %>%
  mutate(Average_heat_output_kW_lag = shift(Average_heat_output_kW,1, type = "lag", fill = NA)) %>%
  mutate(Total_hourly_consumption_kWh_lag = shift(Total_hourly_consumption_kWh, type = "lag", fill = NA)) %>%
  mutate(Total_run_consumption_kWh_lag = shift(Total_run_consumption_kWh, 1, type = "lag", fill = NA)) %>%
  mutate(Work_capacity_percent_lag = shift(Work_capacity_percent, 1, type = "lag", fill = NA)) %>%
  mutate(Average_power_output_per_run_kW_lag = shift(Average_power_output_per_run_kW, 1, type = "lag", fill = NA)) %>%
  mutate(Peak_power_output_kW_lag = shift(Peak_power_output_kW, 1, type = "lag", fill = NA)) %>%
  mutate(Peak_heat_output_kW_lag = shift(Peak_heat_output_kW, 1, type = "lag", fill = NA)) %>%
  mutate(Sd_power_output_kW_lag = shift(Sd_power_output_kW, 1, type = "lag", fill = NA)) %>%
  mutate(Sd_heat_output_kW_lag = shift(Sd_heat_output_kW, 1, type = "lag", fill = NA)) %>%
  mutate(Average_hourly_consumption_kWh_lag = shift(Average_hourly_consumption_kWh,1, type = "lag", fill = NA))

# Moving these new variables before meta-data so they are in view 
filtered_data <- filtered_data  %>%  
  relocate(Average_power_output_kW_lag, .before = Delivery_Contractor) %>%
  relocate(Average_heat_output_kW_lag, .before = Delivery_Contractor) %>%
  relocate(Total_run_consumption_kWh_lag, .before = Delivery_Contractor) %>%
  relocate(Work_capacity_percent_lag, .before = Delivery_Contractor) %>%
  relocate(Average_power_output_per_run_kW_lag, .before = Delivery_Contractor)%>%
  relocate(Peak_power_output_kW_lag, .before = Delivery_Contractor) %>%
  relocate(Peak_heat_output_kW_lag, .before = Delivery_Contractor) %>%
  relocate(Sd_power_output_kW_lag, .before = Delivery_Contractor) %>%
  relocate(Sd_heat_output_kW_lag, .before = Delivery_Contractor) %>%
  relocate(Average_hourly_consumption_kWh_lag, .before = Delivery_Contractor)

# Filtering for consecutive On-off periods
post_processing_full <- filtered_data %>%
  filter(consecutive_new == 1 | consecutive_new == -1)

# Creating a dataframe for flexibility periods within the dataset
flexibility_periods <- post_processing_full %>%
  filter(On_Off_flag == "Off")

# Converting run_id into a factor to be able to plot  
flexibility_periods$run_id <- as.factor(flexibility_periods$run_id)

# Filtering for just Off periods to be able to look at accuracy 
flexibility_periods_full <- flexibility_periods %>%
  filter(On_Off_flag == "Off")

# Move accuracy and error columns so that 
flexibility_periods <- flexibility_periods  %>%  
  relocate(Measured_accuracy_percent, .before = Interval_consumption_kWh) %>%
  relocate(error_rate, .before = Interval_consumption_kWh)

# Technical potential for demand response in each period 
Plot_31 <- ggplot(flexibility_periods_full, aes(x = run_id , y = Technical_potential_demand_response_kW)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red", linetype = 'dotted', size = 1) +
  annotate("label", x = 300, y = mean(flexibility_periods_full$Technical_potential_demand_response_kW) + 0.3, label = "Mean technical potential for demand response", colour = "red", size = 3) +
  labs(x = "'Off' period" ,
       y = "Technical potential for demand response  (kW) ",
       title ="Periods with technical potential for demand response that meet assumptions") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90, size = 1)
  ) 
Plot_31 <- Plot_31 + guides(fill=guide_legend(title="Property ID")) 
Plot_31


# Getting rid of Null values
flexibility_periods_full <- na.omit(flexibility_periods_full)

# Removing period two periods from data which have been assumed
# to be outliers
flexibility_periods_full <- flexibility_periods_full %>%
 filter(run_id != "273563" & run_id != "37061" )

# Summary statistics about the accuracy of calculated results
median(flexibility_periods_full$Measured_accuracy_percent) # median accuracy of results i 98.3%
mean(flexibility_periods_full$Measured_accuracy_percent) # mean accuract is 95%
summary(flexibility_periods_full$Measured_accuracy_percent)

# Checking formatting of variables 
str(post_processing_full)

# Converting run ID into factor to be able to plot 
post_processing_full$run_id <- as.factor(post_processing_full$run_id)
flexibility_periods_full$run_id <- as.factor(flexibility_periods_full$run_id)

# Visualising the accuracy
Plot_32 <- ggplot(flexibility_periods_full, aes(x = run_id , y = Measured_accuracy_percent)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full$Measured_accuracy_percent), colour = "red", linetype = 'dotted', size = 1) +
  annotate("label", x = 300, y = mean(flexibility_periods_full$Measured_accuracy_percent) + 6, label = "Average accuracy of calculated period", colour = "red", size = 3) +
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
Plot_32

## Answers to research question 

# How much demand response
mean(flexibility_periods_full$Technical_potential_demand_response_kW) # 2.06kW  per period
median(flexibility_periods_full$Technical_potential_demand_response_kW) # 1.94kW per period
summary(flexibility_periods_full$Shiftable_energy_potential_kWh) # 5.03 kWh of saved potential energy per event  
sd(flexibility_periods_full$Shiftable_energy_potential_kWh) # 3.79 (very large)

## Summary statistics of results 
# Calculating new summary statistics of dataset 
summary(flexibility_periods_full$Technical_potential_demand_response_kW)
sd(flexibility_periods_full$Internal_temperature_decay)

# Shiftable energy potential
summary(flexibility_periods_full$Shiftable_energy_potential_kWh)
sd(flexibility_periods_full$Shiftable_energy_potential_kWh)

# Length of time the heating can be turned of for
summary(flexibility_periods_full$Period_length_minutes)
sd(flexibility_periods_full$Period_length_minutes)

# Internal temperature decay
summary(flexibility_periods_full$Internal_temperature_decay)
sd(flexibility_periods_full$Internal_temperature_decay)

# Work capacity percent 
summary(flexibility_periods_full$Work_capacity_percent_lag)
sd(flexibility_periods_full$Work_capacity_percent_lag)

# Summary of accuracy of periods
summary(flexibility_periods_full$Measured_accuracy_percent)
sd(flexibility_periods_full$Internal_temperature_decay)

# Histogram of technical potential for demand response 
Plot_33 <- ggplot(flexibility_periods_full, aes(x = Technical_potential_demand_response_kW)) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "orange", linetype = 'dotted', size = 1)+
  #annotate("label", x =  median(flexibility_periods_full$demand_response_potential_kW) , y = 60, label = "Median demand response per period (kW)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red", linetype = 'dotted', size = 1)+
  #annotate("label", x =  mean(flexibility_periods_full$demand_response_potential_kW) , y = 50, label = "Mean demand response per period (kW)", colour = "red", size = 3) +
  labs(x = "Technical potential for demand response (kW)" ,
       y = "Frequency",
       title ="Distribution of technical potential for demand response") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12)
  )
Plot_33 <- Plot_33 + guides(fill=guide_legend(title="Property ID")) 
Plot_33

# Boxplot of technical potential for demand response 
Plot_34 <- ggplot(flexibility_periods_full, aes(y = Technical_potential_demand_response_kW)) +
  geom_boxplot(color = "#000000", fill = "#FFFFFF") +
  labs(x = "" ,
       y = "Technical potential for demand response (kW)",
       title ="Boxplot of technical potential for demand response") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_34 <- Plot_34 + guides(fill=guide_legend(title="Property ID")) 
Plot_34

# Seeing QQ plot of flexibility potential to see if it is normal
qqnorm(flexibility_periods_full$Technical_potential_demand_response_kW , main = "Q-Q plot of work capacity of heat pumps")
qqline(flexibility_periods_full$Technical_potential_demand_response_kW, colour = "red")

Panel_plot1 <- Plot_33 + Plot_34
Panel_plot1

# Bar graph of showing heat free hours
Plot_35 <- ggplot(flexibility_periods_full, aes(x = run_id , y = (Heat_free_hours))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full$Heat_free_hours), colour = "red", linetype = 'dotted', size = 1)+
  annotate("label", x = 300, y = mean(flexibility_periods_full$Heat_free_hours) + .7 , label = "Average length of period", colour = "red", size = 3) +
  labs(x = "Period" ,
       y = "Length of time of period (hours)",
       title ="Length of time of periods with technical potential for demand response") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    #axis.text.x = element_text(angle =90)
  ) 
Plot_35 <- Plot_35 + guides(fill=guide_legend(title="Property ID")) 
Plot_35

# Bar graph of showing shiftable potential energy use
Plot_36 <- ggplot(flexibility_periods_full, aes(x = run_id , y = Shiftable_energy_potential_kWh)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(flexibility_periods_full$Shiftable_energy_potential_kWh), colour = "red", linetype = 'dotted', size = 1)+
  annotate("label", x = 300, y = mean(flexibility_periods_full$Shiftable_energy_potential_kWh) + .7 , label = "Average saved energy per period", colour = "red", size = 3) +
  labs(x = "Period" ,
       y = "Saved potential energy (kWh)",
       title ="Saved potential energy use from periods with demand response potential") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
Plot_36 <- Plot_36 + guides(fill=guide_legend(title="Property ID")) 
Plot_36

# Distribution of energy that has the potential to be shifted
Plot_37 <- ggplot(flexibility_periods_full, aes(x = (Shiftable_energy_potential_kWh))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full$Shiftable_energy_potential_kWh), colour = "orange", linetype = 'dotted', size = 1) +
  #annotate("label", x =  median(flexibility_periods_full$demand_response_potential_kW) , y = 60, label = "Median length of time (hours)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full$Shiftable_energy_potential_kWh), colour = "red", linetype = 'dotted', size = 1) +
  #annotate("label", x =  mean(flexibility_periods_full$demand_response_potential_kW) , y = 50, label = "Mean length of time (hours)", colour = "red", size = 3) +
  labs(x = "Shiftable energy potential (kWh)" ,
       y = "Frequency",
       title ="Distribitution of shiftable energy potential") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12)
  )
Plot_37 <- Plot_37 + guides(fill=guide_legend(title="Property ID")) 
Plot_37

# Creating a variable of the run length but in hours 
flexibility_periods_full <- flexibility_periods_full %>%
  mutate(Heat_free_hours = Heat_free_hours) %>%
  relocate(Total_hourly_consumption_kWh, .before = Delivery_Contractor) %>%
  relocate(Heat_free_hours, .before = Delivery_Contractor)

# Histogram of length of heat free hours 
Plot_38 <- ggplot(flexibility_periods_full, aes(x = (Heat_free_hours))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full$Heat_free_hours), colour = "orange", linetype = 'dotted', size = 1) +
  #annotate("label", x =  median(flexibility_periods_full$demand_response_potential_kW) , y = 60, label = "Median length of time (hours)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full$Heat_free_hours), colour = "red", linetype = 'dotted', size = 1) +
  #annotate("label", x =  mean(flexibility_periods_full$demand_response_potential_kW) , y = 50, label = "Mean length of time (hours)", colour = "red", size = 3) +
  labs(x = "Number of heat free (hours)" ,
       y = "Frequency",
       title ="Distribitution of length of periods with demand response potential") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12)
  )
Plot_38 <- Plot_38 + guides(fill=guide_legend(title="Property ID")) 
Plot_38


# Distribution of internal temperature range
Plot_37 <- ggplot(flexibility_periods_full, aes(x = (Internal_temperature_decay))) +
  geom_histogram(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full$Internal_temperature_decay), colour = "orange", linetype = 'dotted', size = 1)+
  #annotate("label", x =  median(flexibility_periods_full$demand_response_potential_kW) , y = 60, label = "Median length of time (hours)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full$Internal_temperature_decay), colour = "red", linetype = 'dotted', size = 1)+
  #annotate("label", x =  mean(flexibility_periods_full$demand_response_potential_kW) , y = 50, label = "Mean length of time (hours)", colour = "red", size = 3) +
  labs(x = "Internal temperature decay (degrees °C) " ,
       y = "Frequency",
       title ="Internal temperature decay during periods with technical potential for demand response") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_37 <- Plot_37 + guides(fill=guide_legend(title="Property ID")) 
Plot_37


# Percent work output 
Plot_40 <- ggplot(flexibility_periods_full, aes(x = (Work_capacity_percent_lag))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(flexibility_periods_full$Work_capacity_percent_lag), colour = "orange", linetype = 'dotted', size = 1)+
  #annotate("label", x =  median(flexibility_periods_full$demand_response_potential_kW) , y = 60, label = "Median length of time (hours)", colour = "orange", size = 3) +
  geom_vline(xintercept = mean(flexibility_periods_full$Work_capacity_percent_lag), colour = "red", linetype = 'dotted', size = 1)+
  #annotate("label", x =  mean(flexibility_periods_full$demand_response_potential_kW) , y = 50, label = "Mean length of time (hours)", colour = "red", size = 3) +
  labs(x = "Percent work output (%)" ,
       y = "Frequency",
       title ="Histogram of work capacity of heat pumps") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic")
  )
Plot_40 <- Plot_40 + guides(fill=guide_legend(title="Property ID")) 
Plot_40

# Boxplot of the same data
Plot_41 <- ggplot(flexibility_periods_full, aes(y = (Work_capacity_percent_lag))) +
  geom_boxplot(color = "#000000", fill = "#FFFFFF") +
  labs(x = "",
       y = "Work capacity (%)",
       title ="Boxplot of work capacity of heat pumps") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_blank()
  )
Plot_41 <- Plot_41 + guides(fill=guide_legend(title="Property ID")) 
Plot_41

Plot_40 + Plot_41 + facet_grid()

# QQ line of work capacity of heat pumps 
qqnorm(correlation_df$Work_capacity_percent_lag , main = "Q-Q plot of work capacity of heat pumps")
qqline(correlation_df$Work_capacity_percent_lag, colour = "red")

# Summary statistics of results 
summary(correlation_df$Work_capacity_percent_lag)
sd(correlation_df$Work_capacity_percent_lag)

# Comparing results to field trials of flexibility 
average_DR_hour <- median(flexibility_periods_full$Technical_potential_demand_response_kW)
average_DR_hour<- round(average_DR_hour, digits = 2)

# Creating a dataframe from the results of the trial 
field_trial <- c('NEDO','Equinox','HeatFlex','Latent','Crawley et al., 2022','EoH dataset')
hourly_demand_response <-c('0.36','0.6','0.764','0.8','0.78',average_DR_hour)
field_trial_comparison <- data.frame(field_trial,hourly_demand_response)

# Ensure demand response/DR potential is numeric variable
field_trial_comparison$hourly_demand_response <- as.numeric(field_trial_comparison$hourly_demand_response)

# Arranging in ascending order
field_trial_comparison <- arrange(field_trial_comparison, hourly_demand_response)

# Checking formatting of variables
str(field_trial_comparison)

x <- field_trial_comparison$field_trial
y <- field_trial_comparison$hourly_demand_response

# Comparison of results to other trials 
Plot_42 <- ggplot(field_trial_comparison, aes(x = reorder(field_trial,-hourly_demand_response) , y = hourly_demand_response, fill = field_trial)) +
  geom_segment(aes(x=x, xend = x, y = 0, yend = y))+
  geom_point(size = 5, colour = "red",  alpha = 0.7 , shape = 21 , stroke = 2) +
  #geom_bar(stat = "identity") +
  labs(x = "Datasets and trials" ,
       y = "Technical potential & trial results (kW)",
       title ="Technical potential for demand response and measured demand response in field trials") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank())
Plot_42 <- Plot_42 + guides(fill=guide_legend(title="Field trial/dataset")) 
Plot_42

# Comparison to data driven approaches 
Data_driven_approach <- c('Demand response -0.2°C','Demand response at -5.8°C','EoH dataset')
demand_response <-c('1.65','2.15',average_DR_hour)
data_driven_approach_comparison <- data.frame(Data_driven_approach,demand_response)

# Ensure demand response/DR potential is numeric variable
data_driven_approach_comparison$demand_response <- as.numeric(data_driven_approach_comparison$demand_response)

# Arranging in ascending order
data_driven_approach_comparison <- arrange(data_driven_approach_comparison, demand_response)

# Checking formatting of variables
str(data_driven_approach_comparison)

x <- data_driven_approach_comparison$Data_driven_approach
y <- data_driven_approach_comparison$demand_response

# Comparison of results to data-driven approach 
Plot_43 <- ggplot(data_driven_approach_comparison, aes(x = reorder(Data_driven_approach,-demand_response) , y = demand_response, fill = Data_driven_approach)) +
  geom_segment(aes(x=x, xend = x, y = 0, yend = y))+
  geom_point(size = 5, colour = "red",  alpha = 0.7 , shape = 21 , stroke = 2) +
  coord_cartesian(ylim = c(1.5,2.3))+
  #geom_bar(stat = "identity") +
  labs(x = "" ,
       y = "Technical potential & calculated demand response (kW)",
       title ="Comparison of technical potential for demand response to results by Halloran et al.") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank())
Plot_43 <- Plot_43 + guides(fill=guide_legend(title="Datasets and data-driven appraoch")) 
Plot_43

summary(flexibility_periods_full$Average_daily_temperature)
sd(flexibility_periods_full$Average_daily_temperature)
## Correlation analysis 

cor(correlation_df$coefficient_of_variation_power, correlation_df$Technical_potential_demand_response_kW)

# Creating a subset of the dataframe to only keep variables relevant for correlation analysis and general analysis
analysis_df <- flexibility_periods_full %>%
  subset(select = c("Property_ID","date","run_id","Technical_potential_demand_response_kW","Average_daily_temperature","Heat_free_hours","Internal_temperature_decay","External_temp_range","Average_outside_temperature","Average_internal_air_temperature","Shiftable_energy_potential_kWh","Average_power_output_kW_lag","Average_heat_output_kW_lag","Total_run_consumption_kWh_lag","Work_capacity_percent_lag","Average_power_output_per_run_kW_lag","Average_hourly_consumption_kWh_lag","Peak_power_output_kW_lag","Peak_heat_output_kW_lag","Sd_power_output_kW_lag","Sd_heat_output_kW_lag","House_Type","House_Form","House_Age","House_SAP","Total_Floor_Area_m2","No_Adults","No_Child","Heated_rooms","Wall_Type","Floor_Type","Roof_Type","MCS_Flow_Temp","HP_Recommend","Glazed_Type","MCS_Hloss","HP_Size_kW","Measured_accuracy_percent"))

# Coefficient of variation 
analysis_df <- analysis_df %>%
  mutate(Coefficient_of_variation_heat = Sd_heat_output_kW_lag/Average_heat_output_kW_lag) %>%
  mutate(Coefficient_of_variation_power = Sd_power_output_kW_lag/Average_power_output_kW_lag)

# Rounding variables to 2 d.p
analysis_df$Average_daily_temperature <- round(analysis_df$Average_daily_temperature, digits = 2)
analysis_df$Average_outside_temperature <- round(analysis_df$Average_outside_temperature, digits = 2)

# Converting variables that appear as characters into numeric
analysis_df$Total_Floor_Area_m2 <- as.numeric(analysis_df$Total_Floor_Area_m2)
analysis_df$MCS_Hloss <- as.numeric(analysis_df$MCS_Hloss)
analysis_df$Heated_rooms <- as.numeric(analysis_df$Heated_rooms)

# Creating a dataframe specifically for correlation analysis
correlation_df <- analysis_df %>%
  subset(select = c("Technical_potential_demand_response_kW","Average_internal_air_temperature","Average_power_output_kW_lag","Average_heat_output_kW_lag","Peak_power_output_kW_lag","Peak_heat_output_kW_lag","Sd_power_output_kW_lag","Sd_heat_output_kW_lag","Coefficient_of_variation_power","Coefficient_of_variation_heat","Internal_temperature_decay","Average_outside_temperature","Work_capacity_percent_lag","Heat_free_hours","Shiftable_energy_potential_kWh","Total_Floor_Area_m2","HP_Size_kW","MCS_Hloss"))


# Getting rid of any duplicate measures 
correlation_df <- unique(correlation_df)

# Checking format of variables 
str(correlation_df)

# Converting variables that appear as characters into numeric
correlation_df$Total_Floor_Area_m2 <- as.numeric(correlation_df$Total_Floor_Area_m2)
correlation_df$MCS_Hloss <- as.numeric(correlation_df$MCS_Hloss)

# Plotting a correlation matrix for variables in correlation df to see 
#if there is anything of relevance using Pearson's correlation coefficient 
data(correlation_df)
corr <- round(cor(correlation_df), 2)
head(corr[, 1:6]) 
corr

# Computing a matrix of correlation p-values
p.mat <- cor_pmat(correlation_df)
head(p.mat[, 1:14])
p.mat <- round(p.mat, 5)

# Creating a dataframe for this object 
p_value_matrix_pearson <- as.data.frame(p.mat)

# Creating output path for file 
p_value_matrix_pearson_path <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Analysis/Results new/Correlation matrices/p_value_matrix_pearson.csv"

# Saving file 
fwrite(p_value_matrix_pearson, p_value_matrix_pearson_path) 

# Correlation plot 
Plot_Corr1 <- ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3)
Plot_Corr1

# P_value correlation plot
p_value_corr1 <- ggcorrplot(corr, p.mat = p.mat ,hc.order = TRUE, type = "lower", insig = "blank", lab = TRUE, sig.level = 0.05, pch = 8, pch.cex = 1, lab_size = 3)
p_value_corr1

# Scatter plot of most note-worthy relationshops
# Average power output against against run length 

# Average power output against demand response potential
   Plot_44 <- ggplot(flexibility_periods_full, aes(x = Average_power_output_kW_lag, y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE)+
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$Average_power_output_kW_lag),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Average power output (kW)" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against average power output") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 10),
       plot.caption = element_text(face = "italic"),
       axis.text.x = element_text(angle =90))
   Plot_44 <- Plot_44 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_44

   # Removing outlier periods from the dataset identified in Plot 44
   flexibility_periods_full <- flexibility_periods_full %>%
     filter(run_id != "106372" & run_id != "354252" &
              run_id != "226646" )
   
   # Average heat output against against demand response potential
   Plot_45 <- ggplot(flexibility_periods_full, aes(x = Average_heat_output_kW_lag, y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE)+
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$Average_heat_output_kW_lag),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Average heat output (kW)" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against average heat output") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 10),
       plot.caption = element_text(face = "italic"),
       axis.text.x = element_text(angle =90)
     ) 
   Plot_45 <- Plot_45 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_45
   
# Average outside temperature and demand response potential 
   Plot_46 <- ggplot(flexibility_periods_full, aes(x = Average_outside_temperature , y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE)+
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$Average_outside_temperature),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Average external temperature (degrees °C)" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against average external temperature") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 10),
       plot.caption = element_text(face = "italic"),
       axis.text.x = element_text(angle =90)
     ) 
   Plot_46 <- Plot_46 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_46
   
   # Demand response potential against percent work output   
   Plot_47 <- ggplot(flexibility_periods_full, aes(x = Work_capacity_percent_lag , y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE) +
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$Work_capacity_percent),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Work output of heat pump (%)" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against percent work output of maximum capacity") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 10),
       plot.caption = element_text(face = "italic"),
       axis.text.x = element_text(angle =90)
     ) 
   Plot_47 <- Plot_47 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_47

   # Demand response potential against percent work output   
   Plot_48 <- ggplot(flexibility_periods_full, aes(x = HP_Size_kW , y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE) +
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$HP_Size_kW),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Heat pump size (kW)" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against heat pump size ") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 10),
       plot.caption = element_text(face = "italic"),
       axis.title = element_text(size = 14)
          ) 
   Plot_48 <- Plot_48 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_48
   
   # Demand response potential to total floor area 
   Plot_49 <- ggplot(flexibility_periods_full, aes(x = Total_Floor_Area_m2 , y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE) +
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$Total_Floor_Area_m2),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Total floor area (m²)" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against total floor area") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 10),
       plot.caption = element_text(face = "italic"),
       axis.text.x = element_text(angle =90, size = 4),
       axis.title = element_text(size = 14)
     ) 
   Plot_49 <- Plot_49 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_49

   # Standard deviation of power output to demand response potential
   Plot_50 <- ggplot(flexibility_periods_full, aes(x = Sd_power_output_kW_lag, y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE)+
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$Sd_power_output_kW_lag),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Standard deviation of power output (kW)" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against spread of power output ") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 10),
       plot.caption = element_text(face = "italic"),
       axis.text.x = element_text(angle =90)) 
   Plot_50 <- Plot_50 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_50
   
   # Peak power output to demand response potential 
   Plot_51 <- ggplot(flexibility_periods_full, aes(x = Peak_power_output_kW_lag, y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE)+
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$Peak_power_output_kW_lag),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Standard deviation of power output (kW)" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against spread of power output ") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 10),
       plot.caption = element_text(face = "italic"),
       axis.text.x = element_text(angle =90)) 
   Plot_51 <- Plot_51 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_51
   
   # Calculating new summary statistics of data set after treatment of errors 
summary(flexibility_periods_full$Technical_potential_demand_response_kW)
summary(flexibility_periods_full$Shiftable_energy_potential_kWh)
summary(flexibility_periods_full$Period_length_minutes)
summary(flexibility_periods_full$Internal_temperature_decay)
summary(flexibility_periods_full$Work_capacity_percent)
summary(flexibility_periods_full$Measured_accuracy_percent)
summary(flexibility_periods_full$Average_power_output_kW_lag)

   # Ensure heated rooms is numeric variable
   flexibility_periods_full$Heated_rooms <- as.numeric(flexibility_periods_full$Heated_rooms)
   
   # Arranging in ascending order
   flexibility_periods_full <- arrange(flexibility_periods_full, Heated_rooms)
   
   # Demand response potential to number of heated rooms
   Plot_52 <- ggplot(flexibility_periods_full, aes(x = Heated_rooms , y = Technical_potential_demand_response_kW)) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE) +
     geom_hline(yintercept = mean(flexibility_periods_full$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
     geom_vline(xintercept = mean(flexibility_periods_full$Heated_rooms),colour = "red",linetype = 'dotted', size = 1)+
     labs(x = "Number of heated rooms" ,
          y = "Technical potential for demand response (kW)",
          title ="Demand response potential against total floor area") +
     theme_bw()+
     theme(
       plot.title = element_text(hjust = 0.5, size = 12),
       plot.caption = element_text(face = "italic"),
       axis.text.x = element_text(),
       axis.title = element_text(size = 14)
     ) 
   Plot_52 <- Plot_52 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
   Plot_52
   
# Seeing all variables to demand response potential compared   
Plot_44 + Plot_45 + Plot_46 + Plot_47 + Plot_48 + Plot_49 + Plot_50 + facet_grid()

## Secondary regression factors 

# run length and internal temperatural decay
Plot_53 <- ggplot(correlation_df, aes(x = Heat_free_hours , y = Internal_temperature_decay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(flexibility_periods_full$Internal_temperature_decay), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(flexibility_periods_full$Heat_free_hours),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Length of off-time (hours) " ,
       y = "Internal temperature decay(degrees °C)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_53 <- Plot_53 + guides(fill=guide_legend(title="Flexibility provided (kW)"))
Plot_53

max(flexibility_periods_full$Internal_Air_Temperature)

# Calculating new summary statistics of dataset 
summary(flexibility_periods_full$Technical_potential_demand_response_kW)
summary(flexibility_periods_full$Shiftable_energy_potential_kWh)
summary(flexibility_periods_full$Period_length_minutes)
summary(flexibility_periods_full$Internal_temperature_decay)
summary(flexibility_periods_full$Work_capacity_percent)
summary(flexibility_periods_full$Measured_accuracy_percent)

# Shiftable energy potential and internal temperature decay  
Plot_54 <- ggplot(correlation_df, aes(x = Shiftable_energy_potential_kWh , y = Internal_temperature_decay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(flexibility_periods_full$Internal_temperature_decay), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(flexibility_periods_full$Shiftable_energy_potential_kWh),colour = "red",linetype = 'dotted', size = 1) +
  labs(x = "Avoided energy consumption (kWh)" ,
       y = "Internal temperature decay (degrees °C)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_54 <- Plot_54 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_54

# Percent work output and heat pump size 
Plot_55 <- ggplot(correlation_df, aes(x = Work_capacity_percent_lag , y = HP_Size_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(flexibility_periods_full$HP_Size_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(flexibility_periods_full$Work_capacity_percent),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Percent work output of heat pump (%)" ,
       y = "Heat pump size (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_55 <- Plot_55 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_55

Plot_canvas <- Plot_53 + Plot_54 + Plot_55
Plot_canvas

# Spearman's correlation coefficient 
data(correlation_df) 
  corr_spearman <- round(cor(correlation_df, method = "spearman"), 2) 
  head(corr_spearman[, 1:6]) 
corr_spearman

# Creating a dataframe for this object 
correlation_matrix_spearman <- as.data.frame(corr_spearman)

# Correlation matrix using Spearman's correlation coefficient  
Plot_Corr2 <- ggcorrplot(corr_spearman, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 2.5)
Plot_Corr2

# Computing a matrix of correlation p-values
p.mat2 <- cor_pmat(correlation_matrix_spearman)
head(p.mat[, 1:14])
p.mat2 <- round(p.mat, 5)

# P_value correlation plot
p_value_corr2 <- ggcorrplot(corr_spearman, p.mat = p.mat2 ,hc.order = TRUE, type = "lower", insig = "blank", lab = TRUE, sig.level = 0.05, pch = 8, pch.cex = 1, lab_size = 2.5)
p_value_corr2

# Creating a dataframe for this object 
p_value_matrix_spearman <- as.data.frame(p.mat2)

# Creating output path for file 
p_value_matrix_spearman_path <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Analysis/Results new/Correlation matrices/p_value_matrix_spearman.csv"

# Saving file 
fwrite(p_value_matrix_spearman, p_value_matrix_spearman_path)

# Kendall's Tau 
data(correlation_df) 
corr_kendall<- round(cor(correlation_df, method = "kendall"), 2) 
head(corr_kendall[, 1:6]) 
corr_kendall

# Creating a dataframe for this object 
correlation_matrix_kendall <- as.data.frame(corr_kendall)

# Correlation matrix using Kendall's Tau 
Plot_Corr3 <- ggcorrplot(corr_kendall, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3 )
Plot_Corr3

# Computing a matrix of correlation p-values
p.mat3 <- cor_pmat(correlation_matrix_kendall)
head(p.mat[, 1:14])
p.mat3 <- round(p.mat, 5)

# P_value correlation plot
p_value_corr3 <- ggcorrplot(corr_kendall, p.mat = p.mat2 ,hc.order = TRUE, type = "lower", insig = "blank", lab = TRUE, sig.level = 0.05, pch = 8, pch.cex = 1, lab_size = 2.5)
p_value_corr3

# Creating a dataframe for this object 
p_value_matrix_kendall <- as.data.frame(p.mat3)

# Creating output path for file 
p_value_matrix_kendall_path <- "~/SEBE MSc/Dissertation Smart Energy and the Built Environment/SEBE2425-01/Analysis/Results new/Correlation matrices/p_value_matrix_kendall.csv"

# Saving file 
fwrite(p_value_matrix_kendall, p_value_matrix_kendall_path)

# P_value correlation plot
p_value_corr3 <- ggcorrplot(corr_kendall, p.mat = corr_kendall ,hc.order = TRUE, type = "lower", insig = "blank", lab = TRUE, sig.level = 0.05, pch = 8, pch.cex = 1, lab_size = 3)
p_value_corr3

