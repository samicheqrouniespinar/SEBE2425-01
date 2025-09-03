# Distribution of different variables for correlation analysis
# Author: Sami Cheqrouni-Espinar 
# Date: 04/08/2025

# Work capacity (%)
Plot_56 <- ggplot(correlation_df, aes(x = (Work_capacity_percent_lag))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$Work_capacity_percent_lag), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$Work_capacity_percent_lag), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Work capacity (%)" ,
       y = "Frequency")+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_56 <- Plot_56 + guides(fill=guide_legend(title="Property ID")) 
Plot_56

# Average power output
Plot_56 <- ggplot(correlation_df, aes(x = (Average_power_output_kW_lag))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$Average_power_output_kW_lag), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$Average_power_output_kW_lag), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Average power output (kW)" ,
       y = "Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_56 <- Plot_56 + guides(fill=guide_legend(title="Property ID")) 
Plot_56

# Average heat output
Plot_57 <- ggplot(correlation_df, aes(x = (Average_heat_output_kW_lag))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$Average_heat_output_kW_lag), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$Average_heat_output_kW_lag), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Average heat output (kW)" ,
       y = "Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_57 <- Plot_57 + guides(fill=guide_legend(title="Property ID")) 
Plot_57

# Total floor area
Plot_58 <- ggplot(correlation_df, aes(x = (Total_Floor_Area_m2))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$Total_Floor_Area_m2), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$Total_Floor_Area_m2), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Total floor area (m²)" ,
       y = "Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_58 <- Plot_58 + guides(fill=guide_legend(title="Property ID")) 
Plot_58

str(correlation_df)

# Heat pump size
Plot_59 <- ggplot(correlation_df, aes(x = (HP_Size_kW))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$HP_Size_kW), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$HP_Size_kW), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Heat pump size (kW)" ,
       y = "Frequency",
       title = "The distribution of sizes of ASHPs in the dataset") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_59 <- Plot_59 + guides(fill=guide_legend(title="Property ID")) 
Plot_59

sd(EoH_meta_data_ASHP$HP_Size_kW)

# Internal temperature decay 
Plot_60 <- ggplot(correlation_df, aes(x = (Internal_temperature_decay))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$Internal_temperature_decay), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$Internal_temperature_decay), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Internal temperature drop (°C)" ,
       y = "Frequency",
       title = "Distribution of internal temperature drop ") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_60 <- Plot_60 + guides(fill=guide_legend(title="Property ID")) 
Plot_60

# Creating a variable of the run length but in hours 
correlation_df <- correlation_df %>%
  mutate(Period_length_hours = (Period_length_minutes/60))

# Average outside temperature
Plot_60 <- ggplot(correlation_df, aes(x = (Average_outside_temperature))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$Average_outside_temperature), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$Average_outside_temperature), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Average outside temperature(°C)" ,
       y = "Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_61 <- Plot_61 + guides(fill=guide_legend(title="Property ID")) 
Plot_61

# Number of heat free hours 
Plot_62 <- ggplot(correlation_df, aes(x = (Heat_free_hours))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$Heat_free_hours), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$Heat_free_hours), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Heat free hours" ,
       y = "Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_62 <- Plot_62 + guides(fill=guide_legend(title="Property ID")) 
Plot_62

# Shiftable potential energy (kWh)
Plot_63 <- ggplot(correlation_df, aes(x = (Shiftable_energy_potential_kWh))) +
  geom_freqpoly(color = "#000000", fill = "#FFFFFF") +
  geom_vline(xintercept = median(correlation_df$Shiftable_energy_potential_kWh), colour = "orange", linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(correlation_df$Shiftable_energy_potential_kWh), colour = "red", linetype = 'dotted', size = 1) +
  labs(x = "Shiftable energy potential (kWh)" ,
       y = "Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 12)
  ) 
Plot_63 <- Plot_63 + guides(fill=guide_legend(title="Property ID")) 
Plot_63

# Canvas of distributions
par(mfrow = c(3,3))

 Plot_55 + Plot_56 + Plot_57 + Plot_58 + Plot_59 + Plot_60 + Plot_61 + Plot_62 + Plot_63 + facet_grid()

#########################################################

