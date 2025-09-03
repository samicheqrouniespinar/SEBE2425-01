# Panel plot pre-regression


# Average power output against demand response potential
Plot_54 <- ggplot(analysis_df, aes(x = Average_power_output_kW_lag, y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Average_power_output_kW_lag),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Average power output (kW)" ,
       y = "Technical potential to response potential (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) + 
  geom_label( 
  data=analysis_df %>% 
    filter(Measured_accuracy_percent< 50), # Filter data first
  aes(label= run_id),
  nudge_x = -0.25, nudge_y = 0.25)
Plot_54 <- Plot_54 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_54

# Peak power output against demand response potential
Plot_55 <- ggplot(analysis_df, aes(x = Peak_power_output_kW_lag, y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Peak_power_output_kW_lag),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Peak power output (kW)" ,
       y = "Technical potential to response potential (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_55 <- Plot_55 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_55

# Average heat output against against demand response potential
Plot_56 <- ggplot(analysis_df, aes(x = Average_heat_output_kW_lag, y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Average_heat_output_kW),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Average heat output (kW)" ,
       y = "Technical potential to response potential (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_56 <- Plot_56 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_56

# Standard deviation of power output
Plot_57 <- ggplot(analysis_df, aes(x = Sd_power_output_kW_lag, y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Sd_power_output_kW_lag),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Standard deviation of power output (kW)" ,
       y = "Technical potential to response potential (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_57 <- Plot_57 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_57

# Standard deviation of heat output
Plot_58 <- ggplot(analysis_df, aes(x = Sd_heat_output_kW_lag, y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Sd_heat_output_kW_lag),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Standard deviation of heat output (kW)" ,
       y = "Technical potential to response potential (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_58 <- Plot_58 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_58

# Coefficient of variation of heat output
Plot_59 <- ggplot(analysis_df, aes(x = Coefficient_of_variation_heat, y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Coefficient_of_variation_heat),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Coefficient of variation of heat" ,
       y = "Technical potential to response potential (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_59 <- Plot_59 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_59

# Coefficient of variation of heat output
Plot_60 <- ggplot(analysis_df, aes(x = Coefficient_of_variation_power, y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Coefficient_of_variation_power),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Coefficient of variation of power" ,
       y = "Technical potential to response potential (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_60 <- Plot_60 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_60

# Demand response potential of work capacity    
Plot_61 <- ggplot(analysis_df, aes(x = Work_capacity_percent_lag , y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Work_capacity_percent_lag),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Percent work output of heat pump (%)" ,
       y = "Technical potential to demand resonse (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_61 <- Plot_61 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_61

# Average outside temperature and demand response potential 
Plot_62 <- ggplot(analysis_df, aes(x = Average_outside_temperature , y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Average_outside_temperature),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Average external temperature (degrees °C)" ,
       y = "Technical potential to demand resonse (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_62 <- Plot_62 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_62

# Demand response potential to total floor area 
Plot_63 <- ggplot(analysis_df, aes(x = Total_Floor_Area_m2 , y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Total_Floor_Area),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Total floor area (m²)" ,
       y = "Technical potential to demand resonse (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90, size = 4),
    axis.title = element_text(size = 14)
  ) 
Plot_63 <- Plot_63 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_63

# Demand response potential to size of the heat pump size
Plot_64 <- ggplot(analysis_df, aes(x = HP_Size_kW , y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$HP_Size_kW),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Heat pump size (kW)" ,
       y = "Technical potential to demand resonse (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90, size = 4),
    axis.title = element_text(size = 14)
  ) 
Plot_64 <- Plot_64 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_64

# Demand response potential to number of heated rooms
Plot_65 <- ggplot(analysis_df, aes(x = Heated_rooms , y = Technical_potential_demand_response_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(analysis_df$Technical_potential_demand_response_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Heated_rooms),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Number of heated rooms" ,
       y = "Technical potential for demand response (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(),
    axis.title = element_text(size = 14)
  ) 
Plot_65 <- Plot_65 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_65

# Seeing all variables to demand response potential compared   
Plot_54 + Plot_55 + Plot_56 + facet_grid()

Plot_57 + Plot_58 + Plot_59 + facet_grid()

Plot_60 + Plot_61  + Plot_62 + facet_grid()

Plot_63 + Plot_64  + Plot_65 + facet_grid()

# Run length and internal temperatural decay
Plot_66 <- ggplot(analysis_df, aes(x = Period_length_hours , y = Internal_temperature_decay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(analysis_df$Internal_temperature_decay), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Period_length_hours),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Heat-free hours " ,
       y = "Internal temperature drop (degrees °C)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_66 <- Plot_66 + guides(fill=guide_legend(title="Flexibility provided (kW)"))
Plot_66


# Saved potential energy potential and internal temperature decay  
Plot_67 <- ggplot(analysis_df, aes(x = Saved_potential_energy_use_kWh , y = Internal_temperature_decay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(analysis_df$Internal_temperature_decay), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Saved_potential_energy_use_kWh),colour = "red",linetype = 'dotted', size = 1) +
  labs(x = "Shiftable energy potential (kWh)" ,
       y = "Internal temperature drop (degrees °C)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  ) 
Plot_67 <- Plot_67 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_67

# Percent work output and heat pump size 
Plot_68 <- ggplot(analysis_df, aes(x = Work_capacity_percent_lag , y = HP_Size_kW)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(analysis_df$HP_Size_kW), colour = "red",linetype = 'dotted', size = 1) +
  geom_vline(xintercept = mean(analysis_df$Work_capacity_percent),colour = "red",linetype = 'dotted', size = 1)+
  labs(x = "Percent work output of heat pump (%)" ,
       y = "Heat pump size (kW)",
       title ="") +
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 14)
  )
Plot_68 <- Plot_68 + guides(fill=guide_legend(title="Flexibility provided (kW)")) 
Plot_68

Plot_canvas <- Plot_66 + Plot_67 + Plot_68
Plot_canvas

+
 # geom_label( 
  #  data=analysis_df %>% 
  #    filter(Work_capacity_percent_lag< 5), # Filter data first
  #  aes(label= Property_ID),
  #  nudge_x = 0.25, nudge_y = 0.25)