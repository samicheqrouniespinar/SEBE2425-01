# Regression and statistical analysis file for EoH dataset
# Author : Sami Cheqrouni-Espinar
# Date: 30/07/2025

## Regression analysis 

# Based on correlation analysis, only keeping results which had strong correlation and were significant 
statistical_df <- analysis_df %>%
  subset(select = c("Property_ID","date","Technical_potential_demand_response_kW","Heat_free_hours","Internal_temperature_decay","Average_outside_temperature","Shiftable_energy_potential_kWh","Average_power_output_kW_lag","Average_heat_output_kW_lag","Coefficient_of_variation_power","Coefficient_of_variation_heat","Work_capacity_percent_lag","Peak_power_output_kW_lag","Peak_heat_output_kW_lag","Sd_power_output_kW_lag","Sd_heat_output_kW_lag","Average_internal_air_temperature","House_Type","House_Form","House_Age","House_SAP","Total_Floor_Area_m2","Heated_rooms","Wall_Type","Floor_Type","Roof_Type","Glazed_Type","HP_Size_kW","Measured_accuracy_percent","MCS_Hloss"))

# Check to see if numerical variables are correlated to one another
str(statistical_df)

# Converting qualitative variables and variables that are character but are actually
# numeric into a factor or numeric class, issue in the dataset is handling missing data
# May need to use the median

statistical_df$date <-ymd(statistical_df$date)
statistical_df$House_Type <- as.factor(statistical_df$House_Type)
statistical_df$House_Form <- as.factor(statistical_df$House_Form)
statistical_df$House_Age <- as.factor(statistical_df$House_Age)
statistical_df$House_SAP <- as.factor(statistical_df$House_SAP)
statistical_df$Total_Floor_Area_m2 <- as.numeric(statistical_df$Total_Floor_Area_m2)
statistical_df$Heated_rooms <- as.numeric(statistical_df$Heated_rooms)
statistical_df$Wall_Type <- as.factor(statistical_df$Wall_Type)
statistical_df$Floor_Type <- as.factor(statistical_df$Floor_Type)
statistical_df$Roof_Type <- as.factor(statistical_df$Roof_Type)
statistical_df$Glazed_Type <- as.factor(statistical_df$Glazed_Type)
statistical_df$MCS_Hloss <- as.numeric(statistical_df$MCS_Hloss)

# Checking that data has formatted correctly 
str(statistical_df)

# Encoding all variables with factor class type into dummy variables using 
# one-hot encoding 

#statistical_df <- dummy_cols(statistical_df, remove_first_dummy = TRUE)

# To avoid dummy variable, removing one column from each categorical variable.
#statistical_df <- statistical_df[, !names(statistical_df) %in% c("House_Type_null","House_Type_Flat","House_Form_Semi-Detached","House_Age_Pre 1919","House_SAP_null","House_SAP_F","Wall_Type_Solid_No_insulation","Floor_Type_Suspended","Roof_Type_Pitched","HP_Model_VWEL 75/6 A 230V S2")]

# Need to determine the number of variables that can be fitted into models given limitation of sample size
n <- n_distinct(statistical_df)
n # 548

# Effect size we are seeking is small-medium given correlation coefficients, as 610 >> 200 and 610 > 600, sample
# size not a huge issue here

# Solving for maximum number of predictors to use k using 50 + 8k for minimum sample size needed Green (1991) to test model
k <- (n-50)/8
k # limited to 62 variables for testing effects of multiple variables which is fine 

# Solving for k using 100 + k for testing individual predictors in regression model
k1 <- (n-104)
k1 # Can test individually 444 predictors which is much greater than the amount 
# we have so may be easier 

## Linear regression 
# Beginning with one at a time linear regression for each variable which showed signifciance during correlation analysis 
# Omitting power output as  it doesn't show anything useufl 

## Linear regression to technical potential for demand response 

# Average heat output   
lm_average_heat_output <- lm(Technical_potential_demand_response_kW ~ Average_heat_output_kW_lag , data = statistical_df)
summary(lm_average_heat_output) # R^2 of 0.1794 # Significant at all confidence levels so will keep

# Finding Beta 1  value from standardised regression coefficeint 
model.beta <- lm.beta(lm_average_heat_output)
model.beta

# Seeing a residual plot of average_power_output
par(mfrow = c(2,2))
plot(lm_average_heat_output)

# Seeing histogram of the residuals
par(mfrow = c(1,1))
hist_heat_residuals <- hist(lm_average_heat_output$residuals, main = "Histogram of residuals")

# Peak power/heat output   
lm_peak_power_heat_output <- lm(Technical_potential_demand_response_kW ~ Peak_power_output_kW_lag , data = statistical_df)
summary(lm_peak_power_heat_output) # R^2 of 0.1794 # Significant at all confidence levels so will keep

# Standard deviation of heat output  
lm_sd_heat_output <- lm(Technical_potential_demand_response_kW ~ Sd_heat_output_kW_lag , data = statistical_df)
summary(lm_sd_heat_output) # R^2 of 0.1794 # Significant at all confidence levels so will keep

# Standard deviation of power output  
lm_sd_power_output <- lm(Technical_potential_demand_response_kW ~ Sd_power_output_kW_lag , data = statistical_df)
summary(lm_sd_power_output) # R^2 of 0.1794 # Significant at all confidence levels so will keep

# Average outside temperature during event
lm_average_outside_temperature <- lm(Technical_potential_demand_response_kW ~ Average_outside_temperature, data = statistical_df)
summary(lm_average_outside_temperature) # R^2 of 0.0643 # Significant at all confidence levels so will keep

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_average_outside_temperature)
model.beta

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm_average_outside_temperature)

# Total Floor Area 
lm_total_floor_area <- lm(Technical_potential_demand_response_kW ~  Total_Floor_Area_m2  , data = statistical_df)
summary(lm_total_floor_area)   # R^2 of 0.09199  # Significant at all confidence levels so will keep

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_total_floor_area)
model.beta

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm_total_floor_area)

# Seeing histogram of the residuals
par(mfrow = c(1,1))
hist_power_residuals <- hist(lm_total_floor_area$residuals, main = "Histogram of residuals")

# HP Size (kW)
lm_hp_size_kw <- lm(Technical_potential_demand_response_kW ~  HP_Size_kW , data = statistical_df)
summary(lm_hp_size_kw)  # R^2 of 0.05612  # Significant at all confidence levels 

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_hp_size_kw)
model.beta

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm_hp_size_kw)

# Seeing histogram of the residuals
par(mfrow = c(1,1))
hist_power_residuals <- hist(lm_hp_size_kw$residuals, main = "Histogram of residuals")

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_hp_size_kw)
model.beta

# Percent work output of heat pumps
lm_percent_work_output <- lm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag , data = statistical_df)
summary(lm_percent_work_output) # R^2 of 0.08738 # Significant at all confidence levels 

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_percent_work_output)
model.beta

# Heated_rooms
lm_heated_rooms <- lm(Technical_potential_demand_response_kW ~ Heated_rooms , data = statistical_df)
summary(lm_heated_rooms) # R^2 of 0.07387  # Significant at all confidence levels 

# House Type
lm_house_type <- lm(Technical_potential_demand_response_kW ~ factor(House_Type) , data = statistical_df)
summary(lm_house_type) # Not significant at any significance level

# Form type
lm_house_form <- lm(Technical_potential_demand_response_kW ~ factor(House_Form) , data = statistical_df)
summary(lm_house_form) # R^2 of 0.08977  # Signficant to the require confidence level for all archetypes other than end terrace houses.

# Kruksal Wallis to see if form stype is a significant predictor
test <- kruskal.test(statistical_df$Technical_potential_demand_response_kW~statistical_df$House_Form)
test

lm_house_form_detached <- lm(Technical_potential_demand_response_kW ~ House_Form_Detached , data = statistical_df)
summary(lm_house_form_detached) # 0.07211  # Detached archetype significant

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_house_form)
model.beta

# House age
lm_house_age <-lm(Technical_potential_demand_response_kW ~ factor(House_Age), data = statistical_df)
summary(lm_house_age) # R^2 of 0.2133 # Only significant for houses with age band of 1930 - 1949 and 1991-2000

# Regression output in R
par(mfrow = c(2,2))
plot(lm_house_age)

# SAP rating 
lm_SAP <-lm(Technical_potential_demand_response_kW ~ factor(House_SAP), data = statistical_df)
summary(lm_SAP) # R^2 of 0.0412  # Only some are significant, suggesting there may be outliers

# Finding Beta 1 value from standardised regression coefficient 
model.beta <- lm.beta(lm_SAP)
model.beta

# Wall type 
lm_wall_type <-lm(Technical_potential_demand_response_kW ~ factor(Wall_Type), data = statistical_df)
summary(lm_wall_type) # R^2 of 0.01761 # Only significant for cavity walls with no insulations but solid walls with no insulation

# Finding Beta 1 value from standardised regression coefficient 
model.beta <- lm.beta(lm_wall_type)
model.beta

# Floor type
lm_floor_type <- lm(Technical_potential_demand_response_kW ~ factor(Floor_Type), data = statistical_df)
summary(lm_floor_type) # R^2 of 0.01136  # Only significant at the 90% confidence level for both floor types in data so will be omitted from model

# Roof type
lm_roof_type <- lm(Technical_potential_demand_response_kW ~ factor(Roof_Type), data = statistical_df)
summary(lm_roof_type) # R^2 of 0.02055 # Significant at the 95% confidence level for both so will be kept in data 

# Finding Beta 1 value from standardised regression coefficient 
model.beta <- lm.beta(lm_roof_type)
model.beta

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_hp_model)
model.beta

## Looking at saved potential energy and interval_mins and other variables from correlation analysis

# Heat pump size to percent work output 
lm2_hp_size <- lm(Work_capacity_percent_lag ~ HP_Size_kW, data = statistical_df)
summary(lm2_hp_size) # R^2 of 0.07384 # Significant at the 95% confidence level for oth so will be kept in data 

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm2_hp_size)
model.beta

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm2_hp_size)

# Checking model to see if it satisfies assumptions for generalisability
check_model(lm2_hp_size)

hist(lm2_hp_size$residuals, main = "Histogram of residuals of work capacity of heat pump and heat pump size ")

# Durbin Watson test to test for independence of errors 
durbinWatsonTest(lm2_hp_size)

# Testing for homoskedasticity 
bp_test <- ncvTest(lm2_hp_size)
print(bp_test)

# Work capacity of heat pump to heat loss
lm2_MCS_Hloss <- lm(Work_capacity_percent_lag ~ MCS_Hloss, data = statistical_df)
summary(lm2_MCS_Hloss) # R^2 of 0.07384 # Significant at the 95% confidence level for both so will be kept in data 

# Period length and internal temperature decay
lm2_run_length_mins <- lm(formula = Heat_free_hours ~ Internal_temperature_decay, data = statistical_df)
summary(lm2_run_length_mins) # R^2 OF 0.1375  #Significant at the 95% confidence level for oth so will be kept in data 

# Work capacity of heat pump to heat loss
lm2_work_capacity<- lm(Work_capacity_percent_lag ~ Average_outside_temperature, data = statistical_df)
summary(lm2_work_capacity) # R^2 of 0.07384 # Significant at the 95% confidence level for both so will be kept in data 

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm2_run_length_mins)

hist(lm2_run_length_mins$residuals, main = "Histogram of peiod length hours to the internal temperature decay ")

# Checking model to see if it satisfies assumptions for generalisability
check_model(lm2_run_length_mins)

# Durbin Watson test to test for independence of errors 
durbinWatsonTest(lm2_run_length_mins)

# Testing for homoskedasticity 
bp_test2 <- ncvTest(lm2_run_length_mins)
print(bp_test2)

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_hp_size_kw)
model.beta

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm2_run_length_mins)

# Plot of histogram of residuals 
hist(lm2_run_length_mins$residuals, main = "Histogram of residauls of period length and internal temperature decay")

# Durbin Watson test to test for independence of errors 
durbinWatsonTest(lm2_run_length_mins)

# Potential energy use against internal temperature decay
lm2_saved_potential_energy_use <- lm(Shiftable_energy_potential_kWh ~ Internal_temperature_decay, data = statistical_df)
summary(lm2_saved_potential_energy_use) # Non significant 

# Potential energy use against internal temperature decay
lm2_building_age <- lm(Shiftable_energy_potential_kWh ~ factor(House_Age), data = statistical_df)
summary(lm2_building_age) # Non significant 

# Period length and SAP rating
lm2_SAP_rating<- lm(Heat_free_hours ~ factor(House_SAP), data = statistical_df)
summary(lm2_SAP_rating) # Non significant 

# Period length and SAP rating
lm2_SAP_rating<- lm(Heat_free_hours ~ factor(Wall_Type), data = statistical_df)
summary(lm2_SAP_rating) # Non significant 

# Period length and SAP rating
lm2_SAP_rating<- lm(Heat_free_hours ~ factor(Roof_Type), data = statistical_df)
summary(lm2_SAP_rating) # Non significant 

# Period length and SAP rating
lm2_SAP_rating<- lm(Heat_free_hours ~ factor(Floor_Type), data = statistical_df)
summary(lm2_SAP_rating) # Non significant 


# Based on above simple regression most significant numerical predictors are average heat and power output,
# outside temperature, total floor area, HP size and percent work output, No. of heated rooms 
# HP model and house age 

# For the saved energy and the length of time that the heat pump can be switched off for 
# the internal temperature decay is significant 

plot_combined <- plot(lm_average_power_output) + plot(lm_average_heat_output)

## Multiple regression 
# Creating multiple regression model which excludes time invariant factors such as form type or the number of heated rooms 
# Always check VIF in each model to ensure models don't introduce multi-collienarity 
# Using Hierarchichal regression 

# Model 
mlm_model <- lm(Technical_potential_demand_response_kW ~ Average_power_output_kW, data = statistical_df)
summary(mlm_model) # R^2 of 0.07384 # Significant at the 95% confidence level for oth so will be kept in data

## Panel & Multiple regression
# Using power output initially for first panel model for fixed effects model 

# Hierarchical model entry for power 
mlm_model1_power <- lm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag , data = statistical_df)
mlm_model2_power <- lm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + Total_Floor_Area_m2, data = statistical_df)
mlm_model3_power <- lm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + Total_Floor_Area_m2 + HP_Size_kW , data = statistical_df)
mlm_model4_power <- lm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + Total_Floor_Area_m2 + HP_Size_kW + Heated_rooms, data = statistical_df)
mlm_model5_power <- lm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + Total_Floor_Area_m2 + HP_Size_kW + Heated_rooms + Average_outside_temperature, data = statistical_df)
mlm_model6_power <- lm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + HP_Size_kW + Heated_rooms + Average_outside_temperature, data = statistical_df)

# Variance inflation factor for model 6
vif_power_model3 <- vif(mlm_model3_power)
vif_power_model3
mean(vif_power_model3)

# Variance inflation factor for model 6
vif_power_model6 <- vif(mlm_model6_power)
vif_power_model6
mean(vif_power_model6)

# Summary results for all power models derived
summary(mlm_model1_power)
summary(mlm_model2_power)
summary(mlm_model3_power)
summary(mlm_model4_power)
summary(mlm_model5_power)
summary(mlm_model6_power)

# Creating a list out of each model 
power_models <- list(mlm_model1_power,mlm_model2_power,mlm_model3_power,mlm_model4_power,mlm_model5_power,mlm_model6_power)

# Defining model names for aictab() function
mod.names <- c('mlm_model1_power','mlm_model2_power','mlm_model3_power','mlm_model4_power','mlm_model5_power','mlm_model6_power')

# Finding AIC of each multiple regression model
aictab(cand.set = power_models, modnames = mod.names) #Final one is the best model

# Finding BIC of each multiple regression model 
bictab(cand.set = power_models, modnames = mod.names) #Final one is the best model even when using BIC 

# Hierarchical model entry for heat 
mlm_model1_heat  <- lm(Technical_potential_demand_response_kW ~ Average_heat_output_kW + Total_Floor_Area_m2 , data = statistical_df)
mlm_model2_heat <- lm(Technical_potential_demand_response_kW ~ Average_heat_output_kW + Total_Floor_Area_m2 + HP_Size_kW, data = statistical_df)
mlm_model3_heat <- lm(Technical_potential_demand_response_kW ~ Average_heat_output_kW + Total_Floor_Area_m2 + HP_Size_kW + Heated_rooms, data = statistical_df)
mlm_model4_heat <- lm(Technical_potential_demand_response_kW ~ Average_heat_output_kW + Total_Floor_Area_m2 + HP_Size_kW + Heated_rooms + Average_outside_temperature, data = statistical_df)
mlm_model5_heat <- lm(Technical_potential_demand_response_kW ~ Average_heat_output_kW + Total_Floor_Area_m2 + HP_Size_kW + Heated_rooms + Average_outside_temperature + House_Form, data = statistical_df)
mlm_model6_heat <- lm(Technical_potential_demand_response_kW ~ Average_heat_output_kW + Total_Floor_Area_m2 + HP_Size_kW + Heated_rooms + Average_outside_temperature + House_Form + Work_capacity_percent, data = statistical_df)
mlm_model7_heat <- lm(Technical_potential_demand_response_kW ~ Average_heat_output_kW + Total_Floor_Area_m2 + HP_Size_kW + Heated_rooms + Average_outside_temperature + Work_capacity_percent, data = statistical_df)

# Computing VIF for final model 
vif_plm_model_OLS <- vif(mlm_model5_heat)
vif_plm_model_OLS

# Computing VIF for final model 
vif_plm_model_OLS <- vif(mlm_model7_heat)
vif_plm_model_OLS

# Finding results of all linear regression models 
summary(mlm_model1_heat)
summary(mlm_model2_heat)
summary(mlm_model3_heat)
summary(mlm_model4_heat)
summary(mlm_model5_heat)
summary(mlm_model6_heat)
summary(mlm_model7_heat)

# Computing AIC for each model 
# Creating a list out of each model 
heat_models <- list(mlm_model1_heat,mlm_model2_heat,mlm_model3_heat,mlm_model4_heat,mlm_model5_heat,mlm_model6_heat,mlm_model7_heat)

# Defining model names for aictab() function
mod.names <- c('Heat+TFA','Heat+TFA+HP_Size','Heat+TFA+HP_Size+Heated_rooms','Heat+TFA+HP_Size+Heated_rooms+avg_outside_temp','Heat+TFA+HP_Size+Heated_rooms+avg_outside_temp+Form_type','Heat+TFA+HP_Size+Heated_rooms+avg_outside_temp+Form_type+Work_capacity','Heat+TFA+HP_Size+Heated_rooms+avg_outside_temp+Work_capacity')

# Finding AIC of each 
aictab(cand.set = heat_models, modnames = mod.names) #Final one is the best model

# Finding BIC of each model 
bictab(cand.set = heat_models, modnames = mod.names) #Final one is the best model even when using BIC

## Polynomial regression for period length and internal temperature decay
# randomly shuffle data
df <- statistical_df
df.shuffled <- df[sample(nrow(df)),]

# Define number of folds to use for k-fold cross-validation
K <- 10 

# Define degree of polynomials to fit
degree <- 5

# Create k equal-sized folds
folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

# Create object to hold MSE's of models
mse = matrix(data=NA,nrow=K,ncol=degree)

# Perform K-fold cross validation
for(i in 1:K){
  
  # Define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  # Use k-fold cv to evaluate models
  for (j in 1:degree){
    fit.train = lm(Heat_free_hours ~ poly(Internal_temperature_decay,j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$Heat_free_hours)^2) 
  }
}

# Find MSE for each degree 
colMeans(mse)

# Running polynomial regression with period length 

lm2_run_length_mins_poly <- lm(formula = Heat_free_hours ~ poly(Internal_temperature_decay,3, raw = T), data = statistical_df)
summary(lm2_run_length_mins_poly) # R^2 OF 0.1375  #Significant at the 95% confidence level for oth so will be kept in data 

## Panel regression 
# Exploring panel data
# Co-plot 
str(statistical_df)
y <- statistical_df$Technical_potential_demand_response_kW
statistical_df$Technical_potential_demand_response_kW <- as.numeric(statistical_df$Technical_potential_demand_response_kW)

statistical_df$date <- as.factor(statistical_df$date)

scatterplot(Technical_potential_demand_response_kW ~ date|Property_ID, boxplots = FALSE, smooth = TRUE, reg.line = FALSE, data = statistical_df)

# Plot of means of the technical potential for demand response across different properties
plotmeans(Technical_potential_demand_response_kW ~ Property_ID, main = "Heterogeneity across different properties", data = statistical_df)

# Alternative representation of heterogeneity across properties
Plot_69 <- ggplot(statistical_df, aes(x = Property_ID, y = Technical_potential_demand_response_kW))+
  geom_boxplot()+
  labs(x = "Property ID", 
       y = "Technical potential for demand response (kW)",
       title = "Heterogeneity across different properties")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
theme(legend.position="none")
Plot_69


# Plot of means of the technical potential for demand response across different dates
plotmeans(Technical_potential_demand_response_kW ~ date, main = "Heterogeneity across different dates", data = statistical_df)

Plot_70 <- ggplot(statistical_df, aes(x = date, y = Technical_potential_demand_response_kW))+
  geom_boxplot()+
  labs(x = "Property ID", 
       y = "Technical potential for demand response (kW)",
       title = "Heterogeneity across different dates")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic"),
    axis.text.x = element_text(angle =90)
  ) 
theme(legend.position="none")
Plot_70

# Using OLS to start of with (also using Hierarchical regression)
plm_model_OLS <- plm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + HP_Size_kW + Heated_rooms + Average_outside_temperature, index =c("Property_ID","date"), data = statistical_df, model = "pooling")
summary(plm_model_OLS)

check_model(plm_model_OLS, panel = FALSE)

#Plot of OLS model
plot(plm_model_OLS)
qqplot()

# Computing VIF of predictors 
vif_plm_model_OLS <- vif(plm_model_OLS)
vif_plm_model_OLS
mean(vif_plm_model)

# pFtest on power and heat based models
pFtest(plm_model_fixed,plm_model_OLS)

# Testing for time fixed effects.
plmtest(plm_model_OLS,c("time"), type=("bp"))

# Now trying for fixed effects: Power model
plm_model_fixed <- plm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + HP_Size_kW + Heated_rooms + Average_outside_temperature , index =c("Property_ID","date"), data = statistical_df, model = "within")
summary(plm_model_fixed)


check_model(plm_model_fixed)

# Now trying for fixed effects no standard deviation of power output
plm_model_fixed <- plm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag  + HP_Size_kW + Heated_rooms + Average_outside_temperature , index =c("Property_ID","date"), data = statistical_df, model = "within")
summary(plm_model_fixed)

# Running fixed-effects model with the date as a factor to see if there are time fixed effects
plm_model_fixed_time <- plm(Technical_potential_demand_response_kW ~ Average_power_output_kW + Total_Floor_Area_m2 + HP_Size_kW + Heated_rooms + Average_outside_temperature + factor(date), index =c("Property_ID","date"), data = statistical_df, model = "within")
summary(plm_model_fixed_time)

# Displaying fixed effects (constants for each country)
fixef(plm_model_fixed)

# Computing VIF of predictors 
vif_plm_model <- vif(plm_model_fixed)
vif_plm_model

# Plot of regression model
plot.plm(plm_model_fixed)

# Now trying for random effects: Power model
plm_model_random <- plm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + HP_Size_kW + Heated_rooms + Average_outside_temperature, index =c("Property_ID","date"), data = statistical_df, model = "random")
summary(plm_model_random)

check_model(plm_model_random)

# Now trying for random effects: Power model without heated rooms
plm_model_random2 <- plm(Technical_potential_demand_response_kW ~ Work_capacity_percent_lag + Sd_power_output_kW_lag + HP_Size_kW  + Average_outside_temperature, index =c("Property_ID","date"), data = statistical_df, model = "random")
summary(plm_model_random2)


## Test diagnostics of panel regression

# Running Hausman test between fixed and random model
phtest(plm_model_fixed,plm_model_random)

# Testing for fixed effects using F-test 
pFtest(plm_model_fixed, plm_model_OLS)

# Testing for time-fixed effects using F-test and Lagrange Multiplier Test for time effects 
pFtest(plm_model_fixed_time, plm_model_fixed)

plmtest_te <- plmtest(plm_model_fixed, c("time"), type =("bp"))
plmtest_te

# Running Breusch-Pagan Lagrange Multiplier on OLS model
plmtest(plm_model_OLS, type = c("bp"))

# Plot of residuals 

## Model validation for both Panel and multiple regression models 
# Testing for homoscedasticity using Breusch-Pagan test
bptest(plm_model_OLS, varformula = NULL, studentize = TRUE, data = list(), weights = NULL)
bptest(plm_model_fixed, varformula = NULL, studentize = TRUE, data = list(), weights = NULL)
bptest(plm_model_random, varformula = NULL, studentize = TRUE, data = list(), weights = NULL)

# Testing for cross sectional dependence 
pcdtest_lm <- pcdtest(plm_model_fixed, test = c("lm"))
pcdtest_lm
pcdtest_cd <- pcdtest(plm_model_fixed, test = c("cd"))
pcdtest_cd

# Test for independence of errors using Durbin-Watson test for panel models 
pdwtest(plm_model_OLS, data = statistical_df)
pdwtest(plm_model_fixed, data = statistical_df)
pdwtest(plm_model_random, data = statistical_df)

# Testing for unit roots/stationarity 
library(tseries)
Panel.set <- plm.data(statistical_df, index = c("Property_ID","date"))
adf.test(Panel.set$Technical_potential_demand_response_kW, k=2)

## Residual analysis of models  

# Plotting residuals of each model 
res_OLS <- as.numeric(resid(plm_model_OLS))
res_fixed <- as.numeric(resid(plm_model_fixed))
res_random <- as.numeric(resid(plm_model_random))

# Fitted values 
fitted_OLS <- as.numeric(fitted(plm_model_OLS))
fitted_fixed <- as.numeric(fitted(plm_model_fixed))
fitted_random <- as.numeric(fitted(plm_model_random))

plot(plm_)

# Produce residual vs. fitted plot
par(mfrow = c(2,2))
plot(fitted_OLS, res_OLS, main = "Fitted values against residuals for OLS pooled model")
abline(0,0 , aes(colour = "red")) 
plot(fitted_fixed, res_fixed, main = "Fitted values against residuals for fixed effects (FE) model")
abline(0,0 , colour = "red")
plot(fitted_random, res_random,  main = "Fitted values against residuals for random effects (RE) model")
abline(0,0 , colour = "red")

# Producing residuals for each panel regression model 
res_OLS <- residuals(plm_model_OLS)
res_fixed <- residuals(plm_model_fixed)
res_random<- residuals(plm_model_random)

# Q-Q plot OLS 
par(mfrow = c(2,2))
qqnorm(res_OLS, main = "Q-Q plot of OLS pooled model")
qqline(res_OLS, col = "red")

# Q-Q plot fixed effects (FE) model 
qqnorm(res_fixed, main = "Q-Q plot of fixed effects (FE) model")
qqline(res_fixed, col = "red")

# Q-Q plot random effects (RE) model 
qqnorm(res_random, main = "Q-Q plot of random effects (RE) model")
qqline(res_random, col = "red")

ggtitle("QQ plot of panel regression models in RStudio")
# Plotting each model together 

# Plotting QQ line for different panel models
par(mfrow = c(2,2))
qqline(res_OLS, col = "red", main = "Q-Q plot of OLS pooled model")
qqline(res_fixed, col = "red", main = "Q-Q plot of fixed effects (FE) model")
qqline(res_random, col = "red", main = "Q-Q plot of random effects (RE) model")

# Checking for distribution of residuals 
par(mfrow = c(2,2))
hist(plm_model_OLS$residuals, main = "Histogram of residuals of OLS pooled model", xlab = "Residuals")
hist(plm_model_fixed$residuals, main = "Histogram of residuals of fixed effects (FE) model", xlab = "Residuals")
hist(plm_model_random$residuals, main = "Histogram of residuals of random effects (RE) model", xlab = "Residuals")

hist(rstudent(plm_model_OLS))

resid_panel(plm_model_fixed)

plot(plm_model_OLS)
plot(plm_model_fixed)
plot(plm_model_random)

# (Torres-Reyna, 2010) approach to plotting residuals 

# Finding influential points 
cooksd <- cooks.distance(lm2_run_length_mins)

# Create a data frame for plotting
influence_data <- data.frame(
  index = 1:length(cooksd),
  cooksd = cooksd
)

# Plot Cook's Distance
ggplot(influence_data, aes(x = index, y = cooksd)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 4 / (nrow(mtcars) - length(coef(lm2_run_length_mins))), 
             color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance for Influential Observations",
       x = "Observation Index",
       y = "Cook's Distance") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(size = 10)
  ) 

# Print Cook's Distance values (optional)
print(cooksd)
#########################################################################################

## Additional regression to shiftable energy kWh 

# Average heat output   
lm_average_heat_output_kWh <- lm(Saved_potential_energy_use_kWh ~ Average_heat_output_kW_lag , data = statistical_df)
summary(lm_average_heat_output_kWh) # R^2 of 0.1794 # Significant at all confidence levels so will keep

# Finding Beta 1  value from standardised regression coefficeint 
model.beta <- lm.beta(lm_average_heat_output_kWh)
model.beta

# Seeing a residual plot of average_power_output
par(mfrow = c(2,2))
plot(lm_average_heat_output_kWh)

# Seeing histogram of the residuals
par(mfrow = c(1,1))
hist_heat_residuals <- hist(lm_average_heat_output_kWh$residuals, main = "Histogram of residuals")

# Peak power/heat output   
lm_peak_power_heat_output_kWh <- lm(Saved_potential_energy_use_kWh ~ Peak_power_output_kW_lag , data = statistical_df)
summary(lm_peak_power_heat_output_kWh) 

# Standard deviation of heat output  
lm_sd_heat_output_kWh <- lm(Saved_potential_energy_use_kWh ~ Sd_heat_output_kW_lag , data = statistical_df)
summary(lm_sd_heat_output_kWh) 

# Standard deviation of power output  
lm_sd_power_output_kWh <- lm(Saved_potential_energy_use_kWh ~ Sd_power_output_kW_lag , data = statistical_df)
summary(lm_sd_power_output_kWh) # R^2 of 0.1794 # Significant at all confidence levels so will keep

# Average outside temperature during event
lm_average_outside_temperature_kWh <- lm(Saved_potential_energy_use_kWh ~ Average_outside_temperature, data = statistical_df)
summary(lm_average_outside_temperature_kWh) # R^2 of 0.0643 # Significant at all confidence levels so will keep

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_average_outside_temperature_kWh)
model.beta

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm_average_outside_temperature_kWh)

# Total Floor Area 
lm_total_floor_area_kWh <- lm(Saved_potential_energy_use_kWh ~  Total_Floor_Area_m2  , data = statistical_df)
summary(lm_total_floor_area_kWh)   # R^2 of 0.09199  # Significant at all confidence levels so will keep

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_total_floor_area_kWh)
model.beta

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm_total_floor_area_kWh)

# Seeing histogram of the residuals
par(mfrow = c(1,1))
hist_power_residuals <- hist(lm_total_floor_area_kWh$residuals, main = "Histogram of residuals")

# HP Size (kW)
lm_hp_size_kWh <- lm(Saved_potential_energy_use_kWh ~  HP_Size_kW , data = statistical_df)
summary(lm_hp_size_kWh)  # R^2 of 0.05612  # Significant at all confidence levels 

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_hp_size_kWh)
model.beta

# Residual plot of outside temperature
par(mfrow = c(2,2))
plot(lm_hp_size_kWh)

# Seeing histogram of the residuals
par(mfrow = c(1,1))
hist_power_residuals <- hist(lm_hp_size_kWh$residuals, main = "Histogram of residuals")

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_hp_size_kWh)
model.beta

# Percent work output of heat pumps
lm_percent_work_output_kWh <- lm(Saved_potential_energy_use_kWh ~ Work_capacity_percent_lag , data = statistical_df)
summary(lm_percent_work_output_kWh) # R^2 of 0.08738 # Significant at all confidence levels 

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_percent_work_output_kWh)
model.beta

# Heated_rooms
lm_heated_rooms_kWh <- lm(Saved_potential_energy_use_kWh ~ Heated_rooms , data = statistical_df)
summary(lm_heated_rooms_kWh) # R^2 of 0.07387  # Significant at all confidence levels 

# House Type
lm_house_type_kWh <- lm(Saved_potential_energy_use_kWh ~ factor(House_Type) , data = statistical_df)
summary(lm_house_type_kWh) # Not significant at any significance level

# Form type
lm_house_form_kWh <- lm(Saved_potential_energy_use_kWh ~ factor(House_Form) , data = statistical_df)
summary(lm_house_form_kWh) # R^2 of 0.08977  # Signficant to the require confidence level for all archetypes other than end terrace houses.

# Kruksal Wallis to see if form stype is a significant predictor
test <- kruskal.test(statistical_df$Saved_potential_energy_use_kWh~statistical_df$House_Form)
test

lm_house_form_detached_kWh <- lm(Saved_potential_energy_use_kWh ~ House_Form_Detached , data = statistical_df)
summary(lm_house_form_detached_kWh) # 0.07211  # Detached archetype significant

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_house_form_kWh)
model.beta

# House age
lm_house_age_kWh <-lm(Saved_potential_energy_use_kWh ~ factor(House_Age), data = statistical_df)
summary(lm_house_age_kWh) # R^2 of 0.2133 # Only significant for houses with age band of 1930 - 1949 and 1991-2000

# Regression output in R
par(mfrow = c(2,2))
plot(lm_house_age_kWh)

# SAP rating 
lm_SAP_kWh <-lm(Saved_potential_energy_use_kWh ~ factor(House_SAP), data = statistical_df)
summary(lm_SAP_kWh) # R^2 of 0.0412  # Only some are significant, suggesting there may be outliers

# Finding Beta 1 value from standardised regression coefficient 
model.beta <- lm.beta(lm_SAP)
model.beta

# Wall type 
lm_wall_type_kWh <-lm(Saved_potential_energy_use_kWh ~ factor(Wall_Type), data = statistical_df)
summary(lm_wall_type_kWh) # R^2 of 0.01761 # Only significant for cavity walls with no insulations but solid walls with no insulation

# Finding Beta 1 value from standardised regression coefficient 
model.beta <- lm.beta(lm_wall_type)
model.beta

# Floor type
lm_floor_type_kWh <- lm(Saved_potential_energy_use_kWh ~ factor(Floor_Type), data = statistical_df)
summary(lm_floor_type_kWh) # R^2 of 0.01136  # Only significant at the 90% confidence level for both floor types in data so will be omitted from model

# Roof type
lm_roof_type_kWh <- lm(Saved_potential_energy_use_kWh ~ factor(Roof_Type), data = statistical_df)
summary(lm_roof_type_kWh) # R^2 of 0.02055 # Significant at the 95% confidence level for both so will be kept in data 

# Finding Beta 1 value from standardised regression coefficient 
model.beta <- lm.beta(lm_roof_type_kWh)
model.beta

# Finding Beta 1 value from standardised regression coefficeint 
model.beta <- lm.beta(lm_hp_model_kWh)
model.beta

# Internal air temperature
lm2_avg_internal_air_temp_kWh <- lm(Saved_potential_energy_use_kWh ~ Average_internal_air_temperature, data = analysis_df)
summary(lm2_avg_internal_air_temp_kWh) # R^2 of 0.07384 # Significant at the 95% confidence level for oth so will be kept in data 
