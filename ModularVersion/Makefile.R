###########
# R make-like file for Case Study 2
# Harry B, Sharon T, Oscar P
# Created: 25 July 2016
###########

# Set working directory
setwd("C:/Harry/Data_Science/SMU/MSDS_6306_4033/Week10/20160725_case2")

# Load the required libraries and respected packages
source("loadlibpkg.R")

# compareModels <- NULL

# Get the data files into data frames
source("getdatafile.R")

# Data Cleanup
source("Chulwalar_DataCleanup.R")

# Appendix - useful data which might have impact on the forecast
source("Chulwalar_Appendix.R")

# Here we do data analysis in preparation for forecasting
source("Do_DataAnalysis.R")

# Here we do forecasting using different models
## Simple expontential smoothing
source("Simple_Exponential_Smoothing.R")

## Holt's linear trend
source("Holt_Linear_Trend.R")

# expoential trend
source("Holt_Exponential_Trend.R")

# Holt's Dampened trend
source("Holt_Dampened_Trend.R")

# Holt's Dampened Exponential trend
source("Holt_Dampened_Exponential_Trend.R")

# Plotting levels and slopes of all the models here
source("Plot_Level_Slope_Models.R")

## Holt-Winter's seasonal additive method
source("HoltW_Seasonal_Additive.R")

## Holt-Winter's seasonal multiplicative method
source("HoltW_Seasonal_Multiplicative.R")

# Here we plot Holt-Winter's methods
source("Plot_HoltW_Seasonal_Methods.R")

# Convert into point forecast
source("Point_Forecast_Conversion.R")

# Compare Models now
source("Compare_Models.R")
