# Function to compare models
#Compare_Models <- function(this_model) {
#  this_model_df <- as.data.frame(this_model)
#  THIS <- ts(this_model_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
#  compareModels <- ts.union(TotalAsIs_2014, THIS)
#  RMSE_THIS <- rmse(TotalAsIs_2014, THIS)
#  return (RMSE_THIS)
#}

# Compare
#RMSE_SES <- Compare_Models(Model_ses)
#RMSE_LINEAR <- Compare_Models(Model_holt_1)
#RMSE_EXPO <- Compare_Models(Model_holt_2)
#RMSE_DAMPED <- Compare_Models(Model_holt_3)
#RMSE_DAMPEXPO <- Compare_Models(Model_holt_4)
#RMSE_HW_ADD <- Compare_Models(Model_hw_1)
#RMSE_HW_MUL <- Compare_Models(Model_hw_2)

# Now we will compare our model against actual 2014 figures
Model_ses_df <- as.data.frame(Model_ses) 
SES <- ts(Model_ses_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
compareModels <- ts.union(TotalAsIs_2014, SES)
RMSE_SES <- rmse(TotalAsIs_2014, SES)
RMSE_SES

Model_holt1_df <- as.data.frame(Model_holt_1) 
Holt_Linear <- ts(Model_holt1_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
compareModels <- ts.union(compareModels, Holt_Linear)
RMSE_LINEAR <- rmse(TotalAsIs_2014, Holt_Linear)
RMSE_LINEAR

Model_holt2_df <- as.data.frame(Model_holt_2) 
Holt_Exponential <- ts(Model_holt2_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
compareModels <- ts.union(compareModels, Holt_Exponential)
RMSE_EXPO <- rmse(TotalAsIs_2014, Holt_Exponential)
RMSE_EXPO

Model_holt3_df <- as.data.frame(Model_holt_3) 
Damped <- ts(Model_holt3_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
compareModels <- ts.union(compareModels, Damped)
RMSE_DAMPED <- rmse(TotalAsIs_2014, Damped)
RMSE_DAMPED

Model_holt4_df <- as.data.frame(Model_holt_4) 
DampedExpo <- ts(Model_holt4_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
compareModels <- ts.union(compareModels, DampedExpo)
RMSE_DAMPEXPO <- rmse(TotalAsIs_2014, DampedExpo)
RMSE_DAMPEXPO

Model_hw_1_df <- as.data.frame(Model_hw_1) 
HW_Add <- ts(Model_hw_1_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
compareModels <- ts.union(compareModels, HW_Add)
RMSE_HW_ADD <- rmse(TotalAsIs_2014, HW_Add)
RMSE_HW_ADD

Model_hw_2_df <- as.data.frame(Model_hw_2) 
HW_Mul <- ts(Model_hw_2_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
compareModels <- ts.union(compareModels, HW_Mul)
RMSE_HW_MUL <- rmse(TotalAsIs_2014, HW_Mul)
RMSE_HW_MUL

colnames(compareModels) <- c("TotalAsIs2014", "SES", "Holt_Linear", "Holt_Exponential", "Damped", "DampedExpo", "HW_Add", "HW_Mul")

# Plot the models for comparison
plot(Model_hw_1, ylab = "Exports Chulwalar", plot.conf=FALSE, fcol="white", xlab="Year")
lines(fitted(Model_hw_1), col="blue", lty=2)
lines(fitted(Model_hw_2), col="green", lty=2)
lines(Model_hw_1$mean, type="o", col="blue")
lines(Model_hw_2$mean, type="o", col="green")
lines(TotalAsIs_2014, col = "red")
legend("topleft",lty=1, pch=1, col=1:3, c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))

write.csv(compareModels, file='ChulwalarForecastModelsComparison.csv')

# Summarize results
# Holt-Winter's Multiplicative model has the lowest RMSE (235296.6) when measured against historical (test) data.
# However it's only the second best peformance against 2014 actual data 311363.4
# Holt-Winter's Additive model is the best performer predicting 2014 data (RMSE = 271816.7)

rmse_labels <- c("SES", "Holt_Linear", "Holt_Exponential", "Damped", "DampedExpo", "HW_Add", "HW_Mul")
rmse_values <- c(RMSE_SES, RMSE_LINEAR, RMSE_EXPO, RMSE_DAMPED, RMSE_DAMPEXPO, RMSE_HW_ADD, RMSE_HW_MUL)
RMSE_table <- data.frame(rmse_labels, rmse_values)
RMSE_table
g <- ggplot(RMSE_table, aes(rmse_labels, rmse_values))
g + geom_bar(stat = "identity") + scale_fill_grey()

compareModels <- as.data.frame(compareModels)
# attach(compareModels)
compareModels$SES_error <- TotalAsIs_2014 - SES
compareModels$HotlLinear_error <- TotalAsIs_2014 - Holt_Linear
compareModels$HotlExponential_error <- TotalAsIs_2014 - Holt_Exponential
compareModels$Damped_error <- TotalAsIs_2014 - Damped
compareModels$DampedExpo_error <- TotalAsIs_2014 - DampedExpo
compareModels$HW_Add_error <- TotalAsIs_2014 - HW_Add
compareModels$HW_Mul_error <- TotalAsIs_2014 - HW_Mul
errors_comp <- compareModels[, 9:15]
boxplot(errors_comp, cex.axis=0.7, las = 2)

# Boxplot confirms that the Holt-Winter's models have the best performance vs. 2014 actual data.
