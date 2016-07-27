# Holt_Dampened_Exponential_Trend
# Holt's Dampened Exponential trend
Model_holt_4 <- holt(TotalAsIs, exponential=TRUE, damped=TRUE,h=12)
summary(Model_holt_4)
plot(Model_holt_4)
