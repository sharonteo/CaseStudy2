# Holt_Dampened_Trend
Model_holt_3 <- holt(TotalAsIs, damped=TRUE,h=12)
summary(Model_holt_3)
plot(Model_holt_3)
