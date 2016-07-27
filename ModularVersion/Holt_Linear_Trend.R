# Holt_Linear_Trend
# Holt's linear trend method
Model_holt_1 <- holt(TotalAsIs,h=12)
summary(Model_holt_1)
plot(Model_holt_1)
