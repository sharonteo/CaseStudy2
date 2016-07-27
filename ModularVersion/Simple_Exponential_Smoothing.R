# Simple_Exponential_Smoothing
# Model
# Forecasting models with smoothing and related approaches
# Exponential Smoothing uses past values to calculate a forecast.
# The strength with which each value influences the forecast is weakened
# with help of a smoothing parameter. Thus we are dealing with a weighted average,
# whose values fade out the longer ago they were in the past.
# The Akaike's Information Criterion(AIC/AICc) or the Bayesian Information Criterion (BIC)
# should be at minimum.

## Simple expontential smoothing
Model_ses <- ses(TotalAsIs, h=12)
summary(Model_ses)
plot(Model_ses, plot.conf=FALSE, ylab="Exports Chulwalar  )", xlab="Year", main="", fcol="white", type="o")
lines(fitted(Model_ses), col="green", type="o")
lines(Model_ses$mean, col="blue", type="o")
legend("topleft",lty=1, col=c(1,"green"), c("data", expression(alpha == 0.671)),pch=1)
