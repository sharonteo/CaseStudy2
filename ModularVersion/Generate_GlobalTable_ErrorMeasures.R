# Generate_GlobalTable_ErrorMeasures

d1 <- matrix(ncol=8, nrow=8)
sestble <- NULL
h1tble <- NULL
h2tble <- NULL
h3tble <- NULL
h4tble <- NULL
hw1tble <- NULL
hw2tble <- NULL
ErrorMeasuresTable <- NULL
d1[1,1] <- cbind("Models", "ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1")
sestble <- summary.data.frame(Model_ses)
#d1[2,1] <- sestble[1,1]
#sestble <- summary.data.frame(Model_ses)
for (i in 1:18) {
  res <- as.numeric(ldply(strsplit(sestble[1], split = " "))[[i]])
  if (!is.na(res)) {
    cat("res ", res)
    d1[2, i] <- res
  }
}
h1tble <- summary.data.frame(Model_holt_1)
for (i in 1:18) {
  res <- as.numeric(ldply(strsplit(h1tble[1], split = " "))[[i]])
  if (!is.na(res)) {
    cat("res ", res)
    d1[3, i] <- res
  }
}
res <- ldply(strsplit(h1tble[1], split = " "))[[12]]
#strsplit(gsub("([[:alnum:]]{13})", "\\1 ", h1tble[1]), " ")[[1]]
#h2tble <- summary.data.frame(Model_holt_2)
#h3tble <- summary.data.frame(Model_holt_3)
#h4tble <- summary.data.frame(Model_holt_4)
#hw1tble <- summary.data.frame(Model_hw_1)
#hw2tble <- summary.data.frame(Model_hw_2)
#ErrorMeasuresTable <- rbind(hdrtble[1:7], sestble[1,1], h1tble[1,1], h2tble[1,1], h3tble[1,1], h4tble[1,1], hw1tble[1,1], hw2tble[1,1])
