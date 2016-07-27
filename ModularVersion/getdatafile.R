# Function to import the csv data file
getcsvfile <- function(csvfilename) {
  return (read.csv(file=csvfilename, header = F, sep=";", fill = T))
}

ImportedAsIsData <- getcsvfile("Data/ImportedAsIsDataChulwalar.csv")
ImportedPlanData <- getcsvfile("Data/ImportedPlanDataChulwalar.csv")
ImportedIndicators <- getcsvfile("Data/ImportedIndicatorsChulwalar.csv")
