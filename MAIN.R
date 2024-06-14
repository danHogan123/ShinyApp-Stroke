library(readr)

# Making sure we can read the data
strokeDataSet <- read_csv("healthcare-dataset-stroke-data.csv")
strokeDataSet


model1 <- glm(stroke ~ age, data = strokeDataSet, family = binomial())
model1
