library(tidyverse)
library(class)
library(readr)
library(caret)
library(e1071)
library(ggplot2)

url <- "https://raw.githubusercontent.com/businessdatasolutions/courses/main/data%20mining/gitbook/datasets/breastcancer.csv"
rawDF <- read_csv(url)

cleanDF <- rawDF[-1]

cleanDF$diagnosis <- factor(cleanDF$diagnosis, levels= c("B", "M"), labels = c("Benign", "Malignant")) %>% relevel("Malignant")

cntDiag <- table(cleanDF$diagnosis)
percDiag <- round(prop.table(cntDiag) * 100, digits=1)

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

nCols <- dim(cleanDF)[2]
cleanDF_n <- sapply(2:nCols,
                    function(x) {
                        normalize(cleanDF[,x])
}) %>% as.data.frame()

trainDF_feat <- cleanDF_n[1:469,  ]
testDF_feat <- cleanDF_n[470:569,  ]

trainDF_labels <- cleanDF[1:469,  1]
testDF_labels <- cleanDF[470:569,  1]

cleanDF_test_pred <- knn(train = as.matrix(trainDF_feat), test = as.matrix(testDF_feat), cl = as.matrix(trainDF_labels), k = 21)

cleanDF_test_pred <- factor(cleanDF_test_pred, levels = c("Malignant", "Benign"))

cm <- confusionMatrix(cleanDF_test_pred, testDF_labels[[1]], positive = NULL, dnn = c("Prediction", "True"))

fourfoldplot(cm$table, color = c("#d62643", "#3e9e76"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
