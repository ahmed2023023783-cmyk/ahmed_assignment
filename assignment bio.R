


library(kknn)
library(caret)


bc_data <- read.csv("wdbc.data", header = FALSE)


colnames(bc_data) <- c("ID", "Diagnosis", 
                       "Radius_mean", "Texture_mean", "Perimeter_mean", "Area_mean", "Smoothness_mean",
                       "Compactness_mean", "Concavity_mean", "ConcavePoints_mean", "Symmetry_mean", "FractalDimension_mean",
                       "Radius_se", "Texture_se", "Perimeter_se", "Area_se", "Smoothness_se",
                       "Compactness_se", "Concavity_se", "ConcavePoints_se", "Symmetry_se", "FractalDimension_se",
                       "Radius_worst", "Texture_worst", "Perimeter_worst", "Area_worst", "Smoothness_worst",
                       "Compactness_worst", "Concavity_worst", "ConcavePoints_worst", "Symmetry_worst", "FractalDimension_worst")


head(bc_data)
str(bc_data)
summary(bc_data)
sum(is.na(bc_data))
table(bc_data$Diagnosis)




par(mfrow=c(1,1))  
barplot(table(bc_data$Diagnosis),
        main="Diagnosis Distribution",
        xlab="Diagnosis",
        ylab="Count",
        col=c("green", "red"),
        cex.names=1.5,  
        cex.axis=1.5,    
        cex.lab=1.5,     
        cex.main=2)      


plot(bc_data$Radius_mean, bc_data$Area_mean,
     col = ifelse(bc_data$Diagnosis == "B", "green", "red"),
     pch = 19,
     xlab = "Radius_mean",
     ylab = "Area_mean",
     main = "Scatter Plot: Radius_mean vs Area_mean",
     cex.lab=1.5,
     cex.axis=1.5,
     cex.main=2,
     cex=1.5)   
legend("topright", legend=c("Benign","Malignant"), col=c("green","red"), pch=19, cex=1.5)

















# part2


bc_data$Diagnosis <- as.factor(bc_data$Diagnosis)


bc_data$ID <- NULL




set.seed(123)  
train_index <- createDataPartition(bc_data$Diagnosis, p = 0.7, list = FALSE)
train_data <- bc_data[train_index, ]
test_data <- bc_data[-train_index, ]


cat("Training set:", nrow(train_data), "instances\n")
cat("Testing set:", nrow(test_data), "instances\n")



features <- names(bc_data)[names(bc_data) != "Diagnosis"]

knn_model <- kknn(Diagnosis ~ ., 
                  train = train_data[, c("Diagnosis", features)],
                  test = test_data[, c("Diagnosis", features)],
                  k = 5, kernel = "rectangular")


summary(knn_model)


pred <- fitted(knn_model)


pred[1:10]

















# Part 3

conf_mat <- table(Predicted = pred, Actual = test_data$Diagnosis)
cat("Confusion Matrix:\n")
print(conf_mat)




TN <- conf_mat["B","B"]
FP <- conf_mat["B","M"]
FN <- conf_mat["M","B"]
TP <- conf_mat["M","M"]


accuracy <- (TP + TN) / sum(conf_mat)


precision <- TP / (TP + FP)


recall <- TP / (TP + FN)


f1_score <- 2 * (precision * recall) / (precision + recall)


cat("\nModel Performance Metrics:\n")
cat("Accuracy :", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall   :", round(recall, 4), "\n")
cat("F1-Score :", round(f1_score, 4), "\n")