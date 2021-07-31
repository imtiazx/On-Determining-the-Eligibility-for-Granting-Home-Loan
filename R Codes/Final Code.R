# Read the data files

train_df = read.csv("C:/Users/benze/Dropbox/Data/train.csv", header = T, na.strings = c("", "NA"))
test_df = read.csv("C:/Users/benze/Dropbox/Data/test.csv", header = T, na.strings = c("", "NA"))

# Changing from character to numeric (train data)

train_data = subset(train_df, select = -c(Loan_ID))
train_data$Gender <- ifelse(train_data$Gender == "Male", 1, 0)
train_data$Married <- ifelse(train_data$Married == "Yes", 1, 0)
dependents <- c("0" = 0, "1" = 1, "2" = 2, "3+" = 3)
train_data$Dependents <- dependents[train_data$Dependents]
train_data$Education <- ifelse(train_data$Education == "Graduate", 1, 0)
train_data$Self_Employed <- ifelse(train_data$Self_Employed == "Yes", 1, 0)
area <- c("Urban" = 2, "Semiurban" = 1, "Rural" = 0)
train_data$Property_Area <- area[train_data$Property_Area]
train_data$Loan_Status <- ifelse(train_data$Loan_Status == "Y", 1, 0)

# Changing from character to numeric (test data)

test_data = subset(test_df, select = -c(Loan_ID))
test_data$Gender <- ifelse(test_data$Gender == "Male", 1, 0)
test_data$Married <- ifelse(test_data$Married == "Yes", 1, 0)
dependents <- c("0" = 0, "1" = 1, "2" = 2, "3+" = 3)
test_data$Dependents <- dependents[test_data$Dependents]
test_data$Education <- ifelse(test_data$Education == "Graduate", 1, 0)
test_data$Self_Employed <- ifelse(test_data$Self_Employed == "Yes", 1, 0)
area <- c("Urban" = 2, "Semiurban" = 1, "Rural" = 0)
test_data$Property_Area <- area[test_data$Property_Area]

attach(train_data)

# Imputing with median

m1 <- median(LoanAmount, na.rm = T)
train_data[is.na(train_data$LoanAmount), "LoanAmount"] <-  m1
m11 <- median(test_data$LoanAmount, na.rm = T)
test_data[is.na(test_data$LoanAmount), "LoanAmount"] <- m11
m2 <- median(Loan_Amount_Term, na.rm = T)
train_data[is.na(train_data$Loan_Amount_Term), "Loan_Amount_Term"] <-  m2
m22 <- median(test_data$Loan_Amount_Term, na.rm = T)
test_data[is.na(test_data$Loan_Amount_Term), "Loan_Amount_Term"] = m22

# Function to calculate mode

Mode <- function(x, na.rm)
{
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

# Imputing with mode

m3 <- Mode(Gender, na.rm = T)
train_data[is.na(train_data$Gender), "Gender"] <-  m3
m33 <- Mode(test_data$Gender, na.rm = T)
test_data[is.na(test_data$Gender), "Gender"] = m33
m4 <- Mode(Married, na.rm = T)
train_data[is.na(train_data$Married), "Married"] <-  m4
m44 <- Mode(test_data$Married, na.rm = T)
test_data[is.na(test_data$Married), "Married"] = m44
m5 <- Mode(Dependents, na.rm = T)
train_data[is.na(train_data$Dependents), "Dependents"] <-  m5
m55 <- Mode(test_data$Dependents, na.rm = T)
test_data[is.na(test_data$Dependents), "Dependents"] = m55
m6 <- Mode(Self_Employed, na.rm = T)
train_data[is.na(train_data$Self_Employed), "Self_Employed"] <-  m6
m66 <- Mode(test_data$Self_Employed, na.rm = T)
test_data[is.na(test_data$Self_Employed), "Self_Employed"] = m66
m7 <- Mode(Credit_History, na.rm = T)
train_data[is.na(train_data$Credit_History), "Credit_History"] <-  m7
m77 <- Mode(test_data$Credit_History, na.rm = T)
test_data[is.na(test_data$Credit_History), "Credit_History"] = m77

train_data$Gender <- ifelse(train_data$Gender == "1", 1, 0)
train_data$Married <- ifelse(train_data$Married == "1", 1, 0)
dependents <- c("0" = 0, "1" = 1, "2" = 2, "3" = 3)
train_data$Dependents <- dependents[train_data$Dependents]
train_data$Self_Employed <- ifelse(train_data$Self_Employed == "1", 1, 0)
train_data$Credit_History <- ifelse(train_data$Credit_History == "1", 1, 0)

test_data$Gender <- ifelse(test_data$Gender == "1", 1, 0)
dependents <- c("0" = 0, "1" = 1, "2" = 2, "3" = 3)
test_data$Dependents <- dependents[test_data$Dependents]
test_data$Self_Employed <- ifelse(test_data$Self_Employed == "1", 1, 0)
test_data$Credit_History <- ifelse(test_data$Credit_History == "1", 1, 0)

# Plots of categorical variables

require(gridExtra)
library(ggplot2)

plot1 <- ggplot(data = train_data) + geom_bar(aes(x = Gender), fill = "light blue") +
  ggtitle(label = "Barplot of Gender") +
  theme_gray()

plot2 <- ggplot(data = train_data) + geom_bar(aes(x = Married), fill = "light blue") +
  ggtitle(label = "Barplot of Married") +
  theme_gray()

plot3 <- ggplot(data = train_data) + geom_bar(aes(x = Self_Employed), fill = "light blue") +
  ggtitle(label = "Barplot of Self_Employed") +
  theme_gray()

plot4 <- ggplot(data = train_data) + geom_bar(aes(x = Loan_Status), fill = "light blue") +
  ggtitle(label = "Barplot of Loan_Status") +
  theme_gray()

grid.arrange(plot1, plot2, plot3, plot4,  ncol = 2)

# Plots of ordinal variables

plot5 <- ggplot(data = train_data) + geom_bar(aes(x = Dependents), fill = "light sea green") +
  ggtitle(label = "Barplot of Dependents") +
  theme_gray()

plot6 <- ggplot(data = train_data) + geom_bar(aes(x = Education), fill = "light sea green") +
  ggtitle(label = "Barplot of Education") +
  theme_gray()

plot7 <- ggplot(data = train_data) + geom_bar(aes(x = Credit_History), fill = "light sea green") +
  ggtitle(label = "Barplot of Credit_History") +
  theme_gray()

plot8 <- ggplot(data = train_data) + geom_bar(aes(x = Property_Area), fill = "light sea green") +
  ggtitle(label = "Barplot of Property_Area") +
  theme_gray()

grid.arrange(plot5, plot6, plot7, plot8,  ncol = 2)

# Plots of numerical variables

plot9 <- ggplot(data = train_data) + geom_boxplot(aes(y = ApplicantIncome), fill = "plum") +
  ggtitle(label = "Boxplot of ApplicantIncome") +
  theme_gray()

plot10 <- ggplot(data = train_data) + geom_boxplot(aes(y = CoapplicantIncome), fill = "plum") +
  ggtitle(label = "Boxplot of CoapplicantIncome") +
  theme_gray()

plot11 <- ggplot(data = train_data) + geom_boxplot(aes(y = LoanAmount), fill = "plum") +
  ggtitle(label = "Boxplot of LoanAmount") +
  theme_gray()

grid.arrange(plot9, plot10, plot11, ncol = 3)

# Plot of Loan_Amount_Term

ggplot(data = train_data) + geom_bar(aes(x = Loan_Amount_Term), fill = "cadetblue") +
  ggtitle(label = "Barplot of Loan_Amount_Term") +
  theme_gray()

# Bivariate Analysis

counts1 = table(Loan_Status, Gender)
gender <- c(rep("Female", 2), rep("Male", 2))
loan_status <- c(rep(c("No", "Yes"), 2))
values1 <- c(counts1[1:2], counts1[3:4])
data1 <- data.frame(loan_status, gender, values1)
plot13 <- ggplot(data = data1, aes(fill = gender, y = values1, x = loan_status)) +
  geom_bar(position = "dodge", stat="identity") + theme_gray() +
  ggtitle(label = "Grouped barplot of Gender vs Loan_Status")


counts2 = table(Loan_Status, Married)
married <- c(rep("No", 2), rep("Yes", 2))
loan_status <- c(rep(c("No", "Yes"), 2))
values2 <- c(counts2[1:2], counts2[3:4])
data2 <- data.frame(loan_status, married, values2)
plot14 <- ggplot(data = data2, aes(fill = married, y = values2, x = loan_status)) +
  geom_bar(position = "dodge", stat = "identity") + theme_gray() +
  ggtitle(label = "Grouped barplot of Married vs Loan_Status")

counts3 = table(Loan_Status, Self_Employed)
self_employed  <- c(rep("No", 2), rep("Yes", 2))
loan_status <- c(rep(c("No", "Yes"), 2))
values3 <- c(counts3[1:2], counts3[3:4])
data3 <- data.frame(loan_status, self_employed, values3)
plot15 <- ggplot(data = data3, aes(fill = self_employed, y = values3, x = loan_status)) +
  geom_bar(position = "dodge", stat = "identity") + theme_gray() +
  ggtitle(label = "Grouped barplot of Self_Employed vs Loan_Status")

counts4 = table(Loan_Status, Dependents)
dependents  <- c(rep("0", 2), rep("1", 2), rep("2", 2), rep("3+", 2))
loan_status2 <- rep(c(rep(c("No", "Yes"), 2)), 2)
values4 <- c(counts4[1:2], counts4[3:4], counts4[5:6], counts4[7:8])
data4 <- data.frame(loan_status2, dependents, values4)
plot16 <- ggplot(data = data4, aes(fill = dependents, y = values4, x = loan_status2)) +
  geom_bar(position = "dodge", stat = "identity") + theme_gray() +
  ggtitle(label = "Grouped barplot of Dependets vs Loan_Status")

counts5 = table(Loan_Status, Education)
education  <- c(rep("No", 2), rep("Yes", 2))
loan_status <- c(rep(c("No", "Yes"), 2))
values5 <- c(counts5[1:2], counts5[3:4])
data5 <- data.frame(loan_status, education, values5)
plot17 <- ggplot(data = data5, aes(fill = education, y = values5, x = loan_status)) +
  geom_bar(position = "dodge", stat = "identity") + theme_gray() +
  ggtitle(label = "Grouped barplot of Education vs Loan_Status")

counts6 <- table(Loan_Status, Property_Area)
property_area <- c(rep("Rural", 2), rep("Semi-urban", 2), rep("Urban", 2))
loan_status3 <- rep(c("No", "Yes"), 3)
values6 <- c(counts6[1:2], counts6[3:4], counts6[5:6])
data6 <- data.frame(loan_status3, property_area, values6)
plot18 <- ggplot(data = data6, aes(fill = property_area, y = values6, x = loan_status3)) +
  geom_bar(position = "dodge", stat = "identity") + theme_gray() +
  ggtitle(label = "Grouped barplot of Property_Area vs Loan_Status")

grid.arrange(plot13, plot14, plot15, plot16, plot17, plot18,  ncol = 2)

counts7 = table(Loan_Status, Credit_History)
credit_history <- c(rep("0", 2), rep("1", 2))
loan_status <- c(rep(c("No", "Yes"), 2))
values7 <- c(counts7[1:2], counts7[3:4])
data7 <- data.frame(loan_status, credit_history, values7)
plot19 <- ggplot(data = data7, aes(fill = credit_history, y = values7, x = loan_status)) +
  geom_bar(position = "dodge", stat = "identity") + theme_gray() +
  ggtitle(label = "Grouped barplot of Credit_History vs Loan_Status")
plot19

par(mfrow = c(2, 2), bg = "light gray")
ApplicantIncome_Yes <- ApplicantIncome[Loan_Status == 1]
ApplicantIncome_No <- ApplicantIncome[Loan_Status == 0]
hist(ApplicantIncome_Yes, col = "#0000FF75", xlab = "ApplicantIncome", 
     main = "Distribution of ApplicantIncome")
hist(ApplicantIncome_No, add = T, col = "#22222275")

CoapplicantIncome_Yes <- CoapplicantIncome[Loan_Status == 1]
CoapplicantIncome_No <- CoapplicantIncome[Loan_Status == 0]
hist(CoapplicantIncome_Yes, col = "#0000FF75", xlab = "CoapplicantIncome",
     main = "Distribution of CoapplicantIncome")
hist(CoapplicantIncome_No, add = T, col = "#22222275")

legend('topright', title = "Loan Status", c("Yes", "No"),
       fill = c("#0000FF75", "#22222275"))

LoanAmount_Yes <- LoanAmount[Loan_Status == 1]
LoanAmount_No <- LoanAmount[Loan_Status == 0]
hist(LoanAmount_Yes, col = "#0000FF75", xlab = "LoanAmount",
     main = "Distribution of LoanAmount")
hist(LoanAmount_No, add = T, col = "#22222275")

Loan_Amount_Term_Yes <- Loan_Amount_Term[Loan_Status == 1]
Loan_Amount_Term_No <- Loan_Amount_Term[Loan_Status == 0]
hist(Loan_Amount_Term_Yes, col = "#0000FF75", xlab = "Loan_Amount_Term", 
     main = "Distribution of Loan_Amount_Term")
hist(Loan_Amount_Term_No, add = T, col = "#22222275")

# Scaling the dataset

train_data[, c(6:9)] = scale(train_data[, c(6:9)])
test_data[, c(6:9)] = scale(test_data[, c(6:9)])

# Splitting the train set

s = sample(nrow(train_data), nrow(train_data)*0.8)
train_set = train_data[s,]
validation_set = train_data[-s,]

x_train = as.matrix(train_set[, -12])
y_train = train_set$Loan_Status
x_validation = as.matrix(validation_set[, -12])
y_validation = validation_set$Loan_Status

# Required library for LASSO using glmnet

library(glmnet)

# Training the model
lasso_model <- glmnet(x_train, y_train, intercept = F, standardize = F, alpha = 1, family = "binomial")
cv_lassO_model <- cv.glmnet(x_train, y_train, nfolds = 10, intercept = F, standardize = F, alpha = 1, family = "binomial")
optimum_lambda <- cv_lassO_model$lambda.min
lasso_model <- glmnet(x_train, y_train, intercept = F, standardize = F, alpha = 1, family = "binomial", lambda = optimum_lambda)

# Misclassification rate on train set
p1 <- predict(lasso_model, x_train, type = "class", s = optimum_lambda, mode = lambda)
mean(as.numeric(p1) != train_set$Loan_Status)

# Misclassification rate on validation set
q1 <- predict(lasso_model, x_validation, type = "class", s = optimum_lambda, mode = lambda)
mean(as.numeric(q1) != validation_set$Loan_Status)

# Required library for LDA

library(MASS)

# Training the model
lda_model <- lda(Loan_Status ~., data = train_data)

# Misclassification rate on train set
p2 <- predict(lda_model, train_set)
mean(p2$class != train_set$Loan_Status)

# Misclassification rate on validation set
q2 <- predict(lda_model, validation_set)
mean(q2$class != validation_set$Loan_Status)

## Decision tree

# Required library for classifiacation trees

library(tree)

# Training the model
tree_model <- tree(as.factor(Loan_Status)~ ., data = train_data)
plot(tree_model)
text(tree_model, cex = 0.7)

# Misclassification rate on train set
p3 <- predict(tree_model, data = train_set, type = "class") # Issue with subset of train_set
mean(p3 != train_data$Loan_Status)

# Misclassification rate on validation set
q3 <- predict(tree_model, data = validation_set, type = "class") # Issue with subset of train_set
mean(p3 != train_data$Loan_Status)

## Bagging

# Required library for bagging and random forest
library(randomForest)

# Training the model
set.seed(1)
bag_loan <- randomForest(as.factor(Loan_Status)~., data = train_data, mtry = 11, importance = T)

# Misclassification rate on train set
bagging_model_train <- randomForest(as.factor(Loan_Status)~., data = train_set, mtry = 11,
                                    importance = T)
bagging_model_train

# Misclassification rate on validation set
bagging_model_validation <- randomForest(as.factor(Loan_Status)~., data = validation_set, mtry = 11,
                                         importance = T)
bagging_model_validation

## Random Forest

# Training the model
random_forest_model <- randomForest(as.factor(Loan_Status)~., data = train_data, mtry = sqrt(11),
                                    importance = T )

# Misclassification rate in train set
random_forest_model_train <- randomForest(as.factor(Loan_Status)~., data = train_set, mtry = sqrt(11),
                                          importance = T )
random_forest_model_train

# Misclassification rate in validation set 
random_forest_model_validation <- randomForest(as.factor(Loan_Status)~., data = validation_set,
                                               mtry = sqrt(11), importance = T )

# Required library for SVM

library(e1071)

# Training the model
svm_linear <- svm(as.factor(Loan_Status) ~., data = train_data, kernel = 'linear', 
                  cost = 0.01, scale = F)

# Misclassification rate for train set
svm_linear_train <- svm(as.factor(Loan_Status) ~., data = train_set, kernel = 'linear', 
                        cost = 0.01, scale = F)
pred = predict(svm_linear, train_data)
mean(pred!=as.factor(train_data$Loan_Status))

# Misclassification rate on validation set
svm_linear_validation <- svm(as.factor(Loan_Status) ~., data = validation_set, kernel = 'linear', 
                             cost = 0.01, scale = F)
pred = predict(svm_linear, validation_set)
mean(pred!=as.factor(validation_set$Loan_Status))

lda_model <- lda(Loan_Status ~., data = train_data)
lda_model

# predicted values

predicted_loan_status <- predict(lda_model, test_data)
predicted_loan_status$class

table(predicted_loan_status$class)
predicted_loan_status$class[100]

# Important features

lasso_model <- glmnet(x_train, y_train, intercept = F, standardize = F, alpha = 1,
                      family = "binomial", lambda = optimum_lambda)
x1 <- sort(abs(as.vector(coef(lasso_model)))[-1], decreasing = T)
names(x1) <- c("Credit_History", "Married", "CoapplicantIncome", "Education", "LoanAmount",
               "Property_Area", "Loan_Amount_Term", "Dependents", "Gender", "Self_Employed",
               "ApplicantIncome")
x1
# Plot for LASSO using glmnet
barplot(x1, las = 3, col = "light blue")

par(bg = "light grey")
# Plot for RandomForest
random_forest_model <- randomForest(as.factor(Loan_Status)~., data = train_data, mtry = sqrt(11),
                                    importance = T)
varImpPlot(random_forest_model)
