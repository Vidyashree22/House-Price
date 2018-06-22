library(dplyr)
library(caret)

change_char_to_factor <- function(df){
  for(col in names(df)){
    if(class(df[, col]) == "character" ){
      if(sum(is.na(df[,col])) == 0 ){
        df[, col] <- as.factor(df[, col])
      } else {
        df[, col] <- NULL
      }
    }
  }
  return (df)
}

pre_process <- function(df, method){
  pre_model <- preProcess(df, method = method) 
  pre_data <- predict(pre_model, df)
  
  return (pre_data)
}

#Evaluating Model
evalute_model <- function(model, data, y){
  prediction <- predict(model, data)
  pred_df <- data.frame(obs = y, pred=prediction)
  return (pred_df)
}

train <- read.csv('C:/Study Material/Data Analytics/DataSet/train.csv', sep=",",  header = TRUE, stringsAsFactors = FALSE)
glimpse(train)

summary(train)
sum(is.na(train))

sum(is.na(train$Alley))
train$Alley <- NULL

sum(is.na(train$PoolQC))
train$PoolQC <- NULL

sum(is.na(train$Fence))
train$Fence <- NULL

sum(is.na(train$MiscFeature))
train$MiscFeature <- NULL

train$Id<- NULL

train_clean <- change_char_to_factor(train)
glimpse(train_clean)

sum(is.na(train))


set.seed(42)
rows <- sample(nrow((train_clean)))
training <- train_clean[rows,]
split <- round(nrow(train_clean)*.80)
train_data <- training[1:split, ]
test_data <- training[(split + 1):nrow(training), ]
nrow(train_data) + nrow(test_data)

x_train <- train_data[-ncol(train_data)]
y_train <- train_data %>% select(SalePrice)
x_test <- test_data[-ncol(test_data)]
y_test <- test_data %>% select(SalePrice)

x_train_mi <- pre_process(x_train, "medianImpute")
x_test_mi <- pre_process(x_test, "medianImpute")
train_mi <- x_train_mi %>% mutate(SalePrice = train_data[, ncol(train_data)])
test_mi <- x_test_mi %>% mutate(SalePrice = test_data[, ncol(test_data)])
glimpse(train_mi)
glimpse(test_mi)

nrow(test_mi) + nrow(train_mi)
model_lm_mi <- train( SalePrice ~ ., data = train_mi, method="lm")
model_lm_mi

model_glm_mi <- train( SalePrice ~ ., data = train_mi, method="glm")
model_glm_mi
summary(model_glm_mi)
library(e1071)
library(ranger)
model_rf_mi <- train( SalePrice ~ ., data = train_mi, method="ranger")
model_rf_mi
summary(model_rf_mi)
plot(model_rf_mi)

pred_df <- evalute_model(model_lm_mi, test_mi, test_mi$SalePrice)
defaultSummary(pred_df)

pred_df <- evalute_model(model_glm_mi, test_mi, test_mi$SalePrice)
head(pred_df)

xyplot(pred_df$obs ~ pred_df$pred, type = c("p", "g"), xlab = "Predicted", ylab = "Observed")

pred_df <- evalute_model(model_rf_mi, test_mi, test_mi$SalePrice)
defaultSummary(pred_df)

xyplot(pred_df$obs ~ pred_df$pred, type = c("p", "g"), xlab = "Predicted", ylab = "Observed")
