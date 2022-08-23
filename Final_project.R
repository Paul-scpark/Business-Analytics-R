### Business Analytics
# 21500268, Seongchan Park
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(caret)
library(fBasics)
library(cowplot)
library(ROSE)
library(MLmetrics)
library(class)

## 1. Loading data
train <- read.csv('/Users/paul/Desktop/2021-2학기/비즈니스 애널리틱스/GiveMeSomeCredit/cs-training.csv')
test <- read.csv('/Users/paul/Desktop/2021-2학기/비즈니스 애널리틱스/GiveMeSomeCredit/cs-test.csv')
train <- train[, -1]
test <- test[, -1]

colnames(train) <- c('DEFAULT', 'Credit_limit_on_debt', 'Age', 
                     'Delay_30_59_days', 'Debt_ratio', 'Monthly_income', 
                     'Num_open_credit_loans', 'Delay_90_days', 
                     'Num_open_mortage_loans', 'Delay_60_89_days', 'Num_dependents')

train <- train[, c(1, 3, 4, 10, 8, 7, 9, 2, 5, 6, 11)]
str(train)

## 2. Basic data preprocessing
# PART 1. Checking Outlier
total_df <- data.frame()
for (i in 2:ncol(train)){
  col_name <- colnames(train)[i]
  total_df <- rbind(total_df, c(col_name, summary(train[, i], digits = 7)))
}

colnames(total_df) <- c('Variable', 'Min', '1st_Qu', 'Median', 'Mean', '3rd_Qu', 'Max')

train %>% 
  dplyr::filter(Age < 20) %>% 
  select(DEFAULT, Age, Debt_ratio, Monthly_income, Num_dependents)

train <- train %>% dplyr::filter(Age >= 20)

# PART 2. Checking NA
colSums(is.na(train))
summary(train$Monthly_income)
summary(train$Num_dependents)

train <- train %>% 
  mutate(Monthly_income = ifelse(is.na(Monthly_income), 0, Monthly_income), 
         Num_dependents = ifelse(is.na(Num_dependents), 0, Num_dependents))

colSums(is.na(train))

# PART 3. EDA, Standardizing
skewness(train)

hist1 <- train %>% select(Delay_30_59_days) %>% 
  ggplot(aes(x = Delay_30_59_days)) + geom_histogram()
hist2 <- train %>% select(Delay_30_59_days) %>% 
  ggplot(aes(x = log(Delay_30_59_days + 1))) + geom_histogram()
title <- ggdraw() + draw_label('Delay_30_59_days vs. log(Delay_30_59_days)')
plot_grid(title, plot_grid(hist1, hist2), ncol = 1, rel_heights = c(0.1, 1))

hist1 <- train %>% select(Delay_60_89_days) %>% 
  ggplot(aes(x = Delay_60_89_days)) + geom_histogram()
hist2 <- train %>% select(Delay_60_89_days) %>% 
  ggplot(aes(x = log(Delay_60_89_days + 1))) + geom_histogram()
title <- ggdraw() + draw_label('Delay_60_89_days vs. log(Delay_60_89_days)')
plot_grid(title, plot_grid(hist1, hist2), ncol = 1, rel_heights = c(0.1, 1))

hist1 <- train %>% select(Delay_90_days) %>% 
  ggplot(aes(x = Delay_90_days)) + geom_histogram()
hist2 <- train %>% select(Delay_90_days) %>% 
  ggplot(aes(x = log(Delay_90_days + 1))) + geom_histogram()
title <- ggdraw() + draw_label('Delay_90_days vs. log(Delay_90_days)')
plot_grid(title, plot_grid(hist1, hist2), ncol = 1, rel_heights = c(0.1, 1))

hist1 <- train %>% select(Num_open_mortage_loans) %>% 
  ggplot(aes(x = Num_open_mortage_loans)) + geom_histogram()
hist2 <- train %>% select(Num_open_mortage_loans) %>% 
  ggplot(aes(x = log(Num_open_mortage_loans + 1))) + geom_histogram()
title <- ggdraw() + draw_label('Num_open_mortage_loans vs. log(Num_open_mortage_loans)')
plot_grid(title, plot_grid(hist1, hist2), ncol = 1, rel_heights = c(0.1, 1))

hist1 <- train %>% select(Credit_limit_on_debt) %>% 
  ggplot(aes(x = Credit_limit_on_debt)) + geom_histogram()
hist2 <- train %>% select(Credit_limit_on_debt) %>% 
  ggplot(aes(x = log(Credit_limit_on_debt + 1))) + geom_histogram()
title <- ggdraw() + draw_label('Credit_limit_on_debt vs. log(Credit_limit_on_debt)')
plot_grid(title, plot_grid(hist1, hist2), ncol = 1, rel_heights = c(0.1, 1))

hist1 <- train %>% select(Debt_ratio) %>% 
  ggplot(aes(x = Debt_ratio)) + geom_histogram(bins = 30)
hist2 <- train %>% select(Debt_ratio) %>% 
  ggplot(aes(x = log(Debt_ratio + 1))) + geom_histogram()
title <- ggdraw() + draw_label('Debt_ratio vs. log(Debt_ratio)')
plot_grid(title, plot_grid(hist1, hist2), ncol = 1, rel_heights = c(0.1, 1))

hist1 <- train %>% select(Monthly_income) %>% 
  ggplot(aes(x = Monthly_income)) + geom_histogram()
hist2 <- train %>% select(Monthly_income) %>% 
  ggplot(aes(x = log(Monthly_income + 1))) + geom_histogram()
title <- ggdraw() + draw_label('Monthly_income vs. log(Monthly_income)')
plot_grid(title, plot_grid(hist1, hist2), ncol = 1, rel_heights = c(0.1, 1))

train <- train %>% 
  mutate(Delay_30_59_days = log(Delay_30_59_days+1), Delay_30_59_days = 1/Delay_30_59_days,
         Delay_60_89_days = log(Delay_60_89_days+1), Delay_60_89_days = 1/Delay_60_89_days,
         Delay_90_days = log(Delay_90_days+1), Delay_90_days = 1/Delay_90_days,
         Num_open_credit_loans = log(Num_open_credit_loans + 1),
         Num_open_mortage_loans = log(Num_open_mortage_loans + 1),
         Credit_limit_on_debt = log(Credit_limit_on_debt + 1),
         Debt_ratio = log(Debt_ratio + 1), Monthly_income = log(Monthly_income + 1))

is.na(train) <- sapply(train, is.infinite)
train[is.na(train)] <- 0
train <- as.data.frame(sapply(train, as.double))
skewness(train)

train %>% 
  group_by(DEFAULT) %>% 
  dplyr::summarise(count = n()) %>% 
  ggplot(aes(x = reorder(DEFAULT, -count), y = count)) + 
  geom_bar(stat = 'identity') + xlab('DEFAULT') +
  geom_text(aes(label = count), vjust = 1.5, colour = 'white')

corr <- cor(train)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200),  
         type="upper", order="hclust", tl.cex = 0.9, 
         addCoef.col = "black", tl.col="black", tl.srt=33, 
         sig.level = 0.01, insig = "blank", diag=FALSE)

train %>% ggplot() + 
  geom_histogram(aes(x = Delay_30_59_days, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.05, position = 'identity', alpha = 0.4) + xlim(c(0.1, 1.5))

train %>% ggplot() + 
  geom_histogram(aes(x = Delay_60_89_days, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.1, position = 'identity', alpha = 0.4) + xlim(c(0.1, 1.5))

train %>% ggplot() + 
  geom_histogram(aes(x = Delay_90_days, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.1, position = 'identity', alpha = 0.4) + xlim(c(0.1, 1.5))

train %>% ggplot() + 
  geom_histogram(aes(x = Credit_limit_on_debt, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.05, position = 'identity', alpha = 0.4) + xlim(c(0, 1))

train %>% ggplot() + 
  geom_histogram(aes(x = Age, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 1, position = 'identity', alpha = 0.4)

train %>% ggplot() + 
  geom_histogram(aes(x = Num_open_credit_loans, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.1, position = 'identity', alpha = 0.4)

train %>% ggplot() + 
  geom_histogram(aes(x = Num_open_mortage_loans, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.1, position = 'identity', alpha = 0.4) + xlim(c(-0.5, 3))

train %>% ggplot() + 
  geom_histogram(aes(x = Monthly_income, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.1, position = 'identity', alpha = 0.4) + xlim(c(5, 12))

train %>% ggplot() + 
  geom_histogram(aes(x = Debt_ratio, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.1, position = 'identity', alpha = 0.4) + xlim(c(-1, 10))

train %>% ggplot() + 
  geom_histogram(aes(x = Num_dependents, y = ..density.., fill = factor(DEFAULT)), 
                 binwidth = 0.1, position = 'identity', alpha = 0.4) + xlim(c(-1, 8))

## 3. Basic modeling - 1st Trial
model_output <- function(model, train_df, target_variable, threshold){
  train_df <- train_df %>% 
    mutate(pred_prob = predict(model, train_df, type = 'response'), 
           pred = ifelse(pred_prob >= threshold, 1, 0))
  train_output <- table(pred = train_df$pred, actual = train_df[, target_variable])
  
  train_acc <- sum(diag(train_output)) / sum(train_output)
  train_F1 <- (train_output[1, 1] * 2) / ((train_output[1, 1] * 2) + train_output[1, 2] + train_output[2, 1])
  train_precision <- train_output[1, 1] / sum(train_output[1, ])
  train_recall <- train_output[1, 1] / sum(train_output[, 1])
  
  total_df <- as.data.frame(t(c(train_acc, train_F1, train_precision, train_recall, threshold)))
  colnames(total_df) <- c('Accuracy', 'F1', 'Precision', 'Recall', 'Threshold')
  
  return (total_df)
}

logistic_model <- glm(DEFAULT ~ ., data = train, family = binomial(link = 'logit'))
summary(logistic_model)

output_df <- data.frame()
for (cut_off in seq(0.5, 0.97, 0.01)){
  df <- model_output(logistic_model, train, 'DEFAULT', cut_off)
  output_df <- rbind(output_df, df)
}

output1 <- train %>%
  mutate(pred_prob = predict(logistic_model, train, type = 'response'),
         pred = ifelse(pred_prob >= output_df[which.max(output_df$F1), 'Threshold'], 1, 0), 
         correct = ifelse(DEFAULT == pred, 1, 0))

table(Actual = output1$DEFAULT, Pred = output1$pred)

### Train, Valid 나누고, 비율에 맞춰 Sampling 해주기
both_sampling_df <- ovun.sample(DEFAULT ~ ., data = train, method = "both", p = 0.5, N = nrow(train), seed = 1)$data
rose_df <- ROSE(DEFAULT ~ ., data = train, seed = 1)$data
rbind(original = table(train$DEFAULT), both_sampling = table(both_sampling_df$DEFAULT), SMOTE = table(rose_df$DEFAULT))

both_model <- glm(DEFAULT ~ ., data = both_sampling_df, family = binomial(link = 'logit'))
rose_model <- glm(DEFAULT ~ ., data = rose_df, family = binomial(link = 'logit'))
# summary(both_model)
# summary(rose_model)

both_output_df <- data.frame()
for (cut_off in seq(0.5, 0.97, 0.01)){
  df <- model_output(both_model, train, 'DEFAULT', cut_off)
  both_output_df <- rbind(both_output_df, df)
}

rose_output_df <- data.frame()
for (cut_off in seq(0.5, 0.97, 0.01)){
  df <- model_output(rose_model, train, 'DEFAULT', cut_off)
  rose_output_df <- rbind(rose_output_df, df)
}

both_output1 <- train %>%
  mutate(pred_prob = predict(both_model, train, type = 'response'),
         pred = ifelse(pred_prob >= both_output_df[which.max(both_output_df$F1), 'Threshold'], 1, 0), 
         correct = ifelse(DEFAULT == pred, 1, 0))

table(Actual = both_output1$DEFAULT, Pred = both_output1$pred)

rose_output1 <- train %>%
  mutate(pred_prob = predict(rose_model, train, type = 'response'),
         pred = ifelse(pred_prob >= rose_output_df[which.max(rose_output_df$F1), 'Threshold'], 1, 0), 
         correct = ifelse(DEFAULT == pred, 1, 0))

table(Actual = rose_output1$DEFAULT, Pred = rose_output1$pred)

sum(diag(table(Actual = both_output1$DEFAULT, Pred = both_output1$pred))) / nrow(train)
sum(diag(table(Actual = rose_output1$DEFAULT, Pred = rose_output1$pred))) / nrow(train)

## 3. Modeling - 2nd Trial
new_train <- read.csv('/Users/paul/Desktop/2021-2학기/비즈니스 애널리틱스/GiveMeSomeCredit/cs-training.csv')
new_train <- new_train[, -1]

colnames(new_train) <- c('DEFAULT', 'Credit_limit_on_debt', 'Age', 
                         'Delay_30_59_days', 'Debt_ratio', 'Monthly_income', 
                         'Num_open_credit_loans', 'Delay_90_days', 
                         'Num_open_mortage_loans', 'Delay_60_89_days', 'Num_dependents')

new_train <- new_train[, c(1, 3, 4, 10, 8, 7, 9, 2, 5, 6, 11)]
new_train <- new_train %>% dplyr::filter(Age >= 20)
new_train <- new_train %>% 
  mutate(Monthly_income = ifelse(is.na(Monthly_income), 0, Monthly_income), 
         Num_dependents = ifelse(is.na(Num_dependents), 0, Num_dependents))

new_train <- new_train %>% 
  mutate(Retirement = ifelse(Age >= 60, 1, 0),
         Income = Monthly_income ** 2,
         Credibility1 = Delay_30_59_days**2 + Delay_60_89_days**2 + Delay_90_days**2, 
         Credibility2 = Credit_limit_on_debt ** 2,
         Debt_size = Num_open_credit_loans**2 + Num_open_mortage_loans**2)

new_train <- new_train %>% 
  mutate(Delay_30_59_days = log(Delay_30_59_days+1), Delay_30_59_days = 1/Delay_30_59_days,
         Delay_60_89_days = log(Delay_60_89_days+1), Delay_60_89_days = 1/Delay_60_89_days,
         Delay_90_days = log(Delay_90_days+1), Delay_90_days = 1/Delay_90_days,
         Num_open_credit_loans = log(Num_open_credit_loans + 1),
         Num_open_mortage_loans = log(Num_open_mortage_loans + 1),
         Credit_limit_on_debt = log(Credit_limit_on_debt + 1),
         Debt_ratio = log(Debt_ratio + 1), Monthly_income = log(Monthly_income + 1), 
         Income = log(Income + 1), Credibility1 = log(Credibility1 + 1), 
         Credibility2 = log(Credibility2 + 1), Debt_size = log(Debt_size + 1))

is.na(new_train) <- sapply(new_train, is.infinite)
new_train[is.na(new_train)] <- 0
new_train <- as.data.frame(sapply(new_train, as.double))
new_train$Retirement <- as.factor(new_train$Retirement)

rose_df <- ROSE(DEFAULT ~ ., data = new_train, seed = 1)$data
logistic_model <- glm(DEFAULT ~ ., data = rose_df, family = binomial(link = 'logit'))

final_output_df <- data.frame()
for (cut_off in seq(0.5, 0.97, 0.01)){
  df <- model_output(logistic_model, new_train, 'DEFAULT', cut_off)
  final_output_df <- rbind(final_output_df, df)}

output <- new_train %>%
  mutate(pred_prob = predict(logistic_model, new_train, type = 'response'),
         pred = ifelse(pred_prob >= final_output_df[which.max(final_output_df$F1), 'Threshold'],
                       1, 0),
         correct = ifelse(DEFAULT == pred, 1, 0))
table(Actual = output$DEFAULT, Pred = output$pred)
F1_Score(output$DEFAULT, output$pred)


test <- read.csv('/Users/paul/Desktop/2021-2학기/비즈니스 애널리틱스/GiveMeSomeCredit/cs-test.csv')
test <- test[, -1]
colnames(test) <- c('DEFAULT', 'Credit_limit_on_debt', 'Age', 
                    'Delay_30_59_days', 'Debt_ratio', 'Monthly_income', 
                    'Num_open_credit_loans', 'Delay_90_days', 
                    'Num_open_mortage_loans', 'Delay_60_89_days', 'Num_dependents')

test <- test[, c(1, 3, 4, 10, 8, 7, 9, 2, 5, 6, 11)]
test <- test %>% 
  mutate(Monthly_income = ifelse(is.na(Monthly_income), 0, Monthly_income), 
         Num_dependents = ifelse(is.na(Num_dependents), 0, Num_dependents))

test <- test %>% 
  mutate(Retirement = ifelse(Age >= 60, 1, 0),
         Income = Monthly_income ** 2,
         Credibility1 = Delay_30_59_days**2 + Delay_60_89_days**2 + Delay_90_days**2, 
         Credibility2 = Credit_limit_on_debt ** 2,
         Debt_size = Num_open_credit_loans**2 + Num_open_mortage_loans**2)

test <- test %>% 
  mutate(Delay_30_59_days = log(Delay_30_59_days+1), Delay_30_59_days = 1/Delay_30_59_days,
         Delay_60_89_days = log(Delay_60_89_days+1), Delay_60_89_days = 1/Delay_60_89_days,
         Delay_90_days = log(Delay_90_days+1), Delay_90_days = 1/Delay_90_days,
         Num_open_credit_loans = log(Num_open_credit_loans + 1),
         Num_open_mortage_loans = log(Num_open_mortage_loans + 1),
         Credit_limit_on_debt = log(Credit_limit_on_debt + 1),
         Debt_ratio = log(Debt_ratio + 1), Monthly_income = log(Monthly_income + 1), 
         Income = log(Income + 1), Credibility1 = log(Credibility1 + 1), 
         Credibility2 = log(Credibility2 + 1), Debt_size = log(Debt_size + 1))

is.na(test) <- sapply(test, is.infinite)
test[is.na(test)] <- 0
test <- as.data.frame(sapply(test, as.double))
test$Retirement <- as.factor(test$Retirement)

test_output <- test %>% 
  mutate(Id = 1:nrow(test),
         Probability = predict(logistic_model, test, type = 'response')) %>% 
  select(Id, Probability)

write.csv(test_output, row.names = FALSE, '/Users/paul/Desktop/test.csv')

## 6. Insights
curr_df <- new_train[, -1]

tot_withinss <- c()
for (i in 1:20){
  print(i)
  set.seed(1004)
  kmeans_cluster <- kmeans(curr_df, centers = i)
  tot_withinss[i] <- kmeans_cluster$tot.withinss
}

plot(c(1:20), tot_withinss, type="b",
     main="Optimal number of clusters",
     xlab="Number of clusters",
     ylab="Total within-cluster sum of squares")

kmeans_5 <- kmeans(curr_df, centers = 5)
new_train$cluster <- kmeans_5$cluster
new_train$cluster <- as.factor(new_train$cluster)
table(new_train$cluster, new_train$DEFAULT) 

new_train %>% ggplot() +
  geom_histogram(aes(x = Age, y = ..density.., fill = factor(cluster)), 
                 binwidth = 1, position = 'identity', alpha = 0.5)

new_train %>% ggplot() +
  geom_histogram(aes(x = Credit_limit_on_debt, y = ..density.., fill = factor(cluster)), 
                 binwidth = 0.05, position = 'identity', alpha = 0.3) + xlim(c(-0.05, 1))

new_train %>% ggplot() +
  geom_histogram(aes(x = Debt_ratio, y = ..density.., fill = factor(cluster)), 
                 binwidth = 0.3, position = 'identity', alpha = 0.4) + xlim(c(-0.05, 9))

new_train %>% ggplot() +
  geom_histogram(aes(x = Monthly_income, y = ..density.., fill = factor(cluster)), 
                 binwidth = 0.5, position = 'identity', alpha = 0.4) + xlim(c(-0.5, 11))

new_train %>% ggplot() +
  geom_histogram(aes(x = Income, y = ..density.., fill = factor(cluster)), 
                 binwidth = 0.5, position = 'identity', alpha = 0.2)

aggregate(cbind(Age, Delay_30_59_days, Delay_60_89_days, Delay_90_days, 
                Num_open_credit_loans, Num_open_mortage_loans, Credit_limit_on_debt, 
                Debt_ratio, Monthly_income, Num_dependents, Income,
                Credibility1, Credibility2, Debt_size) ~ cluster, new_train, mean)

train_idx <- createDataPartition(new_train$cluster, p = 0.8, list = F)
train_data <- new_train[train_idx, ]
valid_data <- new_train[-train_idx, ]

table(train_data$cluster)
table(valid_data$cluster)

vector_k <- c()
for (k_value in 1:10){
  print(k_value)
  knn_model <- knn(train = train_data[, c(-1, -17)], test = valid_data[, c(-1, -17)], cl = train_data$cluster, k = k_value)
  vector_k[k_value] <- Accuracy(knn_model, valid_data$cluster)
}

plot(c(1:10), vector_k, type="b",
     main="Optimal number of k",
     xlab="k", ylab="Accuracy")

data.frame(colMeans(new_train[, c(-1, -12:-17)]))
df <- t(data.frame(c(colMeans(new_train[, c(-1, -12:-17)]), Retirement = 0, colMeans(new_train[, c(13, 14, 15, 16)]))))
rownames(df) <- NULL

knn(train = new_train[, c(-1, -17)], test = data.frame(df), cl = new_train$cluster, k = 10)

new_train[new_train$cluster == 2, ] %>% 
  summarise(default_mean = mean(DEFAULT))

#### 참고

draw_confusion_matrix <- function(cm) {
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

draw_confusion_matrix(confusionMatrix(factor(output1$pred), factor(output1$DEFAULT)))
