######## Fetal Health Classification ########
library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(tree)
library(caret)
library(MLmetrics)
## Objective is to classify the health of the fetus as Normal, Suspect or pathological using CTG data
df <- read.csv('/Users//kolegraham/desktop/fetal_health_classification/fetal_health.csv')
summary(df)
###EDA
## Baseline vs Fetal Health Histogram
hist <- ggplot(df,aes(baseline_value)) + geom_histogram(aes(fill=factor(fetal_health)),
                                                           bins = 20, 
                                                          color = 'black') + 
                                                          theme_classic() + 
                                                          theme(axis.text.x= element_text(angle = 90, hjust = 1))
show(hist)  
#baseline vs abnormal short term variability
scatter_abnormal_short_term_variability <- ggplot(df,aes(baseline_value,abnormal_short_term_variability)) + 
                                                  geom_point(aes(color = fetal_health)) +
                                                  theme_classic() + 
                                                  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(scatter_abnormal_short_term_variability)
# mean value of short term variability vs baseline
scatter_mean_value_of_short_term_variability_baseline <- ggplot(df,aes(baseline_value,mean_value_of_short_term_variability)) + 
                                                                geom_point(aes(color = fetal_health)) +
                                                                theme_classic() + 
                                                                theme(axis.text.x = element_text(angle = 90, hjust = 1))                        
show(scatter_mean_value_of_short_term_variability_baseline)
# mean value of short term variability vs abnormal short term variability
scatter_mean_value_of_short_term_variability_abnormal_short_term_variability <- ggplot(df,aes(mean_value_of_short_term_variability,abnormal_short_term_variability)) + 
                                                                                        geom_point(aes(color = fetal_health)) +
                                                                                        theme_classic() + 
                                                                                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
df$fetal_health <- as.factor(df$fetal_health)
show(scatter_mean_value_of_short_term_variability_abnormal_short_term_variability)
#light decelerations and fetal health
bar_light_decelerations <- ggplot(df,aes(light_decelerations)) +
                                  geom_bar(aes(fill = fetal_health)) + 
                                  theme_classic() + 
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(bar_light_decelerations)
# severe decelerations by fetal_health
bar_severe_decelerations <- ggplot(df,aes(severe_decelerations)) +
  geom_bar(aes(fill = fetal_health)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(bar_severe_decelerations)
# prolonged decelerations by fetal_health
bar_prolongued_decelerations <- ggplot(df,aes(prolongued_decelerations)) +
  geom_bar(aes(fill = fetal_health)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(bar_prolongued_decelerations)
# number of peaks by fetal_health
bar_histogram_num_peaks <- ggplot(df,aes(histogram_number_of_peaks)) +
  geom_bar(aes(fill = fetal_health)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(bar_histogram_num_peaks)
# number of zeros by fetal health
bar_histogram_num_zeros <- ggplot(df,aes(histogram_number_of_zeroes)) +
  geom_bar(aes(fill = fetal_health)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(bar_histogram_num_zeros)
# histogram variance by fetal_health
bar_histogram_variance <- ggplot(df,aes(histogram_variance)) +
  geom_bar(aes(fill = fetal_health)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(bar_histogram_variance)
# histogram tendencies by fetal_health
bar_histogram_tendency <- ggplot(df,aes(histogram_tendency)) +
  geom_bar(aes(fill = fetal_health)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(bar_histogram_tendency)
# percentage_of_time_with_abnormal_long_term_variability by fetal health
bar_percentage_of_time_with_abnormal_long_term_variability <- ggplot(df,aes(percentage_of_time_with_abnormal_long_term_variability)) +
  geom_bar(aes(fill = fetal_health)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
show(bar_percentage_of_time_with_abnormal_long_term_variability)
set.seed(9)
# create train test splits
sample <- sample.split(df$fetal_health,SplitRatio = .75)
train <- subset(df,sample == TRUE)
test <- subset(df, sample == FALSE)
train$fetal_health <- as.factor(train$fetal_health)
test$fetal_health <- as.factor(test$fetal_health)
# train decision tree
tree <- rpart(fetal_health ~.,method = 'class',data=train)
tree.preds <- predict(tree,test)
tree.preds <- as.data.frame(tree.preds)
add_max_prob_class <- function(dataframe) {
  # Find the column index with the maximum value in each row
  max_col_index <- max.col(dataframe, "first")
  
  # Create a new column with the index of the maximum probability class
  dataframe$max_prob_class <- max_col_index
  
  return(dataframe)
}

df_preds <- add_max_prob_class(tree.preds)
df_preds$max_prob_class <- as.factor(df_preds$max_prob_class)
confusionMatrix(test$fetal_health,df_preds$max_prob_class)
# tree evaluation
rpart.plot(tree)
train$fetal_health <- as.numeric(train$fetal_health)
test$fetal_health <- as.numeric(test$fetal_health)
df_preds$max_prob_class <- as.numeric((df_preds$max_prob_class))
tree_recall <- Recall(test$fetal_health,df_preds$max_prob_class)
tree_precision <- Precision(test$fetal_health,df_preds$max_prob_class)
tree_f1 <- F1_Score(test$fetal_health,df_preds$max_prob_class)
print(paste0('Decision Tree recall ',tree_recall))
print(paste0('Decision Tree precision ',tree_precision))
print(paste0('Decision Tree f1 ',tree_f1))
#create a random Forest model to see if it improves the results 
train$fetal_health <- as.factor(train$fetal_health)
test$fetal_health <- as.factor(test$fetal_health)
rf.model <- randomForest(fetal_health ~., data = train, importance = TRUE)
rf.model$importance
p <- predict(rf.model,test)
p <- as.data.frame(p)
# Random Forest Evaluation
confusionMatrix(test$fetal_health,p$p)
train$fetal_health <- as.numeric(train$fetal_health)
test$fetal_health <- as.numeric(test$fetal_health)
df_preds$max_prob_class <- as.numeric((p$p))
rf_recall <- Recall(test$fetal_health,p$p)
rf_precision <- Precision(test$fetal_health,p$p)
rf_f1 <- F1_Score(test$fetal_health,p$p)

print(paste0('Random Forest recall ',rf_recall))
print(paste0('Random Forest precision ',rf_precision))
print(paste0('Random Forest F1 ',rf_f1))

# create data frame of evaluation metrics 

scores <- data.frame(
  evaluation_metric = c('Randomforest Recall', 'Randomforest Precision', 'Randomforest F1 score', 'Decision Tree Recall', 'Decision Tree Precision', 'Decision Tree F1 score'),
  value = c(0.978260869565217,0.9688995215311,0.973557692307692,0.968599033816425,0.934731934731935,0.951364175563464)
)


# plot of evaluation metrics 
evaluation_metrics <- ggplot(scores, aes(x=evaluation_metric, y = value)) +
                      geom_bar(fill=c('steelblue'),stat = 'identity') +
                      theme_bw() +
                      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = .5)) + 
                      ggtitle('Evaluation Scores')
show(evaluation_metrics)
