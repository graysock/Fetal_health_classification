######## Fetal Health Classification ########
## Objective is to classify the health of the fetus as Normal, Suspect or pathologival using CTG data
df <- read_csv('/Users//kolegraham/desktop/fetal_health_classification/fetal_health.csv')
summary(df)
 
hist <- ggplot(df,aes(baseline_value)) + geom_histogram(aes(fill=fetal_health),
                                                           bins = 20, 
                                                          color = black) + 
                                                          theme_classic + 
                                                          theme(axis.text.x= element_text(angle = 90, hjust = 1))
                      
show(hist)  
set.seed(9)
sample <- sample.split(df$fetal_health,SplitRatio = .75)
train <- subset(df,sample == TRUE)
test <- subset(df, sample ==FALSE)
tree <- rpart(fetal_health ~.,method = 'class' data=train)
p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$fetal_health)
