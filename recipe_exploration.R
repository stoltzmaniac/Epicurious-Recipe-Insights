library(ggplot2)
library(dplyr)
library(Hmisc)
library(corrgram)
library(randomForest)
library(caret)

data = read.csv('epi_r.csv')
summary(data)
colnames(data)

hist(data$rating)

df = data %>%
  filter(calories != 0) %>%
  filter(calories < 10000) %>%
  select(rating,calories,protein,fat,sodium)

rcorr(as.matrix(df))

corrgram(df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corr Plot")



df = data[,7:ncol(data)]

df$tree.nut.free[df$tree.nut.free==1] = 'YES'
df$tree.nut.free[df$tree.nut.free==0] = 'NO'
df$tree.nut.free = factor(df$tree.nut.free)

#Create data for training
sample.ind = sample(2,
                    nrow(df),
                    replace = T,
                    prob = c(0.10,0.90))
data.dev = df[sample.ind==1,]
data.val = df[sample.ind==2,]


prop.table(table(df$tree.nut.free))
prop.table(table(data.dev$tree.nut.free))
prop.table(table(data.val$tree.nut.free))


rf = randomForest(tree.nut.free ~ .,
                  ntree = 150,
                  data = data.dev)
plot(rf)

print(rf)

# Variable Importance
varImpPlot(rf,
           sort = T,
           n.var=15,
           main="Top 15 - Variable Importance")


# Predicting response variable
data.dev$predicted.response = predict(rf , data.dev)

# Create Confusion Matrix
print(
  confusionMatrix(data = data.dev$predicted.response,
                  reference = data.dev$tree.nut.free,
                  positive = 'YES'))


# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)

# Create Confusion Matrix
print(
  confusionMatrix(data=data.val$predicted.response,
                  reference=data.val$tree.nut.free,
                  positive='YES'))



library(e1071)
model_svm <- svm(tree.nut.free ~. , data=data.dev, cost = 1000, gamma = 0.01)
test_svm <- predict(model_svm, newdata = data.val)
table(test_svm, data.val$tree.nut.free)



# p = ggplot(data %>%
#              filter(calories != 0) %>%
#              filter(calories < 10000),
#            aes(x=calories,y=rating))
# p + geom_point()
