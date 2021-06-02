library(ggplot2)
library(ggthemes)
library(corrplot)
library(reshape2)
library(dplyr)
library(randomForest)

#Load in the dataset
fb<-read.csv("H:/Sem 6/SD/imdb_top_1000.csv")
View(fb)
# head(fb,6)

colnames(fb)

dim(fb) #dimensions

summary(fb$Overall)
summary(fb$IMDB_Rating)

cat("Complete Cases No Null ", sum(complete.cases(fb)))

cat('Total Missing Values -> ', sum(is.na(fb)))

cat('Missing Values Rate-> ', mean(is.na(fb)))

list_na <- colnames(fb)[apply(forbes, 2, anyNA)]
cat('Columns with null values -> ', list_na)

fb[is.na(fb)]=0
cat('Total Missing Values -> ', sum(is.na(fb)))

plot(fb)

ggplot(fb,aes(x=IMDB_Rating))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("IMBD Ratings")+
  theme_classic()

ggplot(fb,aes(x=IMDB_Rating,fill=factor(IMDB_Rating)))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,4,1))+
  ggtitle("Distribution of each rating")+
  theme_classic()

ggplot(fb,aes(x=No_of_Votes,fill=factor(IMDB_Rating)))
+goem_density(alpha=0.25)
+geom_vline(aes(xintercept=mean(No_of_Votes[IMDB_Rating==0],na,rm=T)),color="red",linetype="dashed",lwd=1)
+geom_vline(aes(xintercept=mean(No_of_Votes[IMDB_Rating==1],na,rm=T)),color="blue",linetype="dashed",lwd=1)
+ scale_x_continuous(breaks = seq(4,16,1))+
  xlab(label="Number of Votes")+
  ggtitle("Distribution of ratings")+
  theme_classic()

#Corrplot
cr <- cor(fb[5:7])
corrplot(cr)

#NaiveBayes Implementation
split=0.60
trainIndex <- createDataPartition(fb$Genre, p=split, list=FALSE)
data_train <- fb[ trainIndex,]
data_test <- fb[-trainIndex,]
model <- naiveBayes(Genre ~ ., data = data_train)
x_test <- data_test[,1:4]
y_test <- data_test[,5]
predictions <- predict(model, x_test)
predictions
#confusionMatrix(predictions, as.factor(y_test))
