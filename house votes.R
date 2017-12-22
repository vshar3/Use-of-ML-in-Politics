#Loading the data into R studio

mydata <- read.table("C:\\Users\\vibho\\OneDrive\\Documents\\Machine Learning\\Datasets\\house-votes.txt",na.strings=c("?", "NA"), sep=",", header = FALSE)
str(mydata)
install.packages("dplyr")
library(dplyr)
install.packages("plyr")
library(plyr)

#Here I am replacing the NA with y and n.
# I am using a for loop for iteration from column V2 to V17.
#Counting the number of y and n in a column and replacing the NA with the highest count.
o <- c("V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17")
for(i in o){
  mydata1 <- ifelse(count(mydata,i)[1,2] > count(mydata,i)[2,2], "n", "y")
  mydata[[i]] <- as.factor(ifelse(is.na(mydata[[i]]), mydata1, as.character(mydata[[i]])))
}
head(mydata)

#Here I am converting the y and n into 1 and 0 using the mutate function i.e. from categorical to numerical.
#Because to implement the KNN model the dataset should be numerical and not categorical.
# Putting all the data into new variable mydata2
library(dplyr)
mydata2 <- mydata %>%
  mutate(V2YES_NO = (if_else(grepl(pattern = "y", x = mydata$V2),1,0))) %>%
  mutate(V3YES_NO = (if_else(grepl(pattern = "y", x = mydata$V3),1,0))) %>%
  mutate(V4YES_NO = (if_else(grepl(pattern = "y", x = mydata$V4),1,0))) %>%
  mutate(V5YES_NO = (if_else(grepl(pattern = "y", x = mydata$V5),1,0))) %>%
  mutate(V6YES_NO = (if_else(grepl(pattern = "y", x = mydata$V6),1,0))) %>%
  mutate(V7YES_NO = (if_else(grepl(pattern = "y", x = mydata$V7),1,0))) %>%
  mutate(V8YES_NO = (if_else(grepl(pattern = "y", x = mydata$V8),1,0))) %>%
  mutate(V9YES_NO = (if_else(grepl(pattern = "y", x = mydata$V9),1,0)))  %>%
  mutate(V10YES_NO = (if_else(grepl(pattern = "y", x = mydata$V10),1,0))) %>%
  mutate(V11YES_NO = (if_else(grepl(pattern = "y", x = mydata$V11),1,0))) %>%
  mutate(V12YES_NO = (if_else(grepl(pattern = "y", x = mydata$V12),1,0))) %>%
  mutate(V13YES_NO = (if_else(grepl(pattern = "y", x = mydata$V13),1,0))) %>%
  mutate(V14YES_NO = (if_else(grepl(pattern = "y", x = mydata$V14),1,0))) %>%
  mutate(V15YES_NO = (if_else(grepl(pattern = "y", x = mydata$V15),1,0))) %>%
  mutate(V16YES_NO = (if_else(grepl(pattern = "y", x = mydata$V16),1,0))) %>%
  mutate(V17YES_NO = (if_else(grepl(pattern = "y", x = mydata$V17),1,0)))


head(mydata)

#So, because of mutating there will be columns in y and n and in numericals.
# I am removing the columns which contain y and n in the dataset,so that our dataset is numerical.
#I got 11-False positive and 3- False negative. 
#k value is square root of training data.
mydata2 <- mydata2[, -c(2:17)]
head(mydata2)

library(class)
#Dividing data into train and test.
# I have divided 80% of dataset into training and 20% into testing.
mydata2_train <- mydata2[1:348, c(2:17) ]   
mydata2_test <- mydata2[349:435, c(2:17) ]
mydata2_test_pred <- knn(mydata2_train, mydata2_test, mydata2[1:348,1], k= 18)
library(gmodels)

CrossTable(x = mydata2[349:435, 1], y = mydata2_test_pred, prop.chisq = FALSE)
library(e1071)

#Implementing the cross validation and Automated Tuning.
#10 cross validation repeated 3 times.
#I did this to find the optimal model for knn.
#So, with this I can say k = 5 will give the best model as it has high accuracy.
library(caret)
trainCV <- trainControl(method = "repeatedcv", repeats = 3)
CV <- train( V1 ~ ., data = mydata2, method = "knn",trControl = trainCV)
CV
plot(CV)


#Implmenting Neural Networks
#Using model.matrix function to convert the whole dataset to numerical therefore, converting republicans and democrats to 1 and 0.
m <- model.matrix( 
  ~ V1 + V2YES_NO +V3YES_NO + V4YES_NO +V5YES_NO+ V6YES_NO+ V7YES_NO+V8YES_NO+
    V9YES_NO+V10YES_NO+V11YES_NO+V12YES_NO+V13YES_NO+V14YES_NO+V15YES_NO+V16YES_NO+V17YES_NO, data = mydata2 )

head(m)
#When I used model.matrix function it created one extra column intercepts so, here I am removing that column as it is not required.
m <- m[,-c(1)]
head(m)
#converting the dataset m into data frame.
x<-data.frame(m)

#dividing the data into training and testing.
#predicting the correlation between the model created and testing data.
nn_train_5 <- x[1:348, ]
nn_test_5 <- x[349:435, ]
library(neuralnet)
t <- neuralnet(V1republican ~ V2YES_NO +V3YES_NO + V4YES_NO +V5YES_NO+ V6YES_NO+ V7YES_NO+V8YES_NO+
                 V9YES_NO+V10YES_NO+V11YES_NO+V12YES_NO+V13YES_NO+V14YES_NO+V15YES_NO+V16YES_NO+V17YES_NO, data = nn_train_5,hidden =10)
plot(t)
model_results_t <- neuralnet::compute(t , nn_test_5[,1:16])
predicted_strength_t <- model_results_t$net.result
cor(predicted_strength_t, nn_test_5$V1republican)


#Cross validation
#calculating the mean and plotting the boxplot
library(boot)
set.seed(1)
lm.fit <- glm(V1republican~.,data=x)
cv.glm(data,lm.fit,K=10)$delta[1]
fg.ll <- compute(t,nn_test_5[,2:17])
fg.ll 
fg.ll <- fg.ll$net.result*(max(x$V1republican)-min(x$V1republican))+min(x$V1republican)
test.cv.r <- (nn_test_5$V1republican)*(max(x$V1republican)-min(x$V1republican))+min(x$V1republican)
cv.error_1 <- sum((test.cv.r - fg.ll)^2)/nrow(nn_test_5)
mean(cv.error_1)
boxplot(cv.error_1,xlab='Mean',col='cyan',
        border='green',names='CV error (MSE)',
        main='CV for NN',horizontal=TRUE)




# Tuning
library(caret)
modelLookup("nnet")
set.seed(1)
m <- train(V1republican ~ ., data = x, method = "nnet")
m
m$finalModel
plot(m)
p <- predict(m,x)
table(p,x$V1republican)

