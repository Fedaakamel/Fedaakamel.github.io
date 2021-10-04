###random forest for A

library("randomForest")
library(randomForest)
library(stratification)
library(caTools)
eye_sessionsa<- read.csv("C:/Users/fedaaelderdesawe/Google Drive/dataaa/alleye_sessions 81 .csv")
mouseclick_sessionsa <- read.csv("C:/Users/fedaaelderdesawe/Google Drive/dataaa/allmouseclick_sessions 81 .csv")
mouse_sessionsa <- read.csv("C:/Users/fedaaelderdesawe/Google Drive/dataaa/allmousesessions 81 .csv")
allfeatures_a=cbind(mouse_sessionsa ,eye_sessionsa,mouseclick_sessionsa)
allfeatures_a=allfeatures_a[,-c(27,28,53,54)]


longer_data <- survey_data %>%
  pivot_longer(Q1:Q6, names_to = "question", values_to = "response")
print(longer_data)


ggplot(new_metadata) +
  geom_point(aes(x = age_in_days, y= samplemeans))



barplot(table(iris$Species,iris$Sepal.Length),col  = brewer.pal(3,"Set1"))
###################################
ggduo(data = concrete_data, 
      columnsX = 1:8, 
      columnsY = 9, 
      types = list(continuous = "smooth_lm"),
      mapping = ggplot2::aes(color = -Strength, alpha = 0.3)
) +
  theme_bw()

#########################################333
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))

surveys_plot <- ggplot(data = surveys_complete, 
                       mapping = aes(x = weight, y = hindfoot_length))

# Draw the plot
surveys_plot + 
  geom_point()


ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")


hist(training_set$Strength, main = "Training Set", xlab = "Concrete Compressive Strength (MPa)", freq = FALSE)
hist(test_set$Strength, main = "Test Set", xlab = "Concrete Compressive Strength (MPa)", freq = FALSE)
#####Data partitioning  

train_rows = sample.split(allfeatures_a$sub, SplitRatio=0.7)
mySampSize=ceiling(table(allfeatures_a$sub) * 0.632)
test_rows=!train_rows
test1=allfeatures_a[test_rows,]
train =allfeatures_a[ train_rows,]
test  = allfeatures_a[!train_rows,]
sampsize=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
##############################################################
#########The  final result for random forest 
set.seed(25)
try=randomForest(sub~.,data=allfeatures_a,proximity=TRUE,strata=allfeatures_a$sub ,
                 sampsize=mySampSize, xtest=test[,names(test)!='sub'],
                 ytest=test[,'sub'],mtry=25,nodesize = 1,ntree=1000,keep.forest=TRUE)

try###### test error 0 
varImpPlot(try,main=" The important features in random forest model for dataset A",lwd=6)
sum(diag(try$confusion))/81 ####.58
sum(diag(try$confusion))
predicttrian <- predict(try, train, type="class")
trainacc=table(predicttrian ,train$sub)
trainacc
write.csv(try$confusion, file = paste("finalA_confusion matrix", ".csv") ,sep = " ")
write.csv(try, file = paste("tryc", ".csv") ,sep = " ")
write.csv(trainacc, file = paste("prediction_trainc", ".csv") ,sep = " ")
sum(diag(trainacc))/sum(trainacc) #training accuracy 100%
#test
predicttry <- predict(try, test)
result=table(predicttry,test$sub)
result
write.csv(result, file = paste("prediction_testc", ".csv") ,sep = " ")
sum(diag(result))/sum(result)
########################################################
#fit random forest with samplesize
fitrandom1 <- randomForest(sub ~ .-sub,strata=allfeatures_a$sub 
                           ,sampsize=sampsize,importance=TRUE,nodesize = 1,ntree=1000 ,
                           mtry=32, data=allfeatures_a)
fitrandom1
write.csv(fitrandom1$confusion, file = paste("fitrandom1data", ".csv") ,sep = " ")
#accuracy
sum(diag(fitrandom1$confusion))/81 #accuracy =.6419

###################################
r <- randomForest(sub ~.-sub, data=allfeatures_a,strata=allfeatures_a$sub, mtry=25,ntree=1000,importance=TRUE ,nodesize = 1)
r
write.csv(r$confusion, file = paste("rdatc", ".csv") ,sep = " ")
sum(diag(r$confusion))/81 #### accuracy 50% when 
###################################

try1=randomForest(sub~.,data=allfeatures_a,subset=train_rows,nodesize = 1, mtry=30,ntree=800,keep.forest=TRUE)
try1
