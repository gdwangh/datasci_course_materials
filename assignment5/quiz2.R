setwd("D:/workspace/introduce_dataScience/datasci_course_materials/assignment5")

# Step 1 : Read and summarize the data, Answer Questions 1 and 2
rd<-read.csv("seaflow_21min.csv")
summary(rd)

#step 2: Split the data into test and training sets, Answer Question 3
s<-sample(1:nrow(rd), nrow(rd)/2)
train_set<-rd[s,]
test_set<-rd[-s,]

mean(train_set$time)

# Step 3: Plot the data, Plot pe against chl_small  and color by pop, Answer Question 4.
library(ggplot2)
ggplot(rd, aes(x=pe, y=chl_small))+geom_point(aes(color = pop))

# Step 4: Train a decision tree, Answer Questions 5, 6, 7.
library(rpart)
library(rpart.plot)
library(rattle)

model <- rpart(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small, method="class", data=train_set)
print(model)
fancyRpartPlot(model)

# Step 5: Evaluate the decision tree on the test data. Answer Question 8.
predictions1<-predict(model, test_set, type = "class")
a1<-sum(predictions1==test_set$pop)/nrow(test_set)

# Step 6: Build and evaluate a random forest. Answer Question 9 and 10
# there is error message about return is too big, so the training set has to be decreased temperary
# or adjust the function parameters: nodesize or ntree
#install.packages('randomForest')
library(randomForest)
#s<-sample(1:nrow(rd), nrow(rd)/5)
#train_set<-rd[s,]
#test_set<-rd[-s,]
model<-randomForest(pop~fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small, data=train_set,nodesize = 10)

# the higer the mean is, the more important the attr is
importance(model)

predictions2<-predict(model, test_set, type = "class")
a2<-sum(predictions2==test_set$pop)/nrow(test_set)

# Step 7: Train a support vector machine model and compare results.  Answer Question 11.
library(e1071)
model <- svm(pop~fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small, data=train_set)
predictions3<-predict(model, test_set, type = "class")
a3<-sum(predictions3==test_set$pop)/nrow(test_set)

# Step 8: Construct confusion matrices, Answer Question 12
table(pred = predictions1, true = test_set$pop)
table(pred = predictions2, true = test_set$pop)
table(pred = predictions3, true = test_set$pop)

# Step 8: Sanity check the data. Answer Question 13 and 14
par(mfrow=c(2,3))
plot(rd$time,rd$fsc_small)
plot(rd$time, rd$fsc_perp)
plot(rd$time, rd$fsc_big) 
plot(rd$time,rd$pe)
plot(rd$time,rd$chl_small)
plot(rd$time,rd$chl_big)
# fsc_big is uncontinuous

# list of fsc_big is short
unique(rd$fsc_small)
unique(rd$fsc_perp)
unique(rd$fsc_big)
unique(rd$pe)
unique(rd$chl_small)
unique(rd$chl_big)

# something wrong in the file 
library(ggplot2)
qplot(time,chl_big, data=rd,color=file_id)
qplot(time,chl_big, data=rd,color=pop)
qplot(time,chl_big, data=rd,color=pop,facets=.~file_id)


clean_train<-train_set[train_set$file_id!=208,]
clean_test<-test_set[test_set$file_id!=208,]
model <- svm(pop~fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small, data=clean_train)
predictions4<-predict(model, clean_test, type = "class")
a4<-sum(predictions4==clean_test$pop)/nrow(clean_test)
a4 - a3
