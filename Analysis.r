#####-------MODEL
######-------MULTIPLE LOGISTIC REGRESSION

#First try
#Trying which variables make sense to use in the model
mymodel <- glm(preg_bef_21 ~ use_contra_fst + area + fst_sex_14_less + marital_status 
                 + know_contra_mthd + education_lvl + can_preg_fst
               , data=newdata, family = "binomial")
summary(mymodel)

#It is very important to note what is significant and what isn't
#Two of my hypotheses have been disproved
#Rural area and knowledge of modern contraceptive methods are not significant predictors

#run model again without those two variables

mymodel <- glm(preg_bef_21 ~ use_contra_fst + fst_sex_14_less + marital_status 
                  + education_lvl 
               , data=newdata, family = "binomial")
summary(mymodel)

#Make a new dataframe that will include the Predicted Probabilities 
newdata2 <- cbind(newdata, predict(mymodel, newdata, type="link", se=TRUE))

newdata2<- within(newdata2, {PredictedProb <- plogis(fit)})

#Plot the Predicted Probability vs Pregnancy before 20 

plot(newdata2$preg_bef_21, newdata2$PredictedProb,
     ylab="Predicted Probability", xlab="Pregnagncy 21 or Younger",
    main="Predicted Probability vs  Pregnancy at 21 Years of Age or Younger")

####-------MODEL VALIDATION
#I will crossvalidate my model (Explain)
#Run the model 10 times in training data and check it in the test data
library('caret')

crossValSettings <- trainControl(method="repeatedcv", number=20,
                                savePredictions=TRUE)

crossVal <- train(as.factor(preg_bef_21) ~ use_contra_fst + fst_sex_14_less + marital_status 
                  + education_lvl, data=newdata, family="binomial", method="glm",
                 trControl=crossValSettings, na.action=na.exclude)

crossVal

#Another way to check accuracy of the model
#Confusion matrix
#False positives and false negatives

#takes our crossvalidated model and apply it to the original data
pred <- predict(crossVal, newdata = newdata)
confusionMatrix(data=pred, newdata$preg_bef_21)

#The matrix shows my model has more false positives than false negatives
#I have better specificity than sensitivity 
#Sensitivity is the ability to accurately predict a pregnancy when they were actually pregnant
#Specificity is the ability to only categorize those who were pregnant among those who were pregnant
#6651 correctly predicted cases (out of 8574)
#Model is 77.57% accurate 
#I have identified predictors of teen pregnancy
#I need to expand this to include other variables like socioeconomic status and charasteristics of the household
#I will do so for my thesis


