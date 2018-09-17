library(caTools)
library(survey)
library(readr)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(ROCR)
library(pROC)

#preprocessing of data
SpotifyData=read.csv("data.csv", header = TRUE)
SpotifyData$duration_ms=(SpotifyData$duration_ms-min(SpotifyData$duration_ms))/(max(SpotifyData$duration_ms)-min(SpotifyData$duration_ms))
SpotifyData$loudness=(SpotifyData$loudness-min(SpotifyData$loudness))/(max(SpotifyData$loudness)-min(SpotifyData$loudness))
SpotifyData$tempo=(SpotifyData$tempo-min(SpotifyData$tempo))/(max(SpotifyData$tempo)-min(SpotifyData$tempo))
SpotifyData$key=as.factor(SpotifyData$key)
SpotifyData$time_signature=as.factor(SpotifyData$time_signature)
SpotifyData$mode=as.factor(SpotifyData$mode)

#training and test set seperation
split = sample.split(SpotifyData$target, SplitRatio = 0.7)
SpotifyData.Train = SpotifyData[split, ]
SpotifyData.Test = SpotifyData[!split, ]

#Logistic regression
glm.target=glm(target ~ acousticness + danceability + duration_ms + energy + instrumentalness + key + liveness + loudness + mode + speechiness + tempo + time_signature + valence, data=SpotifyData.Train, family = "binomial")
summary(glm.target)
glm.target=glm(target ~ acousticness + danceability + duration_ms + instrumentalness + loudness + speechiness, data=SpotifyData.Train, family = "binomial")
summary(glm.target)
SpotifyPredictTest_LR = predict(glm.target, newdata = SpotifyData.Test, type = "response")
threshold = 0.4
LogReg=table(SpotifyData.Test$target, as.numeric(SpotifyPredictTest_LR >= threshold))
SensLR <- LogReg[1,1]/(LogReg[1,1] + LogReg[2,1])
SpecLR <- LogReg[2,2]/(LogReg[2,2] + LogReg[1,2])
ROC_curve_LR=roc(target~SpotifyPredictTest_LR, data=SpotifyData.Test)
plot(ROC_curve_LR)

#Decision Tree
rtree_fit <- rpart(target ~ acousticness + danceability + duration_ms + energy + instrumentalness + key + liveness + loudness + mode + speechiness + tempo + time_signature + valence, data=SpotifyData.Train, method = "class", 
                   control = rpart.control(minbucket = 0.05*length(SpotifyData.Train$target)))
rpart.plot(rtree_fit)
SpotifyPredictTest <- predict(rtree_fit, newdata = SpotifyData.Test, type = "prob")
plot(roc(SpotifyData.Test$target, SpotifyPredictTest[,2]))
TbDT=table(SpotifyData.Test$target, as.numeric(SpotifyPredictTest[,2] >= threshold))
SensDT <- TbDT[1,1]/(TbDT[1,1] + TbDT[2,1])
SpecDT <- TbDT[2,2]/(TbDT[2,2] + TbDT[1,2])
plot(ROC_curve_LR, col="red")
par(new=TRUE)
plot(roc(SpotifyData.Test$target, SpotifyPredictTest[,2]))
#0.05*length(SpotifyData.Train$target)

