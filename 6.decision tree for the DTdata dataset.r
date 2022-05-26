library("rpart") # load libraries
library("rpart.plot")
play_decision <- read.csv("C:/Users/sahil/Desktop/msc/DTdata.csv", header=TRUE, sep=",")
play_decision
summary(play_decision)
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class",
             data=play_decision,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(fit)

?rpart.plot
rpart.plot(fit, type=4, extra=1)

rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE,
           varlen=0, faclen=0)

newdata <- data.frame(Outlook="rainy", Temperature="mild",
                      Humidity="high", Wind=FALSE)
newdata

predict(fit,newdata=newdata,type="prob")
predict(fit,newdata=newdata,type="class")

