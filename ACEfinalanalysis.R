getwd()
setwd("C:/Users/kiloc/Documents/R")
getwd()
mydata <- read.csv("C:/Users/kiloc/Documents/R/finaldata.csv")
View(mydata)
hist(mydata$ACETOTAL, main = "Total Adversity Scores for Black Participants", xlab = "ACE Aggregate",
     ylab = "Number of Participants", col = "light blue", ylim = range(0,5000), xlim = range(0,11))



hist(mydata[mydata$X_URBSTAT == 'Urban', "ACETOTAL"], main = "Total Adversity Scores for Black Queer", xlab = "ACE Aggregate",
     ylab = "Number of Participants", col = "light blue", ylim = range(0,5000), xlim = range(0,11), breaks = 10)
table(mydata$Queer)

mean(mydata$ACETOTAL[mydata$Queer == 'Queer'])
?hist
?ggplot2
?boxplot
boxplot(ACETOTAL~Queer, data = finishdata, main = "Adversity Scores for Queer vs CisHet People",
        col = "lightblue", xlab = "Is Participant Queer?", ylab = "Aggregate ACE")
means <- tapply(InsectSprays$count,InsectSprays$spray,mean)
points(mean(mydata$ACETOTAL[mydata$Queer == 0]), col = 3, pch = 19)
k = c(0,1)
data_means <- aggregate(mydata$ACETOTAL,                       # Means by group
                                      list(mydata$Queer),
                                     mean)

head(data_means)

points(x = 1:nrow(data_means),                             # Add points to plot
       y = data_means$x,
       col = 3,
       pch = 15)


boxplot(ACETOTAL~X_URBSTAT, data = finishdata, main = "Adversity Scores for Urban vs Rural Living Black People",
        col = "lightblue", xlab = "Does Participant Live in a Rural or Urban Region?", ylab = "Aggregate ACE")

k = c(0,1)
data_means2 <- aggregate(mydata$ACETOTAL,                       # Means by group
                        list(mydata$X_URBSTAT),
                        mean)

head(data_means2)

points(x = 1:nrow(data_means2),                             # Add points to plot
       y = data_means2$x,
       col = 3,
       pch = 15)
require(foreign)
require(ggplot2)
require(MASS)

mydata$Queer <- as.factor(mydata$Queer)
mydata$X_URBSTAT <- as.factor(mydata$X_URBSTAT)
mydata <- within(mydata, {
  Queer <- factor(Queer, levels = 0:1, labels = c("Heterosexual", "Queer"))
  X_URBSTAT <- factor(X_URBSTAT, levels = 1:2, labels = c("Urban", "Rural"))
})
table(mydata)

####-----
#Negative Binomial Regression

summary(neg_bi_total <- glm.nb(formula = ACETOTAL ~ X_URBSTAT + Queer, data = mydata))

summary(neg_bi_sa <- glm.nb(formula = ACESA ~ X_URBSTAT + Queer, data = mydata))
        
summary(neg_bi_dv <- glm.nb(formula = ACEDV ~ X_URBSTAT + Queer, data = mydata))

summary(neg_bi_vtre <- glm.nb(formula = ACEVTrE ~ X_URBSTAT + Queer, data = mydata))