View(mydata)
nrow(mydata)
mydata2 <- mydata[ ,c("ACEDEPRS", "ACEDRINK", "ACEPRISN", "ACEDIVRC",
                     "ACEPUNCH", "ACEHURT1", "ACESWEAR", "ACETOUCH", "ACETTHEM", "ACEDRUGS",
                     "ACEHVSEX", "SOMALE","SOFEMALE", "X_URBSTAT", "X_RACE")] #gathering variables that I want for the research project
mydatablack <- mydata2[mydata$X_RACE == 2, ] #selecting only black participants for study
View(mydatablack)
nrow(mydatablack)
mydatablack$SOMALE[is.na(mydatablack$SOMALE)] <- 0 #replacing na for sexual orientation question
mydatablack$SOFEMALE[is.na(mydatablack$SOFEMALE)] <- 0
nrow(mydatablack)
mydatablack[12:13][mydatablack[12:13]== 2 | mydatablack[12:13] == 7 | mydatablack[12:13] == 9] <- 0 #putting all queer responses into a 'no/yes' category
mydatablack[12:13][mydatablack[12:13]== 1 | mydatablack[12:13] == 3 | mydatablack[12:13] == 4] <- 1

mydatablack$Queer = vector(mode = "numeric", nrow(mydatablack))

for (j in 1:30131){
  mydatablack$Queer[j] <- sum(mydatablack[j,12:13])
} #creating singular queer category regardless of gender

View(mydatablack)

mydatablack <- na.omit(mydatablack)

table(mydatablack$Queer)

nrow(mydatablack)
#restructuring dataframe
mydatabq <- mydatablack[, c(1:11,14,16)]
colnames(mydatabq)
#restructing ACE scores
mydatabq[1:5][mydatabq[1:5] >= 2] <- 0
mydatabq[10][mydatabq[10] >= 2] <- 0

View(mydatabq)

mydatabq[6:9][mydatabq[6:9] == 1] <- 0

mydatabq[6:9][mydatabq[6:9] >= 7] <- 0 

mydatabq[11][mydatabq[11] == 1] <- 0

mydatabq[11][mydatabq[11] >= 7] <- 0 

mydatabq[6:9][mydatabq[6:9] >= 2] <- 1

mydatabq[11][mydatabq[11] >= 2] <- 1

View(mydatabq)

nrow(mydatabq)
mydatabq$ACETOTAL = vector(mode = 'numeric', 11301)
for (j in 1:11301){
  mydatabq$ACETOTAL[j] <- sum(mydatabq[j,1:11])
}
View(mydatabq)
hist(mydatabq$ACETOTAL)
summary(mydatabq$ACETOTAL)

mydatabq$ACESA = vector(mode = 'numeric', nrow(mydatabq))

for (j in 1:11301){
  mydatabq$ACESA[j] <- sum(mydatabq[j,c(8:9,11)])
}

mydatabq$ACEVTrE = vector(mode = 'numeric', nrow(mydatabq))

for (j in 1:11301){
  mydatabq$ACEVTrE[j] <- sum(mydatabq[j,c(1:5,10)])
}

mydatabq$ACEDV = vector(mode = 'numeric', nrow(mydatabq))
for (j in 1:11301){
  mydatabq$ACEDV[j] <- sum(mydatabq[j,c(6:7)])
}

mydatabq$ACETOTAL2 = vector(mode = 'numeric', nrow(mydatabq))
for (j in 1:11301){
  mydatabq$ACETOTAL2[j] <- sum(mydatabq[j,c(15:17)])
}
#creating new CSV for cleaned data
write.csv(finishdata,"C:/Users/kiloc/Documents/R/finaldata.csv", row.names = FALSE)
#descriptive stats
finishdata <- mydatabq[,12:17]
View(finishdata)
finishdata$Queer <- as.numeric(finishdata$Queer)
str(finishdata$Queer)
?boxplot
boxplot(ACETOTAL~Queer, data = finishdata, main = "Adversity Scores for Queer vs CisHet People")
mean(finishdata[finishdata$Queer == 1, "ACETOTAL"])
mean(finishdata[finishdata$Queer == 0, "ACETOTAL"])
boxplot(ACETOTAL~X_URBSTAT, data = finishdata)
mean(finishdata[finishdata$X_URBSTAT == 1, "ACETOTAL"])
mean(finishdata[finishdata$X_URBSTAT == 2, "ACETOTAL"])

var(finishdata[finishdata$Queer >= 0, "ACETOTAL"])
var(finishdata[finishdata$X_URBSTAT >= 1, "ACETOTAL"])
var(finishdata[finishdata$X_URBSTAT == 1, "ACETOTAL"])
var(finishdata[finishdata$X_URBSTAT == 2, "ACETOTAL"])

#normalizing data to fit assumptions
hist(finishdata$ACETOTAL)
x_log <- log10(finishdata$ACETOTAL)
plotNormalHistogram(x_log)
ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
sd(x)
??skewness

if(!require(psych)){install.packages("car")}
if(!require(MASS)){install.packages("MASS")}
if(!require(rcompanion)){install.packages("rcompanion")}
install.packages("rcompanion")
library(rcompanion)
?Gaussianize
LambertW::Gaussianize(finishdata$ACETOTAL)
install.packages('Rcpp')
library(Rcpp)
