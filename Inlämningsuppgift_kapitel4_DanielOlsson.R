rm(list=ls())

#Installerar nödvändiga paket
install.packages("MASS")
install.packages("class")
library(MASS)
library(class)

#Läser in data
Vindata <- read.table("kvalitetvittvin.Rdata", sep = "", header = TRUE) 

#Överblick över data
dim(Vindata)
names(Vindata)
summary(Vindata)
pairs(Vindata)
plot(Vindata)

#Delar upp responsvariabeln
attach(Vindata)
bin.quality <- cut(quality, breaks=c(1, 6, 10), labels=c("Not Excellent", "Excellent"))
Vindata$bin.quality <- bin.quality
Vindata$quality <- as.numeric(Vindata$quality)
Vindata$bin.quality <- as.numeric(Vindata$bin.quality)
levels(bin.quality)
class(bin.quality)
#Ansätter korrekt värden till responsvariabeln
Vindata$bin.quality[Vindata$bin.quality == 1] <- 0
Vindata$bin.quality[Vindata$bin.quality == 2] <- 1

#a) 
#Dela upp materialet i "tränings"- och "test"-del
set.seed(1)
train <- sample(4898, 3918) 
test <- Vindata[-train, ]

#b)
#Logistisk regression
quality.test <- bin.quality[-train]
contrasts(bin.quality)
vin.log.1 <- glm(bin.quality~.-quality, data = Vindata, family = binomial, subset = train)
summary(vin.log.1)
dim(test)
vin.prob <- predict(vin.log.1, test, type = "response")
vin.pred <- rep("Not Excellent", 980)
vin.pred[vin.prob > .5] <- "Excellent"
table(quality.test, vin.pred)
mean.LR <- mean(vin.pred==quality.test)

#LDA
lda.train <- lda(bin.quality~.-quality, data = Vindata, subset = train)
lda.train
#Vi ser att pihatt1 = 0.786 och pihatt2 = 0.214. Detta innebär att 78.6% av träningsdatat korresponderar till
#att vinkvaliteten inte är "excellent" och 21.4% av träningsdatat korresponderar till att vinkvaliteten är "excellent".
plot(lda.train)
lda.pred <- predict(lda.train, test)
names(lda.pred)
lda.class <- lda.pred$class
table(quality.test, lda.class)
mean.LDA <- (712+69)/(712+46+153+69)
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)
lda.pred$posterior [1:20,1]
lda.class [1:20]

#QDA
qda.train <- qda(bin.quality~.-quality, data = Vindata, subset = train)
qda.train
qda.class <- predict(qda.train, test)$class
table(quality.test, qda.class)
mean.QDA <- (581+158)/(581+177+64+158)

#kNN
train.X <- cbind(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, 
                 total.sulfur.dioxide, density, pH, sulphates, alcohol)[train, ]
test.X <- cbind(fixed.acidity, volatile.acidity, citric.acid, residual.sugar, chlorides, free.sulfur.dioxide, 
                 total.sulfur.dioxide, density, pH, sulphates, alcohol)[-train, ] 
train.bin.quality <- bin.quality[train]
set.seed(1)

#Enda närmaste grannen
knn.pred.1 <- knn(train.X, test.X, train.bin.quality, k=1)
table(quality.test, knn.pred.1)
mean.knn.1 <- mean(knn.pred.1==quality.test)
mean.knn.1

#3 närmaste grannarna
knn.pred.3 <- knn(train.X, test.X, train.bin.quality, k=3)
table(quality.test, knn.pred.3)
mean.knn.3 <- mean(knn.pred.3==quality.test)
mean.knn.3

#5 närmaste grannarna
knn.pred.5 <- knn(train.X, test.X, train.bin.quality, k=5)
table(quality.test, knn.pred.5)
mean.knn.5 <- mean(knn.pred.5==quality.test)
mean.knn.5

#7 närmaste grannarna
knn.pred.7 <- knn(train.X, test.X, train.bin.quality, k=7)
table(quality.test, knn.pred.7)
mean.knn.7 <- mean(knn.pred.7==quality.test)
mean.knn.7

#Överblick över metodernas olika prestationer
mean.LR
mean.LDA
mean.QDA
mean.knn.1

#c)
#Se rapport.

#d)
#Logistisk regression
vin.log.2 <- glm(bin.quality~.-quality-citric.acid-total.sulfur.dioxide-alcohol, data = Vindata, family = binomial, subset = train)
summary(vin.log.2)
vin.prob.1 <- predict(vin.log.2, test, type = "response")
vin.pred.1 <- rep("Not Excellent", 980)
vin.pred.1[vin.prob.1 > .5] <- "Excellent"
table(quality.test, vin.pred.1)
mean.LR.1 <- mean(vin.pred.1==quality.test)

#LDA
lda.train.1 <- lda(bin.quality~.-quality-citric.acid-total.sulfur.dioxide-alcohol, data = Vindata, subset = train)
lda.train.1
#Vi ser att pihatt1 = 0.786 och pihatt2 = 0.214. Detta innebär att 78.6% av träningsdatat korresponderar till
#att vinkvaliteten inte är "excellent" och 21.4% av träningsdatat korresponderar till att vinkvaliteten är "excellent".
plot(lda.train.1)
lda.pred.1 <- predict(lda.train.1, test)
names(lda.pred.1)
lda.class.1 <- lda.pred.1$class
table(quality.test, lda.class.1)
mean.LDA.1 <- (717+68)/(717+68+41+154)
sum(lda.pred.1$posterior[,1] >= .5)
sum(lda.pred.1$posterior[,1] < .5)
lda.pred.1$posterior [1:20,1]
lda.class.1 [1:20]

#QDA 
qda.train.1 <- qda(bin.quality~.-quality-citric.acid-total.sulfur.dioxide-alcohol, data = Vindata, subset = train)
qda.train.1
qda.class.1 <- predict(qda.train.1, test)$class
table(quality.test, qda.class.1)
mean.QDA.1 <- (637+131)/(637+131+121+91)

#KNN
train.X.1 <- cbind(fixed.acidity, volatile.acidity, residual.sugar, chlorides, free.sulfur.dioxide, density, pH, sulphates)[train, ]
test.X.1 <- cbind(fixed.acidity, volatile.acidity, residual.sugar, chlorides, free.sulfur.dioxide, density, pH, sulphates)[-train, ] 
train.bin.quality.1 <- bin.quality[train]
set.seed(1)

#Enda närmaste grannen
knn.pred.1.1 <- knn(train.X.1, test.X.1, train.bin.quality.1, k=1)
table(quality.test, knn.pred.1.1)
mean.knn.1.1 <- mean(knn.pred.1.1==quality.test)
mean.knn.1.1

#3 närmaste grannarna
knn.pred.3.1 <- knn(train.X.1, test.X.1, train.bin.quality.1, k=3)
table(quality.test, knn.pred.3.1)
mean.knn.3.1 <- mean(knn.pred.3.1==quality.test)
mean.knn.3.1

#5 närmaste grannarna
knn.pred.5.1 <- knn(train.X.1, test.X.1, train.bin.quality.1, k=5)
table(quality.test, knn.pred.5.1)
mean.knn.5.1 <- mean(knn.pred.5.1==quality.test)
mean.knn.5.1

#7 närmaste grannarna
knn.pred.7.1 <- knn(train.X.1, test.X.1, train.bin.quality.1, k=7)
table(quality.test, knn.pred.7.1)
mean.knn.7.1 <- mean(knn.pred.7.1==quality.test)
mean.knn.7.1

#9 närmaste grannarna
knn.pred.9.1 <- knn(train.X.1, test.X.1, train.bin.quality.1, k=9)
table(quality.test, knn.pred.9.1)
mean.knn.9.1 <- mean(knn.pred.9.1==quality.test)
mean.knn.9.1

#Överblick över metodernas olika prestationer
mean.LR.1
mean.LDA.1
mean.QDA.1
mean.knn.1.1

#Överblick över alla metoders olika prestationer
mean.LR
mean.LR.1
mean.LDA
mean.LDA.1
mean.QDA
mean.QDA.1
mean.knn.1
mean.knn.1.1

#e)
LR.probs <- predict(vin.log.1, test, type = "response")
vin.pred.2 <- rep("Not Excellent", 980)
vin.pred.2[LR.probs > .3] <- "Excellent"
table(quality.test, vin.pred.2)

lda.class.2 <- ifelse(lda.pred$posterior[, 2] > .3, lda.class.2 <- "Excellent", lda.class.2 <- "Not Excellent")
table(quality.test, lda.class.2)