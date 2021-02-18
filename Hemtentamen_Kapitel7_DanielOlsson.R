rm(list=ls())
install.packages("leaps")
library(ISLR)
library(leaps)
library(gam)
library(splines)
attach(Auto)
summary(Auto)

#Ta bort observationer med 3 eller 5 cylindrar
Auto_new <- subset(Auto, cylinders == 4 | cylinders == 6 | cylinders == 8, 
                   select = c(mpg, cylinders, displacement, horsepower, weight, acceleration, year, origin)) 
{
#Koda om eventuella variabler
Auto_new$origin <- as.factor(Auto_new$origin) 
Auto_new$year <- as.factor(Auto_new$year)
Auto_new$cylinders <- as.factor(Auto_new$cylinders)
plot(Auto_new)
summary(Auto_new)
}

{
#Best subset för att hitta variablerna med lägst RSS
regfit.full.1 <- regsubsets(mpg~., Auto_new)
#Möjliggör för fler än 8 variabler
regfit.full <- regsubsets(mpg~., data=Auto_new, nvmax = 12)
reg.summary <- summary(regfit.full)
names(reg.summary)
#Plotta för att välja modell
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(12, reg.summary$adjr2[12], col="red",cex=2,pch =20)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(reg.summary$cp )
points (12, reg.summary$cp[12], col ="red", cex=2, pch =20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(11, reg.summary$bic[11], col = "red", cex=2, pch=20)
plot(regfit.full ,scale="r2")
plot(regfit.full ,scale="adjr2")
plot(regfit.full ,scale="Cp")
plot(regfit.full ,scale="bic")
reg.summary
#Alla plottar visar att cylinders, horsepower, weight, year och origin ska inkluderas i analysen. Vi kan därför utesluta
#displacement och acceleration. 
}

{
#Polynomial regression
#Vi börjar med att kontrollera om det behövs icke-linjära termer för variablerna horsepower och weight. 
horse.1=lm(mpg~horsepower+cylinders+weight+year+origin, data=Auto_new)
horse.2=lm(mpg~poly(horsepower, 2)+cylinders+weight+year+origin, data=Auto_new)
horse.3=lm(mpg~poly(horsepower, 3)+cylinders+weight+year+origin, data=Auto_new)
horse.4=lm(mpg~poly(horsepower, 4)+cylinders+weight+year+origin, data=Auto_new)
horse.5=lm(mpg~poly(horsepower, 5)+cylinders+weight+year+origin, data=Auto_new)
anova(horse.1, horse.2, horse.3, horse.4, horse.5)
#Icke-linjär term behövs för horsepower, vi tittar vidare på weight.
weight.1=lm(mpg~weight+cylinders+horsepower+year+origin, data = Auto_new)
weight.2=lm(mpg~poly(weight, 2)+cylinders+horsepower+year+origin, data = Auto_new)
weight.3=lm(mpg~poly(weight, 3)+cylinders+horsepower+year+origin, data = Auto_new)
weight.4=lm(mpg~poly(weight, 4)+cylinders+horsepower+year+origin, data = Auto_new)
weight.5=lm(mpg~poly(weight, 5)+cylinders+horsepower+year+origin, data = Auto_new)
anova(weight.1, weight.2, weight.3, weight.4, weight.5)
#Icke-linjär term behövs för weight. Låt oss testa olika alternativ för polynomtermerna.
poly.1=lm(mpg~poly(weight, 2)+cylinders+horsepower+year+origin, data = Auto_new)
poly.2=lm(mpg~poly(weight, 2)+cylinders+poly(horsepower, 2)+year+origin, data = Auto_new)
poly.3=lm(mpg~poly(weight, 2)+cylinders+poly(horsepower, 3)+year+origin, data = Auto_new)
poly.4=lm(mpg~poly(weight, 2)+cylinders+poly(horsepower, 4)+year+origin, data = Auto_new)
anova(poly.1, poly.2, poly.3, poly.4)
#Vi ser att vi behöver kvadratisk term för horsepower. 
poly.5=lm(mpg~poly(horsepower, 2)+cylinders+weight+year+origin, data = Auto_new)
poly.6=lm(mpg~poly(horsepower, 2)+cylinders+poly(weight, 2)+year+origin, data = Auto_new)
poly.7=lm(mpg~poly(horsepower, 2)+cylinders+poly(weight, 3)+year+origin, data = Auto_new)
poly.8=lm(mpg~poly(horsepower, 2)+cylinders+poly(weight, 4)+year+origin, data = Auto_new)
anova(poly.5, poly.6, poly.7, poly.8)
poly.9=lm(mpg~horsepower+cylinders+weight+year+origin, data = Auto_new)
poly.10=lm(mpg~poly(horsepower, 2)+cylinders+poly(weight, 2)+year+origin, data = Auto_new)
poly.11=lm(mpg~poly(horsepower, 3)+cylinders+poly(weight, 2)+year+origin, data = Auto_new)
anova(poly.9, poly.10, poly.11)
#Vi ser att en kvadratisk term behövs för både weight och horsepower. Min slutgiltiga modell är alltså poly.10.
mod.1 <- gam(mpg~poly(horsepower, 2)+poly(weight, 2)+cylinders+year+origin, data = Auto_new)
}

{
#Smoothing splines
horselims <- range(Auto_new$horsepower)
horse.grid <- seq(from=horselims[1], to=horselims[2])
spline.1 <- lm(mpg~bs(horsepower, knots = c(75, 93, 129)), data = Auto_new)
pred <- predict(spline.1, newdata = list(horsepower=horse.grid), se=T)
par(mfrow=c(1,1))
plot(horsepower, mpg, col="gray")
lines(horse.grid, pred$fit, lwd = 2)
lines(horse.grid, pred$fit+2*pred$se, lty = "dashed")
lines(horse.grid, pred$fit-2*pred$se, lty = "dashed")
dim(bs(Auto_new$horsepower, knots = c(75, 93, 129)))
dim(bs(Auto_new$horsepower, df=6))
attr(bs(horsepower, df=6), "knots")
}

{
#Figur med 16 df och antal df med korsvalidering.
par(mfrow=c(1,2))
plot(Auto_new$horsepower, Auto_new$mpg, xlim=horselims, cex = .5, col = "darkgrey")
title(main = "Smoothing Spline ")
fit.s1=smooth.spline(Auto_new$horsepower, Auto_new$mpg, df=16)
fit.s2=smooth.spline(Auto_new$horsepower, Auto_new$mpg, cv=TRUE)
fit.s2$df
lines(fit.s1, col="red", lwd = 2)
lines(fit.s2, col="blue", lwd=2)
legend("topright", legend = c("16 DF", "5.1 DF (LOOCV)"), col=c("red","blue"), lty=1,lwd=2, cex =.8)
}

{
#Samma fast för weight
weightlims <- range(weight)
plot(Auto_new$weight, Auto_new$mpg, xlim=weightlims, cex = .5, col = "darkgrey")
title("Smoothing Spline ")
fit.s3=smooth.spline(Auto_new$weight, Auto_new$mpg, df=16)
fit.s4=smooth.spline(Auto_new$weight, Auto_new$mpg, cv=TRUE)
fit.s4$df
lines(fit.s3, col="red", lwd = 2)
lines(fit.s4, col="blue", lwd=2)
legend("topright", legend = c("16 DF", "11.3 DF (LOOCV)"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)
}

{
#GAM
mod.3 <- gam(mpg~ns(horsepower, 5)+ns(weight, 5)+cylinders+year+origin, data = Auto_new)
summary(mod.3)
mod.2 <- gam(mpg~s(horsepower, 5.1)+s(weight, 11.3)+cylinders+year+origin, data = Auto_new)
par(mfrow=c(1,5))
plot(mod.2, se=TRUE, col = "blue")
plot.Gam(mod.3, se=T, col="red")
#Låt oss testa för att se om horsepower eller weight behövs.
h.1 <- gam(mpg~s(weight, 5)+cylinders+year+origin, data = Auto_new)
h.2 <- gam(mpg~horsepower+s(weight, 5)+cylinders+year+origin, data = Auto_new)
h.3 <- gam(mpg~s(horsepower, 5)+s(weight, 5)+cylinders+year+origin, data = Auto_new)
anova(h.1, h.2, h.3, test = "F")
#horsepower behövs som en icke-linjär term. Samma procedur för weight.
w.1 <- gam(mpg~s(horsepower, 5)+cylinders+year+origin, data = Auto_new)
w.2 <- gam(mpg~weight+s(horsepower, 5)+cylinders+year+origin, data = Auto_new)
w.3 <- gam(mpg~s(weight, 5)+s(horsepower, 5)+cylinders+year+origin, data = Auto_new)
anova(w.1, w.2, w.3, test = "F")
#Även weight behövs som en icke-linjär term. 
}

#Jämförelse av de olika modellerna med avseende på AIC. 
summary(mod.1) #AIC = 1837.639
summary(mod.2) #AIC = 1840.908
summary(mod.3) #AIC = 1841.908
#Då mod.1 har lägst AIC, väljer jag att gå vidare med den modellen. Vi ser även att alla variabler i mod.1 är signifikanta, vi
#kan då behålla alla variabler inför våran prediktion. 

#Vidare delar vi upp datamaterialet i en testdel och en träningsdel.
set.seed(1)
train <- sample(385, 308)
test <- Auto_new[-train, ]

#Let's predict this shit
train.mod <- gam(mpg~poly(horsepower, 2)+poly(weight, 2)+cylinders+year+origin, subset = train, data = Auto_new)
preds <- predict.Gam(train.mod, newdata = test)
mean((test$mpg - preds)^2)
install.packages("Metrics")
library(Metrics)
mse(test$mpg, preds)
1 - ((mean((test$mpg - preds)^2))/(mean((test$mpg - mean(test$mpg))^2)))
#Plottar upp den skattade modellen
par(mfrow=c(2,3))
plot(train.mod)