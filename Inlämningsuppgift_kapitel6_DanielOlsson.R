rm(list = ls())

#Installerar nödvändiga paket
install.packages("glmnet")
install.packages("pls")
library(glmnet)
library(pls)

#Läser in data och standardiserar
{ 
  load("UScrime_vt20.Rdata")
  data.crime.sc <- as.data.frame(scale(data.crime))
}
#a)
#Dela upp data i tränings- och testdel
set.seed(1)
train <- sample(1122, 897)
test <- (-train)

#Ridge Regression
x = model.matrix(Crime~., data.crime.sc)[, -95]
y = data.crime.sc$Crime
y.test <- y[test]
grid <- 10^seq(10, -2, length = 100)
mod.RR <- glmnet(x,y,alpha = 0, lambda = grid, standardize = FALSE)
mod.RR.2 <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12, standardize = FALSE)
pred.RR <- predict(mod.RR.2, s=4, newx = x[test,])
mean((pred.RR - y.test)^2)
mean((mean(y[train])-y.test)^2)
pred.RR.2 <- predict(mod.RR.2, s=1e10, newx = x[test, ])
mean((pred.RR.2-y.test)^2)
#Test MSE:n är 0.341 vilket är lägre än fallet då vi endast använder ett intercept i modellen, för det fallet är 
#test MSE:n 0.816. 

#Ordinary Least Squares
pred.RR.3 <- predict(mod.RR.2, s=0, x = x, y = y, newx = x[test, ], exact = T)
mean((pred.RR.3 - y.test) ^ 2)
mean((data.crime.sc[test,]$Crime - predict.lm(mod.OLS, data.crime.sc[test,])) ^ 2)
#Test MSE:n här uppgår till 1.41. Tränings-MSE uppgår till 0.24. 
mod.OLS <- lm(y~x, data = data.crime.sc)
predict(mod.RR.2, s=0, exact=T, x=x, y=y, type = "coefficients")[1:20,]

#Fram till nu har vi använt ett lambda = 4. Låt oss köra korsvalidering för att hitta det "bästa" värdet på lambda. 
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
best.lambda <- cv.out$lambda.min
abline(v = log(best.lambda), lty = 2, lwd = 2, col = 'blue')
pred.RR.4 <- predict(mod.RR.2, s = best.lambda, newx = x[test, ])
mean((pred.RR.4 - y.test) ^ 2)
best.lambda
#Test MSE:n är i detta fall 0.318 vilket är mindre än 0.341.  
#Låt oss till slut skatta ridge regression på hela datamaterialet. 
out <- glmnet(x, y, alpha = 0, standardize = FALSE)
predict(out, s = best.lambda, type = 'coefficients')
#Då alla variabler är skilda från 0 så bekräftar det att inga variabler tas bort. Vidare går vi igenom en metod som gör just
#det. Lasso sätter en del variabler lika med 0, de tas bort med andra ord. 

#Lasso
lasso.mod.1 <- glmnet(x[train, ], y[train], alpha=1, lambda = grid, standardize = FALSE)
plot(lasso.mod.1)
#Vi kan se att olika värden på lambda resulterar i olika antal variabler. Låt oss hitta det "bästa" värdet på lambda med 
#hjälp av korsvalidering. 
set.seed(1)
cv.out.2 <- cv.glmnet(x[train ,],y[ train],alpha=1)
plot(cv.out.2)
bestlam <- cv.out.2$lambda.min 
bestlam
lasso.pred.1 <- predict(lasso.mod.1, s=bestlam, newx = x[test, ])
mean((lasso.pred.1-y.test)^2)
#Detta resulterar i en test MSE på 0.313, vilket är bättre än vad Ridge Regression presterade (0.318). 
out.2 <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out.2, type = "coefficients", s=bestlam)
lasso.coef
#Här ser vi att en del variabler är satta lika med 0. Lasso gör alltså variabelselection. 
lasso.coef[lasso.coef!=0]

#Låt oss fortsätta med PCR med hjälp av träningsdata
set.seed(1)
mod.pcr.2 <- pcr(Crime~., data = data.crime.sc, subset = train, scale = FALSE, validation = "CV")
summary(mod.pcr.2)
validationplot(mod.pcr.2, val.type = "MSEP")
pred.pcr.1 <- predict(mod.pcr.2, x[test,], ncomp = 40)
mean((pred.pcr.1-y.test)^2)
#Test MSE på 0.802. 

#PLS
set.seed(1)
mod.pls.1 <- plsr(Crime~., data = data.crime.sc, subset = train, scale = FALSE, validation = "CV")
summary(mod.pls.1)
validationplot(mod.pls.1, val.type = "MSEP")
#Lägsta CV-error uppnås när man använder M = 10 komponenter.
pred.pls.1 <- predict(mod.pls.1, x[test,], ncomp = 10)
mean((pred.pls.1-y.test)^2)
#Test MSE uppgår till 0.955. 

#b)
#Se rapport.

#c)
#Fick veta att detta var en VG-uppgift, har därför inte gjort den. 