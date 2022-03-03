library(dplyr)
library(glmnet)
load("cancer.RData")
cancer <- filter(cancer, incidenceRate <= 1100)
cancer[which(cancer$AvgHouseholdSize < 1), ]$AvgHouseholdSize <- 
  100*cancer[which(cancer$AvgHouseholdSize < 1), ]$AvgHouseholdSize

#Impute

## Changed so we're not imputing with deathrate anymore
mod1=lm(PctEmployed16_Over~incidenceRate+medIncome+binnedInc+povertyPercent+MedianAgeMale+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+PctBlack+PctMarriedHouseholds+Edu18_24,cancer)
missdf = cancer[which(is.na(cancer$PctEmployed16_Over)==TRUE),]
imputed = predict(mod1,missdf)
cancer[which(is.na(cancer$PctEmployed16_Over)==TRUE),"PctEmployed16_Over"] = imputed

#Transformation
cancer$logpctblack = log(cancer$PctBlack+0.05)
cancer$logmedincome = log(cancer$medIncome)

cancermodel2 = separate(cancer,"Geography", into=c("County","State"),sep=",")[-c(1,4,5,16)]
cmax=lm(deathRate~.,data=cancermodel2)

cvfit1 = cv.glmnet(model.matrix(cmax),cancermodel2$deathRate,alpha=0)
plot(cvfit1)
cvfit1$lambda.min
cvfit1$lambda.1se
abline(v=log(cvfit1$lambda.1se),col="red")
abline(v=log(cvfit1$lambda.min),col="blue")
legend("topright",legend=c("Minimum lambda", "1 standard error larger lambda"),lty=c(1,1),col=c("blue","red"), ins=0.05)
coef(cvfit1, s = "lambda.min")
coef(cvfit1, s = "lambda.1se")
#Ridge
library(glmnet)

fit1<-glmnet(model.matrix(cmax),cancermodel2$deathRate,alpha = 0,lambda=cvfit1$lambda.min)

#Plot
ridgefitted1 = predict(fit1,newx=model.matrix(cmax))
ridgeresid1 = cancermodel2$deathRate-ridgefitted1
plot(ridgeresid1~ridgefitted1)
abline(h=0,col="red")

# Plot is symettrical about zero so have zero mean and constant variance

step()

# Run Stepwise above this line and perform ridge on below

set.seed(153)
ridgecv = cv.glmnet(model.matrix(hybridoptimalBIC),cancermodel$deathRate,alpha=0)
optimallambda = ridgecv$lambda.min
ridgemod = glmnet(model.matrix(hybridoptimalBIC),cancermodel$deathRate,alpha=0,lambda=optimallambda)
coef(ridgemod)

ridgefitted = predict(ridgemod,newx=model.matrix(ridgemod))
ridgeresid = cancermodel$deathRate-ridgefitted
plot(ridgeresid~ridgefitted)
abline(h=0,col="red")



# Plot satisfies assumptions
