#Data cleaning
library(dplyr)
load("cancer.RData")
cancer <- filter(cancer, incidenceRate <= 1100)
cancer[which(cancer$AvgHouseholdSize < 1), ]$AvgHouseholdSize <- 
  100*cancer[which(cancer$AvgHouseholdSize < 1), ]$AvgHouseholdSize

#Impute

mod1=lm(PctEmployed16_Over~+deathRate+incidenceRate+medIncome+binnedInc+povertyPercent+MedianAgeMale+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+PctBlack+PctMarriedHouseholds+Edu18_24,cancer)
missdf = cancer[which(is.na(cancer$PctEmployed16_Over)==TRUE),]
imputed = predict(mod1,missdf)
cancer[which(is.na(cancer$PctEmployed16_Over)==TRUE),"PctEmployed16_Over"] = imputed

#Transformation
cancer$logpctblack = log(cancer$PctBlack+0.05)
cancer$logmedincome = log(cancer$medIncome)

#Drop Geography, binnedInc, deathRate
cancer2<-cancer[,-c(1,4)] 
cancer3<-cancer2[,-c(16)]

#Lasso
library(glmnet)
fit1<-glmnet(cancer3,cancer2$deathRate,alpha = 1)
plot(fit1)
plot(fit1,"lambda",label = T)
coef(fit1,s=1)

#Finding lambda using CV

cvfit1 = cv.glmnet(data.matrix(cancer3),cancer2$deathRate,alpha=1)
plot(cvfit1)
cvfit1$lambda.min
cvfit1$lambda.1se
abline(v=log(cvfit1$lambda.1se),col="red")
abline(v=log(cvfit1$lambda.min),col="blue")
legend("topright",legend=c("Minimum lambda", "1 standard error larger lambda"),lty=c(1,1),col=c("blue","red"), ins=0.05)
coef(cvfit1, s = "lambda.min")
coef(cvfit1, s = "lambda.1se")

#Result
#Drop medIncome, povertyPercent, MedianAgeMale, PctBlack, PctMarriedHouseholds
#medIncome,PctBlack dropped because we introduced the transformations
#PovertyPercent dropped because high correlation to logmedincome(-0.9)
#MedianAgeMale dropped because high correlation to MedainAgeFemale(0.9)
#PctMarriedHouseholds dropped because high correlation to PercentMarried (0.9)

#Correlation Matrix
library(GGally) 
ggcorr(cancer3, geom = "blank", label = TRUE, hjust = 0.8, label_size = 6 ,size = 5) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.6)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = "none", alpha = "none")


#Try fitting linear model
cancer4<-cancer2[,-c(2:4,13,14)]
lm1<-lm(deathRate ~ ., data=cancer4)
lm1<-lm(deathRate ~ incidenceRate+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctEmployed16_Over
        +PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+Edu18_24
        +logpctblack+logmedincome, data=cancer4)
summary(lm1)
anova(lm1)

#With squared AvgHouseholdSize
lm2<-lm(deathRate ~ incidenceRate+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctEmployed16_Over
        +PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+Edu18_24
        +logpctblack+logmedincome+I(AvgHouseholdSize^2), data=cancer4)
summary(lm2)
anova(lm1,lm2)
#Anova returns <0.0005 suggesting adding the squared term

#With squared MedianAgeFemale
lm3<-lm(deathRate ~ incidenceRate+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctEmployed16_Over
        +PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+Edu18_24
        +logpctblack+logmedincome+I(MedianAgeFemale^2), data=cancer4)
summary(lm3)
anova(lm1,lm3)

#With both squared terms
lm4<-lm(deathRate ~ incidenceRate+MedianAgeFemale+AvgHouseholdSize+PercentMarried+PctEmployed16_Over
        +PctUnemployed16_Over+PctPrivateCoverage+PctEmpPrivCoverage+PctPublicCoverage+Edu18_24
        +logpctblack+logmedincome+I(AvgHouseholdSize^2)+I(MedianAgeFemale^2), data=cancer4)
summary(lm4)
anova(lm3,lm4)
#Adding one of them to the model is fine, but adding both causes problem

#Trying to find if the squared terms are correlated
cancer5<-cancer4
cancer5$age2<-(cancer4$MedianAgeFemale)^2
cancer5$size2<-(cancer4$AvgHouseholdSize)^2

ggcorr(cancer5, geom = "blank", label = TRUE, hjust = 0.8, label_size = 6 ,size = 5) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.6)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = "none", alpha = "none")

#The correlation is -0.6

#Play stepwise
step()