#2019-2020
#Applied_Statistical_Modelling
data=read.csv("DefaultData.csv")
data=data[,-1]
head(data)
#boxplots
boxplot(income~default,data=data)
boxplot(balance~default,data=data)
#GLM
mod.logit=glm(default~student+balance+income,
              family=binomial,data=data)
summary(mod.logit)
#calculate fitted probabilities and find max
probs=mod.logit$fitted.values
max.ind=which(probs==max(probs))
probs[max.ind]
data[max.ind,]
summary(data[,"balance"])
#GLM with link function:probit
mod.probit=glm(default~student+balance+income,
               family=binomial(link="probit"),data=data)
probs.probit=mod.probit$fitted.values
max.ind.probit=which(probs.probit==max(probs.probit))
probs.probit[max.ind.probit]
data[max.ind.probit,]