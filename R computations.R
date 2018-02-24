#Projet d'econometrie 
rm(list = ls())


#Installation des packages
#install.packages("aod")
library(aod)
#install.packages("stats")
library(stats)
#install.packages("AER")
library(AER)
#install.packages("stargazer",repos = "http://cran.us.r-project.org")
library(stargazer)
#install.packages("mfx")
library(mfx)
#install.packages("sets")
library (sets)
#install.packages("stringr")
library(stringr)

#Importation des donnees

setwd("/Users/Gauthier/Desktop/Rapport de stage ENSAE/")
data<-read.csv("/Users/Gauthier/Desktop/Rapport de stage ENSAE/Data.csv", header=T, sep=";", na.strings = "NA",dec=",")

names(data)[1]<-"Id" 
names(data)[6]<-"CostSender"
names(data)[40]<-"Regime"
names(data)[43]<-"TargetRegime"
data<-data[data$Id!="",]

summary(data)

################################################################################################
################################## Probit Model ################################################
################################################################################################

###############################################################################################
#Version complete, avec l'ensemble des variables, en enlevant les trade-dispute, les threats sont egalement eliminees
data_full <- data[-c(196:204),]
probit_full <- glm(Success ~  PoliticalAmbition+StrongExports+StrongFinancial
              +CooperationLevel+PriorRelation+GnpRatio+CostTarget+SupportTarget+TargetRegime
              +TradeLinkage +CostSender +HealthStability, 
              family=binomial(link="probit"), data=data_full)

summary(probit_full)
beta<-coefficients(probit_full)
confint(probit_full)

###############################################################################################
#On utilise le critere d'information d'Akaike pour selectionner les modeles
#On etablit la liste des variables
data_subset <-subset (data_full,select=c("Success","PoliticalAmbition","StrongExports","StrongFinancial","CooperationLevel","PriorRelation","GnpRatio","CostTarget","SupportTarget","TargetRegime","TradeLinkage","CostSender","HealthStability","UsOnly"))

#On utilise l'ensemble des parties pour avoir tous les modeles possibles
bib<-as.set(2^as.set(c(2:13)))
bib<-toString(bib)
bib<-gsub('\\list', '',bib)
bib <- str_extract_all(bib, "\\([^()]+\\)")[[1]]
bib <- substring(bib, 2, nchar(bib)-1)

bab<-bib
aic<-vector("numeric", length = length(bib))
for (i in c(1:length(bib))){
  bab[i] <- strsplit(bib[i][[1]], ",")
  bab[i][[1]]=as.numeric(bab[i][[1]])
  probit <- glm(Success ~., 
                family=binomial(link="probit"), data=subset(data_subset, select=c(1,bab[i][[1]]) ))
  aic[i]<-probit$aic
}

#On affiche le modele qui minimise l'AIC
optimal_model<-c(1,bab[which.min(aic)][[1]])
print("variables included:")
print(colnames(data_subset[optimal_model]))

###############################################################################################
#On se concentre sur le modele optimal
probit <- glm(Success ~., 
              family=binomial(link="probit"), data=subset(data_subset, select=optimal_model ))

summary(probit)
stargazer( probit_full, probit, title="Probit Model", align=TRUE, report=('vc*p'))

###############################################################################################
#On cherche a expliquer pourquoi CostTarget est non significatif
reg_cost <- lm(CostTarget ~., data=subset(data_subset, select=c(2:13)))

summary(reg_cost)
stargazer( reg_cost, title="CostTarget analysis", align=TRUE, report=('vc*p'))

######################################
#Wald Test
wald.test(b=coefficients(probit), Sigma=vcov(probit), Terms=1:length(optimal_model))
#on ne rejette pas le modele 

#On calcule les effets marginaux moyens
marginal_effects<-probitmfx(Success ~., data=subset(data_subset, select=optimal_model ), atmean = FALSE, robust = FALSE, start = NULL, control = list())
marginal_effects
#robustness?
stargazer(marginal_effects$mfxest)

################################################################################################
################################## Robustness Checks ###########################################
################################################################################################

#Logit vs Probit
logit <- glm(Success ~., 
              family=binomial(link="logit"), data=subset(data_subset, select=optimal_model ))
summary(logit)
stargazer( probit, logit, title="Probit vs Logit", align=TRUE, report=('vc*p'))

#Usage des sanctions dans un conflit arme
data_nowar<- data_subset[-c(1,9:12,25,26,107,114),]
probit_nowar <- glm(Success ~., 
             family=binomial(link="logit"), data=subset(data_nowar, select=optimal_model ))
summary(probit_nowar)
stargazer( probit, probit_nowar, title="Probit vs Logit", align=TRUE, report=('vc*p'))
rm(data_nowar)

#Trade disputes
data_tradedisputes <-subset (data,select=c("Success","PoliticalAmbition","StrongExports","StrongFinancial","CooperationLevel","PriorRelation","GnpRatio","CostTarget","SupportTarget","TargetRegime","TradeLinkage","CostSender","HealthStability"))
probit_tradedisputes <- glm(Success ~., 
                    family=binomial(link="logit"), data=subset(data_tradedisputes, select=optimal_model ))
summary(probit_tradedisputes)
rm(data_tradedisputes)

#Intervention unilaterale USA
data_US <- subset(data_subset, select=c(optimal_model,14))2^12
probit_US <- glm(Success ~., 
                            family=binomial(link="logit"), data=data_US)
summary(probit_US)
rm(data_US)

#Ensemble des tests
stargazer(probit, logit, probit_nowar, probit_tradedisputes, probit_US, report=('vc*p'))

