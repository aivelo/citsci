#necessary packages

deps = c("car","mirt","likert","dplyr","forcats","dotwhisker","psych",
         "tidyr","tidyverse", "rfigshare","nlme","lme4","parallel",
         "summarytools","RColorBrewer","summarytools");

for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}

#setting up data

url <- "https://ndownloader.figshare.com/files/34787506"
download.file(url, destfile="data.csv", 'libcurl')
data <- read.csv(file="data.csv", sep=";")

data$school <- as.factor(data$school)
data$gender <- as.factor(data$gender)
data$age <- as.numeric(data$age)
data$n_plate<- as.numeric(data$n_plate)
data$n_obs <- as.numeric(data$n_obs)
data$n_conf <- as.numeric(data$n_conf)

#descriptive

summary(data, maxsum=20)

tt <- table(data$school)
rare_levels <- names(tt)[tt<10]
data <- subset(data,!school %in% rare_levels)
data$school <- droplevels(data$school)

summary(data, maxsum=20)


# setting up likert scale answers
lik <- data[,9:28]
lik[lik==1] <- "Strongly disagree"
lik[lik==2] <- "Somewhat disagree"
lik[lik==3] <- "Not disagree or agree"
lik[lik==4] <- "Somewhat agree"
lik[lik==5] <-"Strongly agree"
lik[]<- lapply(lik, as.factor)

likert <- data[,9:28]


#Figure 1 

factlevel <- c("Strongly disagree", "Somewhat disagree","Not disagree or agree", "Somewhat agree","Strongly agree")

factfunc <- function(mydata, factlevel){
  factor(mydata, 
         levels=factlevel, 
         ordered = TRUE)
  fct_unify(mydata, 
            levels=factlevel) 
}

lik_ordr <- factfunc(lik, factlevel) %>%as.data.frame()

names(lik_ordr) <- c(
  X1="I do not think that there is anything wrong with using rats in medical research.",
  X2="I am not interested in learning how humans, plants and animals depend on each other.",
  X3="I like to study biology.",
  X4="It would bother me tremendously to touch a dead body.",
  X5="It would bother me to see a rat run across my path in a park.",
  X6="I want to learn how energy can be saved and used in more effective way.",
  X7="I think that rats belong to urban landscape.",
  X8="It is boring to study biology.",
  X9="Some aspects of biology can only be learned through dissecting preserved animals such as rats.",
  X10="I want to learn about climate change and how to prevent it.",
  X11="I think it is perfectly acceptable for rats to be killed with traps or rodenticides in urban environment.",
  X12="I want to learn about new energy sources from sun, wind, tides, waves etc.",
  X13="Biology is one of my favorite topics in school.",
  X14="It would bother me to be in a science class, and to see a human hand preserved in a jar.",
  X15="I want to learn about technology helps us to handle waste, garbage and sewage. ",
  X16="Usually we have interesting exercises in biology.",
  X17="I might be willing to try eating rat meat, under some circumstances.",
  X18="I would go out of my way to avoid walking through cemetery.",
  X19="For me biology is an easy school subject.",
  X20="Basically, city is a home for humans and we have the right to stop other animals from coming to our cities.")


order <- c("I do not think that there is anything wrong with using rats in medical research.","I think that rats belong to urban landscape.","Some aspects of biology can only be learned through dissecting preserved animals such as rats.","I think it is perfectly acceptable for rats to be killed with traps or rodenticides in urban environment.","Basically, city is a home for humans and we have the right to stop other animals from coming to our cities.",
           "I am not interested in learning how humans, plants and animals depend on each other.","I want to learn how energy can be saved and used in more effective way.","I want to learn about climate change and how to prevent it.","I want to learn about new energy sources from sun, wind, tides, waves etc.","I want to learn about technology helps us to handle waste, garbage and sewage. ",
           "I like to study biology.","It is boring to study biology.","Biology is one of my favorite topics in school.","Usually we have interesting exercises in biology.","For me biology is an easy school subject.",
           "It would bother me tremendously to touch a dead body.","It would bother me to see a rat run across my path in a park.", "It would bother me to be in a science class, and to see a human hand preserved in a jar.","I might be willing to try eating rat meat, under some circumstances.","I would go out of my way to avoid walking through cemetery.")

liklik <- likert(lik_ordr)

pal<-brewer.pal(5,"RdBu")

plot(liklik, ordered=FALSE, group.order=order, colors =pal)


# reverse questions are 1,2, 8, 9, 13, 17 and 20.

data$Item1 <-(6-data$Item1)
data$Item2 <-(6-data$Item2)
data$Item8 <-(6-data$Item8)
data$Item9 <-(6-data$Item9)
data$Item13 <-(6-data$Item13)
data$Item17 <-(6-data$Item17)
data$Item20 <-(6-data$Item20)


# characteristiscs

sum(data$n_plate) #537 respondents, 1257 plates


# accuracy for presence/absence
data$acc = with(data, ifelse(n_obs == 0 & n_conf==0, "true_neg", 
                             ifelse(n_obs > 0 & n_conf > 0, "true_pos",
                                    ifelse(n_obs > 0 & n_conf==0, "false_pos","false_neg")))) 
data$acc <- as.factor(data$acc)
table <- table(data$acc)
sum(table)
sum(table)/nrow(data)
FN <- table[1]
FP <- table[2]
TN <- table[3]
TP <- table[4]

accuracy = round((TP + TN) / sum(TP,FP,TN,FN),2) #0.76
classification_error_rate = round((FP + FN) / sum(TP,FP,TN,FN),2) #0.24
precision = round(TP / (TP + FP), 2) #0.62
sensitivity = round(TP / (TP + FN), 2) #0.82
specificity = round(TN / (TN + FP), 2) #0.73

#by age classes

data$class = as.factor(with(data, ifelse(age >= 16, "upper", "lower")))

table_class <- table (data$acc,data$class)

FN_L <- table_class[1,1]
FP_L <- table_class[2,1]
TN_L <- table_class[3,1]
TP_L <- table_class[4,1]

FN_U <- table_class[1,2]
FP_U <- table_class[2,2]
TN_U <- table_class[3,2]
TP_U <- table_class[4,2]

accuracy_U = round((TP_U + TN_U) / sum(TP_U,FP_U,TN_U,FN_U),2) #0.77
classification_error_rate_U = round((FP_U + FN_U) / sum(TP_U,FP_U,TN_U,FN_U),2) #0.23
precision_U = round(TP_U / (TP_U + FP_U), 2) #0.69
sensitivity_U = round(TP_U / (TP_U + FN_U), 2) #0.84
specificity_U = round(TN_U / (TN_U + FP_U), 2) #0.73

accuracy_L = round((TP_L + TN_L) / sum(TP_L,FP_L,TN_L,FN_L),2) #0.56
classification_error_rate_L = round((FP_L + FN_L) / sum(TP_L,FP_L,TN_L,FN_L),2) #0.44
precision_L = round(TP_L / (TP_L + FP_L), 2) #0.42
sensitivity_L = round(TP_L / (TP_L + FN_L), 2) #0.38
specificity_L = round(TN_L / (TN_L + FP_L), 2) #0.67

# Measuring the error 

data$error <- abs(data$n_obs-data$n_conf)/((data$n_conf+1)*data$n_plate)
data$errorcount <- abs(data$n_obs-data$n_conf)


data$success = as.factor(with(data, ifelse(is.na(error), "0","1"))) 
          
boxplot(data$class,data$error)
describeBy(data$error, data$class)
chisq.test(data$error, data$class)
                      

ctable(
  x = data$class,
  y = data$success
)

#more exploring 

plot(data$n_conf~ data$age)
abline(lm(data$n_conf~ data$age),col='green')

plot(data$n_obs~ data$age)
abline(lm(data$n_obs~ data$age),col='green')

#number of plates - cooperation

data$coop[data$coop == ''] <- NA
data$coop <- as.factor(data$coop)
data$ageclass = as.factor(with(data, ifelse(age <17, "low","upper"))) 


ggplot(data, aes(x=coop, y=error)) + 
  geom_boxplot()+ 
  facet_grid(.~ ageclass)+  
  theme(axis.text=element_text(colour="black"),
        legend.title=element_blank(),
        panel.background=element_rect(fill="white", colour="black"))


glmer_groupwork <- glmer.nb(error ~ ageclass*coop + (1|school), data = data)
summary(glmer_groupwork)

m_class <- lm(data$error ~ data$coop*data$ageclass)
summary(m_class)

#exploring Likert-scale answers

expl4 <- mirt(likert, 4, itemtype="gpcm", method="MHRM")
summary(expl4)


# defining the model

items <- likert[,c(1:8,10:19)] #dropped: 9, 20.

model <- mirt.model('
                    Rats = 1,7,10
                    Environment = 2,6,9,11,14
                    Biology = 3,8,12,15,18
                    Disgust = 4,5,13,16,17
                    COV = Rats*Environment*Biology*Disgust')
#counting statistics for model
#prior values to make model work better

values <- mirt(items, model, pars = 'values', itemtype="gpcm", method="MHRM")

#building the model from Likert data by using 

mod <- mirt(items, model, pars=values, itemtype="gpcm", method="MHRM")
summary(mod)

M2(mod, type="M2*", na.rm=TRUE, QMC=TRUE) #Model fit statistics

fit<-itemfit(mod, na.rm=TRUE, QMC=TRUE)
p.adjust(fit$p.S_X2, method = 'fdr') #Item fit statistics
p.adjust(fit$RMSEA.S_X2, method = 'fdr') 

resid <- residuals(mod, type = 'Q3') #Q3 statisticss
sum(resid>0.2) 

Theta <- fscores(mod, method = 'MAP', QMC=TRUE)

persfit <- personfit(mod, Theta=Theta, method ='MAP', QMC=TRUE) #Person-fit statistics
misfits <- subset(persfit, Zh< -2)

items2 <- items[-as.numeric(rownames(misfits)),] #Removal of misfit rows
mods  <- mirt(items2, model, pars=values, itemtype="gpcm", method="MHRM")

summary(mods)
M2(mods, type="M2*", na.rm=TRUE, QMC=TRUE) #Model fit statistics

Thetas <- fscores(mods, method = 'MAP', QMC=TRUE)

#Covariants for modelling

data2 <- data[-as.numeric(rownames(misfits)),] #Removal of misfit rows
cov <- cbind(data2[,c(2:8,30:33)],as.data.frame(Thetas))

#scale age for stability

cov$age <- scale(cov$age)

#Model for the errors

glmer_error <- glmer.nb(error ~ age+gender+Rats+Disgust+Biology+Environment + (1|school), data = cov)
summary(glmer_error)
coef(glmer_error)
anova(glmer_error)

cov2 <- cov[which(cov[,5]>0),]

glmer_error2 <- glmer.nb(error ~ age+gender+Rats+Disgust+Biology+Environment + (1|school), data = cov2)
summary(glmer_error2)

#Model for the success

glmer_success <- glmer(success ~ age+gender+Rats+Disgust+Biology+Environment + (1|school), 
                       family=binomial, data = cov)
summary(glmer_success)
coef(glmer_success)

# table of estimates with 95% CI

se <- sqrt(diag(vcov(glmer_error)))
(tab <- cbind(Est = fixef(glmer_error), LL = fixef(glmer_error) - 1.96 * se, UL = fixef(glmer_error) + 1.96 *
                se))

biovalues <- with(cov, seq(from = min(Biology), to = max(Biology), length.out = 100))
envvalues <- with(cov, seq(from = min(Environment), to = max(Environment), length.out = 100))
agevalues <- with(cov, seq(from = min(age), to = max(age), length.out = 100))

tmpdat <- cov[, c("error","age", "gender", "Rats", "Disgust", "Biology","Environment","school")]

#Biology

pp <- lapply(biovalues, function(j) {
  tmpdat$Biology <- j
  predict(glmer_error, newdata = tmpdat, type = "response")
})

sapply(pp[c(1, 20, 40, 60, 80, 100)], mean)

plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

plotdat_bio <- as.data.frame(cbind(plotdat, biovalues))
colnames(plotdat_bio) <- c("Error", "Lower", "Upper", "Value")
head(plotdat_bio)

ggplot(plotdat_bio, aes(x = Value, y = Error)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + geom_line(size = 2) + ylim(c(0, 10))


#Environment

pp <- lapply(envvalues, function(j) {
  tmpdat$Environment <- j
  predict(glmer_error, newdata = tmpdat, type = "response")
})

sapply(pp[c(1, 20, 40, 60, 80, 100)], mean)

plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

plotdat_env <- as.data.frame(cbind(plotdat, envvalues))
colnames(plotdat_env) <- c("Error", "Lower", "Upper", "Value")
head(plotdat_env)

ggplot(plotdat_env, aes(x = Value, y = Error)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + geom_line(size = 2) + ylim(c(0, 10))

#Age

pp <- lapply(agevalues, function(j) {
  tmpdat$age <- j
  predict(glmer_error, newdata = tmpdat, type = "response")
})

sapply(pp[c(1, 20, 40, 60, 80, 100)], mean)

plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

plotdat_age <- as.data.frame(cbind(plotdat, agevalues))
colnames(plotdat_age) <- c("Error", "Lower", "Upper", "Value")
head(plotdat_age)

plotdat_age$age.descale <- plotdat_age$age * attr(cov$age, 'scaled:scale') + attr(cov$age, 'scaled:center')


ggplot(plotdat_age, aes(x = age.descale, y = Error)) + geom_linerange(aes(ymin = Lower,ymax = Upper)) + geom_line(size = 2) + ylim(c(0, 10))

factor <- c(rep("age", 100), rep("bio", 100), rep("env", 100))

plot_all <-rbind(plotdat_age, plotdat_bio, plotdat_env)
plot_all$factor <- factor 
colnames(plot_all) <- c("Error", "Lower", "Upper", "Value","Factor")


ggplot(plot_all, aes(x = Value, y = Error,colour=Factor))+ 
  geom_linerange(aes(ymin = Lower,ymax = Upper)) + geom_line(size = 2) + ylim(c(0, 10))+
  theme(axis.text=element_text(colour="black"),
        legend.title=element_blank(),
        panel.background=element_rect(fill="white", colour="black"))

