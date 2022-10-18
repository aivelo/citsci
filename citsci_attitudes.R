#necessary packages

deps = c("car","mirt","likert","plyr","dplyr", "forcats",
         "dotwhisker","tidyr","tidyverse", "rfigshare");

for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}

#setting up data

url <- "https://ndownloader.figshare.com/files/22772264"
download.file(url, destfile="data.csv", 'libcurl')
data <- read.csv(file="data.csv", sep=";")

cov <- data[,5:7]
colnames(cov) <- c("school","age","gender")
cov <- na.omit(cov)


#clean school, recode duplicate letters for schools and remove rare schools

cov$school <- as.factor(str_extract(cov$school, pattern = "[A-X]"))


cov$school[cov$school == "I"] <- "K"
cov$school[cov$school == "X"] <- "S"
cov$school[cov$school == "U"] <- "O"
cov$school[cov$school == "?"] <- "B"

tt <- table(cov$school)
rare_levels <- names(tt)[tt<5]
cov <- subset(cov,!school %in% rare_levels)
cov$school <- droplevels(cov$school)


#clean age

cov$age <- as.numeric(str_extract(cov$age, pattern = "[1][3-9]"))

#clean gender

cov$gender = as.factor(with(cov, ifelse(gender == "Mies", "M", 
                                   ifelse(gender =="Nainen", "F",
                                          ifelse(gender=="Muu", "O","N")))))

cov <- na.omit(cov)

#likert scales

likert <- data[,8:27]
likert <- likert[as.numeric(rownames(cov)),] #Removal of NAs in cov
colnames(likert) <- c("Item1","Item2","Item3","Item4", "Item5",
                   "Item6","Item7","Item8","Item9","Item10",
                   "Item11","Item12","Item13","Item14", "Item15",
                   "Item16","Item17","Item18","Item19","Item20")

# setting up likert scale answers
lik <- likert
lik[lik==1] <- "Strongly disagree"
lik[lik==2] <- "Somewhat disagree"
lik[lik==3] <- "Not disagree or agree"
lik[lik==4] <- "Somewhat agree"
lik[lik==5] <-"Strongly agree"

lik[]<- lapply(lik, as.factor)


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

# reverse questions are 1,2, 8, 9, 17 and 20.

likert$Item1 <-(6-likert$Item1)
likert$Item2 <-(6-likert$Item2)
likert$Item8 <-(6-likert$Item8)
likert$Item9 <-(6-likert$Item9)
likert$Item17 <-(6-likert$Item17)
likert$Item20 <-(6-likert$Item20)


plot(liklik, ordered=FALSE, group.order=order)

# characteristiscs

summary(cov)

#exploring Likert-scale answers

expl4 <- mirt(likert, 4, itemtype="gpcm", method="MHRM") 

summary(expl4)

#likert <- likert[,c(1:19)] #drop Item20


# defining the model

model <- mirt.model('
                    Rats = 1,7,9,11,20
                    Environment = 2,6,10,12,15
                    Biology = 3,8,13,16,19
                    Disgust = 4,5,14,17,18
                    COV = Rats*Environment*Biology*Disgust')



#counting statistics for model
#prior values to make model work better
model_val <- mirt.model('
                    Rats = 1,7,9,11,20
                    Environment = 2,6,10,12,15
                    Biology = 3,8,13,16,19
                    Disgust = 4,5,14,17,18
                    COV = Rats*Environment*Biology*Disgust')

values <- mirt(likert, model_val, pars = 'values', method = "QMCEM", itemtype = 'gpcm')


#building the model from Likert data by using 

mod_gpcm <- mirt(likert, model, pars=values, itemtype="gpcm", method="MHRM")

M2(mod_gpcm, type="M2*", na.rm=TRUE, QMC=TRUE) #Model fit statistics

fit<-itemfit(mod_gpcm, na.rm=TRUE, QMC=TRUE)
p.adjust(fit$p.S_X2, method = 'fdr') #Item fit statistics

resid <- residuals(mod_gpcm, type = 'Q3') #Q3 statisticss
sum(resid>0.2) 

mod_gpcmt <- mirt(na.omit(likert), model, itemtype="gpcm", method="MHRM")

Theta <- fscores(mod_gpcm, method = 'MAP', QMC=TRUE)

persfit <- personfit(mod_gpcm, Theta=Theta, method ='MAP', QMC=TRUE) #Person-fit statistics
misfits <- subset(persfit, Zh< -2)

likert2 <- likert[-as.numeric(rownames(misfits)),] #Removal of misfit rows


# going to latent thingies

cov <- cov[-as.numeric(rownames(misfits)),]
cov$age <- scale(cov$age)
cov <- data.frame(cov)
cov$id<- 1:nrow(cov)

latent_rats <- mixedmirt(likert2, cov, model, itemtype="gpcm", 
                         fixed=~0, lr.fixed = ~ age*gender,
                         lr.random = ~ 1|school, 
                         technical = list(removeEmptyRows = TRUE, BURNIN=1000,
                                          SEMCYCLES=1000, MHDRAWS=2,
                                          NCYCLES=10000, MAXQUAD = 40000))


summary(latent_rats)


#Figure 2 - dot and whisker plot

coef <- coef(latent_rats)$lr.betas
coef <- t(coef)
comb <- rownames(coef)

modl <- strsplit(comb,"_")
model <- (sapply(modl, "[",1))
submodel <- sapply(modl, "[",2)
#term <- recode(submodel, teacherapp2 = "teacherapp",  teacherapp3 = "teacherapp")

coef <- cbind(coef, model, submodel)
coef <- coef[c(2:8,10:16,18:24,26:32),]

work <- as_tibble(coef)

work <- rename(work, estimate = par)

work <- work %>% 
  mutate_at(vars(estimate, CI_2.5, CI_97.5), as.numeric)


ggplot(work, aes(x = estimate, xmin = CI_2.5, xmax = CI_97.5, y = as.factor(submodel), colour = submodel))+
  geom_point()+
  geom_errorbar(width=0.1)+
  theme_bw()+
  facet_grid(cols=vars(model))

#Figure 3a - age x gender plot

age <- c(13,14,15,16,17,18,19)
scaled.age <- scale(age,attr(cov$age, "scaled:center"), attr(cov$age, "scaled:scale"))
scaled.age <- scaled.age[1:7,1]
age.class <-c("13","14","15","16","17","18","19")

#prediction for age, gender, predict, CI_2.5, CI_97.5

age <- rep(age.class,2)
gender <- rep(c("F","M"), each = 7)


pf <- do.call("rbind", replicate(7, as.numeric(coef[1,1:3]), simplify = FALSE))
predictF <- pf*scaled.age

pm <- do.call("rbind", replicate(7, as.numeric(coef[2,1:3]), simplify=FALSE))
pm[is.na(pm)] <- -1.32

pmage <- do.call("rbind", replicate(7, (as.numeric(coef[1,1:3])+as.numeric(coef[5,1:3])), simplify=FALSE))

predictM <- scaled.age*pmage+pm

predict <- rbind(predictF,predictM)

#coef[1,1:3] #age
#coef[2,1:3] #male
#coef[5,1:3] #male x age

pred <-  as.data.frame(cbind(age, gender, predict))
colnames(pred)<- c("age","gender","predict","pred2.5","pred97.5")
pred$age <- as.numeric(pred$age)
pred$predict <- as.numeric(pred$predict)
pred$pred2.5 <- as.numeric(pred$pred2.5)
pred$pred97.5 <- as.numeric(pred$pred97.5)

rats_fix <- fixef(latent_rats)[,1]
rats_stud <- randef(latent_rats)$Theta[,1]
rats_sch <- randef(latent_rats)$school
rats_school <- mapvalues(cov$school, row.names(rats_sch),as.numeric(rats_sch))
rats_school <- as.numeric(levels(rats_school))[rats_school]

m_stud <- replicate(nrow(cov),mean(rats_stud))
m_school <- replicate(nrow(cov),mean(rats_school))

rats_eff <- rats_fix+rats_stud+rats_school-m_stud-m_school
rats_age <- cov$age * attr(cov$age, 'scaled:scale') + attr(cov$age, 'scaled:center')
rats_gender <- cov$gender
raw_rats <- as.data.frame(cbind(rats_eff,rats_age))
raw_rats$gender <- rats_gender                          
colnames(raw_rats)<- c("estimate","age","gender")

raw_rats <- droplevels(raw_rats[!raw_rats$gender == 'O',])
raw_rats <- droplevels(raw_rats[!raw_rats$gender == 'N',])



p1 <- ggplot() +
  geom_jitter(data=raw_rats,aes(y=estimate,x=age,colour=gender)) +
  geom_smooth(method=lm,data=pred,aes(y=predict, x=age,colour=as.factor(gender))) +
  geom_ribbon(data=pred,aes(ymin=pred2.5, ymax=pred97.5,x=age, group=as.factor(gender), fill=as.factor(gender)), alpha = 0.3)+
  xlim(12,20)+
#  scale_colour_manual(values=c("#66c2a5","#fc8d62"))+    
  theme_bw(base_size=22) 
print(p1)

#Figure 3b - age x disgust plot

age <- c(13,14,15,16,17,18,19)
scaled.age <- scale(age,attr(cov$age, "scaled:center"), attr(cov$age, "scaled:scale"))
scaled.age <- scaled.age[1:7,1]
age.class <-c("13","14","15","16","17","18","19")

#prediction for age, gender, predict, CI_2.5, CI_97.5

age <- rep(age.class,2)
gender <- rep(c("F","M"), each = 7)


pf <- do.call("rbind", replicate(7, as.numeric(coef[22,1:3]), simplify = FALSE))
predictF <- pf*scaled.age

pm <- do.call("rbind", replicate(7, as.numeric(coef[23,1:3]), simplify=FALSE))
pm[is.na(pm)] <- -1.32

pmage <- do.call("rbind", replicate(7, (as.numeric(coef[26,1:3])+as.numeric(coef[26,1:3])), simplify=FALSE))

predictM <- scaled.age*pmage+pm

predict <- rbind(predictF,predictM)

pred <-  as.data.frame(cbind(age, gender, predict))
colnames(pred)<- c("age","gender","predict","pred2.5","pred97.5")
pred$age <- as.numeric(pred$age)
pred$predict <- as.numeric(pred$predict)
pred$pred2.5 <- as.numeric(pred$pred2.5)
pred$pred97.5 <- as.numeric(pred$pred97.5)

rats_fix <- fixef(latent_rats)[,4]
rats_stud <- randef(latent_rats)$Theta[,4]
rats_sch <- randef(latent_rats)$school
rats_school <- mapvalues(cov$school, row.names(rats_sch),as.numeric(rats_sch))
rats_school <- as.numeric(levels(rats_school))[rats_school]

m_stud <- replicate(nrow(cov),mean(rats_stud))
m_school <- replicate(nrow(cov),mean(rats_school))

rats_eff <- rats_fix+rats_stud+rats_school-m_stud-m_school
rats_age <- cov$age * attr(cov$age, 'scaled:scale') + attr(cov$age, 'scaled:center')
rats_gender <- cov$gender
raw_rats <- as.data.frame(cbind(rats_eff,rats_age))
raw_rats$gender <- rats_gender                          
colnames(raw_rats)<- c("estimate","age","gender")

raw_rats <- droplevels(raw_rats[!raw_rats$gender == 'O',])
raw_rats <- droplevels(raw_rats[!raw_rats$gender == 'N',])



p1 <- ggplot() +
  geom_jitter(data=raw_rats,aes(y=estimate,x=age,colour=gender)) +
  geom_smooth(method=lm,data=pred,aes(y=predict, x=age,colour=as.factor(gender))) +
  geom_ribbon(data=pred,aes(ymin=pred2.5, ymax=pred97.5,x=age, group=as.factor(gender), fill=as.factor(gender)), alpha = 0.3)+
  xlim(12,20)+
  #  scale_colour_manual(values=c("#66c2a5","#fc8d62"))+    
  theme_bw(base_size=22) 
print(p1)

#Figure 4

corr <- coef(latent_rats)$GroupPars

