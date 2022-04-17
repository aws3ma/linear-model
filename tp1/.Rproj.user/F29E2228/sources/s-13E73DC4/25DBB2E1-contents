Ozone = read.csv2("ozone.csv",sep = ";",header = TRUE)
dim(Ozone)
head(Ozone)
tail(Ozone,3)
summary(Ozone)
install.packages("Hmisc")
library("Hmisc")
describe(Ozone)
colnames(Ozone)
str(Ozone)
colMeans(Ozone)
#ecertype
sapply(Ozone,sd)
sapply(Ozone,var)
var(Ozone$T9)
hist(Ozone$T9,col = c("orange"),main = paste("hist pour T9"),xlab = "EFFECTIF",ylab = "T9")
cor(Ozone)
install.packages("GGally")
library("GGally")
ggpairs(Ozone)
ggcorr(Ozone)
library("corrplot")
r=round(cor(Ozone),2);corrplot(r,method="ellipse",type="upper")#9ad ma ellipse tekbar 9ad ma correlation ti7
plot(Ozone$T12,Ozone$T15)#droite => fortement correler,
plot(Ozone$Vx12,Ozone$Vx15)#droite => fortement correler,
plot(Ozone$Ne15,Ozone$T15)#faiblement
plot(Ozone$Ne15,Ozone$Ne12) #non correler correler
cor.test(Ozone$T12,Ozone$T15)#alternative hypothese 
cor.test(Ozone$Vx12,Ozone$Vx15)#alternative hypothese 
cor.test(Ozone$Ne15,Ozone$T15)#alternative hypothese 
cor.test(Ozone$Ne15,Ozone$Ne12)#alternative hypothese 
cor.test(Ozone$T9,Ozone$Ne9)
cor.test(Ozone$Ne9,Ozone$T12)
mod=lm(maxO3~.,data = Ozone)
summary(mod)
mod0=lm(maxO3~1,data = Ozone)
mod1=lm(maxO3~.,data = Ozone)
anova(mod0,mod1)
plot(mod1,1)#postula 1 satisfait : droite rouge divisé centré reduite
plot(mod1,3)#postula 2 verifier : homocisité : nuage de point centré aligner par rapport ligne rouge
plot(mod1,2)#postula 4 satisfait : gaussien : toutes aligné
plot(mod)#distance de cook : twarik les variables lem2athrin 3al model
f=step(mod1,scope=~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v,direction = c("backward"))
