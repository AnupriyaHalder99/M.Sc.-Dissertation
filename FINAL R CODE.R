##ANUPRIYA HALDER
#SEMESTER 4 DISSERTATION

data=read.csv(file.choose())
head(data)
colnames(data)=c("Gender","Age","Height","Weight","Family_History","High_caloric_food","Vegetables","Main_meals","Between_meals","Smoking","Daily_water_intake","CCM","Physical_activity","Time_using_technology","Alcohol","Transportation","Obesity_levels")

#Check how many duplicate rows there are
dup_data = duplicated(data)
#Drop duplicates
df=data[!duplicated(data),]
dim(df)
library(dplyr)
df=df %>% mutate(agegroup= case_when(Age<=20 ~ '1',Age>=21 & Age<=30 ~ '2', Age>=31  &  Age<=40  ~'3', Age>=41  &  Age<=50  ~'4',Age>=51 ~ '5'))
df$Vegetables=round(df$Vegetables)
df$Main_meals=round(df$Main_meals)
df$Daily_water_intake=round(df$Daily_water_intake)
df$Physical_activity=round(df$Physical_activity)
df$Time_using_technology=round(df$Time_using_technology)
df$Between_meals=factor(df$Between_meals,levels=c("no","Sometimes","Frequently","Always"))
df$Alcohol=factor(df$Alcohol,levels=c("no","Sometimes","Frequently","Always"))
df$Transportation=factor(df$Transportation,levels=c("Walking","Bike","Motorbike","Automobile","Public_Transportation"))
head(df)
summary(df)
attach(df)


##What is your Gender?

pie(table(Gender),labels=c("49.59%","50.47%"),main="How people are distributed according to their Gender?",col=rainbow(2))
legend("topright",c("Female","Male"),fill=rainbow(2))

#Which Age group do they belong?
Age_grp=table(agegroup)
names(Age_grp)=c("<=20","21 to 30","31 to 40","41 to 50","more than 50")
pc=(table(agegroup)/2087)*100
pie(Age_grp,labels=c("27.89%", "50.80%", "11.60%",  "1.48%",  "0.43%"),main="Which Age group do they belong?",col=rainbow(5))
legend(1.5,1,c("<=20","21 to 30","31 to 40","41 to 50","more than 50"),fill=rainbow(5))

#Consumption of vegetables
tabV=(table(round(Vegetables)))
pie(tabV,labels=c("4.89%","47.63%","47.48%"),main="Consumption of Vegetables",col=rainbow(3))
legend(1.5,1,c("Never","Sometimes","Always"),fill=rainbow(3))


#How many meals consumed in a day?
tabmeals=(table(df$Main_meals)/2087)*100
pie(tabmeals,labels=c("22.62%","77.38%"),main="How many meals consumed in a day?",col=rainbow(2))
legend(1.5,1,c("less than 3","3 or more"),fill=rainbow(2))

#How frequently consume food between meals?
df$Between_meals=factor(df$Between_meals,levels=c("no","Sometimes","Frequently","Always"))
tabbemeals=(table(df$Between_meals)/2087)*100
pie(tabbemeals,labels=c("1.77%","84.38%","11.31%","2.54%"),main="How frequently consume food between meals?",col=rainbow(4))
legend(1.5,1,c("No","Sometimes","Frequently","Always"),fill=rainbow(4))

#Daily water intake
tabwater=(100*table(Daily_water_intake))/2087
pie(tabwater,labels=c("22.90%","53.04%","24.05%"),main="Daily water intake",col=rainbow(3))
legend(1.2,1,c("Less than a litre","1 to 2 litres","More than 2 litres"),fill=rainbow(3))

par(mfrow=c(2,2))
#How frequently consume alcohol?
df$Alcohol=factor(df$Alcohol,levels=c("no","Sometimes","Frequently","Always"))
tabAlc=(table(df$Alcohol)/2087)*100
pie(tabAlc,labels=c("30.47%","66.12%","3.40%"),main="How frequently they consume alcohol?",col=rainbow(3))
legend(1.5,1,c("Never","Sometimes","Frequently"),fill=rainbow(3))

#Any physical activity?
tabphy=(table(Physical_activity)/2087)*100
pie(tabphy,labels=c("34.21%","36.37%","23.72%","5.70%"),main="Frequency of Physical Activity in a week",col=rainbow(4))
legend(1.5,1,c("None","1 to 2 days","2 to 4 days","4 to 5 days"),fill=rainbow(4))

#Time using technology in a day
tabtue=(100*table(Time_using_technology))/2087
pie(tabtue,labels=c("44.66%","43.70%","11.64%"),main="Time using technology in a day",col=rainbow(3))
legend(1.3,1,c("0 to 2 hours","3 to 5 hours","More than 5 hours"),fill=rainbow(3))


#Transportation used
df$Transportation=factor(df$Transportation,levels=c("Walking","Bike","Motorbike","Automobile","Public_Transportation"))
tabtrans=(100*table(df$Transportation))/2087
pie(tabtrans,labels=c("2.64%","0.34%","0.53%","21.85%","74.65%"),main="Transportations used",col=rainbow(5))
legend(1.4,1,c("Walking","Bike","Motorbike","Automobile","Public Transportation"),fill=rainbow(5))

par(mfrow=c(2,2))
##How are respondents responding to yes/no questions?

#Regarding family history of obesity
counts_fh=c(length(which(Family_History=="yes")),length(which(Family_History=="no")))
pc_fh=round((counts_fh/2087)*100,2)
bp1=barplot(counts_fh,xlab="Response",ylab="Number of respondants",names.arg=c("yes","no"),main="Response to family history of obesity",col=c("red","blue"))
text(bp1,0,c("82.52%","17.49%"),cex=2,pos=3)

#Regarding Frequent consumption of high caloric food
counts_favc=c(length(which(High_caloric_food=="yes")),length(which(High_caloric_food=="no")))
pc_favc=round((counts_favc/2087)*100,2)
bp2=barplot(counts_favc,xlab="Response",ylab="Number of respondants",names.arg=c("yes","no"),main="Response to frequent consumption of high caloric food",col=c("red","blue"))
text(bp2,0,c("88.36%","11.64%"),cex=2,pos=3)

#Regarding smoking status
counts_smoke=c(length(which(Smoking=="yes")),length(which(Smoking=="no")))
pc_smoke=round((100*counts_smoke)/2087,2)
bp3=barplot(counts_smoke,xlab="Response",ylab="Number of respondants",names.arg=c("yes","no"),main="Response to smoking status",col=c("red","blue"))
text(bp3,0,c("2.11%","97.89%"),cex=2,pos=3)

#Regarding Calorie consumption monitoring
counts_scc=c(length(which(CCM=="yes")),length(which(CCM=="no")))
pc_scc=round((counts_scc*100)/2087,2)
bp4=barplot(counts_scc,xlab="Response",ylab="Number of respondants",names.arg=c("yes","no"),main="Response to Calorie consumption monitoring",col=c("blue","red"))
text(bp4,0,c("4.6%","95.4%"),cex=2,pos=3)

##How are the heights and weights of the respondents distributed?
par(mfrow=c(1,2))
hist(Weight,freq=F,ylim=c(0,0.02))
lines(density(Weight),col="purple",lwd=3)
hist(Height,freq=F,ylim=c(0,5))
lines(density(Height),col="red",lwd=3)

my_df=df[,-c(2,3,4)]
head(my_df)
attach(my_df)

##To observe how the respondents are distributed over different obesity levels i.e., insufficient weight, normal weight,...etc

n1=length(which(df$Obesity_levels=="Insufficient_Weight"))
n2=length(which(df$Obesity_levels=="Normal_Weight"))
n3=length(which(df$Obesity_levels=="Overweight"))
n4=length(which(df$Obesity_levels=="Obese"))
counts=c(n1,n2,n3,n4)
pc=round((100*counts)/2087,2)
bp=barplot(counts,xlab="Obesity Levels", ylab="Number of respondents",main="Number of respondents per different Obesity Levels",names.arg=c("Insufficient_Weight","Normal_Weight","Overweight,"Obese"))
text(bp,0,c("12.79%","13.51%","27.12%","46.57%"),cex=1,pos=3)


##What is the average age by weight classification?
install.packages("ggplot2")
library(ggplot2)
df$Obesity_levels=factor(df$Obesity_levels,levels=Obesity_Levels)
plot.new()
ggplot(data=df,aes(x=Obesity_levels, y=Age, fill=Gender))+
geom_bar(stat="identity", position=position_dodge())
title("What is the average age by weight classification?")

##What is the relationship between weight and height?

plot.new()
ggplot(data=df,aes(x=Weight,y=Height,color=Gender))+
geom_point()+
geom_smooth(method="lm")
title("Relationship between Height(in Meters) and Weight(in Kg) by Gender")

##To check the association of obesity levels and other factors

#Gender vs Obesity

t=table(df$Obesity_levels,Gender)	#Contingency table
t
addmargins(t)
chisq.test(Gender,Obesity_levels)	#test for association
library(vcdExtra)
GKgamma(t)
chisq.test(Gender,Obesity_levels,simulate.p.value=T,B=10000)	#test for homogeneity
t=table(Gender,df$Obesity_levels)
barplot(prop.table(t),legend=T,legend.text=T,args.legend=list(x="topleft"),beside=T,main="Obesity levels and Gender",col=c("Blue","Red"))


#Age vs Obesity

addmargins(table(df$Obesity_levels,agegroup))
prop.table(table(Obesity_levels,df$agegroup))
barplot(prop.table(table(agegroup,df$Obesity_levels)),beside=T,col=c("red","purple","blue","yellow","orange"),main="Obesity levels and Age groups")
legend("top",c("<=20","21 to 30","31 to 40","41 to 50","more than 50"),fil=c("red","purple","blue","yellow","orange"))
chisq.test(table(Obesity_levels,df$agegroup),simulate.p.value=T,B=10000)


#Family History vs Obesity
t2=table( Family_History,df$Obesity_levels)
t2
addmargins(t2)
barplot(prop.table(t2),beside=T,col=c("red","blue"),legend=T,legend.text=T,args.legend=list(x="topleft"),main="Obesity levels and Family History")
chisq.test(t2)
GKgamma(t2)

#High caloric food vs obesity
t3=table(High_caloric_food,df$Obesity_levels)
t3
prop.table(t3)
barplot(prop.table(t3),col=c("blue","red"),legend.text=T,args.legend=list(x="topleft"),beside=T,main="Obesity levels and Consumption of high caloric food")
chisq.test(t3)
GKgamma(t3)

#Vegetables vs obesity
(df$Vegetables)
t4=table((df$Vegetables),df$Obesity_levels)
t4
prop.table(t4)
barplot(prop.table(t4),col=c("blue","red","green"),beside=T,main="Obesity levels and Consumption of vegetables")
legend("topleft",c("Never","Sometimes","Always"),fil=c("blue","red","green"))
chisq.test(t4)
GKgamma(t4)

#No. of main meals vs Obesity
t5=table((Main_meals),df$Obesity_levels)
t5
GKgamma(t5)
prop.table(t5)
barplot(prop.table(t5),col=c("yellow","blue","red","green"),legend.text=T,args.legend=list(x="topleft"),beside=T,main="Obesity levels and No. of main meals")
chisq.test(t5)


#Between meals vs Obesity
t6=table(df$Between_meals,df$Obesity_levels)
t6
GKgamma(t6)
prop.table(t6)
barplot(prop.table(t6),col=c("yellow","red","blue","green"),legend.text=T,args.legend=list(x="topleft"),beside=T,main="Obesity levels and Consumption of food between meals")
chisq.test(t6)


#Smoking vs Obesty
t7=table(df$Smoking,df$Obesity_levels)
t7
GKgamma(t7)
prop.table(t7)
barplot(prop.table(t7),col=c("blue","red"),legend.text=T,args.legend=list(x="topleft"),beside=T,main="Obesity levels and Smoking")
chisq.test(t7)

#daily water intake vs obesity
t8=table(Daily_water_intake,df$Obesity_levels)
t8
GKgamma(t8)
prop.table(t8)
barplot(prop.table(t8),col=c("red","blue","green"),beside=T,main="Obesity levels and daily water intake")
legend("topleft",cex=0.6,c("Less than a liter","1 to 2 liters","more than 2 liters"),fill=c("red","blue","green"))
chisq.test(t8)

#Calorie consumption monitoring vs obesity
t9=table(CCM,df$Obesity_levels)
t9
GKgamma(t9)
prop.table(t9)
barplot(prop.table(t9),col=c("red","blue"),legend.text=T,args.legend=list(x="topleft"),beside=T,main="Obesity levels and Calorie Consumption Monitoring")
chisq.test(t9)


#Physical activity vs obesity
t10=table((Physical_activity),df$Obesity_levels)
t10
GKgamma(t10)
prop.table(t10)
barplot(prop.table(t10),col=c("blue","red","purple","maroon"),beside=T,main="Obesity levels and Frequency of physical activity")
legend("topleft",c("None","1 to 2 days","2 to 4 days","4 to 5 days"),fil=c("blue","red","purple","maroon"))
chisq.test(t10)


#Time using technology vs obesity
t11=table((Time_using_technology),df$Obesity_levels)
t11
GKgamma(t11)
prop.table(t11)
barplot(prop.table(t11),col=c("blue","red","purple"),beside=T,main="Obesity levels and Time using technology")
legend("topleft",c("0 to 2 hours","3 to 5 hours","More than 5 hours"),fill=c("blue","red","purple"))
chisq.test(t11)


#Consumption of alcohol vs Obestiy
t12=table(df$Alcohol,df$Obesity_levels)
t12
GKgamma(t12)
prop.table(t12)
barplot(prop.table(t12),col=c("blue","red","purple"),legend.text=T,args.legend=list(x="topleft"),beside=T,main="Obesity levels and Consumption of Alcohol")
chisq.test(t12)


#Transportation used vs Obesity
t13=table(df$Transportation,df$Obesity_levels)
t13
GKgamma(t13)
prop.table(t13)
barplot(prop.table(t13),col=c("green","purple","maroon","blue","red"),beside=T,main="Obesity levels and Transportation used")
legend("topleft",c("Walking","Bike","Motorbike","Automobile","Public Transportation"),fill=c("green","purple","maroon","blue","red"))
chisq.test(t13)

#Multicollinearity
install.packages("vcdExtra")
library(vcdExtra)
tab1=table(Gender,Family_History)
tab2=table(Gender,High_caloric_food)
tab3=table(Gender,Vegetables)
tab4=table(Gender,Main_meals)
tab5=table(Gender,Between_meals)
tab6=table(Gender,Smoking)
tab7=table(Gender,Daily_water_intake)
tab8=table(Gender,CCM)
tab9=table(Gender,Physical_activity)
tab10=table(Gender,Time_using_technology)
tab11=table(Gender,Alcohol)
tab12=table(Gender,Transportation)
tab22=table(Family_History,High_caloric_food)
tab23=table(Family_History,Vegetables)
tab24=table(Family_History,Main_meals)
tab25=table(Family_History,Between_meals)
tab26=table(Family_History,Smoking)
tab27=table(Family_History,Daily_water_intake)
tab28=table(Family_History,CCM)
tab29=table(Family_History,Physical_activity)
tab210=table(Family_History,Time_using_technology)
tab211=table(Family_History,Alcohol)
tab212=table(Family_History,Transportation)
tab33=table(High_caloric_food,Vegetables)
tab34=table(High_caloric_food,Main_meals)
tab35=table(High_caloric_food,Between_meals)
tab36=table(High_caloric_food,Smoking)
tab37=table(High_caloric_food,Daily_water_intake)
tab38=table(High_caloric_food,CCM)
tab39=table(High_caloric_food,Physical_activity)
tab310=table(High_caloric_food,Time_using_technology)
tab311=table(High_caloric_food,Alcohol)
tab312=table(High_caloric_food,Transportation)
tab44=table(Vegetables,Main_meals)
tab45=table(Vegetables,Between_meals)
tab46=table(Vegetables,Smoking)
tab47=table(Vegetables,Daily_water_intake)
tab48=table(Vegetables,CCM)
tab49=table(Vegetables,Physical_activity)
tab410=table(Vegetables,Time_using_technology)
tab411=table(Vegetables,Alcohol)
tab412=table(Vegetables,Transportation)
tab55=table(Main_meals,Between_meals)
tab56=table(Main_meals,Smoking)
tab57=table(Main_meals,Daily_water_intake)
tab58=table(Main_meals,CCM)
tab59=table(Main_meals,Physical_activity)
tab510=table(Main_meals,Time_using_technology)
tab511=table(Main_meals,Alcohol)
tab512=table(Main_meals,Transportation)
tab66=table(Between_meals,Smoking)
tab67=table(Between_meals,Daily_water_intake)
tab68=table(Between_meals,CCM)
tab69=table(Between_meals,Physical_activity)
tab610=table(Between_meals,Time_using_technology)
tab611=table(Between_meals,Alcohol)
tab612=table(Between_meals,Transportation)
tab77=table(Smoking,Daily_water_intake)
tab78=table(Smoking,CCM)
tab79=table(Smoking,Physical_activity)
tab710=table(Smoking,Time_using_technology)
tab711=table(Smoking,Alcohol)
tab712=table(Smoking,Transportation)
tab88=table(Daily_water_intake,CCM)
tab89=table(Daily_water_intake,Physical_activity)
tab810=table(Daily_water_intake,Time_using_technology)
tab811=table(Daily_water_intake,Alcohol)
tab812=table(Daily_water_intake,Transportation)
tab99=table(CCM,Physical_activity)
tab910=table(CCM,Time_using_technology)
tab911=table(CCM,Alcohol)
tab912=table(CCM,Transportation)
tab1010=table(Physical_activity,Time_using_technology)
tab1011=table(Physical_activity,Alcohol)
tab1012=table(Physical_activity,Transportation)
tab1111=table(Time_using_technology,Alcohol)
tab1112=table(Time_using_technology,Transportation)
tab1212=table(Alcohol,Transportation)

GKgamma(tab1);GKgamma(tab2);GKgamma(tab3);GKgamma(tab4);GKgamma(tab5);GKgamma(tab6);GKgamma(tab7);GKgamma(tab8);GKgamma(tab9);GKgamma(tab10);GKgamma(tab11);GKgamma(tab12);GKgamma(tab13)
GKgamma(tab22);GKgamma(tab23);GKgamma(tab24);GKgamma(tab25);GKgamma(tab26);GKgamma(tab27);GKgamma(tab28);GKgamma(tab28);GKgamma(tab29);GKgamma(tab210);GKgamma(tab211);GKgamma(tab212);GKgamma(tab213)
GKgamma(tab33);GKgamma(tab34);GKgamma(tab35);GKgamma(tab36);GKgamma(tab37);GKgamma(tab38);GKgamma(tab39);GKgamma(tab310);GKgamma(tab311);GKgamma(tab312);GKgamma(tab313)
GKgamma(tab44);GKgamma(tab45);GKgamma(tab46);GKgamma(tab47);GKgamma(tab48);GKgamma(tab49);GKgamma(tab410);GKgamma(tab411);GKgamma(tab412);GKgamma(tab413)
GKgamma(tab55);GKgamma(tab56);GKgamma(tab57);GKgamma(tab58);GKgamma(tab59);GKgamma(tab510);GKgamma(tab511);GKgamma(tab512);GKgamma(tab513)
GKgamma(tab66);GKgamma(tab67);GKgamma(tab68);GKgamma(tab69);GKgamma(tab610);GKgamma(tab611);GKgamma(tab612);GKgamma(tab613)
GKgamma(tab77);GKgamma(tab78);GKgamma(tab79);GKgamma(tab710);GKgamma(tab711);GKgamma(tab712);GKgamma(tab713)
GKgamma(tab88);GKgamma(tab89);GKgamma(tab810);GKgamma(tab811);GKgamma(tab812);GKgamma(tab813)
GKgamma(tab99);GKgamma(tab910);GKgamma(tab911);GKgamma(tab912);GKgamma(tab913)
GKgamma(tab1010);GKgamma(tab1011);GKgamma(tab1012);GKgamma(tab1013)
GKgamma(tab1111);GKgamma(tab1112);GKgamma(tab1113)
GKgamma(tab1212);GKgamma(tab1213)
GKgamma(tab1313)



##
#To fit an orderd logit model
# install.packages("MASS")
library(MASS)
my_df=df[,-c(2,3,4)]
head(as.data.frame(df))
attach(my_df)
my_df$Obesity_levels <- as.factor(my_df$Obesity_levels)
m <- polr(Obesity_levels ~ Gender+Age+Family_History+High_caloric_food+Vegetables+Main_meals+Between_meals+Smoking+Daily_water_intake+CCM+Physical_activity+Time_using_technology+Alcohol+Transportation ,method = "logistic",Hess=T,data=df)
summary(m)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(m)
## OR and CI
exp(cbind(OR = coef(m), ci))
##No need as their are no interaction effects
library(car)
Anova(m)
qchisq(0.95,1)
qchisq(0.95,3)
qchisq(0.95,4)

##Lets now try to enhance this model to obtain better prediction estimates.
summary(update(m, method = "probit", Hess = TRUE), digits = 3)
summary(update(m, method = "logistic", Hess = TRUE), digits = 3)
summary(update(m, method = "cloglog", Hess = TRUE), digits = 3)
##Ordered logistic has least AIC so best.

m0 <- update(m, Hess=TRUE)
#to plot the effects
#install.packages("effects")
library(effects)
Gender.eff <- Effect(focal.predictors = "Gender", mod = m0)
plot(Gender.eff,rug=F)
Age.eff <- Effect(focal.predictors = "agegroup", mod = m0)
plot(Age.eff,rug=F)
FH.eff <- Effect(focal.predictors = "Family_History", mod = m0)
plot(FH.eff,rug=F)
HC.eff <- Effect(focal.predictors = "High_caloric_food", mod = m0)
plot(HC.eff,rug=F)
Veg.eff <- Effect(focal.predictors = "Vegetables", mod = m0)
plot(Veg.eff,rug=F)
MM.eff <- Effect(focal.predictors = "Main_meals", mod = m0)
plot(MM.eff,rug=F)
BM.eff <- Effect(focal.predictors = "Between_meals", mod = m0)
plot(BM.eff,rug=F)
Smoking.eff <- Effect(focal.predictors = "Smoking", mod = m0)
plot(Smoking.eff,rug=F)
Water.eff <- Effect(focal.predictors = "Daily_water_intake", mod = m0)
plot(Water.eff,rug=F)
CCM.eff <- Effect(focal.predictors = "CCM", mod = m0)
plot(CCM.eff,rug=F)
PA.eff <- Effect(focal.predictors = "Physical_activity", mod = m0)
plot(PA.eff,rug=F)
TUT.eff <- Effect(focal.predictors = "Time_using_technology", mod = m0)
plot(TUT.eff,rug=F)
Alcohol.eff <- Effect(focal.predictors = "Alcohol", mod = m0)
plot(Alcohol.eff,rug=F)
Trans.eff <- Effect(focal.predictors = "Transportation", mod = m0)
plot(Trans.eff,rug=F)

