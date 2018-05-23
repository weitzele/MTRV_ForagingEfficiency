#Declining Foraging Efficiency in the Middle Tennessee River Valley Prior to Initial Domestication
#Elic M. Weitzel

###########
#Load data#
###########

data <- read.csv("RData_MTRVFE.csv")
names(data)

#set working directory to source file location
setwd()


#############################
#Overall foraging efficiency#
#############################

#Deer-Shellfish Index (Deer/Deer+Shellfish)

dshglm <- glm((DeerNISP/(DeerNISP+ShellNISP))~Date, family=binomial(link="logit"), data=data, weights=(DeerNISP+ShellNISP))
summary(dshglm)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval1 <- predict(dshglm, newdata=newdata, type="response", se=T)

jpeg("Fig.3_Deer-Shellfish.jpg", height=3.5, width=6, units="in", res=300)

par(mar=c(5,5,2,2))

plot(1, type="n", xlab="", ylab="", xlim=c(0,13000), ylim=c(0.0,1.1)); rect(4200,-1,8200,1.2,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.2, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 1.1, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 1.1, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 1.1, "YD", cex=0.5); text(((4200/2)+0), 1.1, "Late Holocene", cex=0.5); points((DeerNISP/(DeerNISP+ShellNISP))~Date, data=data, pch=16); abline(v=5000, lty=4, col="darkgrey"); text(4600, .6, "Initial Domestication",  srt=90, cex=.75); mtext("cal BP", 1, 3); mtext("Deer-Shellfish Index", 2, 3)

lines(predval1$fit~newdata$Date, col="black"); lines(predval1$fit+(predval1$se*1.96)~newdata$Date, col="black", lty=2); lines(predval1$fit-(predval1$se*1.96)~newdata$Date, col="black", lty=2); box()

dev.off()

#Pseudo R2 = 0.708 (McFadden's pseudo-R2)
with(dshglm, (null.deviance-deviance)/null.deviance)
# p = 0
with(dshglm, 1-pchisq(null.deviance-deviance , df.null-df.residual))


###################################
#Wetland Patch Foraging Efficiency#
###################################

#Waterfowl Index (Anatidae/Anatidae+Shellfish)

wshglm <- glm((Waterfowl/(Waterfowl+ShellNISP))~Date, family=binomial(link="logit"), data=data, weights=(Waterfowl+ShellNISP))
summary(wshglm)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval2 <- predict(wshglm, newdata=newdata, type="response", se=T)

jpeg("Fig.4_Waterfowl.jpg", height=3.5, width=6, units="in", res=300)

par(mar=c(5,5,2,2))

plot(1, type="n", xlab="cal BP", ylab="Waterfowl Index", xlim=c(0,13000), ylim=c(0.0,1.1)); rect(4200,-1,8200,1.3,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.3, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 1.1, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 1.1, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 1.1, "YD", cex=0.5); text(((4200-0)/2), 1.1, "Late Holocene", cex=0.5); points((Waterfowl/(Waterfowl+ShellNISP))~Date, data=data, pch=16); abline(v=5000, lty=4, col="darkgrey"); text(4600, .6, "Initial Domestication",  srt=90, cex=.75)

lines(predval2$fit~newdata$Date, col="black"); lines(predval2$fit+(predval2$se*1.96)~newdata$Date, lty=2); lines(predval2$fit-(predval2$se*1.96)~newdata$Date, lty=2); box()

dev.off()

#Pseudo R2 = 0.9945
with(wshglm, (null.deviance-deviance)/null.deviance)
# p = 0
with(wshglm, 1-pchisq(null.deviance-deviance , df.null-df.residual))


#Fish Index (Fish/Fish+Shellfish)

fshglm <- glm((Fish/(Fish+ShellNISP))~Date, family=binomial(link="logit"), data=data, weights=(Fish+ShellNISP))
summary(fshglm)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval3 <- predict(fshglm, newdata=newdata, type="response", se=T)

jpeg("Fig.5_Fish.jpg", height=3.5, width=6, units="in", res=300)

par(mar=c(5,5,2,2))

plot(1, type="n", xlab="cal BP", ylab="Fish Index", xlim=c(0,13000), ylim=c(0.0,1.1)); rect(4200,-1,8200,1.3,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.3, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 1.1, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 1.1, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 1.1, "YD", cex=0.5); text(((4200-0)/2), 1.1, "Late Holocene", cex=0.5); points((Fish/(Fish+ShellNISP))~Date, data=data, pch=16); abline(v=5000, lty=4, col="darkgrey"); text(4600, .6, "Initial Domestication",  srt=90, cex=.75)

lines(predval3$fit~newdata$Date); lines(predval3$fit+(predval3$se*1.96)~newdata$Date, lty=2); lines(predval3$fit-(predval3$se*1.96)~newdata$Date, lty=2); box()

dev.off()

#Pseudo R2 = 0.9456
with(fshglm, (null.deviance-deviance)/null.deviance)
# p = 0
with(fshglm, 1-pchisq(null.deviance-deviance , df.null-df.residual))


#################################
#Terrestrial Foraging Efficiency#
#################################

#Deer-Squirrel Index (Deer/Deer+Squirrel)

dsqglm <- glm((DeerNISP/(DeerNISP+Squirrel))~Date, family=binomial(link="logit"), data=data, weights=(DeerNISP+Squirrel))
summary(dsqglm)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval4 <- predict(dsqglm, newdata=newdata, type="response", se=T)

jpeg("Fig.6_Deer-Squirrel.jpg", height=3.5, width=6, units="in", res=300)

par(mar=c(5,5,2,2))

plot(1, type="n", xlab="cal BP", ylab="Deer-Squirrel Index", xlim=c(0,13000), ylim=c(0.0,1.1)); rect(4200,-1,8200,1.3,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.3, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 1.1, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 1.1, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 1.1, "YD", cex=0.5); text(((4200-0)/2), 1.1, "Late Holocene", cex=0.5); points((DeerNISP/(DeerNISP+Squirrel))~Date, data=data, pch=16); abline(v=5000, lty=4, col="darkgrey"); text(4600, .28, "Initial Domestication",  srt=90, cex=.7)

lines(predval4$fit~newdata$Date); lines(predval4$fit+(predval4$se*1.96)~newdata$Date, lty=2); lines(predval4$fit-(predval4$se*1.96)~newdata$Date, lty=2); box()

dev.off()

#Pseudo R2 = 0.477
with(dsqglm, (null.deviance-deviance)/null.deviance)
# p = 0
with(dsqglm, 1-pchisq(null.deviance-deviance , df.null-df.residual))



#####################
#Return Rates Figure#
#####################

rate <- read.csv("ReturnRates_MTRVFE.csv")

jpeg("Fig.2_ReturnRates.jpg", height=5, width=6, units="in", res=300)

par(mar=c(6, 5.1, 4.1, 2.1))

boxplot(rate, ylab="kcal/hr", xaxt="n")
axis(1, at=seq(1,13), labels=FALSE)

labels <- paste(c("Deer     ","Raccoon     ","Goose     ","Turkey     ","Opossum     ","Fish     ","Turtle     ","Rabbit     ","Hickory     ","Duck     ","Squirrel     ","Mussels     ", "Chenopodium     "))
text(x=seq_along(labels)+.5, y=par("usr")[3] - 100, srt=45, adj=1, labels=labels, xpd=TRUE)

dev.off()


####################
#Specific Resources#
####################

#Proportion Waterfowl ----

glmanatidae <- glm((Waterfowl/SumSubClass)~poly(Date,2), family=binomial(link="logit"), data=data, weights=(SumSubClass))
summary(glmanatidae)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval5 <- predict(glmanatidae, newdata=newdata, type="response", se=T)

plot((Waterfowl/SumSubClass)~Date, data=data, pch=19, xlab="Date", ylab="Proportion Anatidae", xlim=c(0,14000), ylim=c(0.0,0.1)); rect(4200,-1,8200,1.3,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.3, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 0.1, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 0.1, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 0.1, "YD", cex=0.5); text(((4200-0)/2), 0.1, "Late Holocene", cex=0.5)

lines(predval5$fit~newdata$Date)
lines(predval5$fit+(predval5$se*1.96)~newdata$Date, lty=2)
lines(predval5$fit-(predval5$se*1.96)~newdata$Date, lty=2)

#Pseudo R2 = 0.726
with(glmanatidae, (null.deviance-deviance)/null.deviance)
# p = 0
with(glmanatidae, 1-pchisq(null.deviance-deviance , df.null-df.residual))


#Proportion Fish ----

glmfish <- glm((Fish/SumSubClass)~poly(Date,2), family=binomial(link="logit"), data=data, weights=(SumSubClass))
summary(glmfish)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval6 <- predict(glmfish, newdata=newdata, type="response", se=T)

plot((Fish/SumSubClass)~Date, data=data, pch=16, xlab="Date", ylab="Proportion Fish", xlim=c(0,14000), ylim=c(0.0,0.65)); rect(5800,-1,8200,1.3,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.3, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 0.65, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 0.65, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 0.65, "YD", cex=0.5); text(((4200-0)/2), 0.65, "Late Holocene", cex=0.5)

lines(predval6$fit~newdata$Date)
lines(predval6$fit+(predval6$se*1.96)~newdata$Date, lty=2)
lines(predval6$fit-(predval6$se*1.96)~newdata$Date, lty=2)


#Pseudo R2 = 0.422
with(glmfish, (null.deviance-deviance)/null.deviance)
# p = 0
with(glmfish, 1-pchisq(null.deviance-deviance , df.null-df.residual))


#Proportion Shellfish ----

glmshell <- glm((ShellNISP/SumClass)~poly(Date,3), family=binomial(link="logit"), data=data, weights=(SumClass))
summary(glmshell)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval7 <- predict(glmshell, newdata=newdata, type="response", se=T)

plot((ShellNISP/SumClass)~Date, data=data, pch=16, xlab="Date", ylab="Proportion Shellfish", xlim=c(0,14000), ylim=c(0.0,1.1)); rect(4200,-1,8200,1.3,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.3, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 1.1, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 1.1, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 1.1, "YD", cex=0.5); text(((4200-0)/2), 1.1, "Late Holocene", cex=0.5)

lines(predval7$fit~newdata$Date, col="black")
lines(predval7$fit+(predval7$se*1.96)~newdata$Date, col="black", lty=2)
lines(predval7$fit-(predval7$se*1.96)~newdata$Date, col="black", lty=2)


#Pseudo R2 = 0.986
with(glmshell, (null.deviance-deviance)/null.deviance)
# p = 0
with(glmshell, 1-pchisq(null.deviance-deviance , df.null-df.residual))


#Proportion Deer ----

glmdeer <- glm((DeerNISP/SumSubClass)~poly(Date,2), family=binomial(link="logit"), data=data, weights=(SumSubClass))
summary(glmdeer)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval8 <- predict(glmdeer, newdata=newdata, type="response", se=T)

plot((DeerNISP/(SumSubClass))~Date, data=data, pch=16, xlab="Cal BP", ylab="Proportion Deer", xlim=c(0,13500), ylim=c(0.0,0.6)); rect(4200,-1,8200,1.2,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.2, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 0.59, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 0.59, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 0.59, "YD", cex=0.5); text(((4200/2)+0), 0.59, "Late Holocene", cex=0.5)

lines(predval8$fit~newdata$Date, col="black")
lines(predval8$fit+(predval8$se*1.96)~newdata$Date, col="black", lty=2)
lines(predval8$fit-(predval8$se*1.96)~newdata$Date, col="black", lty=2)

#Pseudo R2 = 0.314
with(glmdeer, (null.deviance-deviance)/null.deviance)
# p = 0
with(glmdeer, 1-pchisq(null.deviance-deviance , df.null-df.residual))


#Proportion Squirrel ----

glmsq <- glm((Squirrel/SumSubClass)~poly(Date,3), family=binomial(link="logit"), data=data, weights=(SumSubClass))
summary(glmsq)

newdata <- data.frame("Date"=seq(0, 12900, length.out=100))
predval9 <- predict(glmsq, newdata=newdata, type="response", se=T)

plot((Squirrel/(SumSubClass))~Date, data=data, pch=16, xlab="Cal BP", ylab="Proportion Squirrel", xlim=c(0,13500), ylim=c(0.0,0.6)); rect(4200,-1,8200,1.2,col='#d6d6c260', lty=0); rect(11650,-1,12900,1.2, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), 0.59, "Middle Holocene", cex=0.5); text(((11650-8200)/2+8200), 0.59, "Early Holocene", cex=0.5); text(((12900-11650)/2+11650), 0.59, "YD", cex=0.5); text(((4200/2)+0), 0.59, "Late Holocene", cex=0.5)

lines(predval9$fit~newdata$Date, col="black")
lines(predval9$fit+(predval9$se*1.96)~newdata$Date, col="black", lty=2)
lines(predval9$fit-(predval9$se*1.96)~newdata$Date, col="black", lty=2)

#Pseudo R2 = 0.721
with(glmsq, (null.deviance-deviance)/null.deviance)
# p = 0
with(glmsq, 1-pchisq(null.deviance-deviance , df.null-df.residual))


###########################
#Plot Proportions Together#
###########################

jpeg("Fig.7-Proportions.jpeg", width = 7, height = 8, units = 'in', res = 300)

par(oma=c(6,4,0,0), mar=c(0, 5, 2.5, 5) + 0.1, xpd=NA, mfrow=c(5,1))

plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,13000), ylim=c(0.0,0.1)); rect(4200,-.58,8200,.11,col='#d6d6c260', lty=0); rect(11650,-.58,12900,.11, col='#d6d6c260', lty=0); text(((8200-4200)/2+4200), .1, "Middle Holocene", cex=0.85); text(((11650-8200)/2+8200), .1, "Early Holocene", cex=0.85); text(((12900-11650)/2+11650), .1, "YD", cex=0.85); text(((4200/2)+0), .1, "Late Holocene", cex=0.85); axis(2, line=0, at=seq(0,.1,.025), labels=T); mtext(2, text="Proportion Waterfowl", line=3.5, cex=.85); text(1, .1, "a."); points((Waterfowl/SumSubClass)~Date, data=data, pch=19); segments(5000, .11, 5000, -.58, lty=4, col="darkgrey"); text(4700, -.48, "Initial Domestication",  srt=90, cex=.75); text(4700, -.065, "Initial Domestication",  srt=90, cex=.75); par(xpd=F) 

lines(predval5$fit~newdata$Date, col="black"); lines(predval5$fit+(predval5$se*1.96)~newdata$Date, col="black", lty=2); lines(predval5$fit-(predval5$se*1.96)~newdata$Date, col="black", lty=2)

plot(1, type="n", xlab="", axes=F, ylab="", xlim=c(0,13000), ylim=c(0.0,.6)); points((Fish/SumSubClass)~Date, data=data, pch=16); axis(2, line=0, at=seq(0,.6,.1), labels=T); mtext(2, text="Proportion Fish", line=3.5, cex=.85); text(1, .6, "b.")

lines(predval6$fit~newdata$Date, col="black"); lines(predval6$fit+(predval6$se*1.96)~newdata$Date, lty=2); lines(predval6$fit-(predval6$se*1.96)~newdata$Date, lty=2)

plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,13000), ylim=c(0.0,1.1)); points(ShellNISP/SumClass~Date, data=data, pch=16); axis(2, line=0, at=seq(0,1,.25), labels=T); mtext(2, text="Proportion Shellfish", line=3.5, cex=.85); text(1, 1.1, "c.")

lines(predval7$fit~newdata$Date); lines(predval7$fit+(predval7$se*1.96)~newdata$Date, lty=2); lines(predval7$fit-(predval7$se*1.96)~newdata$Date, lty=2)

plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,13000), ylim=c(0,.7)); points(DeerNISP/SumSubClass~Date, data=data, pch=16); axis(2, line=0, at=seq(0,.7,.2), labels=T); mtext(2, text="Proportion Deer", line=3.5, cex=.85); text(1, .6, "d.")

lines(predval8$fit~newdata$Date); lines(predval8$fit+(predval8$se*1.96)~newdata$Date, lty=2); lines(predval8$fit-(predval8$se*1.96)~newdata$Date, lty=2)

plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,13000), ylim=c(0,.7)); points(Squirrel/SumSubClass~Date, data=data, pch=16); axis(2, line=0, at=seq(0,.7,.2), labels=T); mtext(2, text="Proportion Squirrel", line=3.5, cex=.85); text(1, .6, "e.")

lines(predval9$fit~newdata$Date); lines(predval9$fit+(predval9$se*1.96)~newdata$Date, lty=2); lines(predval9$fit-(predval9$se*1.96)~newdata$Date, lty=2)

axis(1, line=0.5, at=seq(0,13000, by=2000), labels=c(seq(0,13000,2000))); axis(1, line=0.5, at=seq(0,13000, by=1000), labels=F); mtext(1, text="cal BP", line=3.75, cex=.85)

dev.off()
