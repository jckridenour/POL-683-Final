setwd("C:/Users/Joshua/Dropbox/University of Arizona/2016-2017/Fall 2016/POL 683 - Methods/Final Paper/R")
library(lme4)
library(MASS)
library(pscl)
library(ggplot2)
library(reshape2)
library(car)
library(aod)
library(lmtest)
library(stargazer)
library(psych)

load("DATA methods final 1972.RData")

# Note: variable 'candaff' refers to evaluation of the candidate in the following way: (positive R + negative D) - (positive D + negative R).
	# I refer to this variable as "candidate evaluation" rather than "candidate affect."

# Note: variable 'candft' refers to a unidimensional representation (using feeling thermometers) of affect towards the candidates.
	# High scores indicate a Republican preference, while low scores represent a Democratic preference.
	# I refer to this variable as "candidate affect" or "feelings towards the candidates."



# First, let's compare some model specifications. #




#########################################
########## Model Specification ##########
#########################################


### AIC ###


# We begin with a naive model. #
m1 <- glm(factor(presvt) ~ factor(year) - 1, data=dat2, family=binomial(link="logit"))
# AIC: 14565

# Then, we include demographic variables. #
m2 <- glm(factor(presvt) ~ gender + race + education + income + factor(year) - 1, data=dat2, family=binomial(link="logit"))
# AIC: 12639

# Political variables come next. #
m3 <- glm(factor(presvt) ~ pid + ideo + denom + gender + race + education + income + factor(year) - 1, data=dat2, family=binomial(link="logit"))
# AIC: 5216.8

# Finally, evaluative variables are included. #
m4 <- glm(factor(presvt) ~ pid + ideo + denom + candaff + candft + gender + race + education + income + factor(year) -1, data=dat2, family=binomial(link='logit'))
# AIC: 2486

# Now I add some interactions. #
	# Party ID #
	m5 <- glm(factor(presvt) ~ pid + ideo + denom + candaff + candft + gender + race + education + income + pid*denom + pid*candaff + pid*candft + gender + race + education + income + factor(year) -1, data=dat2, family=binomial(link="logit"))
	# AIC: 2484

	# Ideology #
	m6 <- glm(factor(presvt) ~ pid + ideo + denom + candaff + candft + gender + race + education + income + ideo*denom + ideo*candaff + ideo*candft + gender + race + education + income + factor(year) -1, data=dat2, family=binomial(link="logit"))
	# AIC: 2490.1

	# Candidate Evaluation #
	m7 <- glm(factor(presvt) ~ pid + ideo + denom + candaff + candft + gender + race + education + income + pid*candaff + ideo*candaff + candaff*candft + denom*candaff + gender + race + education + income + factor(year) -1, data=dat2, family=binomial(link="logit"))
	# AIC: 2485.3

	# Just Party ID and Candidate Evaluation #
	m8 <- glm(factor(presvt) ~ pid + ideo + denom + candaff + candft + gender + race + education + income + pid*candaff + gender + race + education + income + factor(year) -1, data=dat2, family=binomial(link="logit"))
	# AIC 2482.2

# I alter the model to allow for some variation in the slopes over time. #
m9 <- glm(factor(presvt) ~ pid + ideo + denom + candaff + candft + gender + race + education + income + year + pid*year + year*ideo + year*denom + year*candaff + year*candft, data=dat2, family=binomial(link="logit"))
# But no year interactions significant

# Changing year to a factor changes very little. #
m10 <- glm(factor(presvt) ~ pid + ideo + denom + candaff + candft + gender + race + education + income + factor(year) - 1 + pid*factor(year) + ideo*factor(year) + denom*factor(year) + candaff*factor(year) + candft*factor(year), data=dat2, family=binomial(link="logit"))
# Year interactions only significant for 2 years on separate variables. 


# In terms of the information criterion, model 'm4' appears to the simplest model that best reduces the AIC. #


### Counts Correctly Predicted ###

# I evaluate here only model 4 and the two specifications that better reduce the AIC. #

# The Null correctly predicts 53.42% of the time. #

hitmiss(m4)
# PCP: 92.73%

hitmiss(m5)
# PCP: 92.81%

hitmiss(m8)
# PCP: 92.79%

# Model 4 incorrectly predicts only 5 more hits than model 5, and only 4 more than model 8.


### Psuedo-R^2 ###

efron.r2<-function(x){
	fit.x<-x$fitted.values
	obs.x<-x$y
	return((sum((obs.x-fit.x)^2))/(sum((obs.x-mean(obs.x))^2)))
}
efron.r2(m4)
# 0.218


mcfadden.r2<-function(a, b){
	return(1-(logLik(a)/logLik(b)))
}
mcfadden.r2(m4, m1)
# 0.832


# I choose model 4 to conduct my analyses. #

### Final Specification ###

m <- glm(factor(presvt) ~ pid + ideo + denom + candaff + candft + gender + race + education + income + factor(year) -1, data=dat2, family=binomial(link='logit'))
summary(m)


########## The Table ##########

stargazer(m, dep.var.labels=c("Presidential Vote Choice"), align=T, no.space=T, title="Full Regression Model",
	covariate.labels=c("Party ID", "Ideology", "Religion", "Candidate Evaluation", "Candidate Affect", "Male", "Race", "College Degree", "Income",
	"1972", "1976", "1980", "1984", "1988", "1992", "1996", "2000", "2004"))






#############################################
########## Predicted Probabilities ##########
#############################################


### Party ID and Ideology Over Time ###

temp <- data.frame(pid=rep(c(1, 2, 3), 27), ideo=rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 9), denom=rep(4, 81),
		candaff=rep(median(dat2$candaff, na.rm=T), 81), candft=rep(median(dat2$candft, na.rm=T), 81), gender=rep(1, 81), 
		race=rep(2, 81), education=rep(1, 81), income=rep(median(dat2$income, na.rm=T), 81), 
		year=factor(c(rep(1, 9), rep(2, 9), rep(3, 9), rep(4, 9), rep(5, 9), rep(6, 9), rep(7, 9), rep(8, 9), rep(9, 9))))

preds <- cbind(temp, predict(m, temp, se=T, type="link"))
preds <- within(preds, {
	PredictedProb <- plogis(fit)
	LL <- plogis(fit - (1.96 * se.fit))
	UL <- plogis(fit + (1.96 * se.fit))
})

lab_year <- c('1'='1972', '2'='1976', '3'='1980', '4'='1984', '5'='1988', '6'='1992', '7'='1996', '8'='2000', '9'='2004')
pidxideo <- ggplot(preds, aes(x = ideo, y = PredictedProb)) +
	geom_line(aes(colour=factor(pid))) +
	scale_colour_manual(name="Party ID",
		values=c("blue", "green", "red"),
		labels=c("Democrats", "Independents", "Republicans")) +
	geom_ribbon(data=preds[preds$pid==1,], aes(ymin=LL, ymax=UL), fill="blue", alpha=0.2) +
	geom_ribbon(data=preds[preds$pid==2,], aes(ymin=LL, ymax=UL), fill="green", alpha=0.2) +
	geom_ribbon(data=preds[preds$pid==3,], aes(ymin=LL, ymax=UL), fill="red", alpha=0.2) +
	facet_wrap(~year, labeller=labeller(year=lab_year)) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3),
		labels=c("Liberal", "Moderate", "Conservative")) +
	ggtitle("Figure 3. Effects of Party ID and Ideology on Voting Republican, 1972-2004") +
	ylab("Predicted Probabilities") +
	theme_bw() +
	theme(plot.title=element_text(size=26, face="bold")) +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="pidxideo.png", plot=pidxideo)


### Candidate Affect and Evaluation Over Time ###

temp <- data.frame(pid=rep(median(dat2$pid, na.rm=T), 378), ideo=rep(median(dat2$ideo, na.rm=T), 378), denom=rep(4, 378),
		candaff=rep(seq(from=-10, to=10, length.out=21), 18), candft=rep(c(rep(36, 21), rep(68, 21)), 9), gender=rep(1, 378),
		race=rep(2, 378), education=rep(1, 378), income=rep(median(dat2$income, na.rm=T), 378),
		year=factor(c(rep(1, 42), rep(2, 42), rep(3, 42), rep(4, 42), rep(5, 42), rep(6, 42), rep(7, 42), rep(8, 42), rep(9, 42))))

preds <- cbind(temp, predict(m, temp, se=T, type="link"))
preds <- within(preds, {
	PredictedProb <- plogis(fit)
	LL <- plogis(fit - (1.96 * se.fit))
	UL <- plogis(fit + (1.96 * se.fit))
})

evalxaff <- ggplot(preds, aes(x = candaff, y = PredictedProb)) +
	geom_line(aes(colour=factor(candft)), lwd=1) +
	scale_colour_manual(name="Candidate Feeling Thermometer",
		values=c("mediumpurple4", "seagreen4"),
		labels=c("25th - High Democrat Affect", "75th - High Republican Affect")) +
	geom_ribbon(data=preds[c(1:21, 43:63, 85:105, 127:147, 169:189, 211:231, 253:273, 295:315, 337:357),], aes(ymin=LL, ymax=UL), fill="mediumpurple", alpha=0.2) +
	geom_ribbon(data=preds[c(22:42, 64:84, 106:126, 148:168, 190:210, 232:252, 274:294, 316:336, 358:378),], aes(ymin=LL, ymax=UL), fill="seagreen4", alpha=0.2) +
	facet_wrap(~year, labeller=labeller(year=lab_year)) +
	scale_x_continuous(name="",
		breaks=c(-8, 0, 8),
		labels=c("Most Democratic", "Neutral", "Most Republican")) +
	ggtitle("Figure 4. Effects of Evaluation and Affect on Voting Republican, 1972-2004") +
	ylab("Predicted Probabilities") +
	theme_bw() +
	theme(plot.title=element_text(face="bold", size=26)) +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="evalxaff.png", plot=evalxaff)


### Party ID and Candidate Affect Over Time ###

temp <- data.frame(pid=rep(c(rep(1, 3), rep(2, 3), rep(3, 3)), 63), ideo=rep(median(dat2$ideo, na.rm=T), 567), denom=rep(4, 567),
		candaff=rep(seq(from=-10, to=10, length.out=21), 27), candft=rep(median(dat2$candft, na.rm=T), 567), gender=rep(1, 567),
		race=rep(2, 567), education=rep(1, 567), income=rep(median(dat2$income, na.rm=T), 567),
		year=factor(c(rep(1, 63), rep(2, 63), rep(3, 63), rep(4, 63), rep(5, 63), rep(6, 63), rep(7, 63), rep(8, 63), rep(9, 63))))

preds <- cbind(temp, predict(m, temp, se=T, type="link"))
preds <- within(preds, {
	PredictedProb <- plogis(fit)
	LL <- plogis(fit - (1.96 * se.fit))
	UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(preds, aes(x = candaff, y = PredictedProb)) +
	geom_line(aes(colour=factor(pid)), lwd=1) +
	scale_colour_manual(name="Party ID",
		values=c("blue", "green", "red"),
		labels=c("Democrat", "Independent", "Republican")) +
	scale_x_continuous("Candidate Evaluation",
		breaks=c(-10, 0, 10),
		labels=c("Most Pro-Democrat", "Neutral", "Most Pro-Republican")) +
	geom_ribbon(data=preds[preds$pid==1,], aes(ymin=LL, ymax=UL), fill="blue", alpha=0.2) +
	geom_ribbon(data=preds[preds$pid==2,], aes(ymin=LL, ymax=UL), fill="green", alpha=0.2) +
	geom_ribbon(data=preds[preds$pid==3,], aes(ymin=LL, ymax=UL), fill="red", alpha=0.2) +
	ggtitle("Figure 2
	facet_wrap(~year, labeller=labeller(year=lab_year)) +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10, angle=90))




##################################################################
########## Assessing Evaluation Independent of Party ID ##########
##################################################################

crossvt <- subset(dat2, cross==1)

cvt <- glm(factor(presvt) ~ ideo + denom + candaff + candft + gender + race + education + income, data=crossvt, family=binomial(link='logit'))
summary(cvt)

mean(crossvt$ideo[crossvt$pid==1], na.rm=T)
mean(dat2$ideo[dat2$pid==1], na.rm=T)
# 2.23, 1.83

mean(crossvt$ideo[crossvt$pid==3], na.rm=T)
mean(dat2$ideo[dat2$pid==3], na.rm=T)
# 2.23, 2.55

t.test(crossvt$ideo[crossvt$pid==1], dat2$ideo[dat2$pid==1])
t.test(crossvt$ideo[crossvt$pid==3], dat2$ideo[dat2$pid==3])
#both groups are significantly different from their party-loyal counterparts.

ggplot(crossvt[crossvt$pid==1 | crossvt$pid==3,], aes(x=ideo)) +
	geom_bar(position="dodge", (aes(fill=factor(pid)))

t.test(dat2$ideo[dat2$pid==3 & dat2$cross==1], dat2$ideo[dat2$pid==1 & dat2$cross==1])



########## The Table ##########

stargazer(cvt, title="Predictors of Vote Choice Among Disloyal Partisans", dep.var.labels=c("Presidential Vote Choice"), align=T, no.space=T,
	covariate.labels=c("Ideology", "Religion", "Candidate Evaluation", "Candidate Affect", "Male", "Race", "College Degree", "Income", "Constant"))


### Candidate Affect and Candidate Evaluation ###

temp <- data.frame(ideo=rep(median(crossvt$ideo, na.rm=T), 42), denom=rep(median(crossvt$denom, na.rm=T), 42), candaff=rep(seq(from=-10, to = 10, length.out=21), 2),
		candft=rep(c(46, 64), 21), gender=rep(1, 42), race=rep(2, 42), education=rep(median(crossvt$education, na.rm=T), 42), income=rep(median(crossvt$income, na.rm=T), 42))

preds <- cbind(temp, predict(cvt, temp, se=T, type="link"))
preds <- within(preds, {
	PredictedProb <- plogis(fit)
	LL <- plogis(fit - (1.96 * se.fit))
	UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(preds, aes(x = candaff, y = PredictedProb)) +
	geom_line(aes(colour=factor(candft)), lwd=1) +
	scale_colour_manual(name="Candidate Feeling Thermometer",
		values=c("mediumpurple4", "seagreen4"),
		labels=c("25th - High Democrat Affect", "75th - High Republican Affect")) +
	geom_ribbon(data=preds[preds$candft==46,], aes(ymin=LL, ymax=UL), fill="mediumpurple4", alpha=0.2) +
	geom_ribbon(data=preds[preds$candft==64,], aes(ymin=LL, ymax=UL), fill="seagreen4", alpha=0.2) +
	ggtitle("Probability of Voting Republican") +
	ylab("Predicted Probability") +
	scale_x_continuous(name="Candidate Evaluation",
		breaks=c(-10, 0, 10),
		labels=c("Most Pro-Democratic", "Neutral", "Most Pro-Republican")) +
	theme_bw()


########## Replicating Without Independents ##########

cpart <- crossvt[crossvt$pid==1 | crossvt$pid==3,]
cpvt <- glm(factor(presvt) ~ ideo + denom + candaff + candft + gender + race + education + income, data=cpart, family=binomial(link='logit'))
summary(cpvt)

t.test(cpart$ideo[cpart$pid==1], dat2$ideo[dat2$pid==1 & dat2$cross==0])
# 2.23, 1.77

t.test(cpart$ideo[cpart$pid==3], dat2$ideo[dat2$pid==3 & dat2$cross==0])
# 2.23, 2.57


########## The Table ##########

stargazer(cpvt, title="Predictors of Vote Choice Among Disloyal Partisans", dep.var.labels=c("Presidential Vote Choice"), align=T, no.space=T,
	covariate.labels=c("Ideology", "Religion", "Candidate Evaluation", "Candidate Affect", "Male", "Race", "College Degree", "Income", "Intercept"))



### Candidate Affect and Candidate Evaluation ###

temp <- data.frame(ideo=rep(median(cpart$ideo, na.rm=T), 42), denom=rep(median(cpart$denom, na.rm=T), 42), candaff=rep(seq(from=-10, to=10, length.out=21), 2),
		candft=rep(c(46, 68), 21), gender=rep(1, 42), race=rep(2, 42), education=rep(median(cpart$education, na.rm=T), 42), income=rep(median(cpart$income, na.rm=T), 42))

preds <- cbind(temp, predict(cpvt, temp, se=T, type='link'))
preds <- within(preds, {
	PredictedProb <- plogis(fit)
	LL <- plogis(fit - (1.96 * se.fit))
	UL <- plogis(fit + (1.96 * se.fit))
})

crossaff <- ggplot(preds, aes(x = candaff, y = PredictedProb)) +
	geom_line(aes(colour=factor(candft)), lwd=1) +
	scale_colour_manual(name="Candidate Feeling Thermometer",
		values=c("mediumpurple4", "seagreen4"),
		labels=c("25th - High Democrat Affect", "75th - High Republican Affect")) +
	geom_ribbon(data=preds[preds$candft==46,], aes(ymin=LL, ymax=UL), fill="mediumpurple4", alpha=0.2) +
	geom_ribbon(data=preds[preds$candft==68,], aes(ymin=LL, ymax=UL), fill="seagreen4", alpha=0.2) +
	ggtitle("Figure 6. Probability of Voting Republican Among Disloyal Partisans") +
	ylab("Predicted Probability") +
	scale_x_continuous(name="",
		breaks=c(-10, 0, 10),
		labels=c("Most Pro-Democratic", "Neutral", "Most Pro-Republican")) +
	theme_bw() +
	theme(plot.title=element_text(size=26, face="bold"))
ggsave(filename="crossaff.png", plot=crossaff)

### Candidate Affect and Ideology ###

temp <- data.frame(ideo=rep(c(1, 2, 3), 2), denom=rep(median(cpart$denom, na.rm=T), 6), candaff=rep(median(cpart$candaff, na.rm=T), 6),
		candft=c(rep(46, 3), rep(68, 3)), gender=rep(1, 6), race=rep(2, 6), education=rep(median(cpart$education, na.rm=T), 42), income=rep(median(cpart$income, na.rm=T), 6))

preds <- cbind(temp, predict(cpvt, temp, se=T, type='link'))

preds <- within(preds, {
	PredictedProb <- plogis(fit)
	LL <- plogis(fit - (1.96 * se.fit))
	UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(preds, aes(x = ideo, y = PredictedProb)) +
	geom_line(aes(colour=factor(candft)), lwd=1) +
	scale_colour_manual(name="Candidate Feeling Thermometer",
		values=c("mediumpurple4", "seagreen4"),
		labels=c("25th - High Democrat Affect", "75th - High Republican Affect")) +
	geom_ribbon(data=preds[preds$candft==46,], aes(ymin=LL, ymax=UL), fill="mediumpurple4", alpha=0.2) +
	geom_ribbon(data=preds[preds$candft==68,], aes(ymin=LL, ymax=UL), fill="seagreen4", alpha=0.2) +
	ggtitle("Probability of Voting Republican Among Disloyal Partisans") +
	ylab("Predicted Probability") +
	scale_x_continuous(name="Ideology",
		breaks=c(1, 2, 3),
		labels=c("Liberal", "Moderate", "Conservative")) +
	theme_bw()


### Candidate Evaluation and Ideology ###

temp <- data.frame(ideo=c(rep(1, 21), rep(2, 21), rep(3, 21)), denom=rep(median(cpart$denom, na.rm=T), 63), candaff=rep(seq(from=-10, to = 10, length.out=21), 3),
		candft=rep(median(cpart$candft, na.rm=T), 63), gender=rep(1, 63), race=rep(2, 63), education=rep(median(cpart$education, na.rm=T), 63), income=rep(median(cpart$income, na.rm=T), 63))

preds <- cbind(temp, predict(cpvt, temp, se=T, type='link'))

preds <- within(preds, {
	PredictedProb <- plogis(fit)
	LL <- plogis(fit - (1.96 * se.fit))
	UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(preds, aes(x = candaff, y = PredictedProb)) +
	geom_line(aes(colour=factor(ideo)), lwd=1) +
	scale_colour_manual(name="Ideology",
		values=c("blue", "green", "red"),
		labels=c("Liberal", "Moderate", "Conservative")) +
	geom_ribbon(data=preds[preds$ideo==1,], aes(ymin=LL, ymax=UL), fill="blue", alpha=0.2) +
	geom_ribbon(data=preds[preds$ideo==2,], aes(ymin=LL, ymax=UL), fill="green", alpha=0.2) +
	geom_ribbon(data=preds[preds$ideo==3,], aes(ymin=LL, ymax=UL), fill="red", alpha=0.2) +
	ggtitle("Probability of Voting Republican Among Disloyal Partisans") +
	ylab("Predicted Probability") +
	scale_x_continuous(name="Candidate Evaluation",
		breaks=c(-9, 0, 9),
		labels=c("Most Pro-Democrat", "Neutral", "Most Pro-Republican")) +
	theme_bw()


########## Cross-Party Voting Over Time ##########

cross <- ggplot(dat2[dat2$pid!=2 & !is.na(dat2$pid),], aes(x = cross)) +
	geom_bar(position="dodge", aes(fill=factor(pid)), alpha=.5) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "red"),
		labels=c("Democrats", "Republicans")) +
	facet_wrap(~year, labeller=labeller(year=lab_year)) +
	scale_x_continuous(name="",
		breaks=c(0, 1),
		labels=c("Did Not Defect", "Defected")) +
	ggtitle("Figure 1. Party Defection, 1972-2004") +
	ylab("Count") +
	theme_bw() +
	theme(plot.title=element_text(size=28, face="bold")) +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=16, vjust=0.5))

ggsave(filename="party defection over time.png", plot=cross)
		


########## Replication for Independents ##########

ind <- dat2[dat2$pid==2,]

indvt <- glm(factor(presvt) ~ ideo + denom + candaff + candft + race + education + gender + income, data=ind, family=binomial(link='logit'))
summary(indvt)

########## The Table ##########

stargazer(indvt, title="Predictors of Vote Choice Among Independents", dep.var.labels=c("Presidential Vote Choice"), align=T, no.space=T,
	covariate.labels=c("Ideology", "Religion", "Candidate Evaluation", "Candidate Affect", "Male", "Race", "College Degree", "Income", "Intercept"))


### Candidate Affect by Candidate Evaluation ###

temp <- data.frame(ideo=rep(median(ind$ideo, na.rm=T), 42), denom=rep(median(ind$denom, na.rm=T), 42), candaff=rep(seq(from=-10, to=10, length.out=21), 2),
		candft=rep(c(46, 68), 21), gender=rep(1, 42), race=rep(2, 42), education=rep(median(ind$education, na.rm=T), 42), income=rep(median(ind$income, na.rm=T), 42))

preds <- cbind(temp, predict(indvt, temp, se=T, type='link'))
preds <- within(preds, {
	PredictedProb <- plogis(fit)
	LL <- plogis(fit - (1.96 * se.fit))
	UL <- plogis(fit + (1.96 * se.fit))
})

indaff <- ggplot(preds, aes(x = candaff, y = PredictedProb)) +
	geom_line(aes(colour=factor(candft)), lwd=1) +
	scale_colour_manual(name="Candidate Feeling Thermometer",
		values=c("mediumpurple4", "seagreen4"),
		labels=c("25th - High Democrat Affect", "75th - High Republican Affect")) +
	geom_ribbon(data=preds[preds$candft==46,], aes(ymin=LL, ymax=UL), fill="mediumpurple4", alpha=0.2) +
	geom_ribbon(data=preds[preds$candft==68,], aes(ymin=LL, ymax=UL), fill="seagreen4", alpha=0.2) +
	ggtitle("Figure 5. Effects of Candidate Evaluation and Affect on Vote Choice") +
	ylab("Predicted Probability") +
	scale_x_continuous(name="",
		breaks=c(-10, 0, 10),
		labels=c("Most Pro-Democrat", "Neutral", "Most Pro-Republican")) +
	theme_bw() +
	theme(plot.title=element_text(size=26, face="bold"))
ggsave(filename="indaff.png", plot=indaff)




############################################################################
########## Comparing Evaluations of Loyal Partisans and Defectors ##########
############################################################################

load("DATA mention categories final 1972.RData")


### Likes ###
# Loyal Partisans -- Democratic Candidate Likes #
p1 <- ggplot(dat4[!is.na(dat4$dcand_like) & dat4$cross==0,], aes(x=dcand_like, fill=factor(pid))) +
	geom_bar(position="dodge", alpha=0.5) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3, 4, 5, 6, 7),
		labels=c("Party Characteristics", "Candidate Characteristics", "Government Philosophy", "Issues", "Group-Based Preferences", "Campaign Events", "Miscellaneous"),
		limits=c(1, 7)) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "red"),
		labels=c("Democrats", "Republicans")) +
	ylab("Count") +
	ggtitle("Mentioned Likes of the Democratic Presidential Candidate -- Loyal Partisans") +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="loyal_dlike.png", plot=p1)

# Disloyal Partisans -- Democratic Candidate Likes #
p2 <- ggplot(dat4[!is.na(dat4$dcand_like) & dat4$cross==1,], aes(x=dcand_like, fill=factor(pid))) +
	geom_bar(position="dodge", alpha=0.5) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3, 4, 5, 6, 7),
		labels=c("Party Characteristics", "Candidate Characteristics", "Government Philosophy", "Issues", "Group-Based Preferences", "Campaign Events", "Miscellaneous"),
		limits=c(1, 7)) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "green", "red"),
		labels=c("Democrats", "Independents", "Republicans")) +
	ylab("Count") +
	ggtitle("Mentioned Likes of the Democratic Presidential Candidate -- Disloyal Partisans") +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="disloyal_dlike.png", plot=p2)


# Loyal Partisans -- Republican Candidate Likes #
p3 <- ggplot(dat4[!is.na(dat4$rcand_like) & dat4$cross==0,], aes(x=rcand_like, fill=factor(pid))) +
	geom_bar(position="dodge", alpha=0.5) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3, 4, 5, 6, 7),
		labels=c("Party Characteristics", "Candidate Characteristics", "Government Philosophy", "Issues", "Group-Based Preferences", "Campaign Events", "Miscellaneous"),
		limits=c(1, 7)) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "red"),
		labels=c("Democrats", "Republicans")) +
	ylab("Count") +
	ggtitle("Mentioned Likes of the Republican Presidential Candidate -- Loyal Partisans") +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="loyal_rlike.png", plot=p3)


# Disloyal Partisans -- Republican Candidate Likes #
p4 <- ggplot(dat4[!is.na(dat4$rcand_like) & dat4$cross==1,], aes(x=rcand_like, fill=factor(pid))) +
	geom_bar(position="dodge", alpha=0.5) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3, 4, 5, 6, 7),
		labels=c("Party Characteristics", "Candidate Characteristics", "Government Philosophy", "Issues", "Group-Based Preferences", "Campaign Events", "Miscellaneous"),
		limits=c(1, 7)) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "green", "red"),
		labels=c("Democrats", "Independents", "Republicans")) +
	ylab("Count") +
	ggtitle("Mentioned Likes of the Republican Presidential Candidate -- Disloyal Partisans") +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="disloyal_dlike.png", plot=p4)


### Dislikes ###
# Loyal Partisans -- Democratic Candidate Dislikes #
p5 <- ggplot(dat4[!is.na(dat4$dcand_dislike) & dat4$cross==0,], aes(x=dcand_dislike, fill=factor(pid))) +
	geom_bar(position="dodge", alpha=0.5) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3, 4, 5, 6, 7),
		labels=c("Party Characteristics", "Candidate Characteristics", "Government Philosophy", "Issues", "Group-Based Preferences", "Campaign Events", "Miscellaneous"),
		limits=c(1, 7)) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "red"),
		labels=c("Democrats", "Republicans")) +
	ylab("Count") +
	ggtitle("Mentioned Dislikes of the Democratic Presidential Candidate -- Loyal Partisans") +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="loyal_ddislike.png", plot=p5)


# Disloyal Partisans -- Democratic Candidate Dislikes #
p6 <- ggplot(dat4[!is.na(dat4$dcand_dislike) & dat4$cross==1,], aes(x=dcand_dislike, fill=factor(pid))) +
	geom_bar(position="dodge", alpha=0.5) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3, 4, 5, 6, 7),
		labels=c("Party Characteristics", "Candidate Characteristics", "Government Philosophy", "Issues", "Group-Based Preferences", "Campaign Events", "Miscellaneous"),
		limits=c(1, 7)) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "green", "red"),
		labels=c("Democrats", "Independents", "Republicans")) +
	ylab("Count") +
	ggtitle("Mentioned Dislikes of the Democratic Presidential Candidate -- Disloyal Partisans") +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="disloyal_ddislike.png", plot=p6)


# Loyal Partisans -- Republican Candidate Dislikes #
p7 <- ggplot(dat4[!is.na(dat4$rcand_dislike) & dat4$cross==0,], aes(x=rcand_dislike, fill=factor(pid))) +
	geom_bar(position="dodge", alpha=0.5) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3, 4, 5, 6, 7),
		labels=c("Party Characteristics", "Candidate Characteristics", "Government Philosophy", "Issues", "Group-Based Preferences", "Campaign Events", "Miscellaneous"),
		limits=c(1, 7)) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "red"),
		labels=c("Democrats", "Republicans")) +
	ylab("Count") +
	ggtitle("Mentioned Dislikes of the Republican Presidential Candidate -- Loyal Partisans") +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="loyal_rdislike.png", plot=p7)


# Disloyal Partisans -- Republican Candidate Dislikes #
p8 <- ggplot(dat4[!is.na(dat4$rcand_dislike) & dat4$cross==1,], aes(x=rcand_dislike, fill=factor(pid))) +
	geom_bar(position="dodge", alpha=0.5) +
	scale_x_continuous(name="",
		breaks=c(1, 2, 3, 4, 5, 6, 7),
		labels=c("Party Characteristics", "Candidate Characteristics", "Government Philosophy", "Issues", "Group-Based Preferences", "Campaign Events", "Miscellaneous"),
		limits=c(1, 7)) +
	scale_fill_manual(name="Party ID",
		values=c("blue", "green", "red"),
		labels=c("Democrats", "Independents", "Republicans")) +
	ylab("Count") +
	ggtitle("Mentioned Dislikes of the Republican Presidential Candidate -- Disloyal Partisans") +
	theme_bw() +
	theme(axis.text.x=element_text(face="plain", color="black",
		size=10))
ggsave(filename="disloyal_rdislike.png", plot=p8)



################################################
########## Some Figures for the Paper ##########
################################################


### Distribution of Candidate Evaluation Over Time ###

eval <- ggplot(dat2[!is.na(dat2$candaff),], aes(x=candaff)) +
	geom_bar(fill="mediumpurple4", alpha=0.4) +
	scale_x_continuous(name="",
		breaks=c(-9, 0, 9),
		labels=c("Most Pro-Democrat", "Neutral / Ambivalent", "Most Pro-Republican")) +
	ylab("Count") +
	ggtitle("Figure 2. Distribution of Candidate Evaluations, 1972-2004") +
	facet_wrap(~year, labeller=labeller(year=lab_year)) +
	theme_bw() +
	theme(plot.title=element_text(size=26, face="bold")) +
	theme(axis.text.x=element_text(size=10, angle=45, vjust=.5))
ggsave(filename="eval.png", plot=eval)

aff <- ggplot(dat2[!is.na(dat2$candft),], aes(x=candft)) +
	geom_density(fill="mediumpurple4", alpha=0.4) +
	scale_x_continuous(name="",
		breaks=c(2, 50, 99),
		labels=c("Most Democrat Affect", "Neutral / Ambivalent", "Most Republican Affect")) +
	ylab("Density") +
	ggtitle("Figure 3. Distribution of Candidate Affect, 1972-2004") +
	facet_wrap(~year, labeller=labeller(year=lab_year)) +
	theme_bw() +
	theme(plot.title=element_text(size=26, face="bold")) +
	theme(axis.text.x=element_text(size=10, angle=45, vjust=0.5))
ggsave(filename="aff.png", plot=aff)




























































