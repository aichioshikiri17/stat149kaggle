#Steve Wang
#Stat 149 Pset 4
#April 20th 2015

#1 Cars
library(MASS)

mycars = Cars93
help(Cars93)

mycars$mpg.3 = cut(mycars$MPG.city, breaks=c(0,17.5,23,100), lables = c("low", "med", "high"), ordered = T)

#a. 

mycars1.po = polr(mpg.3 ~ Weight+Origin+AirBags+DriveTrain, Hess=T, data=mycars)
summary(mycars1.po)

#b

mycars2.po = polr(mpg.3 ~ Origin+AirBags+DriveTrain, Hess=T, data=mycars)
anova(mycars1.po, mycars2.po, test='Chi')

mycars3.po = polr(mpg.3 ~ Weight+Origin+AirBags, Hess=T, data=mycars)
summary(mycars3.po)
anova(mycars1.po, mycars3.po, test='Chi')

#d

mycars4.po = polr(mpg.3 ~ Weight+Origin+AirBags+Luggage.room, Hess=T, data=mycars)

summary(mycars4.po)

#import na.convert function

na.convert = function (frame) 
{
  vars <- names(frame)
  if (!is.null(resp <- attr(attr(frame, "terms"), "response"))) {
    vars <- vars[-resp]
    x <- frame[[resp]]
    pos <- is.na(x)
    if (any(pos)) {
      frame <- frame[!pos, , drop = FALSE]
      warning(paste(sum(pos), "observations omitted due to missing values in the response"))
    }
  }
  for (j in vars) {  #j is variable names
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {   # factors
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      else if (is.matrix(x)) {   # matrices
        ats <- attributes(x)
        #               w <- !pos
        x.na <- 1*pos
        x[pos] <- 0
        #               n <- nrow(x)
        #               TT <- array(1, c(1, n))
        #               xbar <- (TT %*% x)/(TT %*% w)
        #               xbar <- t(TT) %*% xbar
        #               x[pos] <- xbar[pos]
        attributes(x) <- ats
        attributes(x.na) <- ats
        dimnames(x.na)[[2]]=paste(dimnames(x)[[2]],".na",sep='')
        frame[[paste(j,".na",sep='')]] <- x.na 
      } else {   # ordinary numerical vector
        ats <- attributes(x)
        #               x[pos] <- mean(x[!pos])
        x[pos] <- 0
        x.na <- 1*pos
        frame[[paste(j,".na",sep='')]] <- x.na 
        attributes(x) <- ats
      }
      frame[[j]] <- x
    }
  }
  frame
}


na.convert.mean = function (frame) 
{
  vars <- names(frame)
  if (!is.null(resp <- attr(attr(frame, "terms"), "response"))) {
    vars <- vars[-resp]
    x <- frame[[resp]]
    pos <- is.na(x)
    if (any(pos)) {
      frame <- frame[!pos, , drop = FALSE]
      warning(paste(sum(pos), "observations omitted due to missing values in the response"))
    }
  }
  for (j in vars) {  #j is variable names
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {   # factors
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      else if (is.matrix(x)) {   # matrices
        ats <- attributes(x)
        x.na <- 1*pos
        #               x[pos] <- 0
        w <- !pos
        n <- nrow(x)
        TT <- array(1, c(1, n))
        xbar <- (TT %*% x)/(TT %*% w)
        xbar <- t(TT) %*% xbar
        x[pos] <- xbar[pos]
        attributes(x) <- ats
        attributes(x.na) <- ats
        dimnames(x.na)[[2]]=paste(dimnames(x)[[2]],".na",sep='')
        frame[[paste(j,".na",sep='')]] <- x.na 
      } else {   # ordinary numerical vector
        ats <- attributes(x)
        x[pos] <- mean(x[!pos])
        #               x[pos] <- 0
        x.na <- 1*pos
        frame[[paste(j,".na",sep='')]] <- x.na 
        attributes(x) <- ats
      }
      frame[[j]] <- x
    }
  }
  frame
}

mycars2 = na.convert(mycars)

mycars5.po = polr(mpg.3 ~ Weight+Origin+AirBags+Luggage.room+Luggage.room.na, Hess=T, data=mycars2)

summary(mycars5.po)


#2: pneumoconiosis

library(faraway)

help(pneumo)

pneumo2 = data.frame(status = rep(pneumo$status, pneumo$Freq), year = rep(pneumo$year, pneumo$Freq))
summary(pneumo2)
library(nnet)
pneumo.mlr = multinom(status~year, data = pneumo2)
#pneumo.mlr2 = multinom(status~year, data = pneumo)
summary(pneumo.mlr)
#summary(pneumo.mlr2)
year = 25
denominator = exp(4.29+year*-0.08356)+1+exp(-0.7682+year*0.0257)
mild.prob = 1/denominator
normal.prob = exp(4.29+year*-0.08356)/denominator
severe.prob = exp(-0.7682+year*0.0257)/denominator
probs = c(mild.prob, normal.prob, severe.prob)
sum(probs)

#b 

pneumo2$status = ordered(pneumo2$status, levels=c("normal", "mild", "severe"))
#tf = data.frame(year = 25)
#test = multinom(status~year, data = pneumo2)
#summary(test)
#predict(test, newdata = tf, type = "probs")

pneumo.polr = polr(status ~ year, data=pneumo2, Hess=T)
summary(pneumo.polr)
predict(pneumo.polr, type = "probs", newdata = tf)

#c

#4
meat = read.table(file.choose(), header = T)

#a
meat.nb1 = glm.nb(Antcount ~Bread+Filling+Butter, data = meat) #by default log -link function
summary(meat.nb1)

#b

meat.nb2 = glm.nb(Antcount ~1, data = meat)  # null model
meat.nb3 = glm.nb(Antcount ~Filling, data = meat) 
meat.nb4 = glm.nb(Antcount ~Filling + Butter, data = meat) 
meat.nb5 = glm.nb(Antcount ~Bread+Filling+Butter, data = meat) 
meat.nb6 = glm.nb(Antcount ~Filling*Butter, data = meat) 
anova(meat.nb2, meat.nb3, test="Chi")
anova(meat.nb3, meat.nb4, test="Chi")
anova(meat.nb4, meat.nb5, test="Chi")
anova(meat.nb4, meat.nb6, test="Chi")
summary(meat.nb4)  #chosen model


#5:

#a: 
library(aod)
#install.packages("aod")
options(contrasts = c("contr.treatment", "contr.treatment"))
esoph.at = betabin(cbind(ncases,ncontrols) ~agegp+tobgp, ~1, data = esoph, control = list(maxit=10000))
summary(esoph.at)

esoph.1.1 = betabin(cbind(ncases,ncontrols) ~agegp, ~1, 
                    data = esoph, control = list(maxit=10000))

esoph.1.2 = betabin(cbind(ncases,ncontrols) ~tobgp, ~1, 
                    data = esoph, control = list(maxit=10000))

esoph.1.3 = betabin(cbind(ncases,ncontrols) ~alcgp, ~1, 
                    data = esoph, control = list(maxit=10000))

esoph.2.1 = betabin(cbind(ncases,ncontrols) ~tobgp+alcgp, ~1, 
                    data = esoph, control = list(maxit=10000))

esoph.2.2 = betabin(cbind(ncases,ncontrols) ~agegp+alcgp, ~1, 
                    data = esoph, control = list(maxit=10000))

esoph.2.3 = betabin(cbind(ncases,ncontrols) ~agegp+tobgp, ~1, 
                    data = esoph, control = list(maxit=10000))

esoph.3.1 = betabin(cbind(ncases,ncontrols) ~agegp+tobgp+alcgp, ~1, 
                    data = esoph, control = list(maxit=10000))

anova(esoph.1.1, esoph.1.2, esoph.1.3, test = "Chi")
anova(esoph.2.1, esoph.2.2, esoph.2.3, test = "Chi")
anova(esoph.2.2, esoph.3.1, test = "Chi")

#5b

pred.bb = predict(esoph.3.1)

esoph.logit = glm(cbind(ncases, ncontrols) ~agegp+alcgp+tobgp, family = binomial(logit), data = esoph)

esoph.glm = predict(esoph.logit)
esoph.glm2 = 1/(1+exp(-esoph.glm))
hist(esoph.glm2)

plot(esoph.glm2, pred.bb)
hist(esoph.glm)
hist(pred.bb)

#6a
help(rats)
data(rats)
detach("package:aod", unload = TRUE)
library(faraway)
help(rats)
data(rats{faraway})

delete(rats)
rat1.1 = glm(time~ poison, family = Gamma(log), data= rats)
rat1.2 = glm(time~ treat, family = Gamma(log), data= rats)
rat2.1 = glm(time~ treat + poison, family = Gamma(log), data= rats)
rat2.1.int = glm(time~ treat * poison, family = Gamma(log), data= rats)
rat1.null= glm(time~ 1, family = Gamma(log), data= rats)
anova(rat1.1, rat1.2, test = "F")  #okay so poison is better than treatment
anova(rat2.1, rat1.1, test = "F")  #together is better than 1
anova(rat2.1, rat2.1.int, test = "F")  #no interaction effects

#6b
plot(cooks.distance(rat2.1), type="h", lwd=2,
     xlab="Observation index",
     ylab="Cook's distances",
     main="Cook's distances for Rat Poisoning")
abline(h=1,lty=2,col="red")
         
rat.fitted = fitted(rat2.1)
rat.resid = residuals(rat2.1, type = "deviance")
library(arm)
plot(rat.fitted, rat.resid,
     xlab="Fitted Counts (mean)",
     ylab="Deviance Resid",
     pch=19, col.pts="red", cex.pts=1.5,
     main="Fitted vs deviance residual plot
     for rat data")
abline(h=0,lty=2,col="green")

summary(rat2.1)


#b

rat1.1 = glm(time~ poison, family = inverse.gaussian(log), data= rats)
rat1.2 = glm(time~ treat, family = inverse.gaussian(log), data= rats)
rat2.1 = glm(time~ treat + poison, family = inverse.gaussian(log), data= rats)
rat2.1.int = glm(time~ treat * poison, family = inverse.gaussian(log), data= rats)
rat1.null= glm(time~ 1, family = inverse.gaussian(log), data= rats)
anova(rat1.1, rat1.2, test = "F")  #okay so poison is better than treatment
anova(rat2.1, rat1.1, test = "F")  #together is better than 1
anova(rat2.1, rat2.1.int, test = "F")  #no interaction effects
summary(rat2.1)



plot(cooks.distance(rat2.1), type="h", lwd=2,
     xlab="Observation index",
     ylab="Cook's distances (Inverse Gaussian)",
     main="Cook's distances for Rat Poisoning")
abline(h=1,lty=2,col="red")

rat.fitted = fitted(rat2.1)
rat.resid = residuals(rat2.1, type = "deviance")
library(arm)
plot(rat.fitted, rat.resid,
     xlab="Fitted Counts (mean - Inverse Gauss",
     ylab="Deviance Resid",
     pch=19, col.pts="red", cex.pts=1.5,
     main="Fitted vs deviance residual plot
     for rat data")
abline(h=0,lty=2,col="green")





