
#This is the actual Simulations

## This function collects all of the initial simulation
## parameters in one place to make it easy to re-run
## simulations. 
resetDefaultParameters <- function () {
  ## Regression coefficients for multivariate regression. 
  evalA <<- matrix(c(.7,.3,.3,.7),2,2)
  ## Noise std for multivariate regression.
  evalSig <<- c(.65,.65)

  ## Constant noise term in noisy-or model
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## Upper and lower bounds for delta in noisy-or model
  threshlow <<- c(Mechanics=-1,Fluency=-2)
  threshhigh <<- c(Mechanics=2,Fluency=1)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.8,Fluency=.7)

  ## Constant term in learning rate
  eta <<- c(Mechanics=.2,Fluency=.2)
  ## Zone of Proximal Development term in learning rate
  gamma <<- c(Mechanics=.1,Fluency=.15)

  ## Constant parameters of the learning process
  forgetRate <<- c(Mechanics=.02,Fluency=.01)

  ## R(t) = (rTimediff*success + rTimeconst)*t
  rTimediff <<- .9
  rTimeconst <<- .1

  ## Variance of learning processes is sigma^2*t
  sigma <<- c(Mechanics=.02,Fluency=.02)

  ## Mean and variance of the population.
  ## popA %*% t(popA) is the variance
  popA <<- matrix(c(.6,.4,.4,.6),2,2)
  popMean <<- c(Mechanics=1.5,Fluency=1.5)

  timeStep <<- 1
  varInnovation <<- sigma*sigma*timeStep + (timeStep*rTimediff)^2*q0*(1-q0)*eta*eta
  #filterAlpha <<- evalSig^-2/(evalSig^-2+ 1/varInnovation)
  filterAlpha <<- .5
  #meanInnovation <<- timeStep*rTimediff*q0*eta - timeStep*forgetRate
  meanInnovation <<- 0
}

#First the monthly series
resetDefaultParameters()
sime0 <- simulation(12,expFilter,floorPolicy)

# Particle Filter with no refresh
boote0 <- bootFilter(sime0[,3:4],sime0[,7:8],sime0[,9:10],
                    nparticles=1000) 



pdf("Sime0NoBootstrap.pdf",width=6,height=6)
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boote0,sime0)
mtext("Monthly Series, Action Observed, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sime0NoBootstrap.pdf")

plot.ParticleFilter(boote0,sime0,plotname="Sime0/Music")


ci0b0 <- ci.ParticleFilter(boote0)
mean0b0 <- mean.ParticleFilter(boote0)
sime0b0 <- data.frame(sime0[1:12,],mean0b0,ci0b0$lb,ci0b0$ub)

pdf("Sime0B0TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime0b0, reftitle="Exponential Filter")
mtext("Monthly Series, Action Observed, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime0B0TS.pdf")

pdf("Sime0B0PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime0b0,pfonly=TRUE)
mtext("Monthly Series, Action Observed, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime0B0PF.pdf")


##Particle Filter with refresh every four cycles

boote1 <- bootFilter(sime0[,3:4],sime0[,7:8],sime0[,9:10],
                    nparticles=1000,resample=4)

pdf("Sime0Bootstrap.pdf")
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boote1,sime0)
mtext("Monthly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sime0Bootstrap.pdf")

plot.ParticleFilter(boote1,sime0,plotname="Sime0a/Music")

ci0b1 <- ci.ParticleFilter(boote1)
mean0b1 <- mean.ParticleFilter(boote1)
sime0b1 <- data.frame(sime0[1:12,],mean0b1,
                     ci0b1$lb,ci0b1$ub)

pdf("Sime0B1TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime0b1,reftitle="Exponential Filter")
mtext("Monthly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime0B1TS.pdf")

pdf("Sime0B1PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime0b1,TRUE)
mtext("Monthly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime0B1PF.pdf")


## New simulation of weekly time scale
resetDefaultParameters()
varInnovation <<- varInnovation*.25
meanInnovation <<- meanInnovation*.25

sime1 <- simulation(52,expFilter,floorPolicy,
                   cycletime=.25)

boote2 <- bootFilter(sime1[,3:4],sime1[,7:8],sime1[,9:10],
                    cycletime=sime1[,11],
                    nparticles=1000,resample=4)

par(mfrow=c(1,1))
plot.ParticleFilter(boote2,sime1,delay=.5)

plot.ParticleFilter(boote2,sime1,plotname="Sime1/Music")

ci1b2 <- ci.ParticleFilter(boote2)
mean1b2 <- mean.ParticleFilter(boote2)
sime1b2 <- data.frame(sime1[1:52,],mean1b2,
                     ci1b2$lb,ci1b2$ub)

pdf("Sime1B2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime1b2)
mtext("Weekly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime1B2TS.pdf")

pdf("Sime1B2PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime1b2,TRUE)
mtext("Weekly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime1B2PF.pdf")


############################################################
## Simulation 2, lower success rate
## This is the one used in Section 8.3 of the paper.

resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)
  varInnovation <<- sigma*sigma*timeStep + (timeStep*rTimediff)^2*q0*(1-q0)*eta*eta
  #varInnovation <<- varInnovation*.25
  #filterAlpha <<- evalSig^-2/(evalSig^-2+ 1/varInnovation)
  #meanInnovation <<- timeStep*rTimediff*q0*eta - timeStep*forgetRate
#meanInnovation <<- meanInnovation*.25

sime2w <- simulation(52,expFilter,floorPolicy,
                    cycletime=.25)

boote2w <- bootFilter(sime2w[,3:4],sime2w[,7:8],sime2w[,9:10],
                     cycletime=sime2w[,11],
                     nparticles=1000,resample=4)

par(mfrow=c(1,1))
plot.ParticleFilter(boote2w,sime2w,delay=.5)

plot.ParticleFilter(boote2w,sime2w,plotname="Sime2w/Music")

ci2wb2 <- ci.ParticleFilter(boote2w)
mean2wb2 <- mean.ParticleFilter(boote2w)
sime2wb2 <- data.frame(sime2w[1:52,],mean2wb2,
                      ci2wb2$lb,ci2wb2$ub)

sum(sime2wb2$true.Mechanics < sime2wb2$pflb.Mechanics ||
    sime2wb2$true.Mechanics > sime2wb2$pfub.Mechanics)
sum(sime2wb2$true.Fluency < sime2wb2$pflb.Fluency ||
    sime2wb2$true.Fluency > sime2wb2$pfub.Fluency)


pdf("Sime2wB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2wb2, reftitle="Exponential Filter")
mtext("Weekly Series, Simulation 2, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2wB2TS.pdf")

pdf("Sime2wB2PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2wb2,TRUE)
mtext("Weekly Series, Simulation 2, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2wB2PF.pdf")

############################################################
## Sim2 conditions, monthly series

resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)
  varInnovation <<- sigma*sigma*timeStep + (timeStep*rTimediff)^2*q0*(1-q0)*eta*eta
  #filterAlpha <<- evalSig^-2/(evalSig^-2+ 1/varInnovation)
  #meanInnovation <<- timeStep*rTimediff*q0*eta - timeStep*forgetRate

sime2m <- simulation(12,expFilter,floorPolicy,
                    cycletime=1)

boote2m <- bootFilter(sime2m[,3:4],sime2m[,7:8],sime2m[,9:10],
                     cycletime=sime2m[,11],
                    nparticles=1000,resample=4)

pdf("Sime2mBootstrap.pdf",width=6,height=6)
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boote2m,sime2m)
mtext("Monthly Series, Action Observed,Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sime2mBootstrap.pdf")

plot.ParticleFilter(boote2m,sime2m,plotname="Sime2m/Music")

plot.ParticleFilter(boote2m,sime2m,ask=TRUE)

ci2mb2 <- ci.ParticleFilter(boote2m)
mean2mb2 <- mean.ParticleFilter(boote2m)
sime2mb2 <- data.frame(sime2m[1:12,],mean2mb2,
                      ci2mb2$lb,ci2mb2$ub)

sum(sime2mb2$true.Mechanics < sime2mb2$pflb.Mechanics ||
    sime2mb2$true.Mechanics > sime2mb2$pfub.Mechanics)
sum(sime2mb2$true.Fluency < sime2mb2$pflb.Fluency ||
    sime2mb2$true.Fluency > sime2mb2$pfub.Fluency)


pdf("Sime2mB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mb2,reftitle="Exponential Filter")
mtext("Monthly Series, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mB2TS.pdf")


xtable(as.matrix(sime2m[,c(1:4,7:10)]), digits=c(2,2,2,1,1,1,1,0,0))


############################################################
## Sim 3, Adds prerequisite condition
## This is the one used in Section 8.4 of the paper.

resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)
  varInnovation <<- sigma*sigma*timeStep + (timeStep*rTimediff)^2*q0*(1-q0)*eta*eta
  varInnovation <<- varInnovation*.25
  #filterAlpha <<- evalSig^-2/(evalSig^-2+ 1/varInnovation)
  #meanInnovation <<- timeStep*rTimediff*q0*eta - timeStep*forgetRate
  #meanInnovation <<- meanInnovation*.25

sime3w <- simulation(52,expFilterPreq,floorPolicy,
                    cycletime=.25,
                    advance=lessonEffectPreq,
                    initial=randomStudentPreq)

boote3w <- bootFilter(sime3w[,3:4],sime3w[,7:8],sime3w[,9:10],
                     cycletime=sime3w[,11],
                     nparticles=1000,resample=4,
                     advance=lessonEffectPreq,
                     initial=randomStudentPreq)


par(mfrow=c(1,1))
plot.ParticleFilter(boote3w,sime3w,delay=.5)

plot.ParticleFilter(boote3w,sime3w,plotname="Sime3w/Music")

ci3wb3 <- ci.ParticleFilter(boote3w)
mean3wb3 <- mean.ParticleFilter(boote3w)
sime3wb3 <- data.frame(sime3w[1:52,],mean3wb3,
                      ci3wb3$lb,ci3wb3$ub)

pdf("Sime3wB3TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime3wb3, reftitle="Exponential Filter")
mtext("Weekly Series, Simulation 3, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime3wB3TS.pdf")

pdf("Sime3wB3PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime3wb3,TRUE)
mtext("Weekly Series, Simulation 3, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime3wB3PF.pdf")


############################################################
## Monthly Series

resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)
  varInnovation <<- sigma*sigma*timeStep + (timeStep*rTimediff)^2*q0*(1-q0)*eta*eta
  #filterAlpha <<- evalSig^-2/(evalSig^-2+ 1/varInnovation)
  evalSig^-2/(evalSig^-2+ 1/(evalSig^2+varInnovation))
  #meanInnovation <<- timeStep*rTimediff*q0*eta - timeStep*forgetRate

sime3m <- simulation(12,expFilterPreq,floorPolicy,
                    cycletime=1,
                    advance=lessonEffectPreq,
                    initial=randomStudentPreq)

boote3m <- bootFilter(sime3m[,3:4],sime3m[,7:8],sime3m[,9:10],
                     cycletime=sime3m[,11],
                     nparticles=1000,resample=4,
                     advance=lessonEffectPreq,
                     initial=randomStudentPreq)


par(mfrow=c(1,1))
#plot.ParticleFilter(boote3m,sime3m,delay=.5)

plot.ParticleFilter(boote3m,sime3m,plotname="Sime3m/Music")

ci3mb3 <- ci.ParticleFilter(boote3m)
mean3mb3 <- mean.ParticleFilter(boote3m)
sime3mb3 <- data.frame(sime3m[1:12,],mean3mb3,
                      ci3mb3$lb,ci3mb3$ub)

pdf("Sime3mB3TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime3mb3,reftitle="Exponential Filter")
mtext("Monthly Series, Simulation 3, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime3mB3TS.pdf")

pdf("Sime3mB3PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime3mb3,TRUE)
mtext("Monthly Series, Simulation 3, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime3mB3PF.pdf")


############################################################
## New Experiments without observing action results.

# Particle Filter with no refresh
boote0na <- bootFilter(sime0[,3:4],sime0[,7:8],nparticles=1000)

pdf("Sime0NoANoBoote.pdf",width=6,height=6)
par(mfrow=c(3,4))
plot.ParticleFilter(boote0na,sime0)
dev.off()
system("open Sime0NoANoBoote.pdf")

plot.ParticleFilter(boote0,sime0,plotname="Sime0na/Music")

ci0b0na <- ci.ParticleFilter(boote0na)
mean0b0na <- mean.ParticleFilter(boote0na)
sime0b0na <- data.frame(sime0[1:12,],mean0b0na,
                       ci0b0na$lb,ci0b0na$ub)

pdf("Sime0B0NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime0b0na, reftitle="Exponential Filter")
mtext("Monthly Series, Action Unobserved, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime0B0TS.pdf")

pdf("Sime0B0NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime0b0na,pfonly=TRUE)
mtext("Monthly Series, Action Unobserved, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime0B0NAPF.pdf")


############################################################
## Sim 0, Bootstrap filter, action unobserved

boote1na <- bootFilter(sime0[,3:4],sime0[,7:8],nparticles=1000,
                      resample=4)

pdf("Sime0NABoote.pdf")
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boote1na,sime0)
mtext("Monthly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sime0NABoote.pdf")

plot.ParticleFilter(boote1na,sime0,plotname="Sime0ana/Music")

ci0b1na <- ci.ParticleFilter(boote1na)
mean0b1na <- mean.ParticleFilter(boote1na)
sime0b1na <- data.frame(sime0[1:12,],mean0b1na,
                       ci0b1na$lb,ci0b1na$ub)

pdf("Sime0B1NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime0b1na,reftitle="Exponential Filter")
mtext("Monthly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime0B1NATS.pdf")

pdf("Sime0B1NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime0b1na,TRUE)
mtext("Monthly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime0B1NAPF.pdf")


## New simulation of weekly time scale

boote2na <- bootFilter(sime1[,3:4],sime1[,7:8],
                      cycletime=sime1[,11],
                      nparticles=1000,resample=4)

par(mfrow=c(1,1))
plot.ParticleFilter(boote2na,sime1,delay=.5)

plot.ParticleFilter(boote2na,sime1,plotname="Sime1na/Music")

ci1b2na <- ci.ParticleFilter(boote2na)
mean1b2na <- mean.ParticleFilter(boote2na)
sime1b2na <- data.frame(sime1[1:52,],mean1b2na,
                       ci1b2na$lb,ci1b2na$ub)

pdf("Sime1B2NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime1b2na,reftitle="Exponential Filter")
mtext("Weekly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime1B2NATS.pdf")

pdf("Sime1B2NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime1b2na,TRUE)
mtext("Weekly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime1B2NAPF.pdf")


############################################################
## Simulation 2, lower success rate

boote2wna <- bootFilter(sime2w[,3:4],sime2w[,7:8],
                       cycletime=sime2w[,11],
                       nparticles=1000,resample=4)

par(mfrow=c(1,1))
plot.ParticleFilter(boote2wna,sime2w,delay=.5)

plot.ParticleFilter(boote2wna,sime2w,plotname="Sime2wna/Music")

ci2wb2na <- ci.ParticleFilter(boote2wna)
mean2wb2na <- mean.ParticleFilter(boote2wna)
sime2wb2na <- data.frame(sime2w[1:52,],mean2wb2na,
                        ci2wb2na$lb,ci2wb2na$ub)

pdf("Sime2wB2NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2wb2na,reftitle="Exponential Filter")
mtext("Weekly Series, Simulation 2, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2wB2NATS.pdf")

pdf("Sime2wB2NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2wb2na,TRUE)
mtext("Weekly Series, Simulation 2, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2wB2NAPF.pdf")



############################################################
## Now on monthly scale

boote2mna <- bootFilter(sime2m[,3:4],sime2m[,7:8],
                       cycletime=sime2m[,11],
                       nparticles=1000,resample=4)

pdf("Sime2mnaBoote.pdf",width=6,height=6)
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boote2mna,sime2m)
mtext("Monthly Series, Action Unobserved,Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sime2mnaBoote.pdf")

plot.ParticleFilter(boote2mna,sime2m,plotname="Sime2mna/Music")

#plot.ParticleFilter(boote2mna,sime2m,ask=TRUE)

ci2mnab2 <- ci.ParticleFilter(boote2mna)
mean2mnab2 <- mean.ParticleFilter(boote2mna)
sime2mnab2 <- data.frame(sime2m[1:12,],mean2mnab2,
                        ci2mnab2$lb,ci2mnab2$ub)

pdf("Sime2mnaB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mnab2,reftitle="Exponential Filter")
mtext("Monthly Series, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mnaB2TS.pdf")




############################################################
## Sim 3, Adds prerequisite condition

boote3wna <- bootFilter(sime3w[,3:4],sime3w[,7:8],
                       cycletime=sime3w[,11],
                       nparticles=1000,resample=4,
                       advance=lessonEffectPreq,
                       initial=randomStudentPreq)

par(mfrow=c(1,1))
plot.ParticleFilter(boote3wna,sime3w,delay=.5)

plot.ParticleFilter(boote3wna,sime3w,plotname="Sime3wna/Music")

ci3wb3na <- ci.ParticleFilter(boote3wna)
mean3wb3na <- mean.ParticleFilter(boote3wna)
sime3wb3na <- data.frame(sime3w[1:52,],mean3wb3na,
                        ci3wb3na$lb,ci3wb3na$ub)

pdf("Sime3wB3NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime3wb3na,reftitle="Exponential Filter")
mtext("Weekly Series, Simulation 3, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime3wB3NATS.pdf")

pdf("Sime3wB3NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime3wb3na,TRUE)
mtext("Weekly Series, Simulation 3, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime3wB3NAPF.pdf")


############################################################
## Monthly series

boote3mna <- bootFilter(sime3m[,3:4],sime3m[,7:8],
                       cycletime=sime3m[,11],
                       nparticles=1000,resample=4,
                       advance=lessonEffectPreq,
                       initial=randomStudentPreq)

par(mfrow=c(1,1))
#plot.ParticleFilter(boote3mna,sime3m,delay=.5)

plot.ParticleFilter(boote3mna,sime3m,plotname="Sime3mna/Music")


pdf("Sime3mNABoote.pdf")
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boote3mna,sime3m)
mtext("Prerequisite Constraint, Monthly Series, Action Unobserved",
      outer=TRUE,cex=1.25,line=0)
dev.off()
system("open Sime3mNABoote.pdf")

ci3mnab2 <- ci.ParticleFilter(boote3mna)
mean3mnab2 <- mean.ParticleFilter(boote3mna)
sime3mnab2 <- data.frame(sime3m[1:12,],mean3mnab2,
                        ci3mnab2$lb,ci3mnab2$ub)

pdf("Sime3mnaB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime3mnab2,reftitle="Exponential Filter")
mtext("Prerequisite Constraint, Monthly Series, Action Unobserved",
      outer=TRUE,cex=1.25,line=-2)
dev.off()
system("open Sime3mnaB2TS.pdf")



############################################################
## Legend
pdf("Legende.pdf")
par(mfrow=c(1,1))
plot(0:1,0:1,type="n",xlab="",ylab="",tick=FALSE,labels=FALSE)
legend(0:1,0:1,c("True Proficiency", "Benchmark Test",
                 "Exponential Filter", "Particle",
                 "(Colored according", "to weight)"),
       col=c("red","blue","magenta","black",grey(.5),grey(.75)),
       pch=c(9,2,8,1,1,1), cex=2.5,pt.cex=3.5, bty="n")
dev.off()
system("open Legende.pdf")
