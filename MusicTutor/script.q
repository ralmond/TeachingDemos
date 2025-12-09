
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
  threshlow <<- c(Mechanics=-2,Fluency=-1)
  threshhigh <<- c(Mechanics=1,Fluency=2)
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
  varInnovation <<- sigma*sigma*timeStep + (timeStep*rTimediff)^2*q0*(1-q0)
  filterAlpha <<- evalSig^-2/(evalSig^-2+ 1/varInnovation)
}

#First the monthly series
resetDefaultParameters()
sim0 <- simulation(12,raveEstimator,floorPolicy)

sime0 <- simulation(12,expFilter,floorPolicy)

# Particle Filter with no refresh
boot0 <- bootFilter(sim0[,3:4],sim0[,7:8],sim0[,9:10],
                    nparticles=1000) 

# Particle Filter with no refresh
boote0 <- bootFilter(sime0[,3:4],sime0[,7:8],sime0[,9:10],
                    nparticles=1000) 



pdf("Sim0NoBootstrap.pdf",width=6,height=6)
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boot0,sim0)
mtext("Monthly Series, Action Observed, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sim0NoBootstrap.pdf")

plot.ParticleFilter(boot0,sim0,plotname="Sim0/Music")


ci0b0 <- ci.ParticleFilter(boot0)
mean0b0 <- mean.ParticleFilter(boot0)
sim0b0 <- data.frame(sim0[1:12,],mean0b0,ci0b0$lb,ci0b0$ub)

pdf("Sim0B0TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim0b0)
mtext("Monthly Series, Action Observed, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim0B0TS.pdf")

pdf("Sim0B0PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim0b0,pfonly=TRUE)
mtext("Monthly Series, Action Observed, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim0B0PF.pdf")


##Particle Filter with refresh every four cycles

boot1 <- bootFilter(sim0[,3:4],sim0[,7:8],sim0[,9:10],
                    nparticles=1000,resample=4)

pdf("Sim0Bootstrap.pdf")
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boot1,sim0)
mtext("Monthly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sim0Bootstrap.pdf")

plot.ParticleFilter(boot1,sim0,plotname="Sim0a/Music")

ci0b1 <- ci.ParticleFilter(boot1)
mean0b1 <- mean.ParticleFilter(boot1)
sim0b1 <- data.frame(sim0[1:12,],mean0b1,
                     ci0b1$lb,ci0b1$ub)

pdf("Sim0B1TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim0b1)
mtext("Monthly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim0B1TS.pdf")

pdf("Sim0B1PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim0b1,TRUE)
mtext("Monthly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim0B1PF.pdf")


## New simulation of weekly time scale
resetDefaultParameters()
sim1 <- simulation(52,raveEstimator,floorPolicy,
                   cycletime=.25)

boot2 <- bootFilter(sim1[,3:4],sim1[,7:8],sim1[,9:10],
                    cycletime=sim1[,11],
                    nparticles=1000,resample=4)

par(mfrow=c(1,1))
plot.ParticleFilter(boot2,sim1,delay=.5)

plot.ParticleFilter(boot2,sim1,plotname="Sim1/Music")

ci1b2 <- ci.ParticleFilter(boot2)
mean1b2 <- mean.ParticleFilter(boot2)
sim1b2 <- data.frame(sim1[1:52,],mean1b2,
                     ci1b2$lb,ci1b2$ub)

pdf("Sim1B2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim1b2)
mtext("Weekly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim1B2TS.pdf")

pdf("Sim1B2PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim1b2,TRUE)
mtext("Weekly Series, Action Observed, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim1B2PF.pdf")


############################################################
## Simulation 2, lower success rate
## This is the one used in Section 8.3 of the paper.

resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)

sim2w <- simulation(52,raveEstimator,floorPolicy,
                    cycletime=.25)

boot2w <- bootFilter(sim2w[,3:4],sim2w[,7:8],sim2w[,9:10],
                     cycletime=sim2w[,11],
                     nparticles=1000,resample=4)

par(mfrow=c(1,1))
plot.ParticleFilter(boot2w,sim2w,delay=.5)

plot.ParticleFilter(boot2w,sim2w,plotname="Sim2w/Music")

ci2wb2 <- ci.ParticleFilter(boot2w)
mean2wb2 <- mean.ParticleFilter(boot2w)
sim2wb2 <- data.frame(sim2w[1:52,],mean2wb2,
                      ci2wb2$lb,ci2wb2$ub)

pdf("Sim2wB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim2wb2)
mtext("Weekly Series, Simulation 2, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim2wB2TS.pdf")

pdf("Sim2wB2PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim2wb2,TRUE)
mtext("Weekly Series, Simulation 2, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim2wB2PF.pdf")

############################################################
## Sim2 conditions, monthly series

resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)

sim2m <- simulation(12,raveEstimator,floorPolicy,
                    cycletime=1)

boot2m <- bootFilter(sim2m[,3:4],sim2m[,7:8],sim2m[,9:10],
                     cycletime=sim2m[,11],
                    nparticles=1000,resample=4)

pdf("Sim2mBootstrap.pdf",width=6,height=6)
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boot2m,sim2m)
mtext("Monthly Series, Action Observed,Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sim2mBootstrap.pdf")

plot.ParticleFilter(boot2m,sim2m,plotname="Sim2m/Music")

plot.ParticleFilter(boot2m,sim2m,ask=TRUE)

ci2mb2 <- ci.ParticleFilter(boot2m)
mean2mb2 <- mean.ParticleFilter(boot2m)
sim2mb2 <- data.frame(sim2m[1:12,],mean2mb2,
                      ci2mb2$lb,ci2mb2$ub)

pdf("Sim2mB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim2mb2)
mtext("Monthly Series, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim2mB2TS.pdf")



############################################################
## Sim 3, Adds prerequisite condition
## This is the one used in Section 8.4 of the paper.

resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)

sim3w <- simulation(52,raveEstimatorPreq,floorPolicy,
                    cycletime=.25,
                    advance=lessonEffectPreq,
                    initial=randomStudentPreq)

boot3w <- bootFilter(sim3w[,3:4],sim3w[,7:8],sim3w[,9:10],
                     cycletime=sim3w[,11],
                     nparticles=1000,resample=4,
                     advance=lessonEffectPreq,
                     initial=randomStudentPreq)


par(mfrow=c(1,1))
#plot.ParticleFilter(boot3w,sim3w,delay=.5)

plot.ParticleFilter(boot3w,sim3w,plotname="Sim3w/Music")

ci3wb3 <- ci.ParticleFilter(boot3w)
mean3wb3 <- mean.ParticleFilter(boot3w)
sim3wb3 <- data.frame(sim3w[1:52,],mean3wb3,
                      ci3wb3$lb,ci3wb3$ub)

pdf("Sim3wB3TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim3wb3)
mtext("Weekly Series, Simulation 3, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim3wB3TS.pdf")

pdf("Sim3wB3PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim3wb3,TRUE)
mtext("Weekly Series, Simulation 3, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim3wB3PF.pdf")


############################################################
## Monthly Series

resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)

sim3m <- simulation(12,raveEstimatorPreq,floorPolicy,
                    cycletime=1,
                    advance=lessonEffectPreq,
                    initial=randomStudentPreq)

boot3m <- bootFilter(sim3m[,3:4],sim3m[,7:8],sim3m[,9:10],
                     cycletime=sim3m[,11],
                     nparticles=1000,resample=4,
                     advance=lessonEffectPreq,
                     initial=randomStudentPreq)


par(mfrow=c(1,1))
#plot.ParticleFilter(boot3m,sim3m,delay=.5)

plot.ParticleFilter(boot3m,sim3m,plotname="Sim3m/Music")

ci3mb3 <- ci.ParticleFilter(boot3m)
mean3mb3 <- mean.ParticleFilter(boot3m)
sim3mb3 <- data.frame(sim3m[1:12,],mean3mb3,
                      ci3mb3$lb,ci3mb3$ub)

pdf("Sim3mB3TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim3mb3)
mtext("Monthly Series, Simulation 3, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim3mB3TS.pdf")

pdf("Sim3mB3PF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim3mb3,TRUE)
mtext("Monthly Series, Simulation 3, Action Observed",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim3mB3PF.pdf")


############################################################
## New Experiments without observing action results.

# Particle Filter with no refresh
boot0na <- bootFilter(sim0[,3:4],sim0[,7:8],nparticles=1000)

pdf("Sim0NoANoBoot.pdf",width=6,height=6)
par(mfrow=c(3,4))
plot.ParticleFilter(boot0na,sim0)
dev.off()
system("open Sim0NoANoBoot.pdf")

plot.ParticleFilter(boot0,sim0,plotname="Sim0na/Music")

ci0b0na <- ci.ParticleFilter(boot0na)
mean0b0na <- mean.ParticleFilter(boot0na)
sim0b0na <- data.frame(sim0[1:12,],mean0b0na,
                       ci0b0na$lb,ci0b0na$ub)

pdf("Sim0B0NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim0b0na)
mtext("Monthly Series, Action Unobserved, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim0B0TS.pdf")

pdf("Sim0B0NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim0b0na,pfonly=TRUE)
mtext("Monthly Series, Action Unobserved, No Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim0B0NAPF.pdf")


############################################################
## Sim 0, Bootstrap filter, action unobserved

boot1na <- bootFilter(sim0[,3:4],sim0[,7:8],nparticles=1000,
                      resample=4)

pdf("Sim0NABoot.pdf")
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boot1na,sim0)
mtext("Monthly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sim0NABoot.pdf")

plot.ParticleFilter(boot1na,sim0,plotname="Sim0ana/Music")

ci0b1na <- ci.ParticleFilter(boot1na)
mean0b1na <- mean.ParticleFilter(boot1na)
sim0b1na <- data.frame(sim0[1:12,],mean0b1na,
                       ci0b1na$lb,ci0b1na$ub)

pdf("Sim0B1NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim0b1na)
mtext("Monthly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim0B1NATS.pdf")

pdf("Sim0B1NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim0b1na,TRUE)
mtext("Monthly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim0B1NAPF.pdf")


## New simulation of weekly time scale

boot2na <- bootFilter(sim1[,3:4],sim1[,7:8],
                      cycletime=sim1[,11],
                      nparticles=1000,resample=4)

par(mfrow=c(1,1))
plot.ParticleFilter(boot2na,sim1,delay=.5)

plot.ParticleFilter(boot2na,sim1,plotname="Sim1na/Music")

ci1b2na <- ci.ParticleFilter(boot2na)
mean1b2na <- mean.ParticleFilter(boot2na)
sim1b2na <- data.frame(sim1[1:52,],mean1b2na,
                       ci1b2na$lb,ci1b2na$ub)

pdf("Sim1B2NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim1b2na)
mtext("Weekly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim1B2NATS.pdf")

pdf("Sim1B2NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim1b2na,TRUE)
mtext("Weekly Series, Action Unobserved, Bootstrap Filter",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim1B2NAPF.pdf")


############################################################
## Simulation 2, lower success rate

boot2wna <- bootFilter(sim2w[,3:4],sim2w[,7:8],
                       cycletime=sim2w[,11],
                       nparticles=1000,resample=4)

par(mfrow=c(1,1))
plot.ParticleFilter(boot2wna,sim2w,delay=.5)

plot.ParticleFilter(boot2wna,sim2w,plotname="Sim2wna/Music")

ci2wb2na <- ci.ParticleFilter(boot2wna)
mean2wb2na <- mean.ParticleFilter(boot2wna)
sim2wb2na <- data.frame(sim2w[1:52,],mean2wb2na,
                        ci2wb2na$lb,ci2wb2na$ub)

pdf("Sim2wB2NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim2wb2na)
mtext("Weekly Series, Simulation 2, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim2wB2NATS.pdf")

pdf("Sim2wB2NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim2wb2na,TRUE)
mtext("Weekly Series, Simulation 2, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim2wB2NAPF.pdf")



############################################################
## Now on monthly scale

boot2mna <- bootFilter(sim2m[,3:4],sim2m[,7:8],
                       cycletime=sim2m[,11],
                       nparticles=1000,resample=4)

pdf("Sim2mnaBoot.pdf",width=6,height=6)
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boot2mna,sim2m)
mtext("Monthly Series, Action Unobserved,Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sim2mnaBoot.pdf")

plot.ParticleFilter(boot2mna,sim2m,plotname="Sim2mna/Music")

#plot.ParticleFilter(boot2mna,sim2m,ask=TRUE)

ci2mnab2 <- ci.ParticleFilter(boot2mna)
mean2mnab2 <- mean.ParticleFilter(boot2mna)
sim2mnab2 <- data.frame(sim2m[1:12,],mean2mnab2,
                        ci2mnab2$lb,ci2mnab2$ub)

pdf("Sim2mnaB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim2mnab2)
mtext("Monthly Series, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim2mnaB2TS.pdf")




############################################################
## Sim 3, Adds prerequisite condition

boot3wna <- bootFilter(sim3w[,3:4],sim3w[,7:8],
                       cycletime=sim3w[,11],
                       nparticles=1000,resample=4,
                       advance=lessonEffectPreq,
                       initial=randomStudentPreq)

par(mfrow=c(1,1))
plot.ParticleFilter(boot3wna,sim3w,delay=.5)

plot.ParticleFilter(boot3wna,sim3w,plotname="Sim3wna/Music")

ci3wb3na <- ci.ParticleFilter(boot3wna)
mean3wb3na <- mean.ParticleFilter(boot3wna)
sim3wb3na <- data.frame(sim3w[1:52,],mean3wb3na,
                        ci3wb3na$lb,ci3wb3na$ub)

pdf("Sim3wB3NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim3wb3na)
mtext("Weekly Series, Simulation 3, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim3wB3NATS.pdf")

pdf("Sim3wB3NAPF.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim3wb3na,TRUE)
mtext("Weekly Series, Simulation 3, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sim3wB3NAPF.pdf")


############################################################
## Monthly series

boot3mna <- bootFilter(sim3m[,3:4],sim3m[,7:8],
                       cycletime=sim3m[,11],
                       nparticles=1000,resample=4,
                       advance=lessonEffectPreq,
                       initial=randomStudentPreq)

par(mfrow=c(1,1))
#plot.ParticleFilter(boot3mna,sim3m,delay=.5)

plot.ParticleFilter(boot3mna,sim3m,plotname="Sim3mna/Music")


pdf("Sim3mNABoot.pdf")
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boot3mna,sim3m)
mtext("Prerequisite Constraint, Monthly Series, Action Unobserved",
      outer=TRUE,cex=1.25,line=0)
dev.off()
system("open Sim3mNABoot.pdf")

ci3mnab2 <- ci.ParticleFilter(boot3mna)
mean3mnab2 <- mean.ParticleFilter(boot3mna)
sim3mnab2 <- data.frame(sim3m[1:12,],mean3mnab2,
                        ci3mnab2$lb,ci3mnab2$ub)

pdf("Sim3mnaB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sim3mnab2)
mtext("Prerequisite Constraint, Monthly Series, Action Unobserved",
      outer=TRUE,cex=1.25,line=-2)
dev.off()
system("open Sim3mnaB2TS.pdf")



############################################################
## Legend
pdf("Legend.pdf")
par(mfrow=c(1,1))
plot(0:1,0:1,type="n",xlab="",ylab="",tick=FALSE,labels=FALSE)
legend(0:1,0:1,c("True Proficiency", "Benchmark Test",
                 "Running Average", "Particle",
                 "(Colored according", "to weight)"),
       col=c("red","blue","magenta","black",grey(.5),grey(.75)),
       pch=c(9,2,8,1,1,1), cex=2.5,pt.cex=3.5, bty="n")
dev.off()
system("open Legend.pdf")
