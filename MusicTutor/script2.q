#This is the actual Simulations

## This function collects all of the initial simulation
## parameters in one place to make it easy to re-run
## simulations. 
resetParametersSimulation2 <- function () {
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
  q1 <<- c(Mechanics=.4,Fluency=.5)

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
  varInnovation <<- sigma*sigma*timeStep +
    (timeStep*rTimediff)^2*q0*(1-q0)*eta*eta
  filterAlpha <<- .5
  meanInnovation <<- 0

}

#########################################################
## Simulation 2, lower success rate
## This is the one used in Section 8.3 of the paper.

resetParametersSimulation2()


sime2w <- simulation(52,expFilter,floorPolicy,
                    cycletime=.25)

boote2w <- bootFilter(sime2w[,3:4],sime2w[,7:8],
                      sime2w[,9:10],
                      cycletime=sime2w[,11],
                      nparticles=1000,resample=4)

## These two plots produce "animations" of the series
par(mfrow=c(1,1))
plot.ParticleFilter(boote2w,sime2w,delay=.25,
                    legend="topleft")

plot.ParticleFilter(boote2w,sime2w,
                    plotname="Sime2w/Music",
                    legend="topleft")

## Calculate confidince bands
ci2wb2 <- ci.ParticleFilter(boote2w)
mean2wb2 <- mean.ParticleFilter(boote2w)
sime2wb2 <- data.frame(sime2w[1:52,],mean2wb2,
                      ci2wb2$lb,ci2wb2$ub)

## Count the number of times the true series passes 
## outside of the confidence band.  
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


########################################################
## Sim2 conditions, monthly series

resetParametersSimulation2()

sime2m <- simulation(12,expFilter,floorPolicy,
                    cycletime=1)

boote2m <- bootFilter(sime2m[,3:4],sime2m[,7:8],
                      sime2m[,9:10],
                      cycletime=sime2m[,11],
                      nparticles=1000,resample=4)

pdf("Sime2mBootstrap.pdf",width=6,height=6)
par(mfrow=c(3,4),oma=c(0,0,3,0))
plot.ParticleFilter(boote2m,sime2m)
mtext("Monthly Series, Action Observed,Bootstrap Filter",
      outer=TRUE,cex=1.5,line=0)
dev.off()
system("open Sime2mBootstrap.pdf")

## Animations of estimation
plot.ParticleFilter(boote2m,sime2m,
                    plotname="Sime2m/Music",
                    legend="topleft")
plot.ParticleFilter(boote2m,sime2m,delay=.25,
                    legend="topleft")

ci2mb2 <- ci.ParticleFilter(boote2m)
mean2mb2 <- mean.ParticleFilter(boote2m)
sime2mb2 <- data.frame(sime2m[1:12,],mean2mb2,
                      ci2mb2$lb,ci2mb2$ub)

## Number of times true series falls outside of
## confidence bands
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


## Reported table of results
xtable(as.matrix(sime2m[,c(1:4,7:10)]),
       digits=c(2,2,2,1,1,1,1,0,0))


########################################################
## Simulation 2, action unobserved

boote2wna <- bootFilter(sime2w[,3:4],sime2w[,7:8],
                       cycletime=sime2w[,11],
                       nparticles=1000,resample=4)

## Animations
par(mfrow=c(1,1))
plot.ParticleFilter(boote2wna,sime2w,delay=.5,
                    legend="topleft")
plot.ParticleFilter(boote2wna,sime2w,
                    plotname="Sime2wna/Music",
                    legend="topleft")

ci2wb2na <- ci.ParticleFilter(boote2wna)
mean2wb2na <- mean.ParticleFilter(boote2wna)
sime2wb2na <- data.frame(sime2w[1:52,],mean2wb2na,
                        ci2wb2na$lb,ci2wb2na$ub)

## Count the number of times the true series passes 
## outside of the confidence band.  
sum(sime2wb2na$true.Mechanics < sime2wb2na$pflb.Mechanics ||
    sime2wb2na$true.Mechanics > sime2wb2na$pfub.Mechanics)
sum(sime2wb2na$true.Fluency < sime2wb2na$pflb.Fluency ||
    sime2wb2na$true.Fluency > sime2wb2na$pfub.Fluency)

pdf("Sime2wB2NATS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2wb2na,reftitle="Exponential Filter")
mtext("Weekly Series, Simulation 2, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2wB2NATS.pdf")



#########################################################
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

## Animations
plot.ParticleFilter(boote2mna,sime2m,
                    plotname="Sime2mna/Music",
                    legend="topleft")
plot.ParticleFilter(boote2mna,sime2m,delay=.5,
                    legend="topleft")

ci2mnab2 <- ci.ParticleFilter(boote2mna)
mean2mnab2 <- mean.ParticleFilter(boote2mna)
sime2mnab2 <- data.frame(sime2m[1:12,],mean2mnab2,
                        ci2mnab2$lb,ci2mnab2$ub)

## Number of times true series falls outside of
## confidence bands
sum(sime2mb2$true.Mechanics < sime2mb2$pflb.Mechanics ||
    sime2mb2$true.Mechanics > sime2mb2$pfub.Mechanics)
sum(sime2mb2$true.Fluency < sime2mb2$pflb.Fluency ||
    sime2mb2$true.Fluency > sime2mb2$pfub.Fluency)


pdf("Sime2mnaB2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mnab2,reftitle="Exponential Filter")
mtext("Monthly Series, Action Unobserved",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mnaB2TS.pdf")


