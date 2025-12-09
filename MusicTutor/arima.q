resetDefaultParameters()
  q0 <<- c(Mechanics=.8,Fluency=.9)
  ## skill bypass parameters in noisy-or model
  q1 <<- c(Mechanics=.4,Fluency=.5)
  varInnovation <<- sigma*sigma*timeStep + (timeStep*rTimediff)^2*q0*(1-q0)*eta*eta
  filterAlpha <<- evalSig^-2/(evalSig^-2+ 1/varInnovation)
  meanInnovation <<- timeStep*rTimediff*q0*eta - timeStep*forgetRate
varInnovation <<- varInnovation*.25
meanInnovation <<- meanInnovation*.25

sime2w <- simulation(52,expFilter,floorPolicy,
                    cycletime=.25)

acf.Mechanics <- acf(diff(sime2w$true.Mechanics))
acf.Fluency <- acf(diff(sime2w$true.Fluency))

## Convert first order autocorrelation to parameter of IMA(1,1) process.
rhoTotheta <- function (rho) {
  b <- 1/rho
  ker <- sqrt(b^2 - 4)
  theta <- (c(-1,1)*ker-b)/2
  ifelse(abs(theta[1]) < 1, theta[1],theta[2])
}

rhoTotheta(acf.Mechanics[1]$acf)
rhoTotheta(acf.Fluency[1]$acf)

filterAlpha <- 1-c(Mechanics=rhoTotheta(acf.Mechanics[1]$acf),
                   Fluency = rhoTotheta(acf.Fluency[1]$acf))
  
sime2wx <- simulation(52,expFilter,floorPolicy,
                     cycletime=.25)

par(mfrow=c(2,2))
acf(diff(sime2wx$true.Fluency))[1]
acf(diff(sime2wx$true.Mechanics))[1]
acf(diff(sime2w$true.Fluency))[1]
acf(diff(sime2w$true.Mechanics))[1]

refilter <- function (sim, alpha=.5, prof=c("Mechanics","Fluency")) {
  obs <- paste("obs",prof,sep=".")
  est <- paste("est",prof,sep=".")
  N <- nrow(sim)
  sim[1,est] <- sim[1,obs]
  for (i in 2:N) {
    sim[i,est] <- alpha*sim[i,obs]+(1-alpha)*sim[i-1,est]
  }
  sim
}

sime2mf2 <- refilter(sime2mb2,1/2)
sime2mf3 <- refilter(sime2mb2,1/3)
sime2mf4 <- refilter(sime2mb2,1/4)
sime2mf5 <- refilter(sime2mb2,1/5)
sime2mf6 <- refilter(sime2mb2,1/6)
sime2mf7 <- refilter(sime2mb2,1/7)
sime2mf10 <- refilter(sime2mb2,1/10)

pdf("Sime2mf2TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mf2,reftitle="Exponential Filter alpha=1/2")
mtext("Monthly Series, Action Observed:  alpha=1/2",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mf2TS.pdf")

pdf("Sime2mf3TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mf3,reftitle="Exponential Filter alpha=1/3")
mtext("Monthly Series, Action Observed:  alpha=1/3",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mf3TS.pdf")

pdf("Sime2mf4TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mf4,reftitle="Exponential Filter alpha=1/4")
mtext("Monthly Series, Action Observed:  alpha=1/4",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mf4TS.pdf")

pdf("Sime2mf5TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mf5,reftitle="Exponential Filter alpha=1/5")
mtext("Monthly Series, Action Observed:  alpha=1/5",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mf5TS.pdf")

pdf("Sime2mf6TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mf6,reftitle="Exponential Filter alpha=1/6")
mtext("Monthly Series, Action Observed:  alpha=1/6",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mf6TS.pdf")

pdf("Sime2mf7TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mf7,reftitle="Exponential Filter alpha=1/7")
mtext("Monthly Series, Action Observed:  alpha=1/7",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mf7TS.pdf")

pdf("Sime2mf10TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mf10,reftitle="Exponential Filter alpha=1/10")
mtext("Monthly Series, Action Observed:  alpha=1/10",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mf10TS.pdf")

sime2mf25 <- refilter(sime2mb2,1/25)
pdf("Sime2mf25TS.pdf",width=6,height=9)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot.simboot(sime2mf25,reftitle="Exponential Filter alpha=1/25")
mtext("Monthly Series, Action Observed:  alpha=1/25",
      outer=TRUE,cex=1.5,line=-2)
dev.off()
system("open Sime2mf25TS.pdf")




  
  
