## This does simulation for an entire class of students.



## Individualized lessons.  Assumes each student is working
## individually paced with a unique series of lessons.

resetParametersSimulation2()
nstudents <- 100
npaas <- 8

indivData <-
  replicate(nstudents,simulation(npaas,expFilter,floorPolicy),
              simplify=FALSE)

varlabs <- c(paste("M",0:8,sep=""),paste("F",0:8,sep=""))
subjlabs <- paste("St",1:nstudents,sep="")

indivData.true <- t(sapply(indivData,function(x)
                           c(x$true.Mechanics,x$true.Fluency)))
dimnames(indivData.true) <- list(subjlabs,varlabs)

indivData.obs <- t(sapply(indivData,function(x)
                           c(x$obs.Mechanics,x$obs.Fluency)))
dimnames(indivData.obs) <- list(subjlabs,varlabs)

indivData.act <- t(sapply(indivData,function(x)
                           c(x$act.Mechanics,x$act.Fluency)))
dimnames(indivData.act) <- list(subjlabs,varlabs)



