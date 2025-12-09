## Implementation of simple particle filter techniques. 
## <resample> is number of time points between recycling, 
## or zero to suppress resampling.

## In output list, success and map have entry of one size
## less than the est and weight lists (no entry for initial
## time point). 

## <obs>, <act>, and <suc> are matrixes of observations,
## actions, and success values, where rows represent time
## points and columns represent proficiencies.  <suc> 
## may be NULL, in which case random success values 
## will be generated for each time point. 

## <nparticles> controls the number of particles, and
## <resample> controls the number of cycles between
## bootstrap resampling of the particles.

## <cycletime> is the time length of a cycle 
## (relative to the other rate parameters)

## Thes arguments should be functions (or function 
## names) which calculate the given quantities.
## <obsLike> is the likelihood function for the 
## benchmark test
## <successProb> is the probability of success for the
## chosen action 
## <advance> is the forward step generation function of the
## stochastic process
## <initial> is the population prior distribution
bootFilter <- function (obs,act,suc=NULL,
                        resample=0, nparticles = 1000,
                        cycletime=1,
                        obsLike=evalLike,
                        successProb= actionSuccessP,
                        advance = lessonEffect,
                        initial = randomStudent) {
  obs <- as.matrix(obs)
  act <- as.matrix(act)
  if (!is.null(suc)) {
    suc <- as.matrix(suc)
  }
  ncycle <- nrow(obs)-1
  if (length(cycletime) == 1) {
    cycletime <- rep(cycletime,ncycle)
  }
  

  record <- list (est=list(ncycle), weight=list(ncycle),
                  map=list(ncycle-1), suc=list(ncycle-1)) 

  ##Initial setup
  ## The goal is to as much as possible use implicit 
  ## looping functions for operations across particles 
  ## and explicit looping functions for operations
  ## within a particle.
  est <- do.call(initial,list(nparticles))
  record$est[[1]] <- est
  weight <- do.call(obsLike,list(est,obs[1,]))
  record$weight[[1]] <- weight
  nvar <- ncol(est)

  ## Loop over time points
  for (icycle in 2:ncycle) {

    ## Resample if necessary
    if (resample >0 && icycle %% resample == 0) {
      ## Resample
      cat("Resampling at cycle",icycle,"\n")
      map <- wsample(nparticles,weight)
      record$map[[icycle-1]] <- map
      est <- est[map,]
      cat("Ave weight before =",mean(weight),
          "; after=", mean(weight[map]),"\n")
      weight <- rep(1,length(weight))
    } else {
      ## identity map
      map <- 1:nparticles
      record$map[[icycle-1]] <- map
    }

    ## Handle the success variable.  If it is observed, it
    ## provides evidence for the current proficiency.  If it
    ## is unobserved, we need to simulate it.

    sucP <- do.call(successProb,list(est,act[icycle,]))
    
    if (is.null(suc)) {
      ## Need to sample success values
      success <- matrix(runif(length(sucP)) <
                        sucP,nrow(sucP)) 
    } else {
      success <- matrix(suc[icycle,],
                        nparticles,nvar,byrow=TRUE)
      sv <- ifelse(success,sucP,1-sucP)
      weight <- weight * apply(sv,1,prod)
    }
    record$suc[[icycle-1]] <- success

    ## Advance position of particles
    est <- do.call(advance,
              list(est,act[icycle,],
                   success=success,
                   time=cycletime[icycle]))$proficiency
    record$est[[icycle]] <- est
    
    ## Evaluate new observations
    weight <- weight * do.call(obsLike,
                               list(est,obs[icycle,]))
    record$weight[[icycle]] <- weight

  }
  class(record) <- "ParticleFilter"
  record
}
  


## Returns a sample of indexes (1:length(weights)) given
## size using weights as weights
wsample <- function (size, weights) {
  w <- cumsum(weights)/sum(weights)
  ## Outer produces an array of true and false values,
  ## apply counts number of thresholds exceeded.
  apply(outer(runif(size),w,">"),1,sum)+1
}

## Calculates the mean of the particle filter at each time
## point. Argument should be a the output of a bootFilter
## routine. Output is a matrix whose rows correspond to time
## points and whose columns correspond to proficiencies
mean.ParticleFilter <- function (data) {
  vnames <- colnames(data$est[[1]])
  result <- matrix(NA,length(data$est),length(vnames))
  for (icycle in 1:length(data$est)) {
    result[icycle,] <-
      apply(data$est[[icycle]]*data$weight[[icycle]],2,sum) /
        sum(data$weight[[icycle]])
  }
  colnames(result) <- paste("pf",vnames,sep=".")
  result
}

## Calculates an upper and lower quantile of the particle
## distribution and each time point.
## <data> should be a particle filter output and 
## <level> should be a vector of two numbers between zero
## and one for the upper and lower bound.
## Output is list of two matrixes whose rows correspond to
## time points and whose columns correspond to
## proficiencies, one for the upper bound, one for
## the lower bound.  This is not corrected for the number
## of particles.
ci.ParticleFilter <- function (data,level=c(.025,.975)) {
  vnames <- colnames(data$est[[1]])
  ub <- matrix(NA,length(data$est),length(vnames))
  lb <- matrix(NA,length(data$est),length(vnames))
  colnames(ub) <- colnames(lb) <- vnames
  for (icycle in 1:length(data$est)) {
    w <- data$weight[[icycle]]/sum(data$weight[[icycle]])
    for (var in vnames) {
      est <- data$est[[icycle]][,var]
      ord <- order(est)
      cuw <- cumsum(w[ord])
      os <- apply(outer(cuw,level,"<"),2,sum)
      os[2] <- os[2]+1  # Adjust to outer fences
      os <- ifelse(os<1,1,os)
      os <- ifelse(os>length(est),length(est),os)
      ci <- est[ord[os]]
      lb[icycle,var] <- ci[1]
      ub[icycle,var] <- ci[2]
    }
  }
  colnames(ub) <- paste("pfub",vnames,sep=".")
  colnames(lb) <- paste("pflb",vnames,sep=".")
  list(lb=lb,ub=ub)
}

## Produces a series of points for the particle filter, one
## at each time point.  This can be use to produce a series
## of static plots or an animation.
## <pf> is the output of a particle filter and
## <sim> is the simulation object used to gather it.
## If <delay> is non-zero, then a delay of so many seconds
## will be added between plots.  If <ask> is true, 
## then R will prompt between each time frame.
## If <plotname> is non-null, then a series of png files
## will be generated with the given string as a prefix (it
## may contain a "/" to put the generated series in a
## subdirectory.  This image series can then be turned into
## a slide show or an animation. Haven't well thought
## out yet what happens if there are more than two
## proficiency dimensions.
plot.ParticleFilter <-
function(pf,sim,delay=0, plotname=NULL,
         ask=par("ask"), legend=FALSE,
         reftitle="Exponential Filter"
         ) {
  oldpar <- par(ask=ask)
  on.exit(par(oldpar))
  if (!is.null(plotname))
    fmt <- sprintf("%%s%%0%dd.png",
                   floor(log10(length(pf$est)))+1)
  for (i in 1:length(pf$est)) {
    if (!is.null(plotname))
      png(sprintf(fmt,plotname,i))
    g <- 1 - pf$weight[[i]]/max(pf$weight[[i]])
    gord <- order(-g)
    plot(pf$est[[i]][gord,],col=gray(g[gord]),
         main=paste("Time",i), pch=1,
         xlim=c(0,6),ylim=c(0,6))
    points(sim[i,1:2],col="red",pch=9,cex=2)
    points(sim[i,3:4],col="blue",pch=2,cex=2)
    points(sim[i,5:6],col="magenta",pch=8,cex=2)
    if (any(legend !=FALSE)) {
      legend(legend,legend=c("True Proficiency",
                      "Benchmark Test",
                      reftitle, "Particle",
                      "(Colored according", "to weight)"),
       col=c("red","blue","magenta","black",
             grey(.5),grey(.75)),
       pch=c(9,2,8,1,1,1), pt.cex=2)

    }
    if (!is.null(plotname)) dev.off()
    else if (delay >0) Sys.sleep(delay)
  }
  
}

## This produces a series of time series plots, one for each 
## proficiency variable.
## The <simb> input is a data frame formed by joining the
## simulator output, with the mean.ParticleFilter and
## ci.ParticleFilter output.  Currently makes lots of
## assumptions based on the Music Example (hardwired 
## column numbers and names). 
## If <pfonly> is TRUE, then suppressed printing of
## <observed> and <estimate> columns from simulator.
## Need better pass through of graphics parameters 
## to adjust for differences between X11 and PDF
## graphics
plot.simboot <-
function (simb,pfonly=FALSE,reftitle="Running Ave") {
  time <- (1:nrow(simb))-1

  plot(time,simb[,1],type="b",pch=9,col="red",
       ylim=c(0,6),ylab="Mechanics") 
  if (!pfonly) {
    points(time,simb[,3],type="b",pch=2,col="blue")
    points(time,simb[,5],type="b",pch=8,col="magenta")
  }
  points(time,simb[,12],type="b",pch=1,col="black")
  points(time,simb[,14],type="b",pch=1,lty=2,
         col=gray(.25))
  points(time,simb[,16],type="b",pch=1,lty=2,
         col=gray(.25))

  if (pfonly) 
    legend(c(0,max(time)/3),c(6,4.5),
           c("Truth", "Particle Filter Mean",
             "Particle Filter C.I."),
         col=c("red","black",gray(.25)),
         pch=c(9,1,1),cex=.75,
         lty=c(1,1,2))
  else
    legend(c(0,max(time)/3),c(6,3.5),
           c("Truth","Observed",reftitle,
             "Particle Filter Mean",
             "Particle Filter C.I."),
         col=c("red","blue","magenta","black",gray(.25)),
         pch=c(9,2,8,1,1),cex=.75,
         lty=c(1,1,1,1,2))
  

  plot(time,simb[,2],type="b",pch=9,col="red",
       ylim=c(0,6),ylab="Fluency")
  if (!pfonly) {
    points(time,simb[,4],type="b",pch=2,col="blue")
    points(time,simb[,6],type="b",pch=8,col="magenta")
  }
  points(time,simb[,13],type="b",pch=1,col="black")
  points(time,simb[,15],type="b",pch=1,lty=2,col=gray(.25))
  points(time,simb[,17],type="b",pch=1,lty=2,col=gray(.25))
}


## Reverse Filter
## This takes a particle filter data structure and reverses
## the direction of the map, so that it now runs a forward
## and backward filter.

## Although this code is technically correct, it doesn't
## work very well with the bootstrap filter.  The very same
## problem the bootstrap was designed to correct in the
## forward direction (relying on fewer and fewer particles
## to do the estimation) is actually caused by the bootstrap
## in the reverse direction.

reverse.particleFilter <- function (x) {
  nstep <- length(x$est)
  weight <- x$weight[[nstep]] # Weights are taken from the
                              # last step 
  nparticles <- length(weight)
  est <- vector("list",nstep)
  map <- vector("list",nstep)
  ## May or may not need to calculate success
  if (is.null(dim(x$suc[[1]]))) {
    calcSuc <- FALSE
    suc <- x$suc
  } else {
    calcSuc <- TRUE
    suc <- vector("list",nstep)
  }

  

  curmap <- 1:nparticles
  istep <- nstep
  while (istep >0) {
    if (istep < nstep) {
      curmap <- x$map[[istep]][curmap]
    }
    cat("Step = ",istep,"  map = ",curmap,"\n")
    map[[istep]] <- curmap
    est[[istep]] <- x$est[[istep]][curmap,]
    if (calcSuc) {
      suc[[istep]] <- x$est[[istep]][curmap,]
    }
    istep <- istep - 1
  }
  result <- list(est=est,weight=weight,map=map, suc=suc)
  class(result) <- c("ReversedParticleFilter",
                     "ParticleFilter")
  result
}
