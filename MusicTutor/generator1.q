# These are files for the data generation routines.

## This function takes the proficiency expressed as a vector
## (Mechanics, Fluency) and generates a random lesson value.
evalLesson <- function(proficiency) {
  result <- proficiency %*% evalA +
    rnorm(length(evalSig))*evalSig
  result <- round(2*result)/2
  result <- ifelse((result < 0),0,result)
  result <- ifelse((result > 6),6,result)
  result
}

## Calculates likelihood of observation given proficiency.
## Assume Proficiency is a matrix with columns equal to the
## particles.  Assume observation is a vector (Mechanics,
## Fluency) Returns a vector of likelihoods, one for each
## particle 
evalLike <- function (proficiency, observation) {
  if (!is.matrix(proficiency))
    proficiency <- matrix(proficiency,1)
  ## I'm ignoring the normalization constant and 
  ## the rounding error 
  ker <- sweep(sweep(proficiency %*% evalA,2,observation),
               2,evalSig,"/")
  exp(-apply(ker^2,1,sum)/2)
  ## exp(-sum( ( (proficiency %*% evalA -
  ## observation)/evalSig)^2)/2)
}

## Calculates success probability of action based
## on current proficiency level.
## Assumes action is a vectors of the form
## (Mechanics,Fluency) 
## Assumes proficiency is a matrix with columns
## (Mechanics, Fluency) returns a vector of
## probabilities of success for the lesson. 
actionSuccessP <- function (proficiency, action) {
  if (!is.matrix(proficiency)) {
    proficiency <- matrix(proficiency,1)
  }
  ##   delta <- threshlow <= action-proficiency &
  ##             action - proficiency <= threshhigh
  diff <- sweep(proficiency,2,action)
  delta <- sweep(diff,2,threshlow,">=") &
           sweep(diff,2,threshhigh,"<=")
  ##   q0 * prod(q1^(1-delta))
  ## Need to transpose matrix to get rows varying fastest
  qq1 <- apply(q1^(1-t(delta)),2,prod)
  outer(qq1,q0,"*")
}

## Calculates the increase rate based on current
## position and selected action. 
## Assumes proficiency is a matrix and action is
## a vector
learnRate <- function(proficiency,action) {
  if (!is.matrix(proficiency)) {
    proficiency <- matrix(proficiency,1)
  }
#  eta - gamma*(proficiency-action)^2
  lam <- sweep(proficiency,2,action)^2
  sweep(-sweep(lam,2,gamma,"*"),2,eta,"+")
}

## This function calculates what happens between two lessons
## The the Action arguments are assumed to vectors of the
## form (Mechanics, Fluency).  The time argument should be a
## scalar.  Success argument is optionally observed. 
## The proficiency and success arguments should be matrixes
## whose rows correspond to particles and have the
## (Mechanics, Fluency) pattern.  
lessonEffect <-
function (proficiency, action, time=1,
          success = runif(length(proficiency)) <
                    actionSuccessP(proficiency,action)
          ){
  if (!is.matrix(proficiency)) {
    proficiency <- matrix(proficiency,1)
  }
  if (!is.matrix(success)) {
    success <- matrix(success, nrow(proficiency),
                      ncol(proficiency), byrow=TRUE)
  }
  rehearsal <- (rTimediff*success+rTimeconst)*time
  lambda <- learnRate(proficiency,action)
  sigmat <- sigma*sqrt(time)
  proficiency1 <- proficiency + lambda*rehearsal -
                  forgetRate*time 
  proficiency1 <- proficiency1 +
    matrix(rnorm(length(proficiency))*sigmat,
           nrow(proficiency),ncol(proficiency), byrow=TRUE)
  proficiency1 <- ifelse (proficiency1 < 0, 0, proficiency1)
  proficiency1 <- ifelse (proficiency1 > 6, 0, proficiency1)
  list(success=success,proficiency=proficiency1)
}

## Generates the requested number of students
## returns a matrix with rows corresponding to students
## (particles) and columns corresponding to proficiencies.  
randomStudent <- function(nstudents=1) {
  nvar <- length(popMean)
  z <- matrix(rnorm(nstudents*nvar),nstudents,nvar)
  result <- sweep(z%*%popA,2,popMean,"+")
  ##result <- as.vector(popA%*%rnorm(length(popMean))) +
  ## popMean
  result <- ifelse((result < 0),0,result)
  result <- ifelse((result > 6),6,result)
  colnames(result)<-names(popMean)
  result
}


## ncycles -- number of cycles to simulate
## estimator -- function used to estimate current student
## level from observables.
## policy -- function used to estimate next action from
## estimate
## evaluation -- function used to generate observables from
## current proficiencies
## advance -- function used to generate proficiencies at
## next time step from current time step.  Note:  assume
## that this returns a list of two values, success of action
## and new proficiencies  
## initial -- function to generate random initial starting
## value for student.
## cycletime --- time between measurements (could be scaler
## or vector of length ncycles)
simulation <- function(ncycles,estimator,policy,
                       evaluation = evalLesson,
                       advance = lessonEffect,
                       initial = randomStudent,
                       cycletime=1) {
  proficiency <- do.call(initial,list())
  nvar <- length(proficiency)
  tnames <- paste("Time",0:ncycles)
  vnames <- colnames(proficiency)

  
  # We will fill these in as we go.
  truth <- matrix(NA,ncycles+1,nvar,
                  dimnames=list(tnames,vnames))
  obs <- truth
  est <- truth
  act <- truth
  suc <- truth
  if (length(cycletime) == 1) {
    cycletime <- rep(cycletime,ncycles)
  }
  ## Last cycle is stub only
  cycletime <- c(cycletime,0)

  # Now we start the simulation
  for (icycle in 1:ncycles) {
    truth[icycle,] <- proficiency
    observed <- do.call(evaluation,list(proficiency))
    obs[icycle,] <- observed
    estimate <- do.call(estimator,list(obs,est,act,suc))
    est[icycle,] <- estimate
    action <- do.call(policy,list(estimate))
    act[icycle,] <- action
    result <- do.call(advance,
                      list(proficiency, action,
                           time=cycletime[icycle])) 
    suc[icycle,] <- result$success
    
    proficiency <- result$proficiency

  }
  truth[ncycles+1,]<-proficiency
  observed <- do.call(evaluation,list(proficiency))
  obs[ncycles+1,] <- observed
  sim <- data.frame(truth,obs,est,act,suc,cycletime)
  nsim <- paste("true",vnames,sep=".")
  nsim <- c(nsim,paste("obs",vnames,sep="."))
  nsim <- c(nsim,paste("est",vnames,sep="."))
  nsim <- c(nsim,paste("act",vnames,sep="."))
  nsim <- c(nsim,paste("suc",vnames,sep="."))
  nsim <- c(nsim,"cycletime")
  names(sim) <- nsim
  sim
}

expFilter <- function (obs,est,act,suc,
                       alpha = filterAlpha,
                       xi = meanInnovation) {
  nobs <- sum(!is.na(obs[,1]))
  if (nobs == 1) return(obs[1,])
  result <- alpha*obs[nobs,] + (1-alpha)*(est[nobs-1,]+xi)
  result <- ifelse((result < 0),0,result)
  result <- ifelse((result > 6),6,result)
  result
}
  
## Generates an action from a proficiency estimate
## Simple policy which tries to match precisely with
## estimate  
floorPolicy <- function(estimate) {
  floor(2*estimate)/2
}

