evalA
evalSig
sigma

relEval <- function (sig) {
  sigX <- evalA %*% sig %*% t(evalA)
  diag(sig/(sigX+evalSig))
}
relEval1 <- function (sig) {
  sigX <- evalA %*% sig %*% t(evalA)
  sig/(sigX+evalSig)
}

relEvalX <- function (sig) {
  sigX <- evalA %*% sig %*% t(evalA)
  sig %*% solve(sigX+evalSig)
}


popVar <- popA %*% t(popA)
relEval(popVar)

for (t in c(0,1,2,5,10,20,50)) {
  cat("Time = ",t," r = ",relEval(popVar+t*diag(sigma^2)),"\n")
}
