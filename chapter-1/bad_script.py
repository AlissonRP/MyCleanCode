

def logvero(Par):
         y = dados[,1]
         X = dados[,2]
         taux = dados[,3]
         tauy = dados[,4]
         alpha = Par[1]
         beta = Par[2]
         mu = Par[3]
         sigmax = Par[4]
         sigma = Par[5]
         n = length(y)
         Sigma= matrix(c(sigmax,beta*sigmax,beta*sigmax,sigma+beta^2*sigmax),2,2)
         TT <- matrix(rep(c(Sigma),n),n,byrow=T)+cbind(taux,0,0,tauy)
         sol1 <- function(f){ t(matrix(c(X[f]-mu,y[f]-alpha-beta*mu)))%*%solve(matrix(TT[f,],2),tol=1e-10000)%*%matrix(c(X[f]-mu,y[f]-alpha-beta*mu))}
         det1 <- function(f){ det(matrix(TT[f,],2))}
         TT1 <- sapply(1:n, sol1)
         TT2 <- sapply(1:n, det1)
         return(1/2*sum(log(TT2)) + 1/2*sum(TT1))


