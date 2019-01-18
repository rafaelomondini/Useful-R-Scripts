typeII.mean <- function(mu0,muA,sigma,n,alpha,test="two.sided",ITERATIONS=10000){
    tests <- rep(NA,ITERATIONS)
    for(i in 1:ITERATIONS){
      temporary.sample <- rnorm(n=n,mean=muA,sd=sigma)
      temporary.mean <- mean(temporary.sample)
      temporary.sd <- sd(temporary.sample)
      tests[i] <- (temporary.mean-mu0)/(temporary.sd/sqrt(n))
    }
    if(test=="greater"){
      return(mean(pt(tests,df=n-1,lower.tail = F)>=alpha))
    } else if(test=="less"){
      return(mean(pt(tests,df=n-1)>=alpha))
    } else if(test=="two.sided"){
      results <- pt(tests,df=n-1)
      results[tests>0] <- 1-results[tests>0]
      return(mean(results>=alpha/2))
    } else {
      stop("test argument must be passed as either \"less\", \"greater\" or \"two.sided\".")
    }
}