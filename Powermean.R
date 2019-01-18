power.mean <- function(nvec,...){
    nlen <- length(nvec)
    result <- rep(NA,nlen)
    pbar <- txtProgressBar(min = 0,max = nlen,style = 3)
    for(i in 1:nlen){
        result[i] <- 1 - typeII.mean(n=nvec[i],...)
        setTxtProgressBar(pbar,i)
    }
    close(pbar)
    return(result)
}