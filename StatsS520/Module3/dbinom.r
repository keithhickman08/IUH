# Keith Hickman's R function to compute the Binomial PMF
dbinom.kh <- function(x, n, p){
  if (x < 0 | x > n){
	return(0)
  }else if (x == 0){
    return(pbinom(x, n, p))
  }else 
    return(pbinom(x, n, p) - pbinom(x-1, n, p))
}