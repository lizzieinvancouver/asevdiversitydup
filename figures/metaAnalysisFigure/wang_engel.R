
# Wang&Engel function
wang <- function(x, Tmin, Topt, Tmax){
  if((x <= Tmax) & (Tmax > (Tmin + 1)) & (Topt > (Tmin + 0.5)) & (Topt < (Tmax - 0.5)) & (x > Tmin)){
    alpha <-  log(2) / log((Tmax - Tmin) / (Topt - Tmin))
    
    num <- 2 * (x - Tmin)^alpha * (Topt - Tmin)^alpha - (x - Tmin)^(2 * alpha)
    den <- (Topt - Tmin)^(2*alpha)
    
    return(num/den)
  }else{
    return(0)
  }
}

# Parameters
Tmin <- -20
Tmax <- 30
Topt <- 20

x <- seq(-40,40,1) 
y <- sapply(x, wang, Tmin = Tmin, Topt = Topt, Tmax = Tmax)
plot(y~x)
lines(y~x)
