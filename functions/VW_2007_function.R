VW_2007 <- function(x, alpha=.88, beta=.88, lambda=2.25, gain = T){
  if(gain){ #gain
    u = ((1+x)^alpha-1)/alpha  
  }
  else{ #x<0
    u = (-lambda)*((1-x)^beta-1)/beta
  }
  return(u)
}

curve(VW_2007(x, alpha = .70, beta=.88, lambda = 2.25),
      from =0, to =10, ylim = c(0,20), lwd =2)

curve(-VW_2007(x, alpha = .70, beta=.88, lambda = 2.25, gain = F),
      from =0, to =10, add = TRUE, col = "red", lwd =2)

curve(VW_2007(x, alpha = 1.5, beta=.88, lambda = 2.25),
      from =0, to =10, ylim = c(0,20), lwd =2, add = T, lty=2)
curve(x^.88,
      from =0, to =10, add = TRUE, col = "darkcyan", lwd =2)
KT_power <- function(x, alpha, beta, lambda){
  if(x==0){
    u = 0
  }
  else if(x>0){ #gain
    u = ((1+x)^alpha-1)/alpha  
  }
  else{ #x<0
    u = (-lambda)*((1+x)^beta-1)/beta
  }
}

library(dplyr)
c(VW_2007(x = c(-10:0), gain = FALSE), VW_2007(x = c(1:10))) %>% 
  plot(type = "b")  
c(VW_2007(x = c(-10:0)*2, gain = FALSE), VW_2007(x = c(1:10)*2)) %>% 
  plot(type = "b")  


c(1:10)^.8 
c(1:10)^.8

  
  
.a =  function(x, alpha=.88){
  return(((1+x)^alpha-1)/alpha) 
}

(.a(1, .7)/.a(1, .9))/(.a(1/5, .7)/.a(1/5, .9))





