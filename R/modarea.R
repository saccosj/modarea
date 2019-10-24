modarea <- function(x, y, m, xSD, mSD, K) {

  x = scale (x)
  y = scale (y)
  m = scale (m)

  linearMod <- lm(y ~ x + m + x:m)

  b0      =  summary(linearMod)$coefficients[1,1]
  b1      =  summary(linearMod)$coefficients[2,1]
  b2      =  summary(linearMod)$coefficients[3,1]
  b3      =  summary(linearMod)$coefficients[4,1]
  if (abs(b3) >= abs(b2)) {
    areaU <- abs(b3*xSD^2*mSD +((b2^2*mSD)/b3))
    areaS <- abs((b3*xSD^2*mSD +((b2^2*mSD)/b3))/(xSD^2*mSD^2))
  }
  else if (abs(b2) > abs(b3)){
    areaU <- abs((2*b3*xSD*mSD)*(2*xSD))
    areaS <- abs(((2*b3*xSD*mSD)*(2*xSD))/(xSD^2*mSD^2))
  }
  j=0
  areaU_boot = rnorm(K,0,0)
  areaS_boot = rnorm(K,0,0)


  for (j in seq(from=1, to=K, by=1)) {
    x = sample(x, length(x), replace=TRUE)
    y = sample(y, length(y), replace=TRUE)
    m = sample(m, length(m), replace=TRUE)

    tryCatch({
      linearMod <- lm(y ~ x + m + x:m)}, error=function(e){})
    tryCatch({
      suppressWarnings({b0t      =  summary(linearMod)$coefficients[1,1]
      b1t      =  summary(linearMod)$coefficients[2,1]
      b2t      =  summary(linearMod)$coefficients[3,1]
      b3t      =  summary(linearMod)$coefficients[4,1]})
      if (abs(b3t) >= abs(b2t)) {
        areaU_boot [j] <- abs(b3t*xSD^2*mSD +((b2t^2*mSD)/b3t))
        areaS_boot [j] <- abs((b3t*xSD^2*mSD +((b2t^2*mSD)/b3t))/(xSD^2*mSD^2))
      }
      else if (abs(b2t) > abs(b3t)) {
        areaU_boot [j] <- abs((2*b3t*xSD*mSD)*(2*xSD))
        areaS_boot [j] <- abs(((2*b3t*xSD*mSD)*(2*xSD))/(xSD^2*mSD^2))
      }
      }, error=function(e){})
    j= j+1
  }
  print("---------------------------------------------------",quote=FALSE)
  print("", quote=FALSE)
  print("Moderation Area Ver 0.0.2",quote=FALSE)
  print("Created by: Shane J Sacco, MA. University of Connecticut",quote=FALSE)
  print("",quote=FALSE)
  print("Multiple Regression:",quote=FALSE)
  print(paste("b0: ", format(round(b0,digits=4),nsmall=3)),quote=FALSE)
  print(paste("b1: ", format(round(b1,digits=4),nsmall=3)),quote=FALSE)
  print(paste("b2: ", format(round(b2,digits=4),nsmall=3)),quote=FALSE)
  print(paste("b3: ", format(round(b3,digits=4),nsmall=3)),quote=FALSE)
  print("",quote=FALSE)
  print("Area:",quote=FALSE)
  print(paste("Unstandardized: ", round(areaU,digits=4)),quote=FALSE)
  print(paste("Standardized:   ", round(areaS,digits=4)),quote=FALSE)
  print(paste("p-value<0.05:   ", format(round(mean(abs(areaS) < abs(areaS_boot)),digits=4),nsmall=4)),quote=FALSE)
  print("",quote=FALSE)
  print("---------------------------------------------------",quote=FALSE)
}
