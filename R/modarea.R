modarea <- function(x, y, m, xSD, mSD, K) {

  x = scale (x)
  y = scale (y)
  m = scale (m)

  linearMod <- lm(y ~ x + m + x*m)

  b0      =  summary(linearMod)$coefficients[1,1]
  b1      =  summary(linearMod)$coefficients[2,1]
  b2      =  summary(linearMod)$coefficients[3,1]
  b3      =  summary(linearMod)$coefficients[4,1]
  areaU <- b3*xSD^2*mSD +((b2^2*mSD)/b3)
  areaS <- (b3*xSD^2*mSD +((b2^2*mSD)/b3))/(xSD^2*mSD^2)

  j=0
  areaU_boot = rnorm(K,0,0)
  areaS_boot = rnorm(K,0,0)

  for (j in seq(from=1, to=K, by=1)) {
    x = sample(x, length(x), replace=TRUE)
    y = sample(y, length(y), replace=TRUE)
    m = sample(m, length(m), replace=TRUE)
    tryCatch({
      linearMod <- lm(y ~ x + m + x*m)}, error=function(e){})
    tryCatch({
      b0t      =  summary(linearMod)$coefficients[1,1]
      b1t      =  summary(linearMod)$coefficients[2,1]
      b2t      =  summary(linearMod)$coefficients[3,1]
      b3t      =  summary(linearMod)$coefficients[4,1]
      areaU_boot [j] = b3t*xSD^2*mSD +((b2t^2*mSD)/b3t)
      areaS_boot [j] = (b3t*xSD^2*mSD +((b2t^2*mSD)/b3t))/(xSD^2*mSD^2)
      }, error=function(e){})
    j= j+1
  }
  print("---------------------------------------------------",quote=FALSE)
  print("", quote=FALSE)
  print("Moderation Area Ver 0.0.1",quote=FALSE)
  print("Created by: Shane J Sacco, MA. University of Connecticut",quote=FALSE)
  print("",quote=FALSE)
  print("Multiple Regression:",quote=FALSE)
  print(paste("b0: ", round(b0,digits=4)),quote=FALSE)
  print(paste("b1: ", round(b1,digits=4)),quote=FALSE)
  print(paste("b2: ", round(b2,digits=4)),quote=FALSE)
  print(paste("b3: ", round(b3,digits=4)),quote=FALSE)
  print("",quote=FALSE)
  print("Area:",quote=FALSE)
  print(paste("Unstandardized: ", round(areaU,digits=4)),quote=FALSE)
  print(paste("Standardized:   ", round(areaS,digits=4)),quote=FALSE)
  print(paste("p-value<0.05:   ", round(mean(abs(areaU) < abs(areaU_boot)),digits=4)),quote=FALSE)
  print("",quote=FALSE)
  print("---------------------------------------------------",quote=FALSE)
}

