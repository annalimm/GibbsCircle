library(hexbin)
library(RColorBrewer)
library(ggplot2)
library(ggExtra)


rcircle = function(r=1){
  r = r * sqrt(runif(1))
  theta = runif(1) * 2 * pi
  
  x = r * cos(theta)
  y = r * sin(theta)
  return(c(x,y))
}


start <- function(N_points, r, d){
  start_samp <- matrix(-r*100, N_points, 2) #Generate points far away from center
  
  A = rcircle(r)
  x1 <- A[1]
  y1 <- A[2]
  i = 1
  j = 1
  k = 0 #flag for searching the right second point
  c=0
  
  for (i in 1:N_points){
    #the second point's generation
    while (k == 0){
      A = rcircle(r)
      x2 <- A[1]
      y2 <- A[2]
    
      c = c+1
      #If we cant generate point for 10 000 times - break
      if (c == 10000){ 
        warning("Generation timed out")
        return()
      }
      
      #the distance between current point and all that we've already taken
      for (j in 1:N_points){
        x_sq = (start_samp[j,1] - x2)^2
        y_sq = (start_samp[j,2] - y2)^2
        distance = sqrt(x_sq + y_sq)
        
        if (distance < d){
          k = 0
          break
        }
        #if we didn't get into the previous 'if' conditions => our 2nd point is correct and then flag k = 1, 
        #if we got into this conditions then 'break' operator will stop it and there will be selected new x2, y2.
        k = 1
      }
    }
    start_samp[c(i), 1] <- x2
    start_samp[c(i), 2] <- y2
    k = 0
  }
  return(start_samp)
}


sampling <- function(start_samp, r, d, N_samling = 100, silent = T){
  j = 1
  k = 0
  m = 1
  c=0
  
  N_points = NROW(start_samp)
  if (!silent)
    pb = txtProgressBar(min = 0, max = N_samling, style = 3) #fashion progressbar
  for (m in 1:N_samling){
    if (!silent)
      setTxtProgressBar(pb,m)
    
     current = sample(1:N_points, 1) #random index

    #the second point's ganeration
    while (k == 0){
      
      A = rcircle(r)
      xi <- A[1]
      yi <- A[2]
     
      #the distance between current point and all that we've already taken
      for (j in 1:N_points){
        if ( j != current){
         
          x_sq = (start_samp[j,1] - xi)^2
          y_sq = (start_samp[j,2] - yi)^2
          distance = sqrt(x_sq + y_sq)
          
          if ( distance < d){
            k = 0
            break
          }
          k = 1
        }
      }
    }
    start_samp[current, 1] = xi
    start_samp[current, 2] = yi
    k = 0
  }
  if (!silent)
    close(pb)
  
  return(start_samp)
}


#D = 0.9
#R = 1
#N_POINTS = 2
#N_SAMPLING = 1e4
N_POINTS = 200
R = 1
D = 0.1
N_SAMPLING = 100

  A = start(N_points = N_POINTS, r = R, d = D)
  plot(A[,1], A[,2], pch=16, ylim=c(-1,1), xlim=c(-1,1))

  A_resampled= sampling(A, r = R, d = D, N_samling = N_SAMPLING)
  
#------------------------ 
  plot(A_resampled[,1], A_resampled[,2], pch=16)
  library(ggplot2)
  library(ggExtra)
  
  d1 <- as.data.frame(A_resampled, stringsAsFactors=FALSE)
  x <- d1[,1]
  y <- d1[,2]
  
  p <- ggplot(d1, aes(x=x, y=y)) +
    geom_point(fill="slateblue",col="#5499c7") +
    theme(legend.position="none")
  p
  # with marginal histogram
  p1 <- ggMarginal(p, type="histogram", fill = "#d35400",col = "#5499c7", size = 3, xparams = list(  bins=20), yparams = list(  bins=20))
  p1
#------------------
  STOP = 1e4
  #D = 1
  #R = 1
  #N = 2
  #Nsamp = 100
  N = 200
  R = 1
  D = 0.1
  Nsamp = 100
  
  nosamp_matr =start(N_points = N_POINTS, r = R, d = D)
  samp_matr = sampling(A,r = R, d = D, N_samling = Nsamp)
  
  PB = txtProgressBar(min = 0, max = STOP, style = 3)
  
  for (i in 1:(STOP - 1)){
    setTxtProgressBar(PB, i)
    
    A = start(N_points = N, r = R, d = D)
    nosamp_matr = cbind(nosamp_matr, A)
    B = sampling(A,r = R, d = D, N_samling = Nsamp)
    samp_matr = cbind(samp_matr, B)
  }
  
  fir = matrix(samp_matr[1,], byrow = T, ncol = 2)
  sec = matrix(samp_matr[2,], byrow = T, ncol = 2)
  
  fir_NO = matrix(nosamp_matr[1,], byrow = T, ncol = 2)
  sec_NO = matrix(nosamp_matr[2,], byrow = T, ncol = 2)
  
  
  dist1 = data.frame(abs(fir[,1]^2+fir[,2]^2))
  dist2 = data.frame(abs(sec[,1]^2+sec[,2]^2))
  names(dist1) = "dist"
  names(dist2) = "dist"
  dist1$name = "dist1"
  dist2$name = "dist2"
      
  df = rbind(dist1, dist2)
  names(df) = c("dist", "sample")
  ggplot(df, aes(dist, fill = sample)) + geom_histogram(color="#e9ecef",alpha = 0.6, position = "identity")+scale_fill_manual(values=c("#d35400", "#5499c7"))
  ggplot(df, aes(dist, fill = sample)) + geom_density(color="#e9ecef",alpha = 0.6)+scale_fill_manual(values=c("#d35400", "#5499c7"))


  
  dist1_NO = data.frame(abs(fir_NO[,1]^2+fir_NO[,2]^2))
  dist2_NO = data.frame(abs(sec_NO[,1]^2+sec_NO[,2]^2))
  names(dist1_NO) = "dist"
  names(dist2_NO) = "dist"
  dist1_NO$name = "dist1"
  dist2_NO$name = "dist2"
  
  df = rbind(dist1_NO, dist2_NO)
  names(df) = c("dist", "sample")
  ggplot(df, aes(dist, fill = sample)) + geom_histogram(color="#e9ecef",alpha = 0.6, position = "identity")+scale_fill_manual(values=c("#d35400", "#5499c7"))
  ggplot(df, aes(dist, fill = sample)) + geom_density(color="#e9ecef",alpha = 0.6)+scale_fill_manual(values=c("#d35400", "#5499c7"))
  
  
    