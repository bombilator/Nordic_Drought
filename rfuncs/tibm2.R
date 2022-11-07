# I'm not using the mf, rf in this version

tibm2 = function(df, id = df$id, 
                 temp = df$Tday, 
                P = df$RRday, 
                date = df$date){
  pb <- txtProgressBar(min = 0, max = length(date), style = 3)
  Scf = 0.7 # Snowfall correction factor
  Tt = 1 # Threshold temperature
  Cm = 2.74 # Degree-day factor
  Cfr = 0.05 # Refreezing coefficient (default)
  Cwh = 0.1 # Water holding capacity (default)
  rf = 0.0006 # radiation factor for snow
  mf = 1.8 # melt factor
  

  

  tibm <- data.frame(id = matrix(id, nrow = length(date)))
  tibm$date = date
  tibm$A[1] = 0 # Snow storage 0 mm day 1
  tibm$Sliq[1] = 0 #liquid water in the snowpack 0 mm day 1
  tibm$M[1] = 0 # melting (still in snowpack)
  tibm$Q[1] = 0 # snowmelt discharge (liquid water leaving the snowpack) + liquid precipitation, output
  tibm$r[1] = 0 # refreezing of liquid water still in the snowpack
  
  tibm$PR[1] = 0 #only rain (not snow)
  tibm$OSliq[1] = 0 # only snow part
  tibm$OSQ[1] = 0 # only snow part
  tibm$P_snow[1] = 0 # precipitation which is snow
  
  for (i in 2:length(temp)){
    
    if(temp[i] < Tt){ # precipitation as snow
      
      tibm$r[i] = tibm$Sliq[i-1]
      tibm$P_snow[i] = P[i]
      
      if(tibm$r[i] > (Cfr * Cm * (Tt - temp[i]))){
        tibm$r[i] = as.numeric(
          (Cfr * Cm * (Tt - temp[i])),
          2)
      }
      
      
      tibm$A[i] = as.numeric(
        (tibm$A[i-1] + P[i]* Scf + tibm$r[i]),
        2)
      
      tibm$Sliq[i] = as.numeric(
        (tibm$Sliq[i-1] - tibm$r[i] ),
        2)
      }
    
    
    else{ # melting occurs
      tibm$r[i] = 0 # there is no refreezing if the snow melts
      
      tibm$M[i] = as.numeric(
        tibm$A[i-1],
        2)
      if(tibm$M[i] > Cm*(temp[i] - Tt)){
        tibm$M[i] = as.numeric(
          (Cm*(temp[i] - Tt)),
          2)
      }
      
      tibm$A[i] = as.numeric(
        (tibm$A[i-1] - tibm$M[i]),
        2)
      
      tibm$Sliq[i] = as.numeric(
        (tibm$Sliq[i-1] + P[i] + tibm$M[i]),
        2)
      tibm$PR[i] = as.numeric(P[i], 2)
      tibm$OSliq[i] = as.numeric(
        (tibm$Sliq[i-1] + tibm$M[i]),
        2)

      if(tibm$Sliq[i] > Cwh * tibm$A[i]){
        tibm$Q[i] = as.numeric(
          (tibm$Sliq[i] - Cwh * tibm$A[i]),
          2)
        
        tibm$OSQ[i] = as.numeric(
          (tibm$OSliq[i] - Cwh * tibm$A[i]),
          2)
        
        tibm$Sliq[i] = as.numeric(
          (Cwh * tibm$A[i]),
          2)
        tibm$OSliq[i] = as.numeric(
          (Cwh * tibm$A[i]),
          2)
      } 
    }
    
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
    
  }
  
  tibm$snow_total = as.numeric(
    (tibm$A + tibm$Sliq),
    2)
  
  # output <- left_join(df, tibm, by=c("id", "date"))
  
  close(pb)
  
  return(tibm) #output)
}