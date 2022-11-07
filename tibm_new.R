# I'm not using the mf, rf in this version

tibm2 = function(df, id = df$id, 
                 temp = df$Tday, 
                 P = df$RRday, 
                 date = df$date){

  Scf = 0.7 # Snowfall correction factor
  Tt = 1 # Threshold temperature
  Cm = 2.74 # Degree-day factor
  Cfr = 0.05 # Refreezing coefficient (default)
  Cwh = 0.1 # Water holding capacity (default)
  rf = 0.0006 # radiation factor for snow
  mf = 1.8 # melt factor

  
  ### make lists instead and save/output the relevant list at the end ###
  
  # tibm <- data.frame(id = matrix(id, nrow = length(date)))
  # date = date
  A = vector(mode='list', length=length(temp)) # Snow storage 0 mm day 1
  A[1:length(A)] = 0
  Sliq = vector(mode='list', length=length(temp)) #liquid water in the snowpack 0 mm day 1
  Sliq[1:length(Sliq)] = 0
  M = vector(mode='list', length=length(temp)) # melting (still in snowpack)
  M[1:length(M)] = 0
  Q = vector(mode='list', length=length(temp)) # snowmelt discharge (liquid water leaving the snowpack) + liquid precipitation, output
  Q[1:length(Q)] = 0
  r = vector(mode='list', length=length(temp)) # refreezing of liquid water still in the snowpack
  r[1:length(r)] = 0
  # PR[1] = 0 #only rain (not snow)
  # OSliq[1] = 0 # only snow part
  # OSQ[1] = 0 # only snow part
  # P_snow[1] = 0 # precipitation which is snow
  
  for (i in 2:length(temp)){
    print(round(i/length(temp)*100, 2))
    
    if(temp[[i]] < Tt){ # precipitation as snow

      r[[i]] = Sliq[[i-1]]
      # P_snow[[i]] = P[[i]]
      
      
      if(r[[i]] > (Cfr * Cm * (Tt - temp[[i]]))){
        r[[i]] = Cfr * Cm * (Tt - temp[[i]])
        
      }
      
      
      A[[i]] = A[[i-1]] + (P[[i]]* Scf) + r[[i]]

      Sliq[[i]] = Sliq[[i-1]] - r[[i]]
    }
    
    
    else{ # melting occurs
     # r[[i]] = 0 # there is no refreezing if the snow melts

     M[[i]] = A[[i-1]]

     if(M[[i]] > (Cm*(temp[[i]] - Tt))){
       M[[i]] = Cm*(temp[[i]] - Tt)

     }
     A[[i]] = A[[i-1]] - M[[i]]


     Sliq[[i]] = Sliq[[i-1]] + P[[i]] + M[[i]]
     # PR[[i]] = P[[i]]
     # OSliq[[i]] =
     #   (Sliq[[i-1]] + M[[i]]),
     #

     if(Sliq[[i]] > (Cwh * A[[i]])){
       Q[[i]] = Sliq[[i]] - (Cwh * A[[i]])
       # OSQ[[i]] =
       #   (OSliq[[i]] - Cwh * A[[i]]),
       #

       Sliq[[i]] = Cwh * A[[i]]

       # OSliq[[i]] =
       #   (Cwh * A[[i]]),
       #
     }
    }

  }
  
  # snow_total = 
    # (A + Sliq),
    #
  
  output <- cbind(id, Q, M)
  output <- cbind.data.frame(output, date)
  
  # close(pb)
  
  return(output)
}