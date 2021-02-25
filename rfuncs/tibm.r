tibm = function(df, id = df$id, temp = df$Tday, 
                P, # = df$RRday, 
                date = df$date){
  Scf = 0.7 # Snowfall correction factor
  Tt = 0 # Threshold temperature
  Cm = 2.74 # Degree-day factor
  Cfr = 0.05 # Refreezing coefficient (default)
  Cwh = 0.1 # Water holding capacity (default)
  rf = 0.0006 # radiation factor for snow
  mf = 1.8 # melt factor
  
  tibm <- data.frame(r = matrix(0, nrow=length(temp))) #, ncol = 6))
  tibm$date = date
  tibm$id = id
  tibm$ss[1] = 0 # Snow storage 0 mm day 1
  tibm$liq[1] = 0 #liquid water in the snowpack 0 mm day 1 
  tibm$m[1] = 0 # melting (still in snowpack)
  tibm$q[1] = 0 # snowmelt discharge (liquid water leaving the snowpack), output
 
 
  for (i in 2:length(temp)){ 
    if(temp[i] < Tt){ # precipitation as snow
      tibm$r[i] = Cfr * Cm * (Tt - temp[i])
      if(tibm$r[i] > tibm$liq[i-1]){
        tibm$r[i] = tibm$liq[i-1] 
      }
      
      tibm$ss[i] = P[i] * Scf + tibm$ss[i-1] + tibm$r[i]
      tibm$liq[i] = tibm$liq[i-1] - tibm$r[i] 
      tibm$prec[i] = P[i] * Scf
    }
    
    else{ # melting occurs
      tibm$r[i] = 0 # there is no refreezing if the snow melts
      tibm$m[i] = Cm*(temp[t] - Tt)
      
      if(tibm$m[i] > tibm$ss[t-1]){
        tibm$m[i] = tibm$ss[t-1] 
      }
      
      tibm$ss[i] = tibm$ss[i-1] - tibm$m[i]
      tibm$liq[i] = tibm$liq[i-1] + P[i] + tibm$m[i]
      
      if(Cwh*tibm$ss[i] < tibm$liq[i]){
        tibm$q[i] = tibm$liq[t] - Cwh * tibm$ss[i]
        tibm$liq[i] = Cwh * tibm$ss[i]
      }
      
      tibm$prec[i] = P[i]
    }
    
  }
  
  tibm$snow_total = tibm$ss + tibm$liq

  return(tibm)
}