# Physical process-based model (PPBM)
# Data is year, month, day, mean temperature, sum precipitation, mean global irradiation, 
# what do we want? Mean direct radiation, mean diffuse radiation, sun hours? to account for snowmelt? 
# Should factors affecting evapotranspiration in summer be eventually included? such as radiation to estimate amount of 
# evporation and transpiration by plants?

ppbm = function(df, id = df$id, temp = df$Tday, P = df$RRday, date = df$date){
  Scf = 0.7 # Snowfall correction factor
  Tt = 0 # Threshold temperature
  Cm = 2.74 # Degree-day factor
  Cfr = 0.05 # Refreezing coefficient (default)
  Cwh = 0.1 # Water holding capacity (default)
  rf = 0.0006 # radiation factor for snow
  mf = 1.8 # melt factor
  
  # variables
  ppbm <- data.frame(r = matrix(0, nrow=length(solar$Date)))
  ppbm$q[1] = 0 # snowmelt discharge (liquid water leaving the snowpack), output
  ppbm$ss[1] = 0 # Snow storage 0 mm day 1
  ppbm$liq[1] = 0 #liquid water in the snowpack 0 mm day 1 
  ppbm$r[1] = 0 # Freezing/refreezing 
  ppbm$m[1] = 0 # melting (still in snowpack)
  ppbm$prec[1] = 0 # simulated precipitation
  
  for(t in 2:length(temp)){
    if (temp[t] < Tt){ # precipitation as snow, liquid water in snow freezes
      ppbm$r[t] = Cfr * Cm * (Tt-temp[t])
      
      if (ppbm$r[t] > ppbm$liq[t-1]){
        ppbm$r[t] = ppbm$liq[t-1]
      }
      
      ppbm$ss[t] = P[t] * Scf + ppbm$ss[t-1] + ppbm$r[t]
      ppbm$liq[t] = ppbm$liq[t-1] - ppbm$r[t]
      
      ppbm$prec[t] = P[t] * Scf
      
      if(ppbm$ss[t-1] > 0){
        albedo = 0.05
      }
      if(ppbm$m > 0){
        albedo = 0.25
      }
      
    }
    
    else {
      ppbm$m[t] = (mf * temp[t] - Tt) + rf * solar$Irradiance[t] * albedo # I haven't processed global 
      #radiance from Finland yet (it's in the read_precip_netcdf R script)
      
      if(ppbm$m[t] > ppbm$ss[t-1]){
        ppbm$m[t] = ppbm$ss[t-1]
      }
      
      ppbm$ss[t] = ppbm$ss[t-1] - ppbm$m[t]
      ppbm$liq[t] = ppbm$liq[t-1] + P[t] + ppbm$m[t]
      
      if(Cwh*ppbm$ss[t] < ppbm$liq[t]){
        ppbm$q[t] = ppbm$liq[t] - Cwh * ppbm$ss[t]
        ppbm$liq[t] = Cwh * ppbm$ss[t]
      }
      
      ppbm$prec[t] = P[t]
    }
  }
  ppbm$snow_total = ppbm$ss + ppbm$liq
  ppbm$date = date
  ppbm$id = id
  ppbm$month = month(ppbm$Date)

  return(ppbm)
  }