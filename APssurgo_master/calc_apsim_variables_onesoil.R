source(paste0(codes_folder, '/n_policy_git/APssurgo_master/SaxtonRawls.R'))
# horizons_cell_dt <- horizons_cell_dt


calc_apsim_variables <- function(horizons_cell_dt, region_n){
    
  # packages_need <- c('dplyr', 'data.table')
  # lapply(packages_need, require, character.only = TRUE)
  
  soils_list <- list()
  mukey_seq <- sort(unique(horizons_cell_dt$mukey))
  for(mukey_n in mukey_seq){
    # mukey_n <- mukey_seq[2]
    horizon <- horizons_cell_dt[mukey == mukey_n,]
  
    # Calculate new variables -------------------------------------------------------------------
  
    # Soil physical properties
  
    horizon$bd <- ifelse(horizon$wetbd < 0.9, 0.9, ifelse(horizon$wetbd > 1.8, 1.8,horizon$wetbd))
    #horizon$bd <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$BD
    #horizon$ll.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$LL15
    #horizon$dul.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$DUL
  
    horizon$ksat <- pmin(horizon$ksat*100/1.157,SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$KSAT*24) # mm/day
    horizon[ksat >= 1000, ksat := 990] #get a warning if higher than 1000 (GM 08/31/2020)
    
    #horizon$ksat <- ifelse(horizon$ksat > 500, 500, horizon$ksat) # mm/day
  
    horizon$sat <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$SAT/100
    
    horizon$PO <- 1-horizon$bd/2.65 #cm3/cm3
    
    #------------------------------------------------------------------
    # Correct sat to avoid APSIM error
    
    horizon$sat_max <- round(1-horizon$bd/2.64,3) #gm 04/29/2019
    
    horizon$sat <- ifelse(horizon$sat > horizon$sat_max, horizon$sat_max, horizon$sat) #gm 04/29/2019
    #------------------------------------------------------------------

    horizon$Salb <- round(0.15,2) # Bare soil albedo
  
    horizon$MWCON <- 1 #(0-1)
  
    horizon$dul <- horizon$dul/100
  
    horizon$ll <- horizon$ll/100
  
    horizon$SWCON <- (horizon$PO-horizon$dul)/horizon$PO
    
    horizon$SWCON <- ifelse(horizon$SWCON < 0, 0, horizon$SWCON)# #gm 04/29/2019 SWCON=0 means that no water above DUL
  
    horizon$AirDry <- ifelse(horizon$center<=15,0.9,ifelse(horizon$center<=30,0.95,1))*horizon$ll
  
    horizon$U <- ifelse(horizon$clay<=20,5+0.175*horizon$clay,
                        ifelse(horizon$clay<=40,7.5+0.05*horizon$clay,
                               ifelse(horizon$clay<=50,11.5-0.05*horizon$clay,
                                      ifelse(horizon$clay<=70,12.75-0.075*horizon$clay,
                                             ifelse(horizon$clay<=80,11-0.05*horizon$clay,0))))) # mm
  
    horizon$cona <- ifelse(horizon$clay<=30,0.025*horizon$clay+3.25,
                           ifelse(horizon$clay<=50,4,
                                  ifelse(horizon$clay<=70,-0.025*horizon$clay+5.25,
                                         ifelse(horizon$clay<=80,3.5,0)))) # mm/d^5
  
    horizon$DiffusConst <- 40
  
    horizon$DiffusSlope <- 16
  
    horizon$CN2 <- ifelse(is.na(horizon$CN2),80,horizon$CN2)
  
    horizon$CNRed <- 20 #residue runoff
  
    horizon$CNCov	 <- 0.8
  
    horizon$EnrAcoeff	<- 7.4
  
    horizon$EnrBcoeff	<- 0.2
  
    limit_roots <- c(150,200,200)[region_n]
    
    horizon$XF_maize <- ifelse(horizon$bottom <= limit_roots,1,0.1)# ifelse(horizon$center<=150,1,0) Changed by gm on 2/28/2019
    
    horizon$KL_maize <-	ifelse(horizon$center<=20,0.08,0.09*exp(-0.007*horizon$center))
  
    horizon$e	<- 0.5  #ifelse(F4=$BC$3,0.07,IF(F4=$BC$4,0.03,0.05))
  
    # Soil chemical properties
  
    horizon$ph <- 0.52+1.06*horizon$ph #pH 1:5
  
    horizon$OC <- horizon$om/1.72 # %
  
    horizon$OC <- c(horizon$OC[1],
                    ifelse(horizon$center[-1] >= 100 & diff(horizon$OC) == 0,
                           horizon$OC[1]*exp(horizon$center[-1]*-0.035),
                           horizon$OC)) # exponential decay below 100 cm if data is missing
    horizon[OC==0, OC := 0.001]
  
  
    # horizon$FInert <- ifelse(horizon$center<=1,0.4,
    #                          ifelse(horizon$center<=10,0.4,
    #                                 ifelse(horizon$center<60,0.008*horizon$center+0.32,
    #                                        ifelse(horizon$center<=120,0.8,
    #                                               ifelse(horizon$center<180,0.0032*horizon$center+0.42,
    #                                                      ifelse(horizon$center<=300,0.99,0)))))) #(0-1)
    # 
    # horizon$FBiom <- ifelse(horizon$center<=10,0.04,
    #                         ifelse(horizon$center<=20,0.055-0.0015*horizon$center,
    #                                ifelse(horizon$center<=30,0.03-0.0005*horizon$center,
    #                                       ifelse(horizon$center<60,0.0216-0.0002*horizon$center,
    #                                              ifelse(horizon$center<=300,0.01,0))))) #(0-1)
  
    if(region_n == 1){ #South (bit less fbiom)
      # horizon$FBiom = c(0.12, 0.08, 0.04, 0.04, 0.03, 0.01,0.01, 0.01, 0.01,0.01)
      # horizon$FInert = c(0.4, 0.40, 0.420, 0.46, 0.56, 0.72, 0.80,0.80, 0.92, 0.98)
      horizon$FBiom = c(0.05, 0.04, 0.035, 0.025, 0.02, 0.01,0.005, 0.001, 0.001,0.001)
      horizon$FInert = c(0.5, 0.55, 0.6, 0.7, 0.8, 0.85, 0.9,0.95, 0.97, 0.99)
      
    }else if(region_n == 2){ #Central (more Fbiom)
      horizon$FBiom = c(0.08, 0.06, 0.055, 0.035, 0.015, 0.01,0.005, 0.005, 0.001,0.001)
      horizon$FInert = c(0.42, 0.45, 0.55, 0.6, 0.65, 0.7, 0.75,0.80, 0.92, 0.98)
      
    }else if(region_n == 3){ #North
      # horizon$FBiom = c(0.08, 0.06, 0.05, 0.03, 0.03, 0.01, 0.01, 0.005, 0.001, 0.001)
      # horizon$FInert = c(0.4, 0.55, 0.6, 0.7, 0.75, 0.8, 0.9,0.95, 0.98, 0.99)
      horizon$FBiom = c(0.11, 0.10, 0.09, 0.08, 0.06, 0.01, 0.01, 0.005, 0.001, 0.001)
      horizon$FInert = c(0.4, 0.45, 0.55, 0.6, 0.65, 0.7, 0.8,0.9, 0.95, 0.99)
    }
    
    
    
    horizon$RootCN <- 45
  
    horizon$SoilCN <- 13
  
    horizon$RootWt <- 1000
  
    horizon$sw <- horizon$dul
  
    # horizon$no3ppm <- horizon$OC
     
    # horizon$nh4ppm <- horizon$OC*0.5
    
    horizon$no3kgha <- 0
    horizon$nh4kgha <- 0
    
    soils_list[[which(mukey_seq == mukey_n)]] <- horizon
  
  }#end of mukey_n loop
  
  soils_dt <- rbindlist(soils_list)
  soils_dt[,Salb := round(Salb, 2)]
  soils_dt[,CNCov := round(CNCov, 2)]
  
  return(soils_dt)

}


