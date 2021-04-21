source(paste0(codes_folder, '/n_policy_git/APssurgo_master/latlong2county.R'))
library(maps)
library(maptools)

if(FALSE){
  data_soils = horizons_cell2_dt
  badge_name = 'soils_vr_value'
  path = directory
  crops = tolower(c("Maize","Soybean"))
}

make_apsoils_toolbox <- function(data_soils, path, badge_name, crops) {

  folder <- newXMLNode("folder", attrs = list(version="36",
                                              creator="Apsim 7.10",
                                              name= badge_name))

  soil_seq <- sort(as.numeric(unique(data_soils$mukey)))
  for(soil_n in soil_seq){
      # soil_n = soil_seq[2]
  
      data_soil <- data_soils[mukey == soil_n]
  
      print(paste0('/ mukey: ', soil_n))
  
      ## <Soil name="Default">
      # Soil <- newXMLNode("Soil", attrs = list(name = paste(tolower(soil_n))),
      #                    parent = folder)
      Soil <- newXMLNode("Soil", attrs = list(name = paste(tolower(soil_n))),
                                             parent = folder)
      
      ### <RecordNumber>0</RecordNumber>
      RecordNumber <- newXMLNode("RecordNumber",parent = Soil)
      xmlValue(RecordNumber) <-  which(soil_seq == soil_n)
  
      ### <SoilType>Nicollet</SoilType>
      SoilType <- newXMLNode("SoilType",parent = Soil)
      xmlValue(SoilType) <- soil_n
  
      ### <Region>Story</Region>
      # county <- capitalize(latlong2county(lat=mean(data_soil$Y), long=mean(data_soil$X)))
      county <- latlong2county(lat=mean(data_soil$Y), long=mean(data_soil$X))
      Region <- newXMLNode("Region",parent = Soil)
      xmlValue(Region) <- county[2]
  
      ### <State>Iowa</State>
      State <- newXMLNode("State",parent = Soil)
      xmlValue(State) <- county[1]
  
      ### <Country>US</Country>
      Country <- newXMLNode("Country",parent = Soil)
      xmlValue(Country) <- "USA"
  
      ### <ApsoilNumber>1</ApsoilNumber>
      ApsoilNumber <- newXMLNode("ApsoilNumber",parent = Soil)
      xmlValue(ApsoilNumber) <- 1
  
      ### <Latitude>0</Latitude>
      coords <- unique(data_soil[ ,.(X, Y)])
  
      Latitude <- newXMLNode("Latitude",parent = Soil)
      xmlValue(Latitude) <- round(coords$Y[1],2)
  
      ### <Longitude>-0</Longitude>
      Longitude <- newXMLNode("Longitude",parent = Soil)
      xmlValue(Longitude) <- round(coords$X[1],2)
  
      ### <YearOfSampling>0</YearOfSampling>
      YearOfSampling <- newXMLNode("YearOfSampling",parent = Soil)
      xmlValue(YearOfSampling) <- as.character(year(Sys.Date()))
  
      ### <DataSource>ssurgo2apsim</DataSource>
      DataSource <- newXMLNode("DataSource",parent = Soil)
      xmlValue(DataSource) <- "ssurgo2apsim"
  
      ### <Comments></Comments>
      Comments <- newXMLNode("Comments",parent = Soil)
      xmlValue(Comments) <- paste0("This soil was created with data from SSURGO Database, downloaded in R via the FedData package and parameters set using the approach described by Archontoulis et al. (2014, Agron. J. 106(3):1025-1040). This soil type represents ")
      ### <Water>
      Water <- newXMLNode("Water",parent = Soil)
  
      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = Water)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_soil$thick[j]
      }
  
      #### <BD>
      BD <- newXMLNode("BD",parent = Water)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = BD)
        xmlValue(double) <- round(data_soil$bd[j],2)
      }
  
      #### <AirDry>
      AirDry <- newXMLNode("AirDry",parent = Water)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = AirDry)
        xmlValue(double) <- round(data_soil$AirDry[j],2)
      }
  
      #### <LL15>
      LL15 <- newXMLNode("LL15",parent = Water)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = LL15)
        xmlValue(double) <- round(data_soil$ll[j],2)
      }
  
      #### <DUL>
      DUL <- newXMLNode("DUL",parent = Water)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = DUL)
        xmlValue(double) <- round(data_soil$dul[j],2)
      }
  
      #### <SAT>
      SAT <- newXMLNode("SAT",parent = Water)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = SAT)
        xmlValue(double) <- round(data_soil$sat[j],2)
      }
  
      #### <KS>
      KS <- newXMLNode("KS",parent = Water)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = KS)
        xmlValue(double) <- round(data_soil$ksat[j],2)
      }
  
      #### <SoilCrop>
      for(k in 1:length(crops)){
        SoilCrop <- newXMLNode("SoilCrop", attrs = list(name = crops[k]),parent = Water)
  
        ##### <Thickness>
        Thickness <- newXMLNode("Thickness",parent = SoilCrop)
        for(j in 1:length(data_soil$thick)){
          double <- newXMLNode("double",parent = Thickness)
          xmlValue(double) <- data_soil$thick[j]
        }
  
        ##### <LL>
        LL <- newXMLNode("LL",parent = SoilCrop)
        for(j in 1:length(data_soil$thick)){
          double <- newXMLNode("double",parent = LL)
          xmlValue(double) <- round(data_soil$ll[j],2)
        }
  
        ##### <KL>
        KL <- newXMLNode("KL",parent = SoilCrop)
        for(j in 1:length(data_soil$thick)){
          double <- newXMLNode("double",parent = KL)
          xmlValue(double) <- round(data_soil$KL_maize[j],3)
        }
  
        ##### <XF>
        XF <- newXMLNode("XF",parent = SoilCrop)
        for(j in 1:length(data_soil$thick)){
          double <- newXMLNode("double",parent = XF)
          xmlValue(double) <- round(data_soil$XF_maize[j],3)
        }
      }
  
      ### <SoilWater>
      SoilWater <- newXMLNode("SoilWater",parent = Soil)
  
      #### <SummerCona>
      SummerCona <- newXMLNode("SummerCona",parent = SoilWater)
      xmlValue(SummerCona) <-(round(data_soil$cona,2)[1])
  
      #### <SummerU>
      SummerU <- newXMLNode("SummerU",parent = SoilWater)
      xmlValue(SummerU) <- (round(data_soil$U,2)[1])
  
      #### <SummerDate>
      SummerDate <- newXMLNode("SummerDate",parent = SoilWater)
      xmlValue(SummerDate) <- "1-jun"
  
      #### <WinterCona>
      WinterCona <- newXMLNode("WinterCona",parent = SoilWater)
      xmlValue(WinterCona) <-as.character(round(data_soil$cona,2)[1])
  
      #### <WinterU>
      WinterU <- newXMLNode("WinterU",parent = SoilWater)
      xmlValue(WinterU) <- as.character(round(data_soil$U,2)[1])
  
      #### <WinterDate>
      WinterDate <- newXMLNode("WinterDate",parent = SoilWater)
      xmlValue(WinterDate) <- "1-nov"
  
      #### <DiffusConst>
      DiffusConst <- newXMLNode("DiffusConst",parent = SoilWater)
      xmlValue(DiffusConst) <-  mean(data_soil$DiffusConst)
  
      #### <DiffusSlope>
      DiffusSlope <- newXMLNode("DiffusSlope",parent = SoilWater)
      xmlValue(DiffusSlope) <-  mean(data_soil$DiffusSlope)
  
      #### <Salb>
      Salb <- newXMLNode("Salb",parent = SoilWater)
      xmlValue(Salb) <- mean(data_soil$Salb)
  
      #### <CN2Bare>
      CN2Bare <- newXMLNode("CN2Bare",parent = SoilWater)
      xmlValue(CN2Bare) <- (round(mean(data_soil$CN2),0))
  
      #### <CNRed>
      CNRed <- newXMLNode("CNRed",parent = SoilWater)
      xmlValue(CNRed) <- (mean(data_soil$CNRed))
  
      #### <CNCov>
      CNCov <- newXMLNode("CNCov",parent = SoilWater)
      xmlValue(CNCov) <- (mean(data_soil$CNCov))
  
      #### <Slope>NaN</Slope>
      #### <DischargeWidth>NaN</DischargeWidth>
      #### <CatchmentArea>NaN</CatchmentArea>
      #### <MaxPond>NaN</MaxPond>
  
      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = SoilWater)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_soil$thick[j]
      }
  
      #### <SWCON>
      SWCON <- newXMLNode("SWCON",parent = SoilWater)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = SWCON)
        xmlValue(double) <- round(data_soil$SWCON[j],3)
      }
  
  
      ### SoilOrganicMatter
      SoilOrganicMatter <- newXMLNode("SoilOrganicMatter",parent = Soil)
  
      #### <RootCN>
      RootCN <- newXMLNode("RootCN",parent = SoilOrganicMatter)
      xmlValue(RootCN) <- mean(round(data_soil$RootCN))
  
      #### <RootWt>
      RootWt <- newXMLNode("RootWt",parent = SoilOrganicMatter)
      xmlValue(RootWt) <- mean(round(data_soil$RootWt))
  
      #### <SoilCN>13</SoilCN>
      SoilCN <- newXMLNode("SoilCN",parent = SoilOrganicMatter)
      xmlValue(SoilCN) <- mean(round(data_soil$SoilCN))
  
      #### <EnrACoeff>
      EnrACoeff <- newXMLNode("EnrACoeff",parent = SoilOrganicMatter)
      xmlValue(EnrACoeff) <- mean(round(data_soil$EnrAcoeff,2))
  
      #### <EnrBCoeff>
      EnrBCoeff <- newXMLNode("EnrBCoeff",parent = SoilOrganicMatter)
      xmlValue(EnrBCoeff) <- mean(round(data_soil$EnrBcoeff,2))
  
      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = SoilOrganicMatter)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_soil$thick[j]
      }
  
      #### <OC>
      OC <- newXMLNode("OC",parent = SoilOrganicMatter)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = OC)
        xmlValue(double) <- round(data_soil$OC[j],2)
      }
  
      #### <FBiom>
      FBiom <- newXMLNode("FBiom",parent = SoilOrganicMatter)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = FBiom)
        xmlValue(double) <- round(data_soil$FBiom[j],4)
      }
  
      #### <FInert>
      FInert <- newXMLNode("FInert",parent = SoilOrganicMatter)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = FInert)
        xmlValue(double) <- round(data_soil$FInert[j],4)
      }
  
      #### <OCUnits>
      OCUnits <- newXMLNode("OCUnits",parent = SoilOrganicMatter)
      xmlValue(OCUnits) <- "Total"
  
      ### <Analysis>
      Analysis <- newXMLNode("Analysis",parent = Soil)
  
      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = Analysis)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_soil$thick[j]
      }
  
      #### <PH>
      PH <- newXMLNode("PH",parent = Analysis)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = PH)
        xmlValue(double) <- round(data_soil$ph[j],2)
      }
  
      #### <ParticleSizeSand>
      ParticleSizeSand <- newXMLNode("ParticleSizeSand",parent = Analysis)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = ParticleSizeSand)
        xmlValue(double) <- round(data_soil$sand[j],1)
      }
  
      #### <ParticleSizeClay>
      ParticleSizeClay <- newXMLNode("ParticleSizeClay",parent = Analysis)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = ParticleSizeClay)
        xmlValue(double) <- round(data_soil$clay[j],1)
      }
  
      #### <ParticleSizeSilt>
      ParticleSizeSilt <- newXMLNode("ParticleSizeSilt",parent = Analysis)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = ParticleSizeSilt)
        xmlValue(double) <- round(100 - data_soil$sand[j] - data_soil$clay[j],1)
      }
  
      ### Sample
      Sample <- newXMLNode("Sample",parent = Soil, attrs = list(name="Intial conditions"))
  
      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = Sample)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_soil$thick[j]
      }
  
      #### <NO3>
      NO3 <- newXMLNode("NO3",parent = Sample)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = NO3)
        xmlValue(double) <- round(data_soil$no3kgha[j],2) # same as OC but in ppm
      }
  
      #### <NH4>
      NH4 <- newXMLNode("NH4",parent = Sample)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = NH4)
        xmlValue(double) <- round(data_soil$nh4kgha[j],2) # same as 1/2 oc but ppm
      }
  
      #### <SW>
      SW <- newXMLNode("SW",parent = Sample)
      for(j in 1:length(data_soil$thick)){
        double <- newXMLNode("double",parent = SW)
        xmlValue(double) <- round(data_soil$sw[j],2)
      }
  
      #### <NO3Units>
      NO3Units <- newXMLNode("NO3Units",parent = Sample)
      xmlValue(NO3Units) <- "kgha"
  
      #### <NH4Units>
      NH4Units <- newXMLNode("NH4Units",parent = Sample)
      xmlValue(NH4Units) <- "kgha"
  
      #### <SWUnits>
      SWUnits <- newXMLNode("SWUnits",parent = Sample)
      xmlValue(SWUnits) <- "Volumetric"
      
      }#end of soil loop
  writeLines(saveXML(folder), paste0(path,'/',badge_name, ".soils"))
}




