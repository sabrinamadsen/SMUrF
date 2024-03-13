#' subroutine to estimate hourly Reco following VPRM (Gourdji et al. 2022) 
#' combined with methods from UrbanVPRM (Winbourne et al. 2021)
#' @author Sabrina Madsen, 05/03/2022 

library("data.table")
library("raster")

VPRM_LCs = c("ENF", "DBF", "MXF", "SHB", "SVN", "CRP", "CRN", "GRS", "WET", "OTH", "URB")
## Optimization sites
# ENF: NOBS (boreal), NIWOT (montane coniferous), METOLIUS (ponderosa pine)
# ENF: (tropical): DONALDSON
# DBF: HARVARD or Duke
# MXF: HOWLAND
# SHB: Lucky Hills
# SVN: TONZI
# SOY: MEAD-S2
# CRP: MEAD (USED CORN)
# GRS: VAIRA
# WET: PEATLAND
# OTH: 0 fluxes (here used for water)
# URB: Urban

### Model's parameters from Gourdji et al. 2021
VPRM_DBF= c(0, 45, 23, -15, 0.55, -0.1023, 539, 0.12, 0.065, 0.0024, 4.61, 0.116, -0.0005, 0.0009)
VPRM_MXF= c(0, 45, 18, 1, 0.05, -0.1097, 506, 0.47, 0.088, 0.0047, 1.39, -0.530, 0.2063,-0.0054)
VPRM_URB= c(0, 45, 20, 11, 0.1, -0.1273, 673, -6.18, 0.853, -0.0250, 5.19, 1.749, -0.2829, 0.0166) #Used dev-open from Gourdji et al. 2021
VPRM_SHB= c(0, 45, 17, 5, 0.1, -0.0996, 811, 1.53, 0.004, 0.0049, 0.09, -1.787, 0.4537, -0.0138)

VPRM_CRP= c(0, 45, 26, 7, 0.05, -0.0732, 1019, -1.2, 0.234, -0.006, 3.85, 0.032, -0.0429, 0.0090) #Used Crops, other from Gourdji
VPRM_CORN= c(0, 45, 35, -1, 0, -0.0997, 1829, -0.02, 0.083, -0.0018, 4.89, 0.150, -0.1324, 0.0156) 
VPRM_GRS= c(0, 45, 20, 11, 0.1, -0.1273, 673, -6.18, 0.853, -0.0250, 5.19, 1.749, -0.2829, 0.0166)
VPRM_SVN= c(0, 45, 17, 5, 0.1, -0.0996, 811, 1.53, 0.004, 0.0049, 0.09, -1.787, 0.4537, -0.0138)
VPRM_WET= c(0, 45, 29, 6, 0.1, -0.1227, 456, -0.82, 0.261, -0.0051, 3.46, -0.777, 0.0990, 0.0018)

VPRM_ENF= c(0, 45, 18, 1, 0.05, -0.1097, 506, 0.47, 0.088, 0.0047, 1.39, -0.530, 0.2063, -0.0054)
VPRM_OTH= c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

VPRM_param = rbind(VPRM_ENF, VPRM_DBF, VPRM_MXF, VPRM_SHB, VPRM_SVN, VPRM_CRP, VPRM_CORN, VPRM_GRS, VPRM_WET, VPRM_OTH, VPRM_URB)
VPRM_param = as.data.table(VPRM_param)

## Units are: T (°C); PAR(umole m-2 s-1); lambda (umole CO2 m-2 s-1/umole PAR m-2 s-1); alpha (umole CO2 m-2 s-1 / 0C); B (umole CO2 m-2 s-1); 
par_names=c("Tmin", "Tmax", "Topt", "Tcrit", "Tmult", "lambda", "PAR0", "beta", "alpha1", "alpha2", "gam", "theta1", "theta2", "theta3")
setnames(VPRM_param, paste0("V", 1:14),par_names)
VPRM_param = cbind(VPRM_LCs, VPRM_param)
rm(VPRM_CRP,VPRM_DBF,VPRM_ENF,VPRM_GRS,VPRM_MXF,VPRM_OTH,VPRM_SHB,VPRM_SVN,VPRM_URB,VPRM_WET,VPRM_LCs,par_names)


#get.EVI.LSWI = function(files){
#  for (i in 1:length(files)){
#    ## read in raster data
#    file <- stack(files[i])
#    ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
#    file[[1]] <- file[[1]] * 0.0001 #red
#    file[[2]] <- file[[2]] * 0.0001 #NIR
#    file[[3]] <- file[[3]] * 0.0001 #blue
#    file[[7]] <- file[[7]] * 0.0001 #SWIR
#    ## Only keep clear observations (QA band 10 codes 66 or 130)
#    clear_code <- c(16392,16400,16456,16464,16520,16528,16584,16592,16904,16912,16968,16976,17032,17040,17096,17104,24584,24592,24648,24656,24712,24720,24776,24784,25096,25104,25160,25176,25224,25232,25288,25296)
#    bad_pixels <- which(!(values(file[[12]]) %in% clear_code))
#    values(file[[1]])[bad_pixels] <- NA
#    values(file[[2]])[bad_pixels] <- NA
#    values(file[[3]])[bad_pixels] <- NA
#    values(file[[7]])[bad_pixels] <- NA
#    ## calculate EVI #### ADJUST BANDS FOR MODIS EVI & LSWI
#    file[[13]] <- 2.5 * ((file[[2]] - file[[1]]) / (file[[2]] + 6 * file[[1]] - 7.5 * file[[3]] + 1))
#    ## calculate LSWI
#    file[[14]] <- (file[[2]] - file[[7]]) / (file[[2]] + file[[7]])
#    ## convert raster data to data.table and assign Day of Year values
#    ## EVI
#    EVI.dt = as.data.table(as.data.frame(file[[13]], xy=T))
#    EVI.dt = cbind(1:ncell(file[[13]]), EVI.dt)
#    setnames(EVI.dt,c("Index","x","y", "EVI"))
#    setkey(EVI.dt,Index,x,y)
#    ## LSWI
#    LSWI.dt = as.data.table(as.data.frame(file[[14]], xy=T))
#    LSWI.dt = cbind(1:ncell(file[[14]]), LSWI.dt)
#    LSWI.dt$DOY = yday(ymd(paste0('2018',substr(files[i],13,16))))
#    setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
#    setkey(LSWI.dt,Index,x,y)
#    ## save a data.table for each image
#    assign(paste0('EVI_LSWI',i,'.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
#  }
#}

# Moisture Scalar
#getWScale = function(S,E,LSWI_dat){
#  SOS = (MOD.SOS)#-1)*24
#  EOS = (MOD.EOS)#-1)*24
#  SOS<-round(SOS)
#  EOS<-round(EOS)
#  SOS[is.na(SOS)]<-1
#  EOS[is.na(EOS)]<-365
#  LSWI_gsl = values(LSWI)[[values(SOS):values(EOS)]]
#  
#  LSWI_max = max(LSWI_gsl)
#  #LSWI_min = min(LSWI_gsl)
#  rm(LSWI_gsl)
#  
#  #WScale = (LSWI-LSWI_min)/(LSWI_max-LSWI_min) #WScale from Hu at al. & Gourdji et al. 2021
#  WScale = (1+LSWI_dat)/(1+LSWI_max)
#  WScale[WScale < 0] = 0 
#  WScale[WScale > 1] = 1 # WScale should be always between 0 and 1
#  return(WScale)
#}

compute.reco.hrly <- function(EVI, LSWI, WScale, TA.path, TA.varname, lc.path, lc.pattern, timestr, site.ext, proj.rt){
  
  library(raster); library(dplyr); library(ncdf4)
  # ------------------------ Estimate Hourly Reco ---------------------------- #
  cat(paste('\ncompute.reco.hrly(): Loading hourly TA for', timestr, '\n'))
  if (TA.field == 'ERA5') {     # hourly air temp in UTC
    TA.brk <- prep.era5(TA.path, TA.varname, timestr, site.ext, nhrs = 24)
    
  } else stop(paste('compute.reco.hrly(): No function available for loading gridded TA from', TA.field))
  # end if TA.field
  
  # reproject TA to 500m to match GPP, RECO
  # default: bilinear interpolation
  TA.pj   <- raster::projectRaster(TA.brk, proj.rt) #mean.reco.rt) # in degC
  
  MOD.SOS<- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_avg_greenup.tif')
  
  #numdays<-as.numeric(as.Date(paste(yr,'-01-01',sep=""),format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d'))
  #MOD.SOS <-MOD.SOS-numdays
  #MOD.SOS[MOD.SOS<=1]<-2 #FIX THIS
  MOD.SOS<-raster::projectRaster(MOD.SOS, proj.rt,method='ngb')
  
  MOD.EOS <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_avg_Dormancy.tif')
  #MOD.EOS <-MOD.EOS-numdays
  #MOD.EOS[MOD.EOS>365]<-365 #FIX THIS
  MOD.EOS<-raster::projectRaster(MOD.EOS, proj.rt,method='ngb')
  
  
  
  
  setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/MODIS_reflectance/MODIS_V061_GTA_AppEEARS_2021') # landsat data in /urbanVPRM_30m/driver_data/landsat/
  mod_files <- list.files()
  b01_files <- mod_files[grep('b01',mod_files)]
  b02_files <- mod_files[grep('b02',mod_files)]
  b03_files <- mod_files[grep('b03',mod_files)]
  #b07_files <- mod_files[grep('b07',mod_files)]
  qc_files <- mod_files[grep('qc_500m_doy',mod_files)]
  ##mod.stk<-stack(mod_files[1])
  
  
  ##for (i in 1:length(mod_files)){
  ##  ## read in raster data
  ##  mod_file <- stack(mod_files[i])
  ##  file<-raster::projectRaster(mod_file, proj.rt)
  ##  ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
  ##  file[[1]] <- file[[1]] * 0.0001 #red
  ##  file[[2]] <- file[[2]] * 0.0001 #NIR
  ##  file[[3]] <- file[[3]] * 0.0001 #blue
  ##  file[[7]] <- file[[7]] * 0.0001 #SWIR
  ##  ## Only keep clear observations (QA band 10 codes 66 or 130)
  ##  #clear_code <- c(16392,16400,16456,16464,16520,16528,16584,16592,16904,16912,16968,16976,17032,17040,17096,17104,24584,24592,24648,24656,24712,24720,24776,24784,25096,25104,25160,25176,25224,25232,25288,25296)
  ##  #bad_pixels <- which(!(values(file[[12]]) %in% clear_code))
  ##  #values(file[[1]])[bad_pixels] <- NA
  ##  #values(file[[2]])[bad_pixels] <- NA
  ##  #values(file[[3]])[bad_pixels] <- NA
  ##  #values(file[[7]])[bad_pixels] <- NA
  ##  ## calculate EVI #### ADJUST BANDS FOR MODIS EVI & LSWI
  ##  file[[13]] <- 2.5 * ((file[[2]] - file[[1]]) / (file[[2]] + 6 * file[[1]] - 7.5 * file[[3]] + 1))
  ##  ## calculate LSWI
  ##  file[[14]] <- (file[[2]] - file[[7]]) / (file[[2]] + file[[7]])
  ##  ## convert raster data to data.table and assign Day of Year values
  ##  ## EVI
  ##  EVI.dt = as.data.table(as.data.frame(file[[13]], xy=T))
  ##  EVI.dt = cbind(1:ncell(file[[13]]), EVI.dt)
  ##  setnames(EVI.dt,c("Index","x","y", "EVI"))
  ##  setkey(EVI.dt,Index,x,y)
  ##  ## LSWI
  ##  LSWI.dt = as.data.table(as.data.frame(file[[14]], xy=T))
  ##  LSWI.dt = cbind(1:ncell(file[[14]]), LSWI.dt)
  ##  LSWI.dt$DOY = yday(ymd(paste0('2018',substr(mod_files[i],13,16))))
  ##  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
  ##  setkey(LSWI.dt,Index,x,y)
  ##  ## save a data.table for each image
  ##  assign(paste0('EVI_LSWI',i,'.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
  ##  print(i/length(mod_files)*100)
  ##}
  ##rm(file)
  
  
  ##create a raster
  EVI_raster_V061 <-raster()
  ##set the number of columns, rows, and extent NEED TO CHANGE DEPENDING ON SITE
  EVI_raster_V061 <- brick(ncol=624, nrow=552, xmn=-80.9, xmx=-78.3, ymn=42.4, ymx=44.7,nl=380)
  res(EVI_raster_V061)
  ##check the number of cells is 344448 for the GTA
  ncell(EVI_raster_V061)
  names(EVI_raster_V061)<-paste('DOY',-3:376,sep="") #name the layers
  
  #LSWI_raster <- brick(ncol=624, nrow=552, xmn=-80.9, xmx=-78.3, ymn=42.4, ymx=44.7,nl=365)
  #res(LSWI_raster)
  #check the number of cells is 344448 for the GTA
  #ncell(LSWI_raster)
  #names(LSWI_raster)<-paste('DOY',1:365,sep="") #name the layers
  
    
  for (i in 1:length(b01_files)){
    ## read in raster data
    #mod_file <- stack(mod_files[i])
    
    b01_file <- stack(b01_files[i])
    b02_file <- stack(b02_files[i])
    b03_file <- stack(b03_files[i])
    #b07_file <- stack(b07_files[i])
    qc_file <- stack(qc_files[i])#+1])#+2])
    file1<-raster::projectRaster(b01_file, mean.gpp.rt)#proj.rt)
    file2<-raster::projectRaster(b02_file, mean.gpp.rt)#proj.rt)
    file3<-raster::projectRaster(b03_file, mean.gpp.rt)# proj.rt)
    qcfile <- raster::projectRaster(qc_file,mean.gpp.rt)
    #file7<-raster::projectRaster(b07_file, proj.rt)
    # APPLY SCALE FACTOR FOR 2018 BUT NOT FOR 2020 (THE DATA ON APPEEARS CHANGED)
    ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
    #file1 <- file1 * 0.0001 #red
    #file2 <- file2 * 0.0001 #NIR
    #file3 <- file3 * 0.0001 #blue
    ##file7 <- file7 * 0.0001 #SWIR
    
    ok_qc=c(1073741824, 1073954817, 1075838976, 1075838977, 1075838979, 1076051969,
            1076625411, 1077149697, 1077362689, 1077411843, 1119879171, 1121976323,
            1123287043, 1123549187, 1128267777, 1130364929, 1130364931, 1131151363,
            1131675649, 1131937795, 1132462083, 1134559235, 1135345667, 1135869955,
            1136132099, 1811939331, 1814036483, 1815347203, 1815609347, 1858076675,
            1866465283, 1868562435, 1869873155, 1870135299, 1870659587, 1873543171,
            1874329603, 1946157057, 1946370049, 1948254209, 1948254211, 1949040643,
            1949564929, 1949827075, 1992294403, 1995964419, 2000683009, 2000896001,
            2002780161, 2002780163, 2003566595, 2004090881, 2004353027, 2004877315,
            2006974467, 2007760899, 2008285187, 2008547331, 2013265923, 2013478915,
            2015363075, 2016149507, 2016673795, 2016886787, 2016935939, 2059403267,
            2061500419, 2062286851, 2062811139, 2063073283, 2067791875, 2069889027,
            2070675459, 2071199747, 2071461891, 2071986179, 2074083331, 2074869763,
            2075394051, 2075656195)
    #values(file1)[(values(qc_file)!=1075838976) & (values(qc_file)!=1073741824)] <- NA
    #values(file2)[(values(qc_file)!=1075838976) & (values(qc_file)!=1073741824)] <- NA
    #values(file3)[(values(qc_file)!=1075838976) & (values(qc_file)!=1073741824)] <- NA
    
    #Uncomment these 3 lines for QF filter else use 3 lines below to fill missing vals with NAs
    values(file1)[(values(qc_file) %in% ok_qc)==FALSE] <- NA
    values(file2)[(values(qc_file) %in% ok_qc)==FALSE] <- NA
    values(file3)[(values(qc_file) %in% ok_qc)==FALSE] <- NA
    #values(file1)[is.finite(values(file1))==FALSE]<-NA
    #values(file2)[is.finite(values(file2))==FALSE]<-NA
    #values(file3)[is.finite(values(file3))==FALSE]<-NA
    
    ## Only keep clear observations (QA band 10 codes 66 or 130)
    #clear_code <- c(16392,16400,16456,16464,16520,16528,16584,16592,16904,16912,16968,16976,17032,17040,17096,17104,24584,24592,24648,24656,24712,24720,24776,24784,25096,25104,25160,25176,25224,25232,25288,25296)
    #bad_pixels <- which(!(values(file[[12]]) %in% clear_code))
    #values(file[[1]])[bad_pixels] <- NA
    #values(file[[2]])[bad_pixels] <- NA
    #values(file[[3]])[bad_pixels] <- NA
    #values(file[[7]])[bad_pixels] <- NA
    ## calculate EVI #### ADJUST BANDS FOR MODIS EVI & LSWI
    file14 <- 2.5 * ((file2 - file1) / (file2 + 6 * file1 - 7.5 * file3 + 1))
    ## calculate LSWI
#    file[[15]] <- (file[[2]] - file[[7]]) / (file[[2]] + file[[7]])
#    
    file14[abs(file14)>1]<-NA
#    file[[15]][abs(file[[15]])>1]<-NA
    ##DOY_layer<-paste('DOY',values(mod_file$DOY),sep="")
    #DOY_layer<-paste('DOY',yday(ymd(paste('2018',substr(mod_files[i],13,16),sep=""))),sep="")
    yr <- substr(b01_files[i],31,32)
    DOY_layer <- paste('DOY',substr(b01_files[i],33,35),sep="")
    if (yr<21){
      DOY_layer <- paste('DOY',as.character(-(as.numeric(substr(b01_files[i],33,35))-364)),sep = ".")
    }else if (yr==21){
      if(substr(DOY_layer,4,5)=="00"){
        DOY_layer <- paste('DOY',substr(DOY_layer,6,6),sep="")
      }else if(substr(DOY_layer,4,4)=="0"){
       DOY_layer <- paste('DOY',substr(DOY_layer,5,6),sep="")
      }
    }else if (yr>21){
      DOY_layer <- paste('DOY',as.character(365+as.numeric(substr(DOY_layer,6,6))),sep="")
    }
    ##DOY_layer<-paste(substr(mod_files[i],5,6),".",substr(hr,7,8),".",substr(hr,9,10),".00.00",sep=""))
    ##ind<-c(1:length(mod_file$DOY))
    
    EVI_raster_V061[[DOY_layer]]<-file14

#    LSWI_raster[[DOY_layer]]<-file[[15]]
#    ## convert raster data to data.table and assign Day of Year values
#    ## EVI
#    #EVI.dt = as.data.table(as.data.frame(file[[13]], xy=T))
#    #EVI.dt = cbind(1:ncell(file[[13]]), EVI.dt)
#    #setnames(EVI.dt,c("Index","x","y", "EVI"))
#    #setkey(EVI.dt,Index,x,y)
#    ### LSWI
#    #LSWI.dt = as.data.table(as.data.frame(file[[14]], xy=T))
#    #LSWI.dt = cbind(1:ncell(file[[14]]), LSWI.dt)
#    #LSWI.dt$DOY = yday(ymd(paste0('2018',substr(mod_files[i],13,16))))
#    #setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
#    #setkey(LSWI.dt,Index,x,y)
#    ### save a data.table for each image
#    #assign(paste0('EVI_LSWI',i,'.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
    print(round(i/length(b01_files)*100,1))
  }
  
  rm(b01_file,b02_file,b03_file,file1,file2,file3,file14,qcfile)
  
  Inter_EVI_raster_V061 <-raster()
  #set the number of columns, rows, and extent NEED TO CHANGE DEPENDING ON SITE
  Inter_EVI_raster_V061 <- brick(ncol=624, nrow=552, xmn=-80.9, xmx=-78.3, ymn=42.4, ymx=44.7,nl=380)
  res(Inter_EVI_raster_V061)
  #check the number of cells is 344448 for the GTA
  ncell(Inter_EVI_raster_V061)
  names(Inter_EVI_raster_V061)<-paste('DOY',-3:376,sep="") #name the layers
  
  
  #Inter_EVI_raster<- stack("C:/Users/kitty/Documents/Research/SIF/SMUrF/data/MODIS_EVI_2020_qc_test.tif")
  
  EVI_values_V061<-values(EVI_raster_V061)
  EVI_length_V061<-length(values(EVI_raster_V061$DOY1))
  Inter_EVI_values_V061<-values(Inter_EVI_raster_V061)
  
  DOY<-c(-3:376)
  
  interpolate <- function(evi){
    if(sum(evi,na.rm=TRUE)>0){
      pix<-data.frame(DOY,evi)
      names(pix)<-c('DOY','EVI')
      spl<-with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI,spar = .25))
      evi_inter<-predict(spl,DOY)$y
      return(evi_inter)
    }
  }
  
  #evi_list<-list()
  #for(i in 1:366){
  #  if(i==1){
  #    evi_list<-EVI_values[i,]
  #  }else{
  #    evi_list<-append(evi_list,EVI_values[i,]) 
  #  }
  #}
  
  #inter_evi_vals<-apply(EVI_values, 5000, interpolate(x) EVI_values[i,] * i)
  
  #registerDoParallel(4,cores=2) #344448
  
  ##Trying to find optimal spline: (not working)
  #y <- EVI_values_V061[EVI_index,][!is.na(EVI_values_V061[EVI_index,])]
  #DOY_y <- DOY[!is.na(EVI_values_V061[EVI_index,])]
  
  #splineres <- function(spar){
  #  res <- rep(0,length(DOY_y))
  #  pred <-rep(0,length(DOY_y))
  #  pix<-data.frame(DOY_y,y,pred)
  #  names(pix)<-c('DOY_y','y','pred')
  #  for (i in 1:length(DOY_y)){
  #    smooth.spline(DOY_y[-i],y[-i],spar=spar)
  #    #mod <- with(pix[!is.na(pix$y),],smooth.spline(DOY[-i],y[-i],spar=spar))
  #    #pix$pred[i]<-predict(mod,DOY_y[i])$y
  #    #res[i] <- with(pix[!is.na(pix$y[i]),],(pred[i]-y[i]))
  #    res[i] <- predict(mod,DOY_y[i])$y-y[i]
  #  }
  #  return(sum(res^2,na.rm=TRUE))
  #}
  
  #spars <-seq(0.01,1, by=0.01)
  #ss <- rep(0,length(spars))
  #for (i in 1:length(spars)){
  #  ss[i] <- splineres(spars[i])
  #}
  
  #plot(spars,ss)
  #spars[which.min(ss)]
  
  
  library("foreach")
  
  system.time({foreach::foreach(i = 1:344448) %do% {
    EVI<-EVI_values_V061[i,]
    pix<-data.frame(DOY,EVI)
    names(pix)<-c('DOY','EVI')
    if(abs(sum(EVI,na.rm=TRUE))>0){
      spl<- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25)) #.25
      Inter_EVI_values_V061[i,]<-predict(spl, c(-3:376))$y #Change to -3:374 for non-leap year, -3:376 for leap year
      #print(Inter_EVI_values[i,])
      #inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(1:365))$y
    } #else {values(Inter_EVI_raster)[i,]<- NA}
    #print(round(i/EVI_length*100, 2))
  }})
  #stopImplicitCluster()
  
  
  #Use the minimum and maximum values
  #for(i in c(5000:5005)){ #1:length(values(EVI_raster$DOY1))){
  #  #if(is.finite(mean(values(EVI_raster)[i,], na.rm = T)) & length(values(EVI_raster)[i,][which(is.finite(values(EVI_raster)[i,]))]) > 3){
  #  #if(length(values(EVI_raster)[i,][which(is.finite(values(EVI_raster)[i,]))]) > 3){
  #    #EVI_raster2<-EVI_raster
  #    #names(EVI_raster)<-paste(c(1:365))
  #  EVI<-EVI_values[i,]
  #  #DOY<-c(1:366)
  #    
  #  pix<-data.frame(DOY,EVI)
  #    #EVI<-as.data.frame(EVI_list)
  #    #DOY<-as.data.frame(DOY_list)
  #  if(sum(EVI,na.rm=TRUE)>0){
      
  #    spl<- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25))
  #    Inter_EVI_values[i,]<-predict(spl, c(1:366))$y
  #    print(Inter_EVI_values[i,])
  #    #inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(1:365))$y
  #  } else {values(Inter_EVI_raster)[i,]<- NA}
  #  #print(round(i/EVI_length*100, 2))
  #}
#  
  values(Inter_EVI_raster_V061)<-Inter_EVI_values_V061
#  
  Inter_EVI_raster_2020<-dropLayer(Inter_EVI_raster_V061,c(1,2,3,4,371:380)) # select only data in the current year change to 371 for leap & 370 for non-leap year
  
#  #THIS WORKS!!! :D
  writeRaster(Inter_EVI_raster_2020,filename="C:/Users/kitty/Documents/Research/SIF/SMUrF/data/MODIS_V061_EVI_2020_qc_extended.tif",
              overwrite=TRUE)
#  data_test<-brick("C:/Users/kitty/Documents/Research/SIF/SMUrF/data/MODIS_EVI.tif")
#  
#  #EVI_LSWI.dt <- rbind(EVI_LSWI1.dt,EVI_LSWI2.dt,EVI_LSWI3.dt,EVI_LSWI4.dt,
#  #                     EVI_LSWI5.dt,EVI_LSWI6.dt,EVI_LSWI7.dt,EVI_LSWI8.dt,
#  #                     EVI_LSWI9.dt,EVI_LSWI10.dt,EVI_LSWI11.dt, EVI_LSWI12.dt,
#  #                     EVI_LSWI13.dt,EVI_LSWI14.dt,EVI_LSWI15.dt,EVI_LSWI16.dt,
#  #                     EVI_LSWI17.dt,EVI_LSWI18.dt,EVI_LSWI19.dt,EVI_LSWI20.dt,
#  #                     EVI_LSWI21.dt,EVI_LSWI22.dt,EVI_LSWI23.dt,EVI_LSWI24.dt,
#  #                     EVI_LSWI25.dt,EVI_LSWI26.dt,EVI_LSWI27.dt,EVI_LSWI28.dt,
#  #                     EVI_LSWI29.dt,EVI_LSWI30.dt,EVI_LSWI31.dt,EVI_LSWI32.dt,
#  #                     EVI_LSWI33.dt,EVI_LSWI34.dt,EVI_LSWI35.dt,EVI_LSWI36.dt,
#  #                     EVI_LSWI37.dt,EVI_LSWI38.dt,EVI_LSWI39.dt,EVI_LSWI40.dt,
#  #                     EVI_LSWI41.dt,EVI_LSWI42.dt,EVI_LSWI43.dt,EVI_LSWI44.dt,
#  #                     EVI_LSWI45.dt,EVI_LSWI46.dt)
#  
#  #rm(EVI_LSWI1.dt,EVI_LSWI2.dt,EVI_LSWI3.dt,EVI_LSWI4.dt,
#  #   EVI_LSWI5.dt,EVI_LSWI6.dt,EVI_LSWI7.dt,EVI_LSWI8.dt,
#  #   EVI_LSWI9.dt,EVI_LSWI10.dt,EVI_LSWI11.dt, EVI_LSWI12.dt,
#  #   EVI_LSWI13.dt,EVI_LSWI14.dt,EVI_LSWI15.dt,EVI_LSWI16.dt,
#  #   EVI_LSWI17.dt,EVI_LSWI18.dt,EVI_LSWI19.dt,EVI_LSWI20.dt,
#  #   EVI_LSWI21.dt,EVI_LSWI22.dt,EVI_LSWI23.dt,EVI_LSWI24.dt,
#  #   EVI_LSWI25.dt,EVI_LSWI26.dt,EVI_LSWI27.dt,EVI_LSWI28.dt,
#  #   EVI_LSWI29.dt,EVI_LSWI30.dt,EVI_LSWI31.dt,EVI_LSWI32.dt,
#  #   EVI_LSWI33.dt,EVI_LSWI34.dt,EVI_LSWI35.dt,EVI_LSWI36.dt,
#  #   EVI_LSWI37.dt,EVI_LSWI38.dt,EVI_LSWI39.dt,EVI_LSWI40.dt,
#  #   EVI_LSWI41.dt,EVI_LSWI42.dt,EVI_LSWI43.dt,EVI_LSWI44.dt,
#  #   EVI_LSWI45.dt,EVI_LSWI46.dt)
#  
#  ### order by DOY
#  #EVI_LSWI.dt <- EVI_LSWI.dt[order(EVI_LSWI.dt$DOY),]
#  ### convert to dataframe
#  #EVI_LSWI.df <- as.data.frame(EVI_LSWI.dt)
#  ### There are a few erroneous values with EVI = 2.5. These are omitted from the dataset 
#  #EVI_LSWI.df[which(abs(EVI_LSWI.df$EVI) > 1),'EVI'] <- NA
#  
#  #npixel <- length(unique(EVI_LSWI.df$Index))
#  #inter_evi_lswi <- as.data.frame(matrix(ncol = 2, nrow = npixel*365))
#  #colnames(inter_evi_lswi) <- c('Index', 'DOY')
#  #inter_evi_lswi[,1] <- rep(seq(1,npixel,1), 365)
#  #inter_evi_lswi <- inter_evi_lswi[order(inter_evi_lswi[,1]),]
#  #inter_evi_lswi[,2] <- rep(seq(1,365,1), npixel)
#  #inter_evi_lswi <- merge(inter_evi_lswi, EVI_LSWI.df[,c('Index', 'EVI', 'LSWI', 'DOY')], by = c('Index', 'DOY'), all = TRUE)
#  #inter_evi_lswi  <- inter_evi_lswi[order(inter_evi_lswi[,'Index'],inter_evi_lswi [,'DOY']),]
#  
#  # Interpolate daily EVI values for each pixel
#  inter_evi_lswi$EVI_inter <- NA
#  
#  for(i in unique(inter_evi_lswi$Index)){
#    if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$EVI)),'EVI']) > 3){
#      pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
#      spl <- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25)) 
#      inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(1:365))$y
#    } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- NA}
#    print(round(i/npixel*100, 1))
#  }
#  
  
  #####  GO BACK AND FIX FILTERING OF MODIS DATA  #####
  
  #EVI_r <- brick("C:/Users/kitty/Documents/Research/SIF/SMUrF/data/MODIS_EVI.tif")#,layer='DOY171')
  #LSWI_r <- brick("C:/Users/kitty/Documents/Research/SIF/SMUrF/data/MODIS_LSWI.tif") #,layer='DOY171')
  
  #EVI<-EVI_r[[171]]
  #LSWI<-LSWI_r[[171]]
  
  #EVI[abs(EVI)>1]<-NA
  #LSWI[abs(LSWI)>1]<-NA
  
  #LSWI_test<-LSWI
  
  #WScale_r <- brick("C:/Users/kitty/Documents/Research/SIF/SMUrF/data/WScale.tif")#,layer='DOY171')
  #WScale0<-WScale_r[[171]]
  #WScale0<-getWScale(MOD.SOS,MOD.EOS,LSWI)
  
  
  
  ###   NEED TO DEFINE:
  ###   evi_scale, wtr
  
  #lc.path    <- file.path(smurf_wd, 'data/MCD12Q1')
  #lc.pattern <- 'MCD12Q1.006_LC_Type1'
  
  # indicate the latest year available of MCD12Q1
  # if no data beyond 2018, use 2018 LC for 2019 and beyond
  lc.max.yr <- 2018  #IF USING A DIFFERENT YEAR MAKE SURE TO CHANGE predGPP.R CODE ACCORDINGLY
  lc.res    <- 1/240     # horizontal grid spacing of land cover in degrees
  
  #reg.ext <- extent(minlon, maxlon, minlat, maxlat)
  
  lc.rt<-prep.mcd12(lc.path, lc.pattern, yr = '2018', lc.max.yr, reg.name = NULL, site.ext)
  
  ### Get Respiration
  ## Model parameters for Respiration calculation
  #Create raster layers for each of the parameters
  lc.rt$alpha1<-NA
  lc.rt$alpha2<-NA
  lc.rt$beta<-NA
  lc.rt$gam<-NA
  lc.rt$theta1<-NA
  lc.rt$theta2<-NA
  lc.rt$theta3<-NA
  lc.rt$Tcrit<-NA
  lc.rt$Tmult<-NA

  #Classify water as 'other'
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-VPRM_param[VPRM_LCs=='OTH',Tmult]
  
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==1]<-VPRM_param[VPRM_LCs=='ENF',Tmult]
  
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==4]<-VPRM_param[VPRM_LCs=='DBF',Tmult]
  
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==5]<-VPRM_param[VPRM_LCs=='MXF',Tmult]
  
  #Mark deciduous needleleaf as mixed forest? because there are no parameters available from Gourdji et al. 
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==3]<-VPRM_param[VPRM_LCs=='MXF',Tmult]
  
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==6] <- lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==7]<-VPRM_param[VPRM_LCs=='SHB',Tmult]
  
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==8] <- lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==9]<-VPRM_param[VPRM_LCs=='SVN',Tmult]
  
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==10]<-VPRM_param[VPRM_LCs=='GRS',Tmult]
  
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==11]<-VPRM_param[VPRM_LCs=='WET',Tmult]
  
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==12] <- lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==14]<-VPRM_param[VPRM_LCs=='CRP',Tmult]
  
  #Classified Barren/sparesly vegetated as Urban (maybe it should be water instead but this won't account for soil respiration)
  lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$alpha1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',alpha1]
  lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$alpha2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',alpha2]
  lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$beta[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',beta]
  lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$gam[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',gam]
  lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$theta1[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',theta1]
  lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$theta2[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',theta2]
  lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$theta3[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',theta3]
  lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$Tcrit[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',Tcrit]
  lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==13] <- lc.rt$Tmult[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==16]<-VPRM_param[VPRM_LCs=='URB',Tmult]
  
  #Modified air temperature defined by Gourdji et al. 2021 to account for non-zero respiration at low temperatures
  Tp <- TA.pj
  
  hrs<-c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
  
  hrly.timestr<-paste(substr(timestr,1,8),hrs,sep="")
  #for (t in mon.timestr){
  #  if (length(hrly.timestr)==0){hrly.timestr<-paste(substr(t,1,8),hrs,sep="")}
  #  else{hrly.timestr<-append(hrly.timestr,paste(substr(t,1,8),hrs,sep=""))}
  #}
  
  for (hr in hrly.timestr){
    ind<-paste("X",yr,".",substr(hr,5,6),".",substr(hr,7,8),".",substr(hr,9,10),".00.00",sep="")
    Tp[[ind]][TA.pj[[ind]]<lc.rt$Tcrit]<-lc.rt$Tcrit[TA.pj[[ind]]<lc.rt$Tcrit] - lc.rt$Tmult[TA.pj[[ind]]<lc.rt$Tcrit] * (lc.rt$Tcrit[TA.pj[[ind]]<lc.rt$Tcrit] - TA.pj[[ind]][TA.pj[[ind]]<lc.rt$Tcrit])
  }
  #Tp[TA.pj<Tcrit] <- lc.rt$Tcrit - lc.rt$Tmult * (lc.rt$Tcrit - TA.pj[TA.pj<lc.rt$Tcrit])
  
  #Updated Respiration equation from Gourdji et al. 2021
  Re = lc.rt$beta + (lc.rt$alpha1 * Tp) + (lc.rt$alpha2 * Tp^2) + (lc.rt$gam * EVI) + (lc.rt$theta1 * WScale) + (lc.rt$theta2 * WScale * Tp) + (lc.rt$theta3 * WScale * Tp^2); names(Re)<-names(Tp)
  
  ## Partition respiration into heterotrophic and autotrophic respiration
  Ra = 0.5*Re
  Rh = 0.5*Re
  
  isa<-raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/ACI_impervious_GTA_500m.tif")
  isa<-isa/100
  
  EVI_ref<-EVI[194223] #Fully forested pixel: mixed forest located at (-80.224,43.402)
  
  wtr<-lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001
  wtr[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001!=17]<-0 #can also try doing a % using ACI
  wtr[lc.rt$MCD12Q1.006_LC_Type1_doy2018001_aid0001==17]<-1
  
  #for (i in 1:length(values(Re$X2018.06.20.00.00.00))){
  #if(isa[i] > 0.05){
  #  ## Modify Re components: Rh is reduced by ISA and Ra is reduced by EVI 
  #  Rh[i] = Rh[i] * (1-isa[i])
  #  # Ra reduced by EVI, but rescaled to maintain min EVI
  #  EVI_scale = EVI[i]/EVI_ref
  #  EVI_scale[!(EVI_ref>0.05)] = 1
  #  EVI_scale[EVI_scale>1] = 1
  #  EVI_scale[EVI_scale<0] = 0
  #  Ra[i] = Ra[i] * EVI_scale 
  #  EVI_scale = as.data.frame(as.numeric(EVI_scale))
  #} else {
    ## if no ISA in pixel, no scaling
  #  EVI_scale = as.data.frame(rep(NA,time=length(Re[i])))
  #}
  #print(round(i/length(values(Re$X2018.06.20.00.00.00))*100,1))
  #}
  
  Rh = Rh * (1-isa)
  # Ra reduced by EVI, but rescaled to maintain min EVI
  EVI_scale = EVI/EVI_ref
  EVI_scale[!(EVI_ref>0.05)] = 1
  EVI_scale[EVI_scale>1] = 1
  EVI_scale[EVI_scale<0] = 0
  Ra = Ra * EVI_scale 
  
  # Put the respiration components back together
  Re = Ra + Rh
  
  # Modify total respiration by percentage of water within the pixel
  Re = Re * (1-wtr)
  
  return(Re)
}
