#' Main script to generate ecosystem respiration
#' @author: Dien Wu, 05/20/2019
#' ---------------------------------------------------------------------------

#' @GeneralIdeas:
#' 1. Need gridded Tair, Tsoil and SIF-based GPP. 
#'    Modeled GPP can be generated from 'main_script_GPP_temporal.r'
#' 2. Need pretrained NN model derived from FLUXNET, which can be generated from 
#'    'prep_NN_train_reco.r' and 'NN_train_reco.r'

#' @InputData:
#' 0. GPP estimates based SIF, see 'main_script_GPP_temporal.r'
#' 1. Gridded 1km x1km Tmin and Tmax from Daymet 
#' 2. Gridded Tsoil from NLDAS
#' 3. Gridded sub-categories for urban settlements e.g., from NLCD
#' ---------------------------------------------------------------------------

#' @updates, by DW:
#' 06/18/2019 incorporate slurm parallel scripts to this script
#' 08/02/2019 add NN models trained by either NLDAS+daymet (US) or ERA5 (global)
#'            also NN models trained by either FLUXNET or modeled temp + GPP 
#' ---------------------------------------------------------------------------

# when using runthem.py, turn this on
#args <- commandArgs(trailingOnly = TRUE)

memory.limit(size=5e5)

#### source all functions and load all libraries
homedir <- 'C:/Users/kitty/Documents/Research/SIF'
smurf_wd <- file.path(homedir, 'SMUrF'); setwd(smurf_wd)
source('r/dependencies.r')              # source all functions


# ---------------------------------------------------------------------------
# Paths one needs to modify 
# ---------------------------------------------------------------------------
input.path  <- file.path(homedir, 'SMUrF/data')
output.path <- file.path(homedir, 'SMUrF/output2021_500m_CSIF_to_TROPOMI_CSIF_ALL_converted_slps_temp_impervious_R_V061_8day')

## path for the updated 500m IGBP generated from main_script_GPP.r
lc.path <- file.path(smurf_wd, 'data/MCD12Q1')
lc.pattern <- 'MCD12Q1.061_LC_Type1'
lc.max.yr<-2021
#tmpdir <- '/scratch/local/u0947337'  # raster operation may need temporary disk space
tmpdir <- 'C:/Users/kitty/AppData/Local/Temp/R' #NA

ISA.path <- 'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface'
ISA.pattern <- 'GMIS_Toronto_ACI_SOLRIS_'
isa.max.yr<-2021

# ---------------------------------------------------------------------------
# Variables one needs to modify 
# ---------------------------------------------------------------------------
# name your region, needs to be consistent with that in main_script_GPPv2.r
#indx <- as.numeric(args[1])     # get region indx from python code, e.g., 1
indx <- 2
reg.name <- c('westernCONUS', 'easternCONUS',     'westernEurope', 
              'easternChina', 'easternAustralia', 'easternAsia', 
              'southAmerica', 'centralAfrica')[indx]   
              
# output path for the target region
reg.path <- file.path(output.path, reg.name)
cat(paste('Working on', reg.name, '...\n'))

# please make sure this domain is <= than the domain of MODIS land cover,
# 'minlon maxlon, minlat, laxlat' should matche the order of 'reg.name' above
# *** too large a spatial extent may lead to memory issue, DONT DO ENTIRE GLOBE
minlon <- c(-125, -80.9,  -11, 100,  130, 125, -65, -10)[indx]
maxlon <- c( -95, -78.3,   20, 125,  155, 150, -40,  20)[indx]
minlat <- c(  25,  42.4,   35,  20,  -40,  30, -40, -10)[indx]
maxlat <- c(  50,  44.7,   60,  50,  -10,  55, -10,  15)[indx]

# due to limited storage, let's break one year into different months
#yr  <- args[2]    # get year string from python code, YYYY e.g., '2018'
#mon <- args[3]    # get month from python code, MM, e.g., '01'

yr <- '2021'
mon <- '01'
start.date <- as.Date(paste0(yr, formatC(mon, width = 2, flag = 0), '01'), '%Y%m%d')
end.date <- as.Date(paste0(yr, formatC(mon, width = 2, flag = 0), 
                               formatC(lubridate::days_in_month(start.date),
                                       width = 2, flag = 0)), '%Y%m%d')

# timestr in form of YYYYMMDD
timestr <- gsub('-', '', seq(start.date, end.date, by = 'day'))


# ---------------------------------------------------------------------------
# paths and patterns for Tair and Tsoil files
# ---------------------------------------------------------------------------
era5.path   <- file.path(input.path, 'ERA5', yr)    # ERA5 Tair and Tsoil
daymet.path <- file.path(input.path, 'Daymetv3', yr) # for Tair
nldas.path  <- file.path(input.path, 'NLDAS', yr)    # for Tsoil

#' common portions in the filenames before YYYY* for grabbing all available files
#' here are examples of the suitable filenames: 
#' ERA5:   STL1_201801.nc (Tsoil), 2T_201801.nc (Tair)
#' Daymet: daymet_v3_tmax_2018_na.nc4, daymet_v3_tmin_2018_na.nc4
#' NLDAS:  NLDAS_NOAH0125_H.A20180101.0000.002*.nc4
#' SMUrF will search for the correct files that match @param timestr
# choose temperature products and variable names

# if you used ERA5 temp, you need to have NN models trained ysing era5
# nn.indx will decide which fields and NN models to use
nn.indx <- 2
TA.field   <- c('daymet', 'ERA5')[nn.indx]
TS.field   <- c('NLDAS',  'ERA5')[nn.indx]
TA.varname <- c('daymet_v3', '2T')[nn.indx]
TS.varname <- c('SoilT_0_10cm',  'STL1')[nn.indx]

# which NN model to predict Reco, these should match the temp field you chose 
# e.g., if you used ERA5 temp, you need to have NN models trained ysing era5
nn.pattern  <- c('daymet_nldas', 'era5')[nn.indx]           
nn.platform <- 'neuralnet' #'keras' 
# pretrained models are stored under "data/NN_models"
# nn.dir <- file.path(smurf_wd, 'data/NN_models/neuralnet')

# get the correct temp paths; high res daymet and NLDAS, only for US 
TA.path <- ifelse(TA.field == 'daymet', daymet.path, era5.path)
TS.path <- ifelse(TS.field == 'NLDAS', nldas.path, era5.path)


EVI.path <- input.path
EVI.pattern <- 'MODIS_V061_EVI_2021_qc_extended'

if (length(grep(EVI.pattern,list.files(EVI.path)))==0){
    print("Process MODIS EVI data")
    mod_dir='C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/MODIS_reflectance/MODIS_V061_GTA_AppEEARS_2021'
    mod_EVI(mod_dir,yr,minlon,maxlon,minlat,maxlat,reg.name,lc.path,lc.pattern,lc.max.yr,EVI.path,EVI.pattern)
}

EVI_index <- 132707
EVI_ref_min <- raster(paste0(EVI.path,'/',EVI.pattern,'.tif'),band=358)[EVI_index] 
# For V061: The lowest value at the reference pixel occurs on day 49 (February) for 2018
# For V061: The lowest value at the reference pixel occurs on day 50 (February) for 2019
# For V061: The lowest value at the reference pixel occurs on day 5 (January) for 2020
# For V061: The lowest value at the reference pixel occurs on day 358 (December) for 2021
#For V006: The lowest value at the reference pixel occurs on day 5 of the year for 2018 ( I think there was a bug here)
# and day 50 for 2019 (February), day 3 for 2020

#Uncomment to determine the date where EVI is a minimum at the reference pixel
#for (i in 1:366){
#    if (i==1){
#        EVI_list <- raster(paste0(EVI.path,'/',EVI.pattern,'.tif'),band=i)[EVI_index]
#    }else{
#        EVI_list <- append(EVI_list,raster(paste0(EVI.path,'/',EVI.pattern,'.tif'),band=i)[EVI_index])
#    }
#}
#which(EVI_list==min(EVI_list))

#end of determine date

# ---------------------------------------------------------------------------
# use SLURM for parallel simulation settings
# ---------------------------------------------------------------------------
# too many cores may slow the calculations and cause job being killed
n_nodes  <- 1
n_cores  <- 1       # max of 5 cores if running on CHPC @utah
job.time <- '24:00:00'      # total job time
slurm    <- n_nodes > 1  # logical, TF
slurm_options <- list(time = job.time, account = 'lin-kp', partition = 'lin-kp')
jobname <- paste('SMUrF_Reco', reg.name, yr, sep = '_') 
message(jobname)
#stop()

# ----------------------------------------------------------------------------
# Start running Reco model 
# ----------------------------------------------------------------------------
message('Initializing Reco estimates')
message('Number of parallel threads: ', n_nodes * n_cores)
smurf_apply(FUN = predReco_biome, slurm, slurm_options, n_nodes, n_cores, jobname, 
            reg.name, reg.path, minlon, maxlon, minlat, maxlat, timestr, 
            lc.path, lc.pattern, lc.max.yr, TA.path, TA.field, TA.varname,
            TS.path, TS.field, TS.varname, nn.pattern, nn.platform, 
            ISA.path, ISA.pattern, isa.max.yr,
            EVI.path, EVI.pattern, EVI_index, EVI_ref_min, smurf_wd, tmpdir)



print('Done!')
q('no')

# end of script


# ----------------------------------------------------------------------------
# script to re-run SMUrF for missing time stamps, if they exist
# ----------------------------------------------------------------------------
if (F) {

    reco.path <- file.path(output.path, reg.name, 'daily_mean_Reco_neuralnet', nn.pattern, yr)
    reco.files <- list.files(reco.path, '.nc')
    exist.timestr <- substr(reco.files, nchar(reco.files) - 10, nchar(reco.files) - 3)
    
    all.timestr <- gsub('-', '', seq(as.Date(paste0(yr, '-01-01')), 
                                     as.Date(paste0(yr, '-12-31')), by = 'day'))
    miss.timestr <- all.timestr[!substr(all.timestr, 1, 8) %in% exist.timestr]
    print(miss.timestr)
    
    jobname <- paste('SMUrF_Reco', reg.name, yr, 'missing', sep = '_')
    slurm_options <- list(time = '06:00:00', account = 'lin-kp', partition = 'lin-kp')
    smurf_apply(FUN = predReco_biome, slurm = T, slurm_options, n_nodes = 8, 
                n_cores = 3, jobname, reg.name, reg.path, 
                minlon, maxlon, minlat, maxlat, timestr = miss.timestr, 
                lc.path, lc.pattern, TA.path, TA.field, TA.varname, TS.path, 
                TS.field, TS.varname, nn.pattern, nn.platform, smurf_wd, tmpdir)

    q('no')            
}   # end if

