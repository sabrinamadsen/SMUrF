# function to prepare initial and gap-filled MODIS land cover map 
# downloaded from app
# DW, 09/19/2019 

prep.mcd12 <- function(lc.path, lc.pattern, yr = '2018', lc.max.yr, 
                       reg.name = NULL, reg.ext) {

    # load all MCD data and find the correct one according to @param yr
    lc.files <- list.files(lc.path, lc.pattern, full.names = T)

    if (length(lc.files) > 1) {
        lc.file <- lc.files[grepl(yr, lc.files)]
        if (yr > lc.max.yr) lc.file <- lc.files[length(lc.files)]
    } else lc.file = lc.files  

    ## download files from https://lpdaacsvc.cr.usgs.gov/appeears/, which 
    # requires shapefile (can be generated by calling create.shapefile())
    # make sure LC domain > your target `reg.ext`
    if (length(lc.file) == 0) {  # if no land cover data found, create shapefile
        create.shapefile(reg.name, reg.ext, outpath = lc.path, overwrite = T) 
        stop(paste('predGPP(): NO MCD12 land cover data for required region in', yr, 
                   '\nPlease check created shapefiles under', lc.path, 
                   'and download data from https://lpdaacsvc.cr.usgs.gov/appeears/'))
    } else {
        
        if (grepl('nc',  lc.file)) lc.stk <- stack(lc.file, varname = 'LC_Type1')
        if (grepl('tif', lc.file)) lc.stk <- raster(lc.file)
        
        if ( nlayers(lc.stk) > 1 ) {
            lc.names   <- as.numeric(substr(gsub('X', '', names(lc.stk)), 1, 4))
            layer.indx <- findInterval(yr, lc.names)
            lc.rt      <- raster::crop(subset(lc.stk, layer.indx), reg.ext)
        } else lc.rt <- raster::crop(lc.stk, reg.ext)   # end if subset layers
        #NOTE THIS NEEDS TO BE COMMENTED OUT FOR EVERYWHERE OUTSIDE TORONTO
        #lc.rt[values(lc.rt)==9]<-8 #replace savanna with woody savanna to fix fluxes at the rouge
    }   # end if LC file        
    return(lc.rt)
}


#### 
prep.mcd12.gf <- function(lc.path, yr, minlon, maxlon, minlat, maxlat) {
    
    gf.lc.fns <- list.files(lc.path, 'MCD12Q1_gapfill_500m_', recursive = T)
    gf.lc.fns <- gf.lc.fns[grepl(yr, gf.lc.fns)]
    gf.lc.ext <- strsplit.to.df(gsub('.tif', '', basename(gf.lc.fns)))

    which.gf <- which(as.numeric(gf.lc.ext$V5) == minlon &
                      as.numeric(gf.lc.ext$V6) == maxlon &
                      as.numeric(gf.lc.ext$V7) == minlat &
                      as.numeric(gf.lc.ext$V8) == maxlat)

    if (length(which.gf) == 0) {
        which.gf <- which(as.numeric(gf.lc.ext$V5) <= minlon &
                          as.numeric(gf.lc.ext$V6) >= maxlon &
                          as.numeric(gf.lc.ext$V7) <= minlat &
                          as.numeric(gf.lc.ext$V8) >= maxlat)[1]
    }

    gf.lc.fn <- file.path(lc.path, gf.lc.fns[which.gf])
    if (!file.exists(gf.lc.fn)) stop('NO updated gapfilled IGBP, check main_script_GPP.r')

    reg.ext <- raster::extent(minlon, maxlon, minlat, maxlat)
    gf.lc.rt <- suppressWarnings(readAll(crop(raster(gf.lc.fn), reg.ext))) # read LC in memory

    return(gf.lc.rt)
}