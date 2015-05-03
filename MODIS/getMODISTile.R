library(gdata)
library(raster)
library(rgdal)
library(lubridate)
library(plyr)
library(stringr)
library(MODISTools)
library(maptools)
library(MODIS)  


get.MODIStile <- function(coords){
  # Returns the respective MODIS tiles of some points
  (/)
}


ModisTileDownload <- function(x){
  # Direct Download of MODIS tiles
  mtiles <- readOGR("/media/GISbox/Raster/VegetationIndex/modis_sinusoidal_shape/","modis_sinusoidal_grid_world")
  dp = "ftp://e4ftl01.cr.usgs.gov/MOLT/MOD13Q1.005/"
  ddir <- "/media/GISbox/Raster/VegetationIndex/MODIS_MOD13Q1_PREDICTS/"
  sin_coord <- spTransform(sp,CRSobj = CRS(proj4string(mtiles)))
  ov <- over(sin_coord,mtiles);ov$cat <- NULL
  ov$h <- as.character(ov$h)
  l <- str_length(as.character(ov$v))
  ov$v[which(l<2)] <- paste0("0",ov$v[which(l<2)])
  ov$start.date <- pred_gisdata$start.date;ov$start.month <- pred_gisdata$start.month
  ov$end.date <- pred_gisdata$end.date;ov$end.month <- pred_gisdata$end.month
  ov$Sample_start_earliest <-  pred_gisdata$Sample_start_earliest
  ov$Sample_end_latest <- pred_gisdata$Sample_end_latest
  uov <- unique(ov) # Calculate only for unique rows <- reduces dataset to 30
  sink(paste0("/media/GISbox/Raster/VegetationIndex/","modis_dl.sh"))
  for(i in 1:nrow(uov)) {
    tile = paste0("h",uov$h[i],"v",uov$v[i])
    s = paste0(uov$start.date[i],"-",uov$start.month[i],"-01")
    e = paste0(uov$end.date[i],"-",uov$end.month[i],"-",days_in_month(as.numeric(uov$end.month[i])))
    cat(paste("echo","Processing",tile,"from",s,"to",e))
    cat("\n")
    p = paste0("h",uov$h[i],"v",uov$v[i]) 
    cat("mkdir -p",(paste0(ddir,p)))    
    cat("\n")    
    f <- paste0("modis_download.py -r -p MOD13Q1.005 -t ",tile," -e ",s," -f ",e," MODIS_MOD13Q1_PREDICTS/",p)     
    cat(f)
    cat("\n")
    cat(paste("echo fininished downloading.",nrow(unique(uov))-i,"remaining"))
    cat("\n")
    rm(tile,f,p)
  }
  sink()  
  # Stop and download
  stop("Run Modis_download.py script and update files on GISBOX")
 
  # Continue  
  pred_gisdata$meanNDVI <- NA
  pred_gisdata$meanEVI <- NA  
  pred_gisdata$meanInterpNDVI <- NA
  pred_gisdata$meanInterpEVI <- NA
  pred_gisdata$yieldNDVI <- NA
  pred_gisdata$yieldEVI <- NA
  #### Mass NDVI Yield Sampler ####
  require(gdalUtils)
  uov <- unique(ov)
  for(i in 1:nrow(uov)){
    rn <- as.numeric(rownames(match_df(x = ov,y =  uov[i,]))) # Gives us the row names
    tile = paste0("h",uov$h[i],"v",uov$v[i])
    d = paste0(ddir,tile,"/")
    std <- paste0(uov$start.date[i],formatC(yday(uov$Sample_start_earliest[i]),width=3,flag="0")) 
    end <- paste0(uov$end.date[i],formatC(yday(uov$Sample_end_latest[i]),width=3,flag="0"))
    h <- grep(pattern = "xml",list.files(d,pattern = "*.hdf",full.names = T,ignore.case = F),value = T,invert = T)
    # Using the MODIS Orgtime and prestack function to subset the datasets needed
    o <- orgTime(h,nDays = 16,begin = std,end = end,pillow=15)
    vi <- preStack(files = h,timeInfo = o)
    cat("In Total",length(vi),"different modis frames")
    cat("\n")
    # Load in the raster layers
    ras_list <- list()
    #type = NDVI & EVI = 16bit, reliability = 8bit integer
    for(lay in vi){
      
      mod <- getSds(lay,method="gdal")
      ndvi <- raster(mod$SDS4gdal[1])#raster(readGDAL(mod$SDS4gdal[1]))
      evi <- raster(mod$SDS4gdal[2])#evi2 <- raster(readGDAL(mod$SDS4gdal[2]))
      rel <- raster(mod$SDS4gdal[12])#raster(readGDAL(mod$SDS4gdal[12]))
      ras_list[lay] <- stack(ndvi,evi,rel)
      rm(ndvi,evi,rel,mod)
    }    
    # Seperate loop for individual coordinates
    for(p in rn){
      # Spatial subset and transformation
      s <- spTransform(sp[p,],CRS(proj4string(mtiles)))
      res = data.frame(NDVI=numeric(),EVI=numeric(),REL=numeric(),stringsAsFactors = F) 
      for(lay in vi){
        ex <- as.data.frame(raster::extract(ras_list[[lay]],s));names(ex) <- c("NDVI","EVI","REL")
        res <- rbind(res,ex)
        rm(ex)
      }
      names(res) <- c("NDVI","EVI","REL")
      res$NDVI <- res$NDVI *0.0001 # Apply Scaling Facor
      res$EVI <- res$EVI *0.0001 # Apply Scaling Facor
      # Kick out values with -1,23 
      cat("Data not useable from ",length(which(res$REL==0))," Dates out of",nrow(res))
      cat("\n")
      dates <- o$inputLayerDates#[which(res$REL==1)]
      #res <- res[which(res$REL==1),]
      
      # Calculate mean of EVI and NDVI
      pred_gisdata$meanNDVI[p] <-  mean(res$NDVI*0.0001)
      pred_gisdata$meanEVI[p] <-  mean(res$EVI*0.0001)
      if(nrow(res)>1&length(which(!is.na(res$NDVI)))>1){
        ######################
        # Linear interpolation and yield calculation
        ######################
        sout = approx(x=dates, y=res$NDVI*0.0001, method = "linear", n = ((max(dates[!is.na(res$NDVI)])- min(dates[!is.na(res$NDVI)]))-1))
        minobsndvi = min(res$NDVI*0.0001, na.rm = TRUE)    # minimum NDVI observed
        maxobsndvi = max(res$NDVI*0.0001, na.rm = TRUE)    # maximum NDVI observed
        ndvi.yield = (sum(sout$y) - minobsndvi*length(sout$x)) / length(sout$x) #(((365*length(years))-16)*365) # average annual yield  (i.e. work out daily yield * 365
        pred_gisdata$yieldNDVI[p] <- ndvi.yield
        pred_gisdata$meanInterpNDVI[p] <-mean(sout$y)
        
        minobsevi = min(res$EVI*0.0001, na.rm = TRUE)    # minimum NDVI observed
        maxobsevi = max(res$EVI*0.0001, na.rm = TRUE)    # maximum NDVI observed
        evout = approx(x=dates, y=res$EVI*0.0001, method = "linear", n = ((max(dates[!is.na(res$EVI)])- min(dates[!is.na(res$EVI)]))-1))
        evi.yield = (sum(evout$y) - minobsevi*length(evout$x)) / length(evout$x) #(((365*length(years))-16)*365) # average annual yield  (i.e. work out daily yield * 365
        pred_gisdata$yieldEVI[p] <- evi.yield
        pred_gisdata$meanInterpEVI[p] <- mean(evout$y)
        cat("Interpolated and extracted Data. yieldEVI =",evi.yield)
        cat("\n")        
      }
    }
    cat("Finished tile",tile)
    cat("\n")
    rm(ras_list)
  }
  
  cat("NA in",length(which(is.na(pred_gisdata$meanNDVI))),"out of",nrow(pred_gisdata),"\n")
  a = difftime(sites.pred$Sample_end_latest,sites.pred$Sample_start_earliest,units = "days")
  plot(as.vector(a)~pred_gisdata$yieldEVI)
  summary(l<-lm(as.vector(a)~pred_gisdata$yieldEVI));abline(l,col="red",lwd=2)
  setwd(ori.dir)
  
  
}
