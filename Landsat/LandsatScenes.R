#' ScenesStatistics - Shows statistics of the landsat metadata
#' 
#' @author Martin Jung
#' @param mission Which satellite mission (default = "ALL")
#' @return statistics of the scene metadata 

ScenesStatistics <- function(mission="ALL"){
  if(!require(dplyr))install.packages("dplyr");library(dplyr)
  if(!require(ggplot2))install.packages("ggplot2");library(ggplot2)
  
  ll <- list.files("scenes-list/",full.names = T)
  ll <- ll[-grep("report",ll)] # kick out the report
  
  zz <- read.table(ll[1],header = T,sep = ",",dec = ".")   
  
  zz %>% mutate(longr = round(sceneCenterLongitude,1), latr = round(sceneCenterLatitude,1)) %>%
    group_by(latr,longr) %>%
    summarize(N = n(),
              cav = mean(cloudCoverFull)) %>%
    qplot(x=longr,y=latr,color=cav,size=N,geom="point",data=.) + theme_classic()
  
  
}

#' updateLandsatScenes - Update landsat if server version has been modified
#' 
#' @author Martin Jung
#' @return a data.frame containing the tile numbers

#Check online files for last modification date via HTTP headers

#' getLandsatScenes - Downloading landsat metadata
#' 
#' @author Martin Jung
#' @return a data.frame containing the tile numbers

getLandsatScenes <- function(){
  dir.create("scenes-list",showWarnings = FALSE)
  
  print("Downloading Landsat 8")
  # Alternative - Landsat 8
  #http://landsat-pds.s3.amazonaws.com/scene_list.gz
  # Landsat 8  
  d <- try({
    download.file("http://landsat.usgs.gov/metadata_service/bulk_metadata_files/LANDSAT_8.csv.gz",destfile = "scenes-list/LANDSAT_8.csv.gz")
    })
  if(class(d) == "try-error") stop("Download problem with Landsat 8")
  
  print("Downloading Landsat 7 - SLC on")
  # Landsat 7 - SLC on
  d <- try({
    download.file("http://landsat.usgs.gov/metadata_service/bulk_metadata_files/LANDSAT_ETM.csv.gz",destfile = "scenes-list/LANDSAT_ETM.csv.gz")
  })
  if(class(d) == "try-error") stop("Download problem with Landsat 7 - SLC on")
  
  print("Downloading Landsat 7 - SLC off")
  # Landsat 7 - SLC off
  d <- try({
    download.file("http://landsat.usgs.gov/metadata_service/bulk_metadata_files/LANDSAT_ETM_SLC_OFF.csv.gz",destfile = "scenes-list/LANDSAT_ETM_SLC_OFF.csv.gz")
  })
  if(class(d) == "try-error") stop("Download problem with Landsat 7 - SLC off")

  print("Downloading Landsat 4-5 80s")
  # Landsat 4-5 80s
  d <- try({
    download.file("http://landsat.usgs.gov/metadata_service/bulk_metadata_files/LANDSAT_TM-1980-1989.csv.gz",destfile = "scenes-list/LANDSAT_TM-1980-1989.csv.gz")
  })
  if(class(d) == "try-error") stop("Download problem with Landsat 4-5 80s")

  print("Downloading Landsat 4-5 90s")
  # Landsat 4-5 90s
  d <- try({
    download.file("http://landsat.usgs.gov/metadata_service/bulk_metadata_files/LANDSAT_TM-1990-1999.csv.gz",destfile = "scenes-list/LANDSAT_TM-1990-1999.csv.gz")
  })
  if(class(d) == "try-error") stop("Download problem with Landsat 4-5 90s")
  
  print("Downloading Landsat 4-5 00s")
  # Landsat 4-5 00s
  d <- try({
    download.file("http://landsat.usgs.gov/metadata_service/bulk_metadata_files/LANDSAT_TM-2000-2009.csv.gz",destfile = "scenes-list/LANDSAT_TM-2000-2009.csv.gz")
  })
  if(class(d) == "try-error") stop("Download problem with Landsat 4-5 00s")  
  
  print("Downloading Landsat 4-5 10s")
  # Landsat 4-5 10s
  d <- try({
    download.file("http://landsat.usgs.gov/metadata_service/bulk_metadata_files/LANDSAT_TM-2010-2012.csv.gz",destfile = "scenes-list/LANDSAT_TM-2010-2012.csv.gz")
  })
  if(class(d) == "try-error") stop("Download problem with Landsat 4-5 10s")    
  # ------- #
  print("Download finished. Creating report file")
  # Create a result data frame with a list of files, date and time and md5 chksum
  # does the md5 checksum 
  if(!require(lubridate))install.packages("lubridate");library(lubridate)
  if(!require(tools))install.packages("tools");library(tools)
  
  d <- now()
  mdl <- md5sum(dir("scenes-list/", pattern = "*", full.names = TRUE))  
  report <- data.frame(
    date = rep(strftime(d,"%d-%m-%Y"),times = length(mdl)),
    time = rep(strftime(d,"%X"),times = length(mdl)),
    filenames = names(mdl),
    md5_chksum = as.vector(mdl)
    )
  write.csv(report,"scenes-list/report.csv")
}

#' getWRS - Download the WRS shapefiles and save them in rds objects
#' 
#' @author Martin Jung
#' @return saves the pathrow data as rds objects

getWRS <- function(){
  if(!require(rgdal)){install.packages("rgdal");library(rgdal)}
  
  if(file.exists("wrs1_asc.desc.rds")){print("WRS1 rds container already exists.")} else {
    print("Downloading WRS 1 shapefile...")
    tryCatch({
      download.file(url = "http://landsat.usgs.gov/documents/wrs1_asc_desc.zip",destfile = "wrs1_asc_desc.zip",
                    quiet = FALSE, mode = "wb")},silent = FALSE, condition = print("ERROR. Could not be downloaded!")
    )
    # Unzipping and deleting
    unzip("wrs1_asc_desc.zip")
    file.remove("wrs1_asc_desc.zip")
    
    wrs1 <- readOGR("wrs1_asc_desc/","wrs1_asc_desc")
    saveRDS(wrs1,"wrs1_asc.desc.rds")
    rm(wrs1)
  }
  
  if(file.exists("wrs2_asc.desc.rds")){print("WRS2 rds container already exists.")} else {
    print("Downloading WRS 2 shapefile...")
    tryCatch({
      download.file(url = "http://landsat.usgs.gov/documents/wrs2_asc_desc.zip",destfile = "wrs2_asc_desc.zip",
                    quiet = FALSE, mode = "wb")},silent = FALSE, condition = print("ERROR. Could not be downloaded!")
    )
    # Unzipping and deleting
    unzip("wrs2_asc_desc.zip")
    file.remove("wrs2_asc_desc.zip")
    
    wrs2 <- readOGR("wrs2_asc_desc/","wrs2_asc_desc")
    saveRDS(wrs2,"wrs2_asc.desc.rds")
    rm(wrs2)
  }
  
}

#' getLandsatRowPath - Returns the row and path of given coordinates
#' 
#' @author Martin Jung
#' @param df.coord A data.frame containing x and y coordinates
#' @param pry The projection of x and y
#' @return saves the pathrow data as rds objects

getLandsatRowPath <- function(df.coord,prj="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
  if(!require(rgdal)){install.packages("rgdal");library(rgdal)}
  if(!require(stringr)){install.packages("stringr");library(stringr)}
  
  # Sanity check
  # Expects a data.frame with lat long or other x y coordinates
  if(!( class(df.coord) %in% "data.frame")) stop("Please provide a data.frame!")
  if(any(!( c("x","y") %in% names(df.coord)))) stop("Please make sure the data.frame has a 'x' and 'y' column !")
  
  # Convert data.frame to spatialPoints object
  coordinates(df.coord) <- ~y+x
  proj4string(df.coord) <- CRS(prj)
  
  # Check if rds files are present
  if(!file.exists("wrs1_asc.desc.rds")) stop("wrs1_asc.desc.rds does not exist. Run getWRS first.")
  if(!file.exists("wrs2_asc.desc.rds")) stop("wrs2_asc.desc.rds does not exist. Run getWRS first.")
  
  # Loading
  wrs1 <- readRDS("wrs1_asc.desc.rds")
  wrs2 <- readRDS("wrs2_asc.desc.rds")
  
  # Returns the respective path and wrs1 and wrs2
  print("Intersecting spatial objects")
  ov1 <- over(df.coord,wrs1)  
  ov2 <- over(df.coord,wrs2)
  
  out <- as.data.frame(df.coord)
  out$wrs1.row <- as.character(ov1$ROW_)
  out$wrs1.path <- as.character(ov1$PATH)
  out$wrs2.row <- as.character(ov2$ROW)
  out$wrs2.path <- as.character(ov2$PATH)
  x <- as.character(ov1$SEQ)
  y <- as.character(ov2$PR_ID)
  if(x == y) out$PR_ID <- x else{
    out$wrs1.pr_id <- x
    out$wrs2.pr_id <- y
  }
  return(out)
}
  