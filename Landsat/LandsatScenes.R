
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
  