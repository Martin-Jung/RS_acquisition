#' getMODISTilename 
#' - A simple script to extract the MODIS horizontal and vertical tile numbers
#' 
#' @author Martin Jung
#' @param df.coord A data.frame with the columns x and y 
#' @param prj The default projection for the points in the data frame
#' @param show Should the tiles be shown on a map ? (requires ggmap)
#' @return a data.frame containing the tile numbers

getMODISTilename <- function(df.coord,prj="+proj=longlat +ellps=WGS84",show=FALSE){
  # ------------ # 
  # Returns the MODIS grid identifier for a given number of coordinates  
  library("rgdal", character.only = T, warn.conflicts = F, quietly = T)
  library("stringr", character.only = T, warn.conflicts = F, quietly = T)
  
  # Sanity check
  # Expects a data.frame with lat long or other x y coordinates
  if(!( class(df.coord) %in% "data.frame")) stop("Please provide a data.frame!")
  if(any(!( c("x","y") %in% names(df.coord)))) stop("Please make sure the data.frame has a 'x' and 'y' column !")
  
  # Convert data.frame to spatialPoints object
  coordinates(df.coord) <- ~y+x
  proj4string(df.coord) <- CRS(prj)
  
  # Check if modis grid shape is present, otherwise download 
  if(length(list.files(".","modis_sinusoidal.zip"))==0) {
    print("MODIS sinusoidal grid not found. Downloading ...")
    tryCatch({
      download.file(url = "http://gis.cri.fmach.it/data/modis_sinusoidal.zip",destfile = "modis_sinusoidal.zip",
      quiet = FALSE, mode = "wb")},silent = FALSE, condition = print("ERROR. Could not be downloaded!")
    )
  }
  
  # Unzip to tempdir and load object
  unzip("modis_sinusoidal.zip",exdir = paste0(tempdir(),"/modisgrid/"),junkpaths = TRUE)  
  tiles <- readOGR(paste0(tempdir(),"/modisgrid/"),"modis_sinusoidal_grid_world",verbose = FALSE)
  
  # Transform points to grid projection
  sin.coord <- spTransform(df.coord,CRSobj = CRS(proj4string(tiles)))
  
  # Returns the respective MODIS tiles of the points  
  ov <- over(sin_coord,tiles)
  
  if(show){
    s <- subset(tiles,cat%in%ov$cat)
    s <- spTransform(s,CRS("+proj=longlat +ellps=WGS84"))
    library("ggmap", character.only = T, warn.conflicts = F, quietly = T)    
    world <- map_data("world")
    ggplot() + geom_path(data = world, 
                         aes(long, lat, group = group)) +
      geom_polygon(data= fortify(s),aes(long,lat,group=group),fill="red",alpha=0.85) +
      theme_bw() + labs(x="Longitude",y="Latitude",title=paste0("Total coverage \n",nrow(ov)," - MODIS tile"))
    rm(s,world) # clean up
  }
  ov$cat <- NULL
  # Pad zeros if necessary
  ov$h <- str_pad(ov$h, 2,pad = 0)
  ov$v <- str_pad(ov$v, 2,pad = 0)
  ov$name <- paste0("h",ov$h,"v",ov$v)
  return(ov)
}

