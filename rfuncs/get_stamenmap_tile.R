get_stamenmap_tile <- function(maptype, zoom, x, y, force = FALSE, messaging = TRUE, where = tempdir()){
  
  # check arguments
  is.wholenumber <- function (x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  
  stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
  stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
  stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
  
  # format url http://tile.stamen.com/[maptype]/[zoom]/[x]/[y].jpg
  if(maptype %in% c("watercolor")){
    filetype <- "png"
  } else {
    filetype <- "png"
  }
  url <- sprintf("http://tile.stamen.com/%s/%i/%i/%i.%s", maptype, zoom, x, y, filetype)
  
  # lookup in archive
  tile <- file_drawer_get(url)
  if (!is.null(tile) && !force) return(tile)
  
  # grab if not in archive
  tmp <- tempfile()
  downloaded <- suppressWarnings(try(
    download.file(url, destfile = tmp, quiet = !messaging, mode = "wb"), silent = TRUE
  ))
  
  # message url
  download_error <- inherits(downloaded, "try-error")
  if(download_error) {
    message(paste0("Source FAILED : ", url))
  } else {
    message(paste0("Source : ", url))
  }
  
  # read in/format tile
  if (download_error) {
    
    tile <- array(NA, dim = c(256L, 256L))
    
  } else {
    
    # read in
    if(filetype == "jpg"){
      tile <- readJPEG(tmp)
    } else {
      tile <- png::readPNG(tmp)
    }
    
    # convert to colors
    # toner-lines treated differently for alpha
    if(maptype %in% c("toner-hybrid", "toner-labels", "toner-lines",
      "terrain-labels", "terrain-lines")){
      tile <- t(apply(tile, 1:2, function(x) rgb(x[1], x[2], x[3], x[4])))
    } else {
      tile <- t(apply(tile, 2, rgb))
    }
    
  }
  
  
  # determine bbox of map. note : not the same as the argument bounding box -
  # the map is only a covering of the bounding box extent the idea is to get
  # the lower left tile and the upper right tile and compute their bounding boxes
  # tiles are referenced by top left of tile, starting at 0,0
  # see http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  lonlat_upperleft <- XY2LonLat(x, y, zoom)
  lonlat_lowerright <- XY2LonLat(x, y, zoom, 255, 255)
  bbox <- c(
    left = lonlat_upperleft$lon,
    bottom = lonlat_lowerright$lat,
    right = lonlat_lowerright$lon,
    top = lonlat_upperleft$lat
  )
  bb <- data.frame(
    ll.lat = unname(bbox["bottom"]),
    ll.lon = unname(bbox["left"]),
    ur.lat = unname(bbox["top"]),
    ur.lon = unname(bbox["right"])
  )
  
  # format
  class(tile) <- c("ggmap", "raster")
  attr(tile, "bb") <- bb
  
  # store
  if(!download_error) file_drawer_set(url, tile)
  
  # return
  tile
}

