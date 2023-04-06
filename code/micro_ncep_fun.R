
micro_ncep_fun <- function(){
  setwd("C:/Users/Katerina/Documents/Master thesis/micro")
  
  library(NicheMapR)
  library(microclima)
  library(raster)
  library(lubridate)
  
  lon <- -8.5898
  lat <- 41.1073 
  dstart <- "20/05/2022"
  dfinish <- "30/06/2022"
  minshade <- 0
  maxshade <- 30
  rainmult <- 1
  Thcond <- 2.5
  SpecHeat <- 870
  Density <- 2.45
  BulkDensity <- 1.3
  windfac <- 1
  REFL <- 0.2
  cap <- FALSE
  SLE <- 0.95
  warm <- 0
  Usrhyt <- 0.01
  clearsky <- FALSE
  soilgrids <- 0
  spatial <- NA
  ERR <- 1
  
  # dem <- microclima::get_dem(r = NA, lat = lat, lon = lon, resolution = 30, 
                       #      zmin = -20, xdims = 100, ydims = 100)
  # save(dem, file = "dem.Rda")
  
  load("dem.Rda")
  
  elev <- raster::extract(dem, cbind(lon, lat))[1]        
  xy <- data.frame(x = lon, y = lat)
  sp::coordinates(xy) = ~x + y
  sp::proj4string(xy) = "+init=epsg:4326"
  xy <- as.data.frame(sp::spTransform(xy, raster::crs(dem)))
  slope <- raster::terrain(dem, unit = "degrees")
  slope <- raster::extract(slope, xy)
  aspect <- raster::terrain(dem, opt = "aspect", unit = "degrees")
  aspect <- raster::extract(aspect, xy)
  ha36 <- 0
  for (i in 0:35) {
    har <- microclima::horizonangle(dem, i * 10, raster::res(dem)[1])
    ha36[i + 1] <- atan(raster::extract(har, xy)) * (180/pi)
  }
  hori <- spline(x = ha36, n = 24, method =  'periodic')$y
  hori[hori < 0] <- 0
  hori[hori > 90] <- 90
  
  micro <- micro_ncep(loc = c(lon, lat), SLE = SLE, warm = warm, soilgrids = soilgrids, 
                      dstart = dstart, dfinish = dfinish, Usrhyt = Usrhyt,
                      slope = slope, aspect = aspect, REFL = REFL,
                      hori = hori, minshade = minshade, maxshade = maxshade, 
                      rainmult = rainmult, BulkDensity =  BulkDensity, cap = cap,
                      Density = Density, Thcond = Thcond, SpecHeat = SpecHeat,
                      windfac = windfac, spatial = spatial, ERR = ERR, dem = dem, 
                      runshade = 1, run.gads = 1, snowmodel = 1, runmoist = 1,
                      save = 2)
  
  load("micro_ncep.Rda")
  
  return(micro)
}