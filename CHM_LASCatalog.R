library(lidR)
library(raster)
library(dplyr)

lascatA2 <- readLAScatalog("C:/LiDAR_2018/LAS-PointCloud/")

las_check(lascatA2)

opt_select(ctg) <- "xyz" #Only use x y and z information (no information on intensity needed)
opt_chunk_buffer <- 50 #change buffer to 50 feet

# We can restrict processing to only selected tiles covering a buffer around city limits (list generated using Qgis)
a2tiles <- read.csv("Index_A2LidarOnly.csv") %>%
  mutate(filename = paste0("C:/LiDAR_2018/LAS-PointCloud/",Name,".las"))
lascatA2$processed <- FALSE
lascatA2$processed[lascatA2$filename %in% a2tiles$filename] <- TRUE
  

# Code from: https://cran.r-project.org/web/packages/lidR/vignettes/lidR-catalog-apply-examples.html
filter_noise <- function(chunk, sensitivity)
{
  p95 <- grid_metrics(chunk, ~quantile(Z, probs = 0.95), 10)
  chunk <- merge_spatial(chunk, p95, "p95")
  chunk <- filter_poi(chunk, Z < p95*sensitivity)
  chunk$p95 <- NULL
  return(chunk)
}

norm_chm <- function(chunk){
  las <- readLAS(chunk)
  if(is.empty(las)) return(NULL)
  
  filtered <- filter_noise(las, sensitivity = 1.2)
  filtered <- normalize_height(filtered, tin(), na.rm=TRUE)
  filtered <- filter_poi(filtered, buffer == 0)
  return(filtered)
  # chm <- grid_canopy(filtered, res, pitfree(c(0,10,20), c(0,6), subcircle = 1))
  # 
  # #need to crop out buffer
  # clip_extent <- raster::extent(chunk)
  # chm <- raster::crop(chm, crop_extent)
  # return(chm)
}


opt_output_files(lascatA2) <-  "C:/LiDAR_2018/LAS-PointCloud/Output/{*}_norm"
lascatA2_norm <- catalog_apply(lascatA2, norm_chm)

lascatA2_norm <- readLAScatalog("C:/LiDAR_2018/LAS-PointCloud/Output/")

opt_output_files(lascatA2_norm) <-  "C:/LiDAR_2018/LAS-PointCloud/CHM_5/{*}_chm5"
chmA2_5 <- grid_canopy(lascatA2_norm, 5, pitfree(c(0,10,20), c(0,6), subcircle = 1))

opt_output_files(lascatA2_norm) <-  "C:/LiDAR_2018/LAS-PointCloud/CHM_3/{*}_chm3"
chmA2_3 <- grid_canopy(lascatA2_norm, 3, pitfree(c(0,10,20), c(0,6), subcircle = 1))

opt_output_files(lascatA2_norm) <-  "C:/LiDAR_2018/LAS-PointCloud/CHM_1/{*}_chm1"
chmA2_1 <- grid_canopy(lascatA2_norm, 1, pitfree(c(0,10,20), c(0,6), subcircle = 1))

