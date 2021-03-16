library(sf)
library(raster)
library(tidyverse)

dir.create("Temp")
rasterOptions(tmpdir = "Temp")

P1 <- read_sf("1005pkt/Subset2021_1005pkt.shp")
P2 <- read_sf("150pkt/Subset2021_150pkt.shp")

Points <- rbind(P1, P2)

DK <- getData(name = "GADM", country = "DNK", level = 1) %>% 
  st_as_sf() %>% 
  st_transform(crs = st_crs(Points))

ggplot() +
  geom_sf(data = DK) + 
  geom_sf(data = Points) +
  theme_bw()


dir.create("CHELSA")

library(readr)
Paths <- read_csv("Biopaths.txt", col_names = FALSE) %>% 
  dplyr::rename(Paths = X1) %>% 
  mutate(Layer = str_replace_all(Paths,"https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/bio/CHELSA_bio10", "bio")) %>% 
  dplyr::filter(str_detect(Layer, "CHELSA", negate = T))


e <- new("Extent", xmin = 0.279897527389391, xmax = 20.197669941918, 
         ymin = 50.3703546146877, ymax = 60.5744824940345)

for(i in 1:nrow(Paths)){
  dir.create("Temp")
  rasterOptions(tmpdir = "Temp")
  download.file(Paths$Paths[i], paste0("CHELSA/", Paths$Layer[i]))
  Temp <- raster(paste0("CHELSA/", Paths$Layer[i])) %>% 
    crop(e) %>% 
    projectRaster(crs = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs") %>% 
    crop(DK)
  writeRaster(Temp, paste0("CHELSA/", Paths$Layer[i]), overwrite = T)
  unlink("Temp", recursive = T)
  message(paste(i, "of", nrow(Paths), "ready!", Sys.time()))
}



Names <- Paths$Layer %>% str_remove_all(".tif")

Bios <- list.files(path = "CHELSA/", full.names = T) %>% 
  purrr::map(raster) %>% 
  reduce(stack)

names(Bios) <- Names

PointBios <- raster::extract(Bios, Points)
PointBios <- as.data.frame(PointBios)
