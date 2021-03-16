library(fasterize)
library(janitor)
library(raster)
library(sf)
library(tidyverse)
library(vegan)

Layers <- read_rds("BIO_DK.rds")
Title_9 <- read_sf("O:/Nat_Ecoinformatics/B_Read/Denmark/LandCover/BES_NATURTYPER_SHAPE/bes_naturtyper.shp") %>% 
  dplyr::select(NATYP_NAVN) 

Type <-Title_9 %>% pull(NATYP_NAVN) %>% unique()
NatID <- tibble(NATYP_NAVN = Type) %>% tibble::rowid_to_column(var = "ID")
saveRDS(NatID, "NatID.rds")

Title_9 <- Title_9 %>% merge(NatID) %>% fasterize(raster = Layers[[1]], field = "ID")
saveRDS(Title_9, "Naturtype.rds")


SoilRasters <- list.files(path = "O:/AUIT_Geodata/Denmark/Natural_ressources/Soil_geology",pattern = ".tif", recursive = T, full.names = T)
SoilRasters <- SoilRasters[str_detect(SoilRasters,pattern = ".aux", negate = T)]
SoilRasters <- SoilRasters[str_detect(SoilRasters,pattern = ".xml", negate = T)]
SoilRasters <- SoilRasters[str_detect(SoilRasters,pattern = ".ovr", negate = T)]

SoilRasters <- SoilRasters[c(6,45,33,78,87)]

Soils <- list()
for(i in 1:length(SoilRasters)){ 
  Soils[[i]] <- stack(SoilRasters[i]) %>% resample(Layers[[1]])
}

Soils <- Soils %>% purrr::reduce(stack)
saveRDS(Soils, "Soils.rds")


Climate <- stack("o:/Nat_Ecoinformatics/B_Read/DataL")

ForK <- as.data.frame(Layers) %>%
  mutate_all(scale) %>% 
  tibble::rowid_to_column(var = "ID") %>% 
  dplyr::filter_all(~!is.na(.x)) 
  
Test <- cascadeKM(ForK[,-1],inf.gr = 2, sup.gr = 20, iter = 100)
  
plot(Test)
  
beepr::beep(8)
  
Results <- Test$results %>% t %>% 
    as.data.frame()
  
Results$Groups <- make_clean_names(rownames(Results))
  
Selected <- Results %>% 
    dplyr::filter(calinski == max(calinski))
    pull(Groups)
  
Partition <- Test$partition %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  pull(Selected)

Groups <- Layers[[1]]

values(Groups)[ForK$ID] <- Partition  

plot(Groups, colNA = "black")


Test2 <- cascadeKM(ForK[,-1],inf.gr = 2, sup.gr = 10, iter = 100, criterion = "ssi")

plot(Test2)

beepr::beep(8)

Results2 <- Test2$results %>% t %>% 
  as.data.frame()

Results2$Groups <- make_clean_names(rownames(Results2))

Selected2 <- Results2 %>% 
  dplyr::filter(ssi == max(ssi)) %>% 
  dplyr::pull(Groups)

Partition2 <- Test2$partition %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  pull(Selected2)

Groups2 <- Layers[[1]]

values(Groups2)[ForK$ID] <- Partition2 

plot(Groups2, colNA = "black")

saveRDS(Groups2, "TestGroups.rds")


TestGroups_DF <- Groups2 %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  rename(Group  =  bio_01) %>% 
  mutate(Group = as.character(Group))

DK <- readRDS("DK_Shape.rds")

ggplot() + 
  geom_tile(data = TestGroups_DF, aes(x = x, y = y, fill = Group)) +
  geom_sf(data = DK, alpha = 0) +
  theme_bw() +
  scale_fill_viridis_d() + 
  labs(y = NULL, x = NULL)
