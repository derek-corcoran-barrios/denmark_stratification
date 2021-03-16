library(janitor)
library(raster)
library(tidyverse)
library(vegan)

Layers <- read_rds("BIO_DK.rds")

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
    dplyr::filter(calinski == max(calinski)) %>% 
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

Selected <- Results2 %>% 
  dplyr::filter(calinski == max(calinski)) %>% 
  pull(Groups)

Partition <- Test$partition %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  pull(Selected)

Groups2 <- Layers[[1]]

values(Groups2)[ForK$ID] <- Partition  

plot(Groups2, colNA = "black")
