# Step 4: Combine with TRY
library(tidyverse)
library(data.table)

setwd("c:/Daten/iDiv4/sDiv/2023sUnderfoot/Databases/")

all.data3 <- read.csv("3_All/All_databases11.csv", fileEncoding = "UTF-8")
all.data3 <- as_tibble(all.data3)
names(all.data3)
str(all.data3)
# 10,453 × 51

all.data3 %>% filter(grepl("var.", Aggregated_name, fixed=T)) %>% pull(Aggregated_name) #0
all.data3 %>% filter(grepl("subsp.", Aggregated_name, fixed=T)) %>% pull(Aggregated_name) #0

table(all.data3$BBB_Growthform, exclude=NULL)
table(all.data3$BPT, all.data3$is.clonal, exclude=NULL)  
CLOPLA5 %>% filter(BPT==1 & is.clonal) %>% pull(Aggregated_name)
CLOPLA5 %>% filter(BPT==1 & is.na(is.clonal)) %>% pull(Aggregated_name)
CLOPLA5 %>% filter(BPT==4 & is.na(is.woody.above)) %>% pull(Aggregated_name)
all.data3 %>% filter(BPT==5.2 & is.na(is.clonal)) %>% pull(Aggregated_name)
CLOPLA4 %>% filter(Aggregated_name=="Gentiana prostrata")
CLOPLA5 %>% filter(Aggregated_name=="Chrysanthemum integrifolium") %>% as.data.frame()
all.data3 %>% filter(Aggregated_name=="Salvia rosmarinus") %>% as.data.frame()
all.data3 %>% filter(BPT==6 & is.na(is.woody.above)) %>% pull(Aggregated_name)
all.data3 %>% filter(Aggregated_name=="Stomatanthes oblongifolius") %>% as.data.frame()
all.data3 %>% filter(grepl("Asclepias incarnata",Aggregated_name)) %>% as.data.frame()
all.data3 %>% filter(grepl("Populus euphratica",Aggregated_name)) %>% as.data.frame()
all.data3 %>% filter(grepl("Hesperostipa comata",Aggregated_name)) %>% as.data.frame()
all.data3 %>% filter(grepl("Hesperostipa comata",RSIP_Original_name)) %>% as.data.frame()

all.data3 <- all.data3 %>% 
  mutate(is.clonal = if_else(Aggregated_name=="Poranthera microphylla", F, is.clonal),
         is.resprouting = if_else(Aggregated_name=="Poranthera microphylla", F, is.resprouting),
         BBB_Growthform = if_else(Aggregated_name=="Poranthera microphylla", "annual", BBB_Growthform),
         BPT = if_else(Aggregated_name=="Poranthera microphylla", 1, BPT),
         is.clonal = if_else(Aggregated_name=="Krascheninnikovia ceratoides", F, is.clonal),
         is.woody.above = if_else(Aggregated_name=="Krascheninnikovia ceratoides", F, is.woody.above),
         is.clonal = if_else(Aggregated_name=="Krascheninnikovia ceratoides", F, is.clonal),
         is.clonal = if_else(Aggregated_name=="Betula occidentalis", F, is.clonal),
         is.clonal = if_else(Aggregated_name=="Chenopodium leptophyllum", F, is.clonal),
         BPT = if_else(Aggregated_name=="Chrysanthemum integrifolium", 5.1, BPT),
         is.woody.below = if_else(Aggregated_name=="Chrysanthemum integrifolium", T, is.woody.below),
         is.clonal = if_else(Aggregated_name=="Chrysanthemum integrifolium", F, is.clonal),
         is.clonal = if_else(Aggregated_name=="Euphorbia glyptosperma", F, is.clonal),
         is.clonal = if_else(Aggregated_name=="Gentiana prostrata", F, is.clonal),
         is.clonal = if_else(Aggregated_name=="Tomostima reptans", F, is.clonal),
         is.woody.above = if_else(Aggregated_name=="Asclepias tuberosa", F, is.woody.above),
         is.woody.below = if_else(Aggregated_name=="Asclepias tuberosa", T, is.woody.below),
         is.resprouting = if_else(Aggregated_name=="Asclepias tuberosa", T, is.resprouting),
         is.clonal = if_else(Aggregated_name=="Asclepias tuberosa", F, is.clonal),
         BPT = if_else(Aggregated_name=="Asclepias tuberosa", 5.1, BPT),
         is.woody.above = if_else(Aggregated_name=="Asclepias verticillata", F, is.woody.above),
         is.woody.below = if_else(Aggregated_name=="Asclepias verticillata", F, is.woody.below),
         is.clonal = if_else(Aggregated_name=="Asclepias verticillata", T, is.clonal),
         BPT = if_else(Aggregated_name=="Asclepias verticillata", 3, BPT),
         is.woody.above = if_else(Aggregated_name=="Asclepias incarnata", F, is.woody.above),
         is.woody.below = if_else(Aggregated_name=="Asclepias incarnata", F, is.woody.below),
         is.resprouting = if_else(Aggregated_name=="Asclepias incarnata", T, is.resprouting),
         is.clonal = if_else(Aggregated_name=="Asclepias incarnata", F, is.clonal),
         BPT = if_else(Aggregated_name=="Asclepias incarnata", 2, BPT),
  )
table(all.data3$BPT, all.data3$is.clonal, exclude=NULL)           

table(all.data3$BPT, all.data3$is.woody.below, exclude=NULL)
table(all.data3$BPT, all.data3$is.woody.above, exclude=NULL)

Backbone_sUnderfoot <- read.csv("sUnderfoot_Backbone/Backbone_sUnderfoot.csv", fileEncoding = "UTF-8")
backbone_sPlot <- read_rds("c:/Daten/iDiv4/sPlot/sPlot4.1/Data/backbone_sPlot4.1.RDS")
str(backbone_sPlot) # 413663 × 25
backbone_sPlot$Harmonized_name_wfo[grep("  var.", backbone_sPlot$Harmonized_name_wfo)]
backbone_sPlot$Resolved_name[grep("  var.", backbone_sPlot$Resolved_name)]
# there is a mistake in the sPlot backbone, with two "  " before "var."
# which originated from the WFO backbone
backbone_sPlot$Resolved_name <- gsub("  var."," var.",backbone_sPlot$Resolved_name, fixed=T)
# there is a mistake in the sPlot backbone, with two "  " before "subsp."
# which originated from the WFO backbone
backbone_sPlot$Harmonized_name_wfo[grep("  subsp.", backbone_sPlot$Harmonized_name_wfo)]
backbone_sPlot$Resolved_name[grep("  subsp.", backbone_sPlot$Resolved_name)]
backbone_sPlot$Resolved_name <- gsub("  subsp."," subsp.",backbone_sPlot$Resolved_name, fixed=T)


TRY1 <- fread("TRY/TRYPlantMorphologicalTraits/30186.txt")
# fileEncoding = "latin1"

#,  
#                 nrows = 1000000, quote="\"")
str(TRY1) # 27205222 obs. of  29 variables
names(TRY1)
table(TRY1$TraitName, exclude = NULL)
'
                                                   
                                          24084397 
                          Plant clonal growth form 
                                              9045 
                       Plant functional type (PFT) 
                                            100338 
                                 Plant growth form 
                                           2326396 
           Plant growth form detailed consolidated 
                                            165540 
       Plant growth form factor (according to FIA) 
                                                43 
             Plant growth form simple consolidated 
                                            213372 
                        Plant lifespan (longevity) 
                                             70333 
                        Plant resprouting capacity 
                                             19704 
            Plant vegetative regeneration capacity 
                                              7228 
Plant vegetative reproduction: clonal growth organ 
                                               535 
                                   Plant woodiness 
                                            208291 
'
TRY2 <- TRY1 %>% filter(TraitName=="Plant growth form") %>% 
  dplyr::select(SpeciesName, OrigValueStr) %>% 
  rename(Plant_growth_form = OrigValueStr)
length(TRY2$Plant_growth_form) #2326396
TRY3 <- TRY1 %>% filter(TraitName=="Plant woodiness") %>% 
  dplyr::select(SpeciesName, OrigValueStr) %>% 
  rename(Plant_woodiness=OrigValueStr)
length(TRY3$Plant_woodiness) #208291
TRY4 <- TRY1 %>% filter(TraitName=="Plant resprouting capacity") %>% 
  dplyr::select(SpeciesName, OrigValueStr) %>% 
  rename(Plant_resprouting_capacity=OrigValueStr)
length(TRY4$Plant_resprouting_capacity) #19704
TRY5 <- TRY1 %>% filter(TraitName=="Plant vegetative regeneration capacity") %>% 
  dplyr::select(SpeciesName, OrigValueStr) %>% 
  rename(Plant_vegetative_regeneration_capacity=OrigValueStr)
length(TRY5$Plant_vegetative_regeneration_capacity) #7228
table(TRY5$Plant_vegetative_regeneration_capacity, exclude = NULL)
TRY6 <- TRY1 %>% filter(TraitName=="Plant lifespan (longevity)") %>% 
  dplyr::select(SpeciesName, OrigValueStr) %>% 
  rename(Plant_lifespan=OrigValueStr)
length(TRY6$Plant_lifespan) #70333
table(TRY6$Plant_lifespan, exclude = NULL)

index1 <- match(TRY2$SpeciesName, backbone_sPlot$Resolved_name)
table(!is.na(index1))
'   FALSE    TRUE 
 536113 1790283 '
TRY2$Resolved_name <- backbone_sPlot$Resolved_name[index1]
index2 <- match(TRY2$SpeciesName[is.na(TRY2$Resolved_name)], backbone_sPlot$Edited_name)
table(!is.na(index2))
'  FALSE   TRUE 
259878 276235 '
TRY2$Resolved_name[is.na(TRY2$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]
index3 <- match(TRY2$SpeciesName[is.na(TRY2$Resolved_name)], backbone_sPlot$Original_name)
table(!is.na(index3))
'  FALSE   TRUE 
254691   5187 '
TRY2$Resolved_name[is.na(TRY2$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]
index4 <- match(TRY2$SpeciesName[is.na(TRY2$Resolved_name)], Backbone_sUnderfoot$Resolved_name)
table(!is.na(index4))
' FALSE   TRUE 
254677     14 '
TRY2$Resolved_name[is.na(TRY2$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index4]
index5 <- match(TRY2$SpeciesName[is.na(TRY2$Resolved_name)], Backbone_sUnderfoot$Edited_name)
table(!is.na(index5))
'  FALSE   TRUE 
254346    331 '
TRY2$Resolved_name[is.na(TRY2$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index5]
index6 <- match(TRY2$SpeciesName[is.na(TRY2$Resolved_name)], Backbone_sUnderfoot$Original_name)
table(!is.na(index6))
'  FALSE 
254346 '

TRY2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 207451
unique(TRY2$Resolved_name[grep(" var.",TRY2$Resolved_name,fixed=T)]) # 3074
unique(TRY2$Resolved_name[grep("  var.",TRY2$Resolved_name,fixed=T)]) # 0
unique(TRY2$Resolved_name[grep(" subsp.",TRY2$Resolved_name,fixed=T)]) # 1770
unique(TRY2$Resolved_name[grep("  subsp.",TRY2$Resolved_name,fixed=T)]) # 0

# check for duplicate and missing taxon names 
# remove subspecies and variants
TRY2 <- TRY2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 204506

index1 <- match(TRY3$SpeciesName, backbone_sPlot$Resolved_name)
table(!is.na(index1))
'  FALSE   TRUE 
 46876 161415 '
TRY3$Resolved_name <- backbone_sPlot$Resolved_name[index1]
index2 <- match(TRY3$SpeciesName[is.na(TRY3$Resolved_name)], backbone_sPlot$Edited_name)
table(!is.na(index3))
' FALSE   TRUE 
254691   5187 '
TRY3$Resolved_name[is.na(TRY3$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]
index3 <- match(TRY3$SpeciesName[is.na(TRY3$Resolved_name)], backbone_sPlot$Original_name)
table(!is.na(index3))
' FALSE  TRUE 
 8334   225 '
TRY3$Resolved_name[is.na(TRY3$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]
index4 <- match(TRY3$SpeciesName[is.na(TRY3$Resolved_name)], Backbone_sUnderfoot$Resolved_name)
table(!is.na(index4))
' FALSE 
 8334  '
# TRY3$Resolved_name[is.na(TRY3$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index4]
index5 <- match(TRY3$SpeciesName[is.na(TRY3$Resolved_name)], Backbone_sUnderfoot$Edited_name)
table(!is.na(index5))
' FALSE  TRUE 
 8273    61 '
TRY3$Resolved_name[is.na(TRY3$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index5]
index6 <- match(TRY3$SpeciesName[is.na(TRY3$Resolved_name)], Backbone_sUnderfoot$Original_name)
table(!is.na(index6))
'FALSE 
 8273  '

TRY3 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 76113
unique(TRY3$Resolved_name[grep(" var.",TRY3$Resolved_name,fixed=T)]) # 641
unique(TRY3$Resolved_name[grep("  var.",TRY3$Resolved_name,fixed=T)]) # 0
unique(TRY3$Resolved_name[grep(" subsp.",TRY3$Resolved_name,fixed=T)]) #1037
unique(TRY3$Resolved_name[grep("  subsp.",TRY3$Resolved_name,fixed=T)]) #0

# remove subspecies and variants
TRY3 <- TRY3 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY3 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 75590

index1 <- match(TRY4$SpeciesName, backbone_sPlot$Resolved_name)
table(!is.na(index1))
' FALSE  TRUE 
17880  1824 '
TRY4$Resolved_name <- backbone_sPlot$Resolved_name[index1]
index2 <- match(TRY4$SpeciesName[is.na(TRY4$Resolved_name)], backbone_sPlot$Edited_name)
table(!is.na(index3))
' FALSE  TRUE 
 8334   225 '
TRY4$Resolved_name[is.na(TRY4$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]
index3 <- match(TRY4$SpeciesName[is.na(TRY4$Resolved_name)], backbone_sPlot$Original_name)
table(!is.na(index3))
' FALSE  TRUE 
17464    61 '
TRY4$Resolved_name[is.na(TRY4$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]
index4 <- match(TRY4$SpeciesName[is.na(TRY4$Resolved_name)], Backbone_sUnderfoot$Resolved_name)
table(!is.na(index4))
' FALSE 
17464 '
# TRY4$Resolved_name[is.na(TRY4$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index4]
index5 <- match(TRY4$SpeciesName[is.na(TRY4$Resolved_name)], Backbone_sUnderfoot$Edited_name)
table(!is.na(index5))
' FALSE 
17464 '
# TRY4$Resolved_name[is.na(TRY4$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index5]
index6 <- match(TRY4$SpeciesName[is.na(TRY4$Resolved_name)], Backbone_sUnderfoot$Original_name)
table(!is.na(index6))
'FALSE 
17464  '

TRY4 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 1886
unique(TRY4$Resolved_name[grep(" var.",TRY4$Resolved_name,fixed=T)]) # 15
unique(TRY4$Resolved_name[grep("  var.",TRY4$Resolved_name,fixed=T)]) # 0
unique(TRY4$Resolved_name[grep(" subsp.",TRY4$Resolved_name,fixed=T)]) #16
unique(TRY4$Resolved_name[grep("  subsp.",TRY4$Resolved_name,fixed=T)]) # 0

# remove subspecies and variants
TRY4 <- TRY4 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY4 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 1877

index1 <- match(TRY5$SpeciesName, backbone_sPlot$Resolved_name)
table(!is.na(index1))
' FALSE  TRUE 
 2384  4844 '
TRY5$Resolved_name <- backbone_sPlot$Resolved_name[index1]
index2 <- match(TRY5$SpeciesName[is.na(TRY5$Resolved_name)], backbone_sPlot$Edited_name)
table(!is.na(index3))
' FALSE  TRUE 
17464    61  '
TRY5$Resolved_name[is.na(TRY5$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]
index3 <- match(TRY5$SpeciesName[is.na(TRY5$Resolved_name)], backbone_sPlot$Original_name)
table(!is.na(index3))
' FALSE  TRUE 
 1847    23 '
TRY5$Resolved_name[is.na(TRY5$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]
index4 <- match(TRY5$SpeciesName[is.na(TRY5$Resolved_name)], Backbone_sUnderfoot$Resolved_name)
table(!is.na(index4))
'FALSE 
 1847 '
# TRY5$Resolved_name[is.na(TRY5$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index4]
index5 <- match(TRY5$SpeciesName[is.na(TRY5$Resolved_name)], Backbone_sUnderfoot$Edited_name)
table(!is.na(index5))
' FALSE  TRUE 
 1846     1 '
TRY5$Resolved_name[is.na(TRY5$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index5]
index6 <- match(TRY5$SpeciesName[is.na(TRY5$Resolved_name)], Backbone_sUnderfoot$Original_name)
table(!is.na(index6))
'FALSE 
 1846 '

TRY5 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 2722
unique(TRY5$Resolved_name[grep(" var.",TRY5$Resolved_name,fixed=T)]) # 6
unique(TRY5$Resolved_name[grep("  var.",TRY5$Resolved_name,fixed=T)]) # 0
unique(TRY5$Resolved_name[grep(" subsp.",TRY5$Resolved_name,fixed=T)]) # 111
unique(TRY5$Resolved_name[grep("  subsp.",TRY5$Resolved_name,fixed=T)]) # 0

# remove subspecies and variants
TRY5 <- TRY5 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY5 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 2719

index1 <- match(TRY6$SpeciesName, backbone_sPlot$Resolved_name)
table(!is.na(index1))
'FALSE  TRUE 
30051 40282 '
TRY6$Resolved_name <- backbone_sPlot$Resolved_name[index1]
index2 <- match(TRY6$SpeciesName[is.na(TRY6$Resolved_name)], backbone_sPlot$Edited_name)
table(!is.na(index3))
' FALSE  TRUE 
 1847    23 '
TRY6$Resolved_name[is.na(TRY6$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]
index3 <- match(TRY6$SpeciesName[is.na(TRY6$Resolved_name)], backbone_sPlot$Original_name)
table(!is.na(index3))
' ALSE  TRUE 
22623   398 '
TRY6$Resolved_name[is.na(TRY6$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]
index4 <- match(TRY6$SpeciesName[is.na(TRY6$Resolved_name)], Backbone_sUnderfoot$Resolved_name)
table(!is.na(index4))
'FALSE  TRUE 
22621     2 '
TRY6$Resolved_name[is.na(TRY6$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index4]
index5 <- match(TRY6$SpeciesName[is.na(TRY6$Resolved_name)], Backbone_sUnderfoot$Edited_name)
table(!is.na(index5))
' FALSE  TRUE 
22581     40 '
TRY6$Resolved_name[is.na(TRY6$Resolved_name)] <- Backbone_sUnderfoot$Resolved_name[index5]
index6 <- match(TRY6$SpeciesName[is.na(TRY6$Resolved_name)], Backbone_sUnderfoot$Original_name)
table(!is.na(index6))
'FALSE 
22581 '

TRY6 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 17121
unique(TRY6$Resolved_name[grep(" var.",TRY6$Resolved_name,fixed=T)]) # 132
unique(TRY6$Resolved_name[grep("  var.",TRY6$Resolved_name,fixed=T)]) # 0
unique(TRY6$Resolved_name[grep(" subsp.",TRY6$Resolved_name,fixed=T)]) # 571
unique(TRY6$Resolved_name[grep("  subsp.",TRY6$Resolved_name,fixed=T)]) # 0

# remove subspecies and variants
TRY6 <- TRY6 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY6 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
#  17044

### Growthforms, TRY2
index3 <- match(all.data3$Aggregated_name, TRY2$Aggregated_name)
table(!is.na(index3), exclude=NULL)
'FALSE  TRUE 
  852  9601  '
all.data3$Growthform_TRY <- TRY2$Plant_growth_form[index3]

table(all.data3$Growthform_TRY, exclude=NULL)
table(all.data3$BBB_Growthform,  exclude = NULL)

all.data3$Growthform_unified <- all.data3$BBB_Growthform
all.data3$Growthform_unified[all.data3$Growthform_unified==""] <- NA
table(all.data3$Growthform_unified,  exclude = NULL)


index3 <- match(all.data3$Aggregated_name, TRY3$Aggregated_name)
table(!is.na(index3))
'FALSE  TRUE 
 2137  8316  '
table(TRY3$Plant_woodiness[index3])
all.data3$is.woody_TRY <- TRY3$Plant_woodiness[index3]

index3 <- match(all.data3$Aggregated_name, TRY4$Aggregated_name)
table(!is.na(index3))
'FALSE  TRUE 
 9741   712  '
table(TRY4$Plant_resprouting_capacity[index3])
all.data3$Resprouting_TRY <- TRY4$Plant_resprouting_capacity[index3]

index3 <- match(all.data3$Aggregated_name, TRY5$Aggregated_name)
table(!is.na(index3))
'FALSE  TRUE 
 8546  1908  '
table(TRY5$Plant_vegetative_regeneration_capacity[index3])
all.data3$Plant_vegetative_regeneration_capacity_TRY <- TRY5$Plant_vegetative_regeneration_capacity[index3]

index3 <- match(all.data3$Aggregated_name, TRY6$Aggregated_name)
table(!is.na(index3))
'FALSE  TRUE 
 5164  5289 '
table(TRY6$Plant_lifespan[index3])
all.data3$Plant_lifespan_TRY <- TRY6$Plant_lifespan[index3]

table(all.data3$is.clonal,  exclude = NULL)
'FALSE  TRUE  <NA> 
 3937  3251  3265  '

table(all.data3$is.woody.above,  exclude = NULL)
'FALSE  TRUE  <NA> 
 5560  1908  2985  '

table(all.data3$is.resprouting,  exclude = NULL)
'FALSE  TRUE  <NA> 
  828  1768  7857 '


table(all.data3$Growthform_TRY,  exclude = NULL)
table(all.data3$Growthform_unified,  exclude = NULL)
all.data3$Growthform_unified[all.data3$Growthform_TRY=="always Climbing using tendrils" & 
          is.na(all.data3$Growthform_unified)] <- "climber"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="always Spread climbing" & 
          is.na(all.data3$Growthform_unified)] <- "climber"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Annual forb" & 
          is.na(all.data3$Growthform_unified)] <- "annual"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Annual Herb" & 
          is.na(all.data3$Growthform_unified)] <- "annual"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="aquatic" & 
          is.na(all.data3$Growthform_unified)] <- "aquatic"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Aquatic" & 
          is.na(all.data3$Growthform_unified)] <- "aquatic"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="aquatic salt water" & 
          is.na(all.data3$Growthform_unified)] <- "marine"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="C4 grass" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="CARNIVORE" & 
          is.na(all.data3$Growthform_unified)] <- "carnivore"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="climber" & 
          is.na(all.data3$Growthform_unified)] <- "climber"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Climber" & 
          is.na(all.data3$Growthform_unified)] <- "climber"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="climber/parasitic" & 
          is.na(all.data3$Growthform_unified)] <- "parasitic"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="climber/woody" & 
          is.na(all.data3$Growthform_unified)] <- "climber"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="conifer" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="drwarf shrub" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Dwarf shrub" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="epiphyte" & 
          is.na(all.data3$Growthform_unified)] <- "epiphyte"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="fern" & 
          is.na(all.data3$Growthform_unified)] <- "fern"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="FERN ALLY" & 
          is.na(all.data3$Growthform_unified)] <- "fern"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="fern/non-woody" & 
          is.na(all.data3$Growthform_unified)] <- "fern"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="fern" & 
          is.na(all.data3$Growthform_unified)] <- "fern"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Ferns and allies (Lycophytes)" & 
          is.na(all.data3$Growthform_unified)] <- "fern"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="forb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Forb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Forb/herb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Forb/herb, Shrub" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Forb/herb, Subshrub" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Forb/herb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Forb/herb, Vine" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Forbs" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Free-floating plants" & 
          is.na(all.data3$Growthform_unified)] <- "aquatic"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="graminoid" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Graminoid" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="GRAMINOID" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="graminoid/non-wood" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Graminoids" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="grass" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Grass" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="grass (clonal)" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Grasses&Sedges" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Grass" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Hemiepiphite" & 
          is.na(all.data3$Growthform_unified)] <- "hemiepiphyte"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="hemiepiphyte" & 
          is.na(all.data3$Growthform_unified)] <- "hemiepiphyte"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Herb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Herb (H)" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb." & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb/climber/non-woody" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb/hemiparasitic/non-woody" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb/hemiparasitic/non-woody" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb/non-woody" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb/parasitic/non-woody" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb/shrub" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herb/succulent/non-woody" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Herbaceous Forb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herbaceous legume" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herbaceous plant" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="herbs" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Hydrophytes" & 
          is.na(all.data3$Growthform_unified)] <- "aquatic"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="leguminous forb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="liana" & 
          is.na(all.data3$Growthform_unified)] <- "liana"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Liana" & 
          is.na(all.data3$Growthform_unified)] <- "liana"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="lianas" & 
          is.na(all.data3$Growthform_unified)] <- "liana"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Lianas (wody climbers)" & 
          is.na(all.data3$Growthform_unified)] <- "liana"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Palm" & 
          is.na(all.data3$Growthform_unified)] <- "palm"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="PALM" & 
          is.na(all.data3$Growthform_unified)] <- "palm"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Parasite" & 
          is.na(all.data3$Growthform_unified)] <- "parasite"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="parasitic" & 
          is.na(all.data3$Growthform_unified)] <- "parasite"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="pasture grass" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Perennial forb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="perennial forb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="perennial herb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="perennial leguminous herb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Sedge" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="SEDGE" & 
          is.na(all.data3$Growthform_unified)] <- "graminoid"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="shrub" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Shrub" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="shrub or chamaephyt" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Shrub, Subshrub" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Shrub, Tree" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Shrub, Vine" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Shrub/Subshrub" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="shrub/succulent/woody" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="shrub/tree" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Shrub/Tree" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Shrub/Tree intermediate" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="shrub/tree/woody" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="shrub/woody" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="shrubs" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="sub-shrub" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Sub-Shrub (Chamaephyte)" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="submerged" & 
          is.na(all.data3$Growthform_unified)] <- "aquatic"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Submerged attached to the substrate" & 
          is.na(all.data3$Growthform_unified)] <- "aquatic"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="subshrub" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Subshrub, Forb/herb" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Subshrub, Shrub, Forb/herb" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="tree" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Tree" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="TREE" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Tree (deciduous)" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Tree (evergreen)" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY==" tree / shrub" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Tree shrub intermediate" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Tree, Shrub" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Tree/Large Shrub" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="tree/palmoid/woody" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="tree/shrub" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="tree/woody" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="trees" & 
          is.na(all.data3$Growthform_unified)] <- "tree"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Vine, Forb/herb" & 
          is.na(all.data3$Growthform_unified)] <- "forb"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Vine, Shrub" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Vine, Subshrub" & 
          is.na(all.data3$Growthform_unified)] <- "subshrub"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Vines (non-woody climbers)" & 
          is.na(all.data3$Growthform_unified)] <- "climber"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Woody Liana" & 
          is.na(all.data3$Growthform_unified)] <- "liana"
all.data3$Growthform_unified[all.data3$Growthform_TRY=="Woody shrub" & 
          is.na(all.data3$Growthform_unified)] <- "shrub"
table(all.data3$Growthform_unified,  exclude = NULL)
'                                                   annual 
                                                       787 
                                  annual, clonal perennial 
                                                         2 
                                              annual, forb 
                                                         9 
                        annual, forb, non-clonal perennial 
                                                         1 
                                     annual, HEMI-PARASITE 
                                                         2 
                          annual, herb/succulent/non-woody 
                                                         1 
                              annual, non-clonal perennial 
                                                         2 
                                             annual, shrub 
                                                         1 
                       annual, Stem ascending to prostrate 
                                                         1 
                                        annual, Stem erect 
                                                         7 
                                    annual, Stem prostrate 
                                                         1 
                                                   aquatic 
                                                         4 
                                                   climber 
                                                        33 
                                          clonal perennial 
                                                      2008 
                            clonal perennial, clonal woody 
                                                         1 
                                   clonal perennial, erect 
                                                         1 
                                    clonal perennial, forb 
                                                       124 
                         clonal perennial, forb, graminoid 
                                                         1 
                clonal perennial, forb, herbaceous dicotyl 
                                                         1 
                clonal perennial, forb, Herbaceous Monocot 
                                                         1 
                        clonal perennial, forb, Stem erect 
                                                         2 
                               clonal perennial, graminoid 
                                                        42 
                   clonal perennial, graminoid, Stem erect 
                                                         1 
                           clonal perennial, HEMI-PARASITE 
                                                         2 
            clonal perennial, herb/hemiparasitic/non-woody 
                                                         1 
                              clonal perennial, herbaceous 
                                                         1 
                      clonal perennial, herbaceous dicotyl 
                                                        10 
clonal perennial, herbaceous dicotyl, non-clonal perennial 
                                                         1 
            clonal perennial, herbaceous dicotyl, subshrub 
                                                         1 
                      clonal perennial, Herbaceous Monocot 
                                                        15 
                    clonal perennial, herbaceous monocotyl 
                                                         8 
                                      clonal perennial, No 
                                                         1 
                    clonal perennial, non-clonal perennial 
                                                         9 
        clonal perennial, non-clonal perennial, Stem erect 
                                                         1 
                           clonal perennial, non-succulent 
                                                         4 
                                   clonal perennial, shrub 
                                                         3 
             clonal perennial, Stem ascending to prostrate 
                                                        18 
                              clonal perennial, Stem erect 
                                                       113 
                          clonal perennial, Stem prostrate 
                                                         6 
                                    clonal perennial, tree 
                                                         3 
                                              clonal woody 
                                                        98 
                            clonal woody, non-clonal woody 
                                                         2 
                                       clonal woody, shrub 
                                                         2 
                                  clonal woody, Stem erect 
                                                         1 
                                        clonal woody, tree 
                                                         2 
                                                  epiphyte 
                                                         1 
                                                      fern 
                                                        23 
                                                   Fibrous 
                                                        19 
                                                      forb 
                                                      1456 
                                           forb, graminoid 
                                                         2 
                                  forb, Herbaceous Monocot 
                                                         1 
                                               forb, liana 
                                                         1 
                                forb, non-clonal perennial 
                                                        71 
   forb, non-clonal perennial, Stem ascending to prostrate 
                                                         1 
                    forb, non-clonal perennial, Stem erect 
                                                         1 
                                    forb, non-clonal woody 
                                                         1 
                                               forb, shrub 
                                                         3 
                                            forb, subshrub 
                                                        13 
                                                 graminoid 
                                                       292 
                                             HEMI-PARASITE 
                                                         1 
                       HEMI-PARASITE, non-clonal perennial 
                                                         1 
                                              hemiepiphyte 
                                                         1 
                              herb/hemiparasitic/non-woody 
                                                         3 
                                                herb/shrub 
                                                         2 
                                  herb/succulent/non-woody 
                                                         2 
                                                herbaceous 
                                                         1 
                                        herbaceous dicotyl 
                                                         1 
                  herbaceous dicotyl, non-clonal perennial 
                                                         3 
                                        Herbaceous Monocot 
                                                         2 
                                      herbaceous monocotyl 
                                                         1 
                                                     liana 
                                                        24 
                                                        No 
                                                         3 
                                  No, non-clonal perennial 
                                                         1 
                                      non-clonal perennial 
                                                       994 
                       non-clonal perennial, non-succulent 
                                                         4 
                               non-clonal perennial, shrub 
                                                         3 
         non-clonal perennial, Stem ascending to prostrate 
                                                        13 
                          non-clonal perennial, Stem erect 
                                                        23 
                      non-clonal perennial, Stem prostrate 
                                                         4 
                            non-clonal perennial, subshrub 
                                                         2 
                           non-clonal perennial, succulent 
                                                         1 
                                          non-clonal woody 
                                                       243 
                                   non-clonal woody, shrub 
                                                         9 
                              non-clonal woody, Stem erect 
                                                         2 
                                    non-clonal woody, tree 
                                                         5 
                                             non-succulent 
                                                        10 
                                                      palm 
                                                         1 
                                                  parasite 
                                                         5 
                                                  Parasite 
                                                         2 
                                                     shrub 
                                                       919 
                        shrub, Stem ascending to prostrate 
                                                         1 
                                           shrub, subshrub 
                                                         5 
                                               shrub, tree 
                                                         2 
                               Stem ascending to prostrate 
                                                         3 
                                                Stem erect 
                                                        20 
                                                  subshrub 
                                                       195 
                                            subshrub, tree 
                                                         1 
                                                 succulent 
                                                         1 
                                                      tree 
                                                      1266 
                                                      <NA> 
                                                      1453 '
all.data3 %>% filter(Growthform_unified=="epiphyte")
# Dimerandra emarginata: orchid
all.data3 %>% filter(Growthform_unified=="creeper")
# 0

# Fill in further information from TRY
table(all.data3$is.clonal, exclude=NULL)
'FALSE  TRUE  <NA> 
 3937  3251  3265 '

all.data3$is.clonal[grepl("annual",all.data3$Growthform_unified) & 
                      is.na(all.data3$is.clonal)] <- F
all.data3$is.clonal[grepl("clonal",all.data3$Growthform_unified) & 
                      !grepl("non-clonal",all.data3$Growthform_unified) &
                      is.na(all.data3$is.clonal)] <- T
all.data3$is.clonal[grepl("non-clonal",all.data3$Growthform_unified) & 
                      is.na(all.data3$is.clonal)] <- F
table(all.data3$is.clonal,  exclude = NULL)
'FALSE  TRUE  <NA> 
 3939  3253  3261 '

table(all.data3$is.woody.below, exclude=NULL)
'FALSE  TRUE  <NA> 
 4822  2373  3258  '
table(all.data3$is.woody_TRY, exclude=NULL)
all.data3$is.woody.below[all.data3$is.woody_TRY=="semi-woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$is.woody_TRY=="Semi-woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$is.woody_TRY=="Suffrutex" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$is.woody_TRY=="w" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$is.woody_TRY=="W" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$is.woody_TRY=="woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$is.woody_TRY=="Woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$is.woody_TRY=="woody at base" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$is.woody_TRY=="Y" & 
                     is.na(all.data3$is.woody.below)] <- T
# all.data3$is.woody.below[all.data3$is.woody_TRY=="False" & 
#                      is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$is.woody_TRY=="Grass&Sedges" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$is.woody_TRY=="h" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$is.woody_TRY=="H" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$is.woody_TRY=="Herb" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$is.woody_TRY=="Herbaceous" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$is.woody_TRY=="not woody" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$is.woody_TRY=="non-woody" & 
                     is.na(all.data3$is.woody.below)] <- F

all.data3$is.woody.below[all.data3$Growthform_TRY=="Annual Herb" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="Annual forb" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="climber/woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="conifer" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="fern/non-woody" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herb/climber/non-woody" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herb/hemiparasitic/non-woody" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herb/non-woody" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herb/shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="herb/succulent/non-woody" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herbaceous" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="Herbaceous" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="Herbaceous Dicot" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="Herbaceous dicots" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herbaceous dicotyl" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="Herbaceous Forb" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herbaceous legume" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="Herbaceous Monocot" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herbaceous monocotyl" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herbaceous plant" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="herbs" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="liana" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Liana" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="lianas" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Lianas (wody climbers)" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Lianas and climbers" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="non-woody" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="Palm" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="PALM" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Sedge" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="SEDGE" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Shrub (S)" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="shrub or chamaephyt" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Shrub, Subshrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Shrub, Tree" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Shrub, Vine" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Shrub/Subshrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="shrub/succulent/woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="shrub/tree" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Shrub/Tree" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Shrub/Tree intermediate" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="shrub/tree/woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="shrub/woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="shrubs" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="stem-succulent/woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="sub-shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Sub-Shrub (Chamaephyte)" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="subshrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Subshrub, Forb/herb" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Subshrub, Shrub, Forb/herb" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="succulent/non-woodys" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="tree" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Tree" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="TREE" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Tree (deciduous)" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Tree (evergreen)" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="tree / shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Tree (evergreen)" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Tree shrub intermediate" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Tree, Shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Tree/Large Shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="tree/palmoid/woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="tree/shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="tree/woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="trees" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Vine, Forb/herb" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="Vine, Shrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Vine, Subshrub" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Vines (non-woody climbers)" & 
                     is.na(all.data3$is.woody.below)] <- F
all.data3$is.woody.below[all.data3$Growthform_TRY=="woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Woody" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Woody Liana" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="woody plant" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="woody plants" & 
                     is.na(all.data3$is.woody.below)] <- T
all.data3$is.woody.below[all.data3$Growthform_TRY=="Woody shrub" & 
                     is.na(all.data3$is.woody.below)] <- T

table(all.data3$is.woody.below,  exclude = NULL)
'FALSE  TRUE  <NA> 
 5986  3855   612 '

table(all.data3$is.woody.below, all.data3$RSIP_is.woody, exclude = NULL)
'        Herbaceous Woody <NA>
  FALSE       1319    79 4588
  TRUE          35   952 2868
  <NA>         190    95  327'

all.data3 %>% filter(is.na(BPT) & is.woody.below & all.data3$RSIP_is.woody=="Herbaceous")  %>% 
  dplyr::select(Aggregated_name,BPT, is.woody.below, is.woody.above, is.resprouting, is.clonal, Growthform_unified)  %>% print(n=50)
# 25
all.data3 %>% filter(is.na(BPT) & !is.woody.below & all.data3$RSIP_is.woody=="Woody")  %>% 
  dplyr::select(Aggregated_name) # 38
all.data3 %>% filter(!is.woody.below & all.data3$RSIP_is.woody=="Woody")  %>% 
  dplyr::select(Aggregated_name)  %>%  print(n=100) # 79
all.data3 %>% filter(is.na(BPT) & grepl("annual",Growthform_unified))  %>% 
  dplyr::select(Aggregated_name,BPT, is.woody.below, is.woody.above, is.resprouting, is.clonal, Growthform_unified)  %>% print(n=50)
# 1
all.data3 %>% filter(Aggregated_name=="Thymus pulegioides")  %>% as.data.frame() 
# is.woody.below == F, because Growthform_unified had been "clonal perennial" before, thus BPT == 3
all.data3 %>% filter(grepl("Thymus pulegioides", Aggregated_name))  %>% as.data.frame() 
# most Erica is woody below!

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Doniophyton anomalum", 2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Doniophyton anomalum", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Doniophyton anomalum", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Doniophyton anomalum", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Doniophyton anomalum", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Plantago ovata", 1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Plantago ovata", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Plantago ovata", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Plantago ovata", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Plantago ovata", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Petrosimonia brachiata", 1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Petrosimonia brachiata", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Petrosimonia brachiata", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Petrosimonia brachiata", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Petrosimonia brachiata", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Rubus lasiococcus", 6.1, BPT))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Rubus lasiococcus", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Rubus lasiococcus", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Rubus lasiococcus", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Rubus lasiococcus", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Rubus pedatus", 6.1, BPT))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Rubus pedatus", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Rubus pedatus", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Rubus pedatus", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Rubus pedatus", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT= if_else(is.na(BPT) & is.woody.below & RSIP_is.woody=="Herbaceous", 5.1, BPT))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Convolvulus prostratus", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Convolvulus prostratus", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Convolvulus prostratus", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Convolvulus prostratus", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Convolvulus prostratus", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Deverra tortuosa", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Deverra tortuosa", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Deverra tortuosa", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Deverra tortuosa", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Deverra tortuosa", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Trianthema hereroense", 1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Trianthema hereroense", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Trianthema hereroense", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Trianthema hereroense", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Trianthema hereroense", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Zinnia grandiflora", 6.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Zinnia grandiflora", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Zinnia grandiflora", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Zinnia grandiflora", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Zinnia grandiflorae", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Ajania fruticulosa", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Ajania fruticulosa", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Ajania fruticulosa", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Ajania fruticulosa", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Ajania fruticulosa", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Erica herbacea", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Erica herbacea", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Erica herbacea", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Erica herbacea", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Erica herbacea", T, is.resprouting))

# Thymus pulegioides  subsp. pannonicus
all.data3 %>% filter(is.na(is.woody.below)) %>% pull(Aggregated_name)
all.data3 %>% filter(Aggregated_name=="Rubus lasiococcus") %>% as.data.frame()

# all.data3$is.woody[all.data3$Aggregated_name=="Calluna vulgaris"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Cassiope tetragona"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Empetrum nigrum"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Erica herbacea"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Kalmia procumbens"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Phyllodoce caerulea"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Salix retusa"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Thymus pulegioides"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Thymus pulegioides  subsp. pannonicus"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Vaccinium myrtillus"] <- T
# all.data3$is.woody[all.data3$Aggregated_name=="Vaccinium uliginosum"] <- T

all.data3 <- all.data3 %>% mutate(is.woody.below = if_else(is.na(is.woody.below) & 
                                                             RSIP_is.woody == "Herbaceous", F,
                                  is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.below = if_else(is.na(is.woody.below) & 
                                                             RSIP_is.woody == "Woody", T,
                                                           is.woody.below))

table(all.data3$is.woody.below, all.data3$RSIP_is.woody, exclude = NULL)
'        Herbaceous Woody <NA>
  FALSE       1511    74 4588
  TRUE          33  1052 2868
  <NA>           0     0  327'
#Backbone_sUnderfoot[grep("  var",Backbone_sUnderfoot$Resolved_name),]

#all.data3$is.woody[all.data3$Resolved_name=="Potentilla gracilis  var. fastigiata"] <- F
table(all.data3$Growthform_unified)
#all.data3a <- all.data3
#all.data3 <- all.data3a
table(all.data3$RSIP_Growthform, exclude=NULL)
table(all.data3$Growthform_unified, all.data3$RSIP_Growthform, exclude = NULL)

# all.data3$Growthform_unified[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Growthform)] <- if_else(
#   all.data3$RSIP_Growthform[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Growthform)]=="Grass","graminoid",NA)
# all.data3$Growthform_unified[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Growthform)] <- if_else(
#   all.data3$RSIP_Growthform[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Growthform)]=="Semi-shrub","shrub",NA)
# all.data3$Growthform_unified[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Growthform)] <- if_else(
#   all.data3$RSIP_Growthform[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Growthform)]=="Shrub","shrub",NA)
# all.data3$Growthform_unified[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Growthform)] <- if_else(
#   all.data3$RSIP_Growthform[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Growthform)]=="Tree","tree",NA)
# table(all.data3$Growthform_unified, all.data3$RSIP_Growthform, exclude = NULL)

table(all.data3$RSIP_Lifespan, exclude=NULL)
table(all.data3$BPT==1, all.data3$RSIP_Lifespan, exclude = NULL)
'           A    P <NA>
  FALSE   42 1103 5159
  TRUE   173   20  657
  <NA>   132 1256 1912'

all.data3 %>% filter(is.na(BPT) & RSIP_Lifespan=="A")  %>% 
  dplyr::select(Aggregated_name) #132
# I do not believe that they are annuals!

# all.data3$Growthform_unified[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Lifespan)] <- if_else(
#   all.data3$RSIP_Lifespan[is.na(all.data3$Growthform_unified) & !is.na(all.data3$RSIP_Lifespan)]=="A","annual",NA)

# Resprouting
table(all.data3$Resprouting_TRY,  exclude = NULL)
unusable.categories <- sort(unique(all.data3$Resprouting_TRY))
unusable.categories <- unusable.categories[c(1:18,46)]
resprouting.categories <- sort(unique(all.data3$Resprouting_TRY))
resprouting.categories <- resprouting.categories[c(20:22,25:27,33:45,47:54)]
nonresprouting.categories <- sort(unique(all.data3$Resprouting_TRY))
nonresprouting.categories <- nonresprouting.categories[c(19,28:32)]

all.data3 <- all.data3 %>% mutate(is.resprouting = if_else(is.na(is.resprouting) & 
                                                             Resprouting_TRY %in% resprouting.categories, T,
                                                           is.resprouting))
all.data3 <- all.data3 %>% mutate(is.resprouting = if_else(is.na(is.resprouting) & 
                                                             Resprouting_TRY %in% nonresprouting.categories, F,
                                                           is.resprouting))

table(all.data3$is.resprouting,  exclude = NULL)
'FALSE  TRUE  <NA> 
  937  1877  7639  '

# use is.woody, is.clonal, is.resprouting for BPT
table(all.data3$BPT, exclude=NULL)
'   1    2    3    4    5  5.1  5.2    6  6.1  6.2 <NA> 
 850 1279 2665  193    8  400 1229    7   77  446 3299 '
table(all.data3$Growthform_unified, exclude=NULL)

all.data3 %>% filter(is.na(BPT) & !is.woody.below & !is.woody.above & !is.resprouting &
                       grepl("annual",Growthform_unified))  %>% 
  dplyr::select(Aggregated_name) #0
all.data3 %>% filter(is.na(BPT) & grepl("annual",Growthform_unified))  %>% 
  dplyr::select(Aggregated_name) #0

all.data3 %>% filter(is.na(CSI) & BPT==1)  %>% 
  dplyr::select(Aggregated_name) #27

all.data3 <- all.data3 %>% mutate(CSI = if_else(is.na(CSI) & BPT==1, 0, CSI))

all.data3 %>% filter(is.na(CSI) & grepl("annual",Growthform_unified))  %>% 
  dplyr::select(Aggregated_name) #0

all.data3 %>% filter(is.na(BPT) & !is.woody.below & !is.clonal)  %>% 
  dplyr::select(Aggregated_name) # 7
all.data3 %>% filter(is.na(BPT) & !is.woody.below & !is.clonal)  %>% 
  dplyr::select(Aggregated_name, Growthform_unified, Plant_lifespan_TRY) # 7

all.data3 <- all.data3 %>% mutate(BPT = if_else(is.na(BPT) & !is.woody.below & !is.clonal &
                  Plant_lifespan_TRY %in% (c("Perennial","perennial","Biennial")), 2, BPT))

all.data3 %>% filter(is.na(BPT) & !is.woody.below & is.clonal)  %>% 
  dplyr::select(Aggregated_name) # 56

all.data3 <- all.data3 %>% mutate(BPT = if_else(is.na(BPT) & !is.woody.below & is.clonal, 3, BPT))

all.data3 %>% filter(is.na(BPT) & is.woody.below & !is.clonal)  %>% 
  dplyr::select(Aggregated_name) #0

all.data3 %>% filter(is.na(BPT) & is.woody.below & !is.clonal & !is.resprouting)  %>% 
  dplyr::select(Aggregated_name) #0
all.data3 %>% filter(is.na(BPT) & is.woody.below & is.woody.above & !is.clonal)  %>% 
  dplyr::select(Aggregated_name) #0
all.data3 %>% filter(is.na(BPT) & Growthform_unified=="palm")  %>% 
  dplyr::select(Aggregated_name) #1
all.data3 %>% filter(is.na(BPT) & Growthform_unified=="palm")  %>% as.data.frame()
all.data3 %>% filter(grepl("Prestoea", Aggregated_name))  %>% as.data.frame()

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Euterpe oleracea", 6.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Euterpe oleracea", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Euterpe oleracea", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Euterpe oleracea", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Euterpe oleracea", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Prestoea montana", 6.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Prestoea montana", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Prestoea montana", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Prestoea montana", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Prestoea montana", T, is.resprouting))

all.data3 %>% filter(is.na(BPT) & !is.woody.below & is.clonal)  %>% 
  dplyr::select(Aggregated_name) #0
all.data3 %>% filter(is.na(BPT) & is.woody.below & is.clonal)  %>% 
  dplyr::select(Aggregated_name) #0
all.data3 %>% filter(is.na(BPT) & !is.woody.below & is.resprouting)  %>% 
  dplyr::select(Aggregated_name, BPT, is.clonal) # 12

all.data3 %>% filter(is.na(BPT) & !is.woody.below & is.resprouting & !is.clonal)  %>% 
  dplyr::select(Aggregated_name, BPT) #0
all.data3 %>% filter(Aggregated_name=="Ulex parviflorus")  %>% as.data.frame()
all.data3 %>% filter(is.na(BPT) & !is.woody.below & is.resprouting & !is.clonal)  %>% 
  dplyr::select(Aggregated_name, BPT) #0

all.data3 %>% filter(grepl("Abies", Aggregated_name))  %>% as.data.frame() 

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Amphipogon caricinus", 3, BPT))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Amphipogon caricinus", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Amphipogon caricinus", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Amphipogon caricinus", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Amphipogon caricinus", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Aristida oligantha", 1, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Aristida oligantha", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Aristida oligantha", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Aristida oligantha", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Aristida oligantha", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Aristida oligantha", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cenchrus americanus", 1, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Cenchrus americanus", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Cenchrus americanus", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cenchrus americanus", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cenchrus americanus", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Cenchrus americanus", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cymbopogon obtectus", 2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Cymbopogon obtectus", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Cymbopogon obtectus", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cymbopogon obtectus", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cymbopogon obtectus", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Cymbopogon obtectus", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Eulalia aurea", 3, BPT))
# all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Eulalia aurea", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Eulalia aurea", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Eulalia aurea", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Eulalia aurea", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Eulalia aurea", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Panicum decompositum", 2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Panicum decompositum", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Panicum decompositum", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Panicum decompositum", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Panicum decompositum", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Ratibida pinnata", 3, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Ratibida pinnata", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Ratibida pinnata", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Ratibida pinnata", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Ratibida pinnata", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Sisyrinchium albidum", 2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Sisyrinchium albidum", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Sisyrinchium albidum", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Sisyrinchium albidum", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Sisyrinchium albidum", F, is.clonal))

all.data3 %>% filter(is.na(BPT) & is.woody.below & is.resprouting & !is.clonal)  %>% 
  dplyr::select(Aggregated_name, BPT) #0
all.data3 %>% filter(is.na(BPT) & is.woody.below & is.resprouting)  %>% 
  dplyr::select(Aggregated_name, BPT, is.woody.above, is.clonal)  %>% print(n=100)
# 69

all.data3 %>% filter(Aggregated_name=="Coffea arabica")  %>% as.data.frame() 
all.data3 %>% filter(grepl("Amelanchier", Aggregated_name))  %>% as.data.frame() 


all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Abies concolor", 4, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Abies concolor", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Abies concolor", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Abies concolor", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Abies concolor", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Abies concolor", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Acacia auriculiformis", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Acacia auriculiformis", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Acacia auriculiformis", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Acacia auriculiformis", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Acacia auriculiformis", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Acacia coriacea", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Acacia coriacea", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Acacia coriacea", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Acacia coriacea", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Acacia coriacea", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Acacia crassicarpa", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Acacia crassicarpa", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Acacia crassicarpa", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Acacia crassicarpa", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Acacia crassicarpa", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Acacia irrorata", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Acacia irrorata", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Acacia irrorata", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Acacia irrorata", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Acacia irrorata", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Acacia pulchella", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Acacia pulchella", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Acacia pulchella", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Acacia pulchella", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Acacia pulchella", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Acacia saligna", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Acacia saligna", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Acacia saligna", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Acacia saligna", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Acacia saligna", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Acacia tetragonophylla", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Acacia tetragonophylla", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Acacia tetragonophylla", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Acacia tetragonophylla", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Acacia tetragonophylla", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Aleurites moluccanus", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Aleurites moluccanus", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Aleurites moluccanus", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Aleurites moluccanus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Aleurites moluccanus", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Amelanchier alnifolia", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Amelanchier alnifolia", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Amelanchier alnifolia", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Amelanchier alnifolia", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Amelanchier alnifolia", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Amelanchier utahensis", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Amelanchier utahensis", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Amelanchier utahensis", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Amelanchier utahensis", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Amelanchier utahensis", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Arenaria montana", 5.1, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Arenaria montana", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Arenaria montana", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Arenaria montana", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Arenaria montana", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Aspidosperma tomentosum", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Aspidosperma tomentosum", 0, CSI))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Aspidosperma tomentosum", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Aspidosperma tomentosum", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Aspidosperma tomentosum", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Atriplex nummularia", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Atriplex nummularia", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Atriplex nummularia", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Atriplex nummularia", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Betula populifolia", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Betula populifolia", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Betula populifolia", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Betula populifolia", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Bursera simaruba", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Bursera simaruba", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Bursera simaruba", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Bursera simaruba", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Celtis australis", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Celtis australis", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Celtis australis", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Celtis australis", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cercocarpus betuloides", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Cercocarpus betuloides", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cercocarpus betuloides", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cercocarpus betuloides", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Chrysothamnus viscidiflorus", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Chrysothamnus viscidiflorus", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Chrysothamnus viscidiflorus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Chrysothamnus viscidiflorus", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cinnamomum camphora", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Cinnamomum camphora", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cinnamomum camphora", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cinnamomum camphora", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cliffortia ruscifolia", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Cliffortia ruscifolia", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cliffortia ruscifolia", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cliffortia ruscifolia", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Coffea arabica", 4, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Coffea arabica", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Coffea arabica", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Coffea arabica", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Coffea arabica", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cordia alliodora", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Cordia alliodora", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cordia alliodora", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cordia alliodora", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cytisus striatus", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Cytisus striatus", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cytisus striatus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cytisus striatus", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Daphne gnidium", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Daphne gnidium", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Daphne gnidium", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Daphne gnidium", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Didymopanax morototoni", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Didymopanax morototoni", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Didymopanax morototoni", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Didymopanax morototoni", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Dombeya rotundifolia", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Dombeya rotundifolia", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Dombeya rotundifolia", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Dombeya rotundifolia", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Eucalyptus globulus", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Eucalyptus globulus", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Eucalyptus globulus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Eucalyptus globulus", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Eucalyptus gomphocephala", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Eucalyptus gomphocephala", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Eucalyptus gomphocephala", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Eucalyptus gomphocephala", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Fraxinus ornus", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Fraxinus ornus", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Fraxinus ornus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Fraxinus ornus", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Grayia spinosa", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Grayia spinosa", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Grayia spinosa", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Grayia spinosa", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Hakea ilicifolia", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Hakea ilicifolia", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Hakea ilicifolia", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Hakea ilicifolia", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Hibbertia subvaginata", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Hibbertia subvaginata", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Hibbertia subvaginata", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Hibbertia subvaginata", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Hymenaea courbaril", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Hymenaea courbaril", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Hymenaea courbaril", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Hymenaea courbaril", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Jacksonia furcellata", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Jacksonia furcellata", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Jacksonia furcellata", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Jacksonia furcellata", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Larix kaempferi", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Larix kaempferi", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Larix kaempferi", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Larix kaempferi", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Larix occidentalis", 4, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Larix occidentalis", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Larix occidentalis", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Larix occidentalis", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Larix occidentalis", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Lespedeza bicolor", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Lespedeza bicolor", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Lespedeza bicolor", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Lespedeza bicolor", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Myrsine coriacea", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Myrsine coriacea", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Myrsine coriacea", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Myrsine coriacea", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Nerium oleander", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Nerium oleander", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Nerium oleander", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Nerium oleander", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Nothofagus antarctica", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Nothofagus antarctica", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Nothofagus antarctica", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Nothofagus antarctica", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Nothofagus antarctica", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Parkinsonia microphylla", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Parkinsonia microphylla", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Parkinsonia microphylla", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Parkinsonia microphylla", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Philotheca spicata", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Philotheca spicatar", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Philotheca spicata", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Philotheca spicata", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pinus canariensis", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Pinus canariensis", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pinus canariensis", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pinus canariensis", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pinus caribaea", 4, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Pinus caribaea", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pinus caribaea", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pinus caribaea", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Pinus caribaea", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pinus massoniana", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Pinus massoniana", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pinus massoniana", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pinus massoniana", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pinus patula", 4, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Pinus patula", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pinus patula", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pinus patula", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Pinus patula", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pistacia terebinthus", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Pistacia terebinthus", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pistacia terebinthus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pistacia terebinthusa", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Populus angustifolia", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Populus angustifolia", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Populus angustifolia", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Populus angustifolia", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Populus fremontii", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Populus fremontii", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Populus fremontii", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Populus fremontii", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Prunus armeniaca", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Prunus armeniaca", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Prunus armeniaca", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Prunus armeniaca", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Prunus mahaleb", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Prunus mahaleb", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Prunus mahaleb", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Prunus mahaleb", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Purshia tridentata", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Purshia tridentata", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Purshia tridentata", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Purshia tridentata", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Purshia tridentata", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Quercus nigra", 5.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Quercus nigra", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Quercus nigra", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Quercus nigra", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Quercus nigra", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Quercus pubescens", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Quercus pubescens", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Quercus pubescens", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Quercus pubescens", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Quercus turbinella", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Quercus turbinella", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Quercus turbinella", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Quercus turbinella", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Quercus wislizeni", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Quercus wislizeni", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Quercus wislizeni", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Quercus wislizeni", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Retama sphaerocarpa", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Retama sphaerocarpa", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Retama sphaerocarpa", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Retama sphaerocarpaa", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Rosa nutkana", 6.2, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Rosa nutkana", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Rosa nutkana", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Rosa nutkana", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Salix amygdaloides", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Salix amygdaloides", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Salix amygdaloides", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Salix amygdaloides", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Senegalia greggii", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Senegalia greggii", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Senegalia greggii", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Senegalia greggii", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Senegalia nigrescens", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Senegalia nigrescens", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Senegalia nigrescens", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Senegalia nigrescens", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Styphelia xerophylla", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Styphelia xerophylla", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Styphelia xerophylla", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Styphelia xerophylla", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Swietenia macrophylla", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Swietenia macrophylla", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Swietenia macrophylla", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Swietenia macrophylla", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Ulex europaeus", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Ulex europaeus", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Ulex europaeus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Ulex europaeus", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Ulex jussiaei", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Ulex jussiaei", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Ulex jussiaei", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Ulex jussiaei", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Ulex parviflorus", 4, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Ulex parviflorus", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Ulex parviflorus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Ulex parviflorus", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Ulex parviflorus", F, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Vachellia nilotica", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Vachellia nilotica", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Vachellia nilotica", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Vachellia nilotica", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Vachellia robusta", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Vachellia robusta", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Vachellia robusta", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Vachellia robusta", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Vitis riparia", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Vitis riparia", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Vitis riparia", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Vitis riparia", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Vitis vinifera", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Vitis vinifera", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Vitis vinifera", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Vitis vinifera", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Ziziphus mucronata", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Ziziphus mucronata", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Ziziphus mucronata", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Ziziphus mucronata", F, is.clonal))


all.data3 %>% filter(is.na(BPT) & is.woody.below & is.resprouting)  %>% 
     dplyr::select(Aggregated_name, BPT, is.woody.above, is.clonal)  %>% print(n=100)
# 0

all.data3 %>% filter(is.na(BPT) & is.woody.below & !is.clonal)  %>% 
  dplyr::select(Aggregated_name, BPT) #0

names(all.data3)

table(all.data3$BPT)
'   1    2    3    4    5  5.1  5.2    6  6.1  6.2 
 852 1289 2722  199    8  401 1283    7   77  458  '
table(all.data3$BPT, all.data3$Growthform_unified, exclude=NULL)

all.data3 %>% filter(BPT==5.2 & Growthform_unified=="annual")  %>% as.data.frame() 
all.data3 <- all.data3 %>% mutate(Growthform_unified=if_else(Aggregated_name=="Viburnum edule", "shrub", Growthform_unified))
table(is.na(all.data3$BPT), all.data3$Growthform_unified, exclude=NULL)

all.data3 %>% filter(is.na(BPT) & Growthform_unified=="aquatic")  %>% as.data.frame() 

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Lilaeopsis ruthiana", 3, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Lilaeopsis ruthiana", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Lilaeopsis ruthiana", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Lilaeopsis ruthiana", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Myriophyllum quitense", 3, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Myriophyllum quitense", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Myriophyllum quitense", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Myriophyllum quitense", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pontederia crassipes", 3, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Pontederia crassipes", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pontederia crassipes", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pontederia crassipes", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Potamogeton cheesemanii", 3, BPT))
#all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Potamogeton cheesemanii", 0, CSI))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Potamogeton cheesemanii", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Potamogeton cheesemanii", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Potamogeton cheesemanii", T, is.clonal))

all.data3 %>% filter(is.na(CSI) & Growthform_unified=="annual")  %>% as.data.frame() 
# 0
all.data3 %>% filter(is.na(CSI) & is.woody.below==F & is.clonal==F)  %>% as.data.frame() 
table(all.data3$CSI, exclude=NULL)
'   0    1    2 <NA> 
3795 1114 1325 4219  '

all.data3$CSI[all.data3$is.woody.below==F & all.data3$is.clonal==F &
                is.na(all.data3$CSI)] <- 0
all.data3$CSI[all.data3$is.woody.below==T & all.data3$is.clonal==T &
                is.na(all.data3$CSI)] <- 2
table(all.data3$CSI, exclude=NULL)
'    0    1    2 <NA> 
3951 1114 1517 3871  '

table(all.data3$BPT, all.data3$CSI, exclude=NULL)
'          0    1    2 <NA>
  1     851    1    0    0
  2    1288    0    1    0
  3       2 1110  979  635
  4     192    0    1    6
  5       6    0    0    2
  5.1   349    0   11   41
  5.2  1246    0    3   34
  6       0    0    7    0
  6.1     1    1   75    0
  6.2    16    2  440    0
  <NA>    0    0    0 3153'

all.data3 %>% filter(BPT==1 & CSI==1) %>% as.data.frame()
all.data3 %>% filter(BPT==2 & CSI==2) %>% as.data.frame()

all.data3 %>% filter(grepl("Graphistylis argyrotricha", Aggregated_name))  %>% as.data.frame() 
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Graphistylis argyrotricha", 3, BPT))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Graphistylis argyrotricha", T, is.clonal))
# had been wrongly assigned BPT=2 in the Brazilian Asteraceae database, despite
# having epigeogenous rhizomes

all.data3 %>% filter(BPT==3 & CSI==0) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Armoracia rusticana", NA, CSI))
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Viola adunca", NA, CSI))

all.data3 %>% filter(BPT==4 & CSI==2) %>% as.data.frame()

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cornus sericea", 6.2, BPT))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cornus sericea", T, is.clonal))

all.data3 %>% filter(BPT==5) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Crotalaria griseofusca", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Crotalaria griseofusca", F, is.woody.above))
#all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Crotalaria griseofusca", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Dissotis", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Dissotis", T, is.woody.above))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Galianthe fastigiata", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Galianthe fastigiata", F, is.woody.above))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Hibiscus micranthus", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Hibiscus micranthus", T, is.woody.above))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Richardia grandiflora", 3, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Richardia grandiflora", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Richardia grandiflora", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Richardia grandiflora", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Stomatanthes oblongifolius", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Stomatanthes oblongifolius", F, is.woody.above))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Vernonia fastigiata", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Vernonia fastigiata", F, is.woody.above))

all.data3 %>% filter(grepl("Erica umbellata", Aggregated_name))  %>% as.data.frame() 
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Erica umbellata", 4, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Erica umbellata", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Erica umbellata", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Erica umbellata", F, is.resprouting))

all.data3 %>% filter(grepl("Genista triacanthos", Aggregated_name))  %>% as.data.frame() 
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Genista triacanthos", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Genista triacanthos", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Genista triacanthos", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Genista triacanthos", T, is.resprouting))

all.data3 %>% filter(grepl("Ulex minor", Aggregated_name))  %>% as.data.frame() 
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Ulex minor", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Ulex minor", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Ulex minor", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Ulex minor", T, is.resprouting))

all.data3 %>% filter(grepl("Genista tridentata", Aggregated_name))  %>% as.data.frame() 
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Genista tridentata", 5.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Genista tridentata", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Genista tridentata", F, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Genista tridentata", T, is.resprouting))


all.data3 %>% filter(BPT==6) %>% as.data.frame()

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Adenodolichos mendesii", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Adenodolichos mendesii", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Adenodolichos mendesii", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Chamaecytisus hirsutus", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Chamaecytisus hirsutus", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Chamaecytisus hirsutus", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Clerodendrum pusillum", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Clerodendrum pusillum", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Clerodendrum pusillum", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cryptosepalum", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cryptosepalum", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cryptosepalum", F, is.clonal))

all.data3 %>% filter(Aggregated_name %in% "Fadogia") %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Fadogia", 5.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Fadogia", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Fadogia", F, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Ochna pygmaea", 6.1, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Ochna pygmaea", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Ochna pygmaea", T, is.clonal))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Searsia humpatensis", 6.2, BPT))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Searsia humpatensis", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Searsia humpatensis", T, is.clonal))

all.data3 %>% filter(BPT==6.1 & CSI==0) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(CSI=if_else(Aggregated_name=="Elephantorrhiza elephantina", 2, CSI))
' https://onlinelibrary.wiley.com/doi/pdf/10.1155/2017/6403905'

all.data3 %>% filter(BPT==6.2 & CSI==0) %>% as.data.frame()
# 16
all.data3 <- all.data3 %>% mutate(CSI=if_else(BPT==6.2 & CSI==0, NA, CSI))

table(all.data3$BPT, all.data3$CSI, exclude=NULL)
'          0    1    2 <NA>
  1     851    1    0    0
  2    1288    0    0    0
  3       0 1110  980  638
  4     191    0    0    7
  5       1    0    0    0
  5.1   352    0   16   42
  5.2  1249    0    3   36
  6.1     0    1   77    0
  6.2     0    2  442   16
  <NA>    0    0    0 3150'

table(all.data3$Growthform_unified, exclude=NULL)
'                                                    annual 
                                                       786 
                                  annual, clonal perennial 
                                                         2 
                                              annual, forb 
                                                         9 
                        annual, forb, non-clonal perennial 
                                                         1 
                                     annual, HEMI-PARASITE 
                                                         2 
                          annual, herb/succulent/non-woody 
                                                         1 
                              annual, non-clonal perennial 
                                                         2 
                                             annual, shrub 
                                                         1 
                       annual, Stem ascending to prostrate 
                                                         1 
                                        annual, Stem erect 
                                                         7 
                                    annual, Stem prostrate 
                                                         1 
                                                   aquatic 
                                                         4 
                                                   climber 
                                                        33 
                                          clonal perennial 
                                                      2008 
                            clonal perennial, clonal woody 
                                                         1 
                                   clonal perennial, erect 
                                                         1 
                                    clonal perennial, forb 
                                                       124 
                         clonal perennial, forb, graminoid 
                                                         1 
                clonal perennial, forb, herbaceous dicotyl 
                                                         1 
                clonal perennial, forb, Herbaceous Monocot 
                                                         1 
                        clonal perennial, forb, Stem erect 
                                                         2 
                               clonal perennial, graminoid 
                                                        42 
                   clonal perennial, graminoid, Stem erect 
                                                         1 
                           clonal perennial, HEMI-PARASITE 
                                                         2 
            clonal perennial, herb/hemiparasitic/non-woody 
                                                         1 
                              clonal perennial, herbaceous 
                                                         1 
                      clonal perennial, herbaceous dicotyl 
                                                        10 
clonal perennial, herbaceous dicotyl, non-clonal perennial 
                                                         1 
            clonal perennial, herbaceous dicotyl, subshrub 
                                                         1 
                      clonal perennial, Herbaceous Monocot 
                                                        15 
                    clonal perennial, herbaceous monocotyl 
                                                         8 
                                      clonal perennial, No 
                                                         1 
                    clonal perennial, non-clonal perennial 
                                                         9 
        clonal perennial, non-clonal perennial, Stem erect 
                                                         1 
                           clonal perennial, non-succulent 
                                                         4 
                                   clonal perennial, shrub 
                                                         3 
             clonal perennial, Stem ascending to prostrate 
                                                        18 
                              clonal perennial, Stem erect 
                                                       113 
                          clonal perennial, Stem prostrate 
                                                         6 
                                    clonal perennial, tree 
                                                         3 
                                              clonal woody 
                                                        98 
                            clonal woody, non-clonal woody 
                                                         2 
                                       clonal woody, shrub 
                                                         2 
                                  clonal woody, Stem erect 
                                                         1 
                                        clonal woody, tree 
                                                         2 
                                                  epiphyte 
                                                         1 
                                                      fern 
                                                        23 
                                                   Fibrous 
                                                        19 
                                                      forb 
                                                      1456 
                                           forb, graminoid 
                                                         2 
                                  forb, Herbaceous Monocot 
                                                         1 
                                               forb, liana 
                                                         1 
                                forb, non-clonal perennial 
                                                        71 
   forb, non-clonal perennial, Stem ascending to prostrate 
                                                         1 
                    forb, non-clonal perennial, Stem erect 
                                                         1 
                                    forb, non-clonal woody 
                                                         1 
                                               forb, shrub 
                                                         3 
                                            forb, subshrub 
                                                        13 
                                                 graminoid 
                                                       292 
                                             HEMI-PARASITE 
                                                         1 
                       HEMI-PARASITE, non-clonal perennial 
                                                         1 
                                              hemiepiphyte 
                                                         1 
                              herb/hemiparasitic/non-woody 
                                                         3 
                                                herb/shrub 
                                                         2 
                                  herb/succulent/non-woody 
                                                         2 
                                                herbaceous 
                                                         1 
                                        herbaceous dicotyl 
                                                         1 
                  herbaceous dicotyl, non-clonal perennial 
                                                         3 
                                        Herbaceous Monocot 
                                                         2 
                                      herbaceous monocotyl 
                                                         1 
                                                     liana 
                                                        24 
                                                        No 
                                                         3 
                                  No, non-clonal perennial 
                                                         1 
                                      non-clonal perennial 
                                                       994 
                       non-clonal perennial, non-succulent 
                                                         4 
                               non-clonal perennial, shrub 
                                                         3 
         non-clonal perennial, Stem ascending to prostrate 
                                                        13 
                          non-clonal perennial, Stem erect 
                                                        23 
                      non-clonal perennial, Stem prostrate 
                                                         4 
                            non-clonal perennial, subshrub 
                                                         2 
                           non-clonal perennial, succulent 
                                                         1 
                                          non-clonal woody 
                                                       243 
                                   non-clonal woody, shrub 
                                                         9 
                              non-clonal woody, Stem erect 
                                                         2 
                                    non-clonal woody, tree 
                                                         5 
                                             non-succulent 
                                                        10 
                                                      palm 
                                                         1 
                                                  parasite 
                                                         5 
                                                  Parasite 
                                                         2 
                                                     shrub 
                                                       920 
                        shrub, Stem ascending to prostrate 
                                                         1 
                                           shrub, subshrub 
                                                         5 
                                               shrub, tree 
                                                         2 
                               Stem ascending to prostrate 
                                                         3 
                                                Stem erect 
                                                        20 
                                                  subshrub 
                                                       195 
                                            subshrub, tree 
                                                         1 
                                                 succulent 
                                                         1 
                                                      tree 
                                                      1266 
                                                      <NA> 
                                                      1453  '
# make a list of all growthforms
growthform.list <- sort(unique(all.data3$Growthform_unified))
growthform.list <- sort(unique(c(tstrsplit(growthform.list, ", ", fixed=T)[[1]],tstrsplit(growthform.list, ", ", fixed=T)[[2]])))
growthform.list <- data.frame(growthform=growthform.list,n=NA)
for(i in 1:length(growthform.list$growthform)){
  x <- grepl(growthform.list[i,1], all.data3$Growthform_unified, fixed=T)
  growthform.list$n[i] <- length(x[x])
}
growthform.list

all.data3 %>% filter(Growthform_unified=="succulent") %>% as.data.frame()
all.data3 %>% filter(grepl("non-succulent",Growthform_unified)) %>% as.data.frame()
all.data3 %>% filter(grepl("Taraxacum",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(grepl("non-succulent",Growthform_unified) &
              grepl("non-clonal perennial",Growthform_unified), "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(grepl("non-succulent",Growthform_unified) &
                                      grepl("clonal perennial",Growthform_unified), "clonal perennial", Growthform_unified))

all.data3 %>% filter(grepl("Rosa acicularis",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Rosa acicularis", 6.2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Rosa acicularis", "shrub", Growthform_unified))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Rosa acicularis", F, is.woody.below))
#all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Rosa acicularis", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Rosa acicularis", T, is.clonal))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Rosa acicularis", T, is.resprouting))

all.data3 %>% filter(grepl("Cardamine cordifolia",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cardamine cordifolia", 3, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Cardamine cordifolia", "clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Cardamine cordifolia", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cardamine cordifolia", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cardamine cordifolia", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Cardamine cordifolia", T, is.resprouting))

all.data3 %>% filter(grepl("Cerastium beeringianum",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Cerastium beeringianum", 3, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Cerastium beeringianum", "clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Cerastium beeringianum", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Cerastium beeringianum", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Cerastium beeringianum", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Cerastium beeringianum", T, is.resprouting))

all.data3 %>% filter(grepl("Draba aurea",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Draba aurea", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Draba aurea", "non-clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Draba aurea", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Draba aurea", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Draba aurea", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Draba aurea", T, is.resprouting))

all.data3 %>% filter(grepl("Heuchera parvifolia",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Heuchera parvifolia", 3, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Heuchera parvifolia", "clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Heuchera parvifolia", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Heuchera parvifolia", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Heuchera parvifolia", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Heuchera parvifolia", T, is.resprouting))

all.data3 %>% filter(grepl(" var. ",Aggregated_name)) %>% as.data.frame() # 36
all.data3 %>% filter(grepl(" subsp. ",Aggregated_name)) %>% as.data.frame() # 36

all.data3 %>% filter(grepl("Senecio amplectens",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Senecio amplectens var. holmii", 3, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Senecio amplectens var. holmii", "clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Senecio amplectens var. holmii", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Senecio amplectens var. holmii", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Senecio amplectens var. holmii", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Senecio amplectens var. holmii", T, is.resprouting))

all.data3 %>% filter(grepl("Silene kingii",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Silene kingii", 3, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Silene kingii", "clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Silene kingii", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Silene kingii", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Silene kingii", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Silene kingii", T, is.resprouting))

all.data3 %>% filter(grepl("Smelowskia calycina",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Smelowskia calycina", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Smelowskia calycina", "non-clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Smelowskia calycina", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Smelowskia calycina", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Smelowskia calycina", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Smelowskia calycina", T, is.resprouting))

all.data3 %>% filter(grepl("Taraxacum ceratophorum",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Taraxacum ceratophorum", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Taraxacum ceratophorum", "non-clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Taraxacum ceratophorum", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Taraxacum ceratophorum", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Taraxacum ceratophorum", F, is.clonal))

all.data3 %>% filter(grepl("Veronica besseya",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Veronica besseya", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Veronica besseya", "non-clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Veronica besseya", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Veronica besseya", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Veronica besseya", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Veronica besseya", T, is.resprouting))

all.data3 %>% filter(grepl("succulent",Growthform_unified) | 
                       grepl("Succulent",Growthform_unified)) %>% as.data.frame()

all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Sedum rosea", "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Claytonia megarhiza", "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Sedum atratum", "annual", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Sedum tenellum", "non-clonal perennial", Growthform_unified))

all.data3 %>% filter(grepl("Sempervivum caucasicum",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Sempervivum caucasicum", "clonal perennial", Growthform_unified))

all.data3 %>% filter(grepl("Honckenya peploides",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Honckenya peploides", "clonal perennial", Growthform_unified))


all.data3 %>% filter(Growthform_unified=="Fibrous") %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Growthform_unified=="Fibrous" &
                                      BBB_CGO=="clonal herb" , "clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Growthform_unified=="Fibrous" &
                                      Growthform_TRY=="Shrub" , "shrub", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Growthform_unified=="Fibrous" &
                                      Growthform_TRY=="shrub" , "shrub", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Growthform_unified=="Fibrous" &
                                      BBB_CGO=="woody non-clonal" , "non-clonal perennial", Growthform_unified))

all.data3 %>% filter(Growthform_unified=="HEMI-PARASITE") %>% as.data.frame()
all.data3 %>% filter(grepl("HEMI-PARASITE",Growthform_unified)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Euphrasia frigida", "annual", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Melampyrum sylvaticum", "annual", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Rhinanthus minor", "annual", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Bartsia alpina", "clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Pedicularis lapponica", "clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Pedicularis hirsuta", "non-clonal perennial", Growthform_unified))

all.data3 %>% filter(Growthform_unified=="epiphyte") %>% as.data.frame()

all.data3 %>% filter(grepl("parasite",Growthform_unified) | 
                       grepl("Parasite",Growthform_unified) | 
                       grepl("parasitic",Growthform_unified)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Thesium atrum", "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Thesium lycopodioides", "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Thesium triste", "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Pedicularis rostratospicata", "clonal perennial", Growthform_unified))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Exocarpos nanus", 6.2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Exocarpos nanus", "shrub", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Exocarpos nanus", T, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Exocarpos nanus", T, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Exocarpos nanus", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Exocarpos nanus", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pedicularis aquilina", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Pedicularis aquilina", "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Pedicularis aquilina", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pedicularis aquilina", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pedicularis aquilina", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Pedicularis aquilina", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pedicularis lyrata", 1, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Pedicularis lyrata", "annual", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Pedicularis lyrata", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pedicularis lyrata", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pedicularis lyrata", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Pedicularis aquilina", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pedicularis caucasica", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Pedicularis caucasica", "non-clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Pedicularis caucasica", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pedicularis caucasica", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pedicularis caucasica", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Pedicularis caucasica", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pedicularis comosa", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Pedicularis comosa", "non-clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Pedicularis comosa", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pedicularis comosa", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pedicularis comosa", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Pedicularis comosa", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Pedicularis nordmanniana", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Pedicularis nordmanniana", "non-clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Pedicularis nordmanniana", F, is.woody.below))
# all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Pedicularis nordmanniana", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Pedicularis nordmanniana", F, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Pedicularis nordmanniana", T, is.resprouting))

all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Alectra", "non-clonal perennial", Growthform_unified))


all.data3 %>% filter(Growthform_unified=="hemiepiphyte") %>% as.data.frame()
all.data3 %>% filter(grepl("hemiepiphyte",Growthform_unified)) %>% as.data.frame()

# all.data3 <- all.data3 %>% 
#   mutate(Growthform_unified=if_else(Aggregated_name=="Clusia", NA, Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Ficus pertusa", "tree", Growthform_unified))

all.data3 %>% filter(Growthform_unified=="herb/hemiparasitic/non-woody") %>% as.data.frame()
all.data3 %>% filter(grepl("Pedicularis", Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Growthform_unified=="herb/hemiparasitic/non-woody", "forb", Growthform_unified))

all.data3 %>% filter(Growthform_unified=="herb/shrub") %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Senecio gunnii", 3, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Senecio gunnii", "forb", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Senecio gunnii", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Senecio gunnii", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Senecio gunnii", T, is.clonal))
# all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Senecio gunnii", T, is.resprouting))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Senecio pinnatifolius", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Senecio pinnatifolius", "forb", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Senecio pinnatifolius", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Senecio pinnatifolius", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Senecio pinnatifolius", F, is.clonal))
# all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Senecio pinnatifolius", F, is.resprouting))

all.data3 %>% filter(Growthform_unified=="herb/succulent/non-woody") %>% as.data.frame()

all.data3 %>% filter(Growthform_unified=="herbaceous") %>% as.data.frame()
all.data3 %>% filter(grepl("herbaceous",Growthform_unified)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(grepl("herbaceous",Growthform_unified) & BPT==3, "clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(grepl("herbaceous",Growthform_unified) & BPT==2, "non-clonal perennial", Growthform_unified))

# all.data3 <- all.data3 %>% 
#   mutate(Growthform_unified=if_else(Aggregated_name=="Adenostyles alpina  subsp. alpina", "clonal perennial", Growthform_unified))
# all.data3 <- all.data3 %>% 
#   mutate(Growthform_unified=if_else(Aggregated_name=="Poa hothamensis", "clonal perennial", Growthform_unified))

# all.data3 %>% filter(grepl("herbaceous dicotyl",Growthform_unified)) %>% as.data.frame()
# all.data3 %>% filter(Growthform_unified=="herbaceous dicotyl") %>% as.data.frame()
# all.data3 <- all.data3 %>% 
#   mutate(Growthform_unified=if_else(Aggregated_name=="Plantago euryphylla", "clonal perennial", Growthform_unified))
# all.data3 %>% filter(grepl("herbaceous dicotyl",Growthform_unified)) %>% as.data.frame()

all.data3 %>% filter(Growthform_unified=="Herbaceous Monocot") %>% as.data.frame()
all.data3 %>% filter(grepl("Herbaceous",Growthform_unified)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(grepl("Herbaceous",Growthform_unified) & BPT==3, "clonal perennial", Growthform_unified))

all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Carex", "graminoid", Growthform_unified))
# all.data3 <- all.data3 %>% 
#   mutate(Growthform_unified=if_else(Aggregated_name=="Arctagrostis latifolia", "clonal perennial", Growthform_unified))
all.data3 %>% filter(grepl("Festuca varia",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Festuca varia", "clonal perennial", Growthform_unified))

all.data3 %>% filter(Growthform_unified=="herbaceous monocotyl") %>% as.data.frame()

all.data3 %>% filter(grepl("Elymus trachycaulus",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Elymus trachycaulus", 3, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Elymus trachycaulus", "clonal perennial", Growthform_unified))
#all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Elymus trachycaulus", F, is.woody.below))
#all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Elymus trachycaulus", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Elymus trachycaulus", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Elymus trachycaulus", NA, is.resprouting))

all.data3 %>% filter(Growthform_unified=="No") %>% as.data.frame()
all.data3 %>% filter(grepl("No",Growthform_unified)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Anthemis cretica", "clonal perennial", Growthform_unified))

all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Artemisia tilesii", 3, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Artemisia tilesii", "clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Artemisia tilesii", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Artemisia tilesii", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Artemisia tilesii", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Artemisia tilesii", NA, is.resprouting))


all.data3 %>% filter(grepl("Jacobaea incana",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Jacobaea incana", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Jacobaea incana", "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Jacobaea incana", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Jacobaea incana", F, is.woody.above))

all.data3 %>% filter(grepl("Solidago multiradiata",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Solidago multiradiata", 2, BPT))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(Aggregated_name=="Solidago multiradiata", "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% mutate(is.woody.below=if_else(Aggregated_name=="Solidago multiradiata", F, is.woody.below))
all.data3 <- all.data3 %>% mutate(is.woody.above=if_else(Aggregated_name=="Solidago multiradiata", F, is.woody.above))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Solidago multiradiata", F, is.clonal))
# all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Solidago multiradiata", T, is.resprouting))

all.data3 %>% filter(grepl("Saxifraga exarata",Aggregated_name)) %>% as.data.frame()
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Saxifraga exarata", "non-clonal perennial", Growthform_unified))

all.data3 %>% filter(grepl("Stem ascending to prostrate",Growthform_unified)) %>% as.data.frame()
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(grepl("Stem ascending to prostrate",Growthform_unified) &
                    BPT==2, "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(grepl("Stem ascending to prostrate",Growthform_unified) &
                    BPT==3, "clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Linum catharticum", "annual", Growthform_unified))
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Diapensia lapponica", "shrub", Growthform_unified))
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Phyllodoce caerulea", "shrub", Growthform_unified))
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Comarum palustre", "subshrub", Growthform_unified))

all.data3 %>% filter(grepl("Stem prostrate",Growthform_unified)) %>% as.data.frame()
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(grepl("Stem prostrate",Growthform_unified) & BPT==1, "annual", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(grepl("Stem prostrate",Growthform_unified) & BPT==2, "non-clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>% 
  mutate(Growthform_unified=if_else(grepl("Stem prostrate",Growthform_unified) & BPT==3, "clonal perennial", Growthform_unified))
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Kalmia procumbens", "subshrub", Growthform_unified))
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Salix herbacea", "subshrub", Growthform_unified))

all.data3 %>% filter(Growthform_unified=="clonal woody" & BPT==5.2) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Diospyros virginiana", 6.2, BPT))
all.data3 <- all.data3 %>% mutate(is.clonal=if_else(Aggregated_name=="Diospyros virginiana", T, is.clonal))
#all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Diospyros virginiana", T, is.resprouting))

all.data3 %>% filter(Growthform_unified=="graminoid" & BPT==6.2) %>% as.data.frame()
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Amelanchier utahensis", "clonal woody", Growthform_unified))

all.data3 %>% filter(Growthform_unified=="graminoid" & BPT==5.2) %>% as.data.frame()
# all.data3 <- all.data3 %>%
#   mutate(Growthform_unified=if_else(Aggregated_name=="Cercocarpus montanus", "shrub", Growthform_unified))
all.data3 %>% filter(Growthform_unified=="graminoid" & BPT==5) %>% as.data.frame()
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(Aggregated_name=="Indigofera", "shrub", Growthform_unified))

all.data3 %>% filter(grepl("non-clonal woody",Growthform_unified) & BPT==6.2) %>% as.data.frame()
all.data3 <- all.data3 %>%
  mutate(Growthform_unified=if_else(grepl("non-clonal woody",Growthform_unified) & BPT==6.2, 
                                    "clonal woody", Growthform_unified))

# make a list of all growthforms
growthform.list <- sort(unique(all.data3$Growthform_unified))
growthform.list <- sort(unique(c(tstrsplit(growthform.list, ", ", fixed=T)[[1]],tstrsplit(growthform.list, ", ", fixed=T)[[2]])))
growthform.list <- data.frame(growthform=growthform.list,n=NA)
for(i in 1:length(growthform.list$growthform)){
  x <- grepl(growthform.list[i,1], all.data3$Growthform_unified, fixed=T)
  growthform.list$n[i] <- length(x[x])
}
growthform.list
'             growthform    n
1                annual  815
2               aquatic    4
3               climber   33
4      clonal perennial 3546
5          clonal woody  367
6              epiphyte    1
7                 erect  172
8                  fern   23
9                  forb 1687
10            graminoid  336
11                liana   25
12 non-clonal perennial 1153
13     non-clonal woody  244
14        non-succulent    1
15                 palm    1
16                shrub 1164
17           Stem erect  171
18       Stem prostrate    1
19             subshrub  219
20                 tree 1278
'
# make a list of all CGO
CGO.list <- sort(unique(all.data3$BBB_CGO))
all.data3 <- all.data3 %>% 
  mutate(BBB_CGO = gsub("Non-woody rhizome","non-woody rhizome",BBB_CGO, fixed=T))
CGO.list <- sort(unique(all.data3$BBB_CGO))
CGO.list <- sort(unique(c(tstrsplit(CGO.list, ", ", fixed=T)[[1]],tstrsplit(CGO.list, ", ", fixed=T)[[2]])))
CGO.list <- data.frame(CGO=CGO.list,n=NA)
for(i in 1:length(CGO.list$CGO)){
  x <- grepl(CGO.list[i,1], all.data3$BBB_CGO, fixed=T)
  CGO.list$n[i] <- length(x[x])
  # x <- regexpr(CGO.list[i,1], all.data3$BBB_CGO, fixed=T)
  # CGO.list$n[i] <- length(x[x==1 & !is.na(x)])
}
CGO.list

all.data3 %>% filter(grepl("tree",BBB_CGO)) %>% as.data.frame()
all.data3 <- all.data3 %>% mutate(BPT=if_else(Aggregated_name=="Baccharis dracunculifolia", 4, BPT))
all.data3 <- all.data3 %>% mutate(is.resprouting=if_else(Aggregated_name=="Baccharis dracunculifolia", F, is.resprouting))

all.data3 %>% filter(grepl("bud-bearing root",BBB_CGO)) %>% as.data.frame()
all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(grepl("bud-bearing root",BBB_CGO), "root sprouting", BBB_CGO))
all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(grepl("corm",BBB_CGO), "stem tuber", BBB_CGO))
all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(grepl("root crown",BBB_CGO), "main root", BBB_CGO))
all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(grepl("roots clonal",BBB_CGO), "root sprouting", BBB_CGO))
all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(grepl("taproot tuber-non clonal",BBB_CGO), "main root", BBB_CGO))

all.data3 %>% filter(grepl("herbaceous perennial non clonal",BBB_CGO)) %>% as.data.frame()
all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(Aggregated_name=="Graphistylis argyrotricha", "herbaceous perennial clonal", BBB_CGO))
all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(grepl("herbaceous perennial non clonal",BBB_CGO) &
                           grepl("xylopodium",BBB_CGO)    , "xylopodium", BBB_CGO))
table(all.data3$BPT==2, all.data3$BBB_CGO)
all.data3 %>% filter(grepl("stem tuber",BBB_CGO) & BPT==2) %>% as.data.frame()
all.data3 %>% filter(grepl("lignotuber",BBB_CGO)) %>% as.data.frame()

all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(BPT==2, "herbaceous perennial non-clonal", BBB_CGO))
all.data3 %>% filter(grepl("epigeogenous rhizome",BBB_CGO) & BPT==5.2) %>% as.data.frame()
all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(Aggregated_name=="Erica herbacea", "subshrub", BBB_CGO))

all.data3 <- all.data3 %>%
  mutate(BBB_CGO=if_else(BPT==4, "woody non-clonal", BBB_CGO))

table(all.data3$BPT, all.data3$CSI, exclude=NULL)
'
          0    1    2 <NA>
  1     851    1    0    1
  2    1288    1    0   10
  3       0 1109  980  645
  4     192    0    0    7
  5       1    0    0    0
  5.1   352    0   16   42
  5.2  1247    0    3   36
  6.1     0    1   77    0
  6.2     1    2  442   18
  <NA>    0    0    0 3130
'
# adapt names to the concept paper of Joseph Tumber-Davila
names(all.data3)
all.data4 <- all.data3 %>% 
  rename(RN=Root_N_concentration,
         Coarse.fine.ratio=Coarse_root_fine_root_mass_ratio,
         RD=Mean_Root_diameter,
         RTD=Root_tissue_density,
         SRL=Specific_root_length,
         n_RN=n_Root_N_concentration,
         n_Coarse.fine.ratio=n_Coarse_root_fine_root_mass_ratio,
         n_RD=n_Mean_Root_diameter,
         n_RTD=n_Root_tissue_density,
         n_SRL=n_Specific_root_length,
         RDepth=Rooting_depth,
         LRExtent=Lateral_root_spread,
         n_RDepth=n_Rooting_depth,
         n_LRExtent=n_Lateral_root_spread,
         n.offspring.per.shoot=multiplication,
         Lateral.expansion=spread,
         Connection.persistence=persistence,
         CloGenExt=ClonalGenetExtent,
         n_n.offspring.per.shoot=n_multiplication,
         n_Lateral.expansion=n_spread,
         n_Connection.persistence=n_persistence,
         n_CloGenExt=n_ClonalGenetExtent,
         CGO=BBB_CGO) %>% 
  dplyr::select(Aggregated_name,GRooT_Edited_name, GRooT_Parsed_name, GRooT_Resolved_name, 
              RN, RD,  RTD, SRL, Coarse.fine.ratio,
              n_RN, n_RD, n_RTD,              
              n_SRL, n_Coarse.fine.ratio, 
              RSIP_Original_name, RSIP_Edited_name, RSIP_Parsed_name,RSIP_Resolved_name,
              RDepth, LRExtent, n_RDepth, n_LRExtent,
              BBB_Original_name, BBB_Edited_name, BBB_Parsed_name,BBB_Resolved_name,
              BBdepth, BBsize, n.offspring.per.shoot, Lateral.expansion, Connection.persistence,
              CloGenExt, 
              n_BBdepth, n_BBsize, n_n.offspring.per.shoot, n_Lateral.expansion,
              n_Connection.persistence, n_CloGenExt, n_numeric_entries_per_row,
              is.woody.below, is.woody.above, is.clonal, is.resprouting,
              BPT, CSI, 
              CGO, Growthform_unified) %>% 
  mutate(BPT = case_when(
    BPT==5.1 ~ "5a",
    BPT==5.2 ~ "5b",
    BPT==6.1 ~ "6a",
    BPT==6.2 ~ "6b",
    .default = as.character(BPT)))
str(all.data4)  #[10,453 × 47]

write.csv(all.data4, file="3_All/All_databases_TRY12.csv",  
          row.names = F, fileEncoding = "UTF-8")

# all.data4 <- read.csv("3_All/All_databases_TRY12.csv")
table(all.data4$BPT, exclude=NULL)
'  1    2    3    4    5   5a   5b   6a   6b <NA> 
 853 1299 2734  199    1  410 1286   78  463 3130  '
table(is.na(all.data4$BPT))
'FALSE  TRUE 
 7323  3130 '
dim(all.data4)
#10453    48
all.data4 %>% filter(grepl("comata",Aggregated_name)) %>% as.data.frame()
all.data4 %>% filter(grepl("Ranunculus tuberosus",Aggregated_name)) %>% as.data.frame()
all.data4 %>% filter(grepl("Jacaranda",Aggregated_name)) %>% as.data.frame()

