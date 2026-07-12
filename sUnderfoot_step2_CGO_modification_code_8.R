# Step 2
# Goal of this script: merge the the Resolved species names (..._Resolved2.csv)
# from Helges script (sUnderfoot8.R) with the modified CGO terminology 
# in the bud bank datasets
# first version was written by Justus
# all datasets produced are uploaded to Google Drive > sUnderfoot > Data > 
# 2.Resolved and reclassified CGO

library(here)
library(tidyverse)
library(readxl)

setwd("c:/Daten/iDiv4/sDiv/2023sUnderfoot/Databases/")

list.files(path = "1_Harmonized_Taxa")
' [1] "20210830_Species_list_BG-groups_Resolved6.csv"              
 [2] "BBBdb_2017.11_Resolved6.csv"                                
 [3] "belowground_traits_Resolved6.csv"                           
 [4] "Budbank_Brazilian_Asteraceae_Table1_Resolved6.csv"          
 [5] "DataBaseNew_AlineAlessandra_Dec2023_Resolved6.csv"          
 [6] "GRooTFullVersion_Resolved6.csv"                             
 [7] "iDivTable_BBB_Resolved6.csv"                                
 [8] "iDivTable_BBB_South_Africa_data_Resolved6.csv"              
 [9] "itex_spp_clonal_traits_Resolved6.csv"                       
[10] "laughlin_sUnderfoot_2023_additions_Resolved6.csv"           
[11] "nph18031-sup-0001-datasets1_Resolved6.csv"                  
[12] "Sandhills_roottraits_Resolved6.csv"                         
[13] "Siebert_Frances_More data from Africa_Nov2023_Resolved6.csv"
[14] "sumfile-merged-all_Resolved6.csv"    
'
###. 1. Combine with TRY
TRY1 <- fread("TRY/TRYPlantMorphologicalTraits/30186.txt")
# fileEncoding = "latin1
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


'x <- TRY2 %>% dplyr::select(OrigValueStr) %>% 
  table(., exclude = NULL) # does not work
sum(x)'

# backbone_sPlot <- read_rds("c:/Daten/iDiv3/sPlot/sPlot4.0/Data/backbone_sPlot4_v.3.0.RDS")
# str(backbone_sPlot) # 426994 obs. of  102
backbone_sPlot <- read_rds("c:/Daten/iDiv4/sPlot/sPlot4.1/Data/backbone_sPlot4.1.RDS")
str(backbone_sPlot) # 413,663 × 25

backbone_sPlot$Harmonized_name_wfo[grep("  var.", backbone_sPlot$Harmonized_name_wfo)]
backbone_sPlot$Resolved_name[grep("  var.", backbone_sPlot$Resolved_name)]
any(is.na(backbone_sPlot$Resolved_name)) #F
# there is a mistake in the sPlot backbone, with two "  " before "var."
# which originated from the WFO backbone
backbone_sPlot$Resolved_name <- gsub("  var."," var.",backbone_sPlot$Resolved_name, fixed=T)
# there is a mistake in the sPlot backbone, with two "  " before "subsp."
# which originated from the WFO backbone
backbone_sPlot$Harmonized_name_wfo[grep("  subsp.", backbone_sPlot$Harmonized_name_wfo)]
backbone_sPlot$Resolved_name <- gsub("  subsp."," subsp.",backbone_sPlot$Resolved_name, fixed=T)

index1 <- match(TRY2$SpeciesName, backbone_sPlot$Original_name)
table(!is.na(index1))
#     FALSE    TRUE 
#    269766 2056630 
TRY2$Resolved_name <- backbone_sPlot$Resolved_name[index1]

TRY2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
#  205855
unique(TRY2$Resolved_name[grep(" var.",TRY2$Resolved_name,fixed=T)]) # 3061
unique(TRY2$Resolved_name[grep("  var.",TRY2$Resolved_name,fixed=T)]) # 0
unique(TRY2$Resolved_name[grep(" subsp.",TRY2$Resolved_name,fixed=T)]) #1754
unique(TRY2$Resolved_name[grep("  subsp.",TRY2$Resolved_name,fixed=T)]) # 0

# check for duplicate and missing taxon names 
# remove subspecies and variants
TRY2 <- TRY2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 202927

index1 <- match(TRY3$SpeciesName, backbone_sPlot$Original_name)
table(!is.na(index1))
' FALSE   TRUE 
  9608 198683 '
TRY3$Resolved_name <- backbone_sPlot$Resolved_name[index1]

TRY3 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 75872
unique(TRY3$Resolved_name[grep(" var.",TRY3$Resolved_name,fixed=T)]) # 636
unique(TRY3$Resolved_name[grep("  var.",TRY3$Resolved_name,fixed=T)]) # 0
unique(TRY3$Resolved_name[grep(" subsp.",TRY3$Resolved_name,fixed=T)]) #1024
unique(TRY3$Resolved_name[grep("  subsp.",TRY3$Resolved_name,fixed=T)]) # 0

# remove subspecies and variants
TRY3 <- TRY3 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY3 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 75354

index2 <- match(Backbone_sUnderfoot$Original_name, TRY3$SpeciesName)
table(!is.na(index2))
'FALSE  TRUE 
10583  7104  '


index1 <- match(TRY4$SpeciesName, backbone_sPlot$Original_name)
table(!is.na(index1))
'FALSE  TRUE 
17471  2233  '
TRY4$Resolved_name <- backbone_sPlot$Resolved_name[index1]

TRY4 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 1882
unique(TRY4$Resolved_name[grep(" var.",TRY4$Resolved_name,fixed=T)]) # 15
unique(TRY4$Resolved_name[grep("  var.",TRY4$Resolved_name,fixed=T)]) # 0
unique(TRY4$Resolved_name[grep(" subsp.",TRY4$Resolved_name,fixed=T)]) #16
unique(TRY4$Resolved_name[grep("  subsp.",TRY4$Resolved_name,fixed=T)]) #0
# remove subspecies and variants
TRY4 <- TRY4 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY4 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 1873


index2 <- match(Backbone_sUnderfoot$Original_name, TRY4$SpeciesName)
table(!is.na(index2))
'FALSE  TRUE 
17246   441  '

index1 <- match(TRY5$SpeciesName, backbone_sPlot$Original_name)
table(!is.na(index1))
'FALSE  TRUE 
 1946  5282 '
TRY5$Resolved_name <- backbone_sPlot$Resolved_name[index1]

TRY5 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 2678
unique(TRY5$Resolved_name[grep(" var.",TRY5$Resolved_name,fixed=T)]) # 6
unique(TRY5$Resolved_name[grep("  var.",TRY5$Resolved_name,fixed=T)]) # 0
unique(TRY5$Resolved_name[grep(" subsp.",TRY5$Resolved_name,fixed=T)]) #111
unique(TRY5$Resolved_name[grep("  subsp.",TRY5$Resolved_name,fixed=T)]) # 0

# remove subspecies and variants
TRY5 <- TRY5 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY5 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 2675

index2 <- match(Backbone_sUnderfoot$Original_name, TRY5$SpeciesName)
table(!is.na(index2))
'FALSE  TRUE 
16270  1417 '

index1 <- match(TRY6$SpeciesName, backbone_sPlot$Original_name)
table(!is.na(index1))
'FALSE  TRUE 
23296 47037  '
TRY6$Resolved_name <- backbone_sPlot$Resolved_name[index1]

TRY6 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 16974
unique(TRY6$Resolved_name[grep(" var.",TRY6$Resolved_name,fixed=T)]) # 130
unique(TRY6$Resolved_name[grep("  var.",TRY6$Resolved_name,fixed=T)]) # 0
unique(TRY6$Resolved_name[grep(" subsp.",TRY6$Resolved_name,fixed=T)]) #547
unique(TRY6$Resolved_name[grep("  subsp.",TRY6$Resolved_name,fixed=T)]) # 0

# remove subspecies and variants
TRY6 <- TRY6 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])

TRY6 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
#  16899

index2 <- match(Backbone_sUnderfoot$Original_name, TRY6$SpeciesName)
table(!is.na(index2))
'FALSE  TRUE 
13521  4166 '

Backbone_sUnderfoot <- read.csv(file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv")
index2 <- match(Backbone_sUnderfoot$Original_name, TRY2$SpeciesName)
table(!is.na(index2))
'FALSE  TRUE 
10402 13851 '


### 4. BBB: A global Belowground Bud Bank database
# from: https://www.uv.es/jgpausas/bbb.htm
# Pausas, J.G., Lamont, B.B., Paula, S., Appezzato‐da‐Glória, B.,
# Fidelis, A., 2018. Unearthing belowground bud banks in 
# fire‐prone ecosystems. New Phytol 217, 1435–1448.
# https://doi.org/10.1111/nph.14982

BBB2 <- read.csv("1_Harmonized_Taxa/BBBdb_2017.11_Harmonized6.csv",  
        encoding = "latin1")
str(BBB2)
table(BBB2$BBB, exclude=NULL)
table(BBB2$Woodiness)
table(BBB2$Woodiness, BBB2$BBB)
BBB2 %>% filter(Woodiness=="Herb" & BBB=="Lignotuber") #Stenanthemum sublineare
TRY3 %>% filter(Resolved_name %in% "Stenanthemum")
TRY2 %>% filter(grepl("Spondias tuberosa", SpeciesName))
TRY3 %>% filter(grepl("Spondias tuberosa", SpeciesName))
BBB2 %>% filter(grepl("Cyclopia", Resolved_name))

BBB2 <- BBB2 %>%
  dplyr::rename(CGO = BBB) %>%
  mutate(Growthform=if_else(Woodiness=="herb","Herb",Woodiness)) %>% 
  # new variable for all clonal databases
  mutate(is.woody=if_else(Woodiness=="Woody" | Woodiness=="Suffrutex",T,F),
         is.woody.below=if_else(Woodiness=="Woody" | Woodiness=="Suffrutex" | Woodiness=="Fibrous" 
                                | Woodiness=="Variable",T,F),
         is.woody.below=if_else(CGO == "Xylopodium" & !is.na(CGO),T,is.woody.below),
         is.woody.above=if_else(Woodiness=="Woody"| Woodiness=="Fibrous",T,F)) %>% 
  mutate(is.woody.below=if_else(Resolved_name == "Stenanthemum sublineare",T, is.woody.below)) %>% 
  mutate(is.woody.below=if_else(Resolved_name == "Chimaphila umbellata",T, is.woody.below)) %>% 
  mutate(is.clonal=if_else(CGO %in% c("clonal herb","Roots- clonal",
                                      "Bulb","Woody rhizomes","Stem tubers","Corm"),T,F)) %>% 
  mutate(is.resprouting=if_else(CGO %in% c("Lignotuber","Xylopodium","Root crown"),T,F)) %>% 
  mutate(BPI = case_when(
             #CGO == "Root- clonal" ~ NA,# ATTENTION: THIS NEEDS CHECKING FROM ALE
             CGO == "Roots- clonal" & is.woody.below ~ 6,    
             CGO == "Roots- clonal" & !is.woody.below ~ 3,  
             CGO == "Taproot tuber- non clonal" ~ 2,  
             CGO == "Xylopodium" ~ 5, 
             CGO == "Lignotuber" ~5, 
             CGO == "Woody rhizomes" ~ 6, 
             CGO == "Root crown" ~ 5, 
             CGO == "Bulb" ~ 3, 
             CGO == "woody non-clonal" ~ 5, 
             CGO == "clonal herb" ~ 3, 
             CGO == "Corm" ~3, 
             CGO == "Stem tubers" ~ 3, 
             TRUE ~ NA)
  ) %>%
  mutate(CSI =
           case_when(
             CGO == "Root- clonal" ~ 2,# ATTENTION: THIS NEEDS CHECKING FROM ALE 
             CGO == "Taproot tuber- non clonal" ~ 0, 
             CGO == "Xylopodium" ~ 0, 
             CGO == "Lignotuber" ~ 0, 
             CGO == "Woody rhizomes" ~ 2, 
             CGO == "Root crown" ~ 0, 
             CGO == "Bulb" ~ 1, 
             CGO == "woody non-clonal" ~ 0, 
             CGO == "clonal herb" ~ NA, # ATTENTION: THIS NEEDS CHECKING FROM ALE 
             CGO == "Corm" ~1, 
             CGO == "Stem tubers" ~ 1, 
             TRUE ~ NA)
  ) %>% 
  mutate(BPI = if_else(BPI==2 & is.woody.below, 4, BPI),
         BPI = if_else(BPI==2 & is.resprouting, 5, BPI),
         is.woody.below = if_else(BPI==5 & !is.na(BPI), T, is.woody.below),
         BPI = if_else(BPI==3 & is.woody.below, 6, BPI),
         BPI = if_else(BPI==5 & !is.woody.below & Woodiness=="Herb", 
                       2, BPI),
         BPI = if_else(is.woody.below & !is.clonal & !is.resprouting,4,BPI),
         BPI = if_else(!is.woody.below & !is.clonal & !is.resprouting,2,BPI)) %>% 
  mutate(BPT=BPI) %>% 
  mutate(BPT = case_when(
    BPI == 5 & !is.woody.above ~ 5.1,
    BPI == 5 & is.woody.above ~ 5.2,
    BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    BPI == 6 & is.woody.above ~ 6.2,
    TRUE~BPT) ) 

#BBB2 %>% filter(!is.woody & BPI==6) #Stenanthemum sublineare
# BBB2 <- BBB2 %>% mutate(BPI = if_else(BPI==5 & !is.woody.below & Woodiness=="Herb", 
#                                       2, BPI))

table(BBB2$is.woody, exclude=NULL)
table(BBB2$is.woody.above, exclude=NULL)
table(BBB2$is.woody.below, exclude=NULL)
table(BBB2$is.clonal, exclude=NULL)
table(BBB2$BPT,BBB2$is.resprouting, exclude=NULL)
table(BBB2$BPI,BBB2$is.woody, exclude=NULL)
table(BBB2$BPT,BBB2$is.woody.below, exclude=NULL)
'      FALSE TRUE
  2     125    0
  3     257    0
  4       0   39
  5.1     0  203
  5.2     0 1177
  6.1     0   26
  6.2     0  287'
table(BBB2$BPT,BBB2$is.woody.above, exclude=NULL)

## HB changes made 26.11.2024
BBB2 %>% filter(is.na(BPT))
BBB2 %>% filter(is.na(BPT) & is.woody.below)
BBB2 %>% filter(BPI==2 & is.woody.below)
BBB2 %>% filter(BPI==2 & is.resprouting)

write.csv(BBB2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                     "BBBdb_2017.11_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "latin1")


### 7. Bud bank Afrotropical grasslands
# from: https://zenodo.org/record/5521402
# from the publication:
# Meller, P., Stellmes, M., Fidelis, A., Finckh, M., 2022. 
# Correlates of geoxyle diversity in Afrotropical grasslands. 
# Journal of Biogeography 49, 339–352. https://doi.org/10.1111/jbi.14305
BBAFTG2 <- read.csv("1_Harmonized_Taxa/20210830_Species_list_BG-groups_Harmonized6.csv", 
                    encoding = "latin1")
str(BBAFTG2)
table(BBAFTG2$BGO, exclude=NULL) # 1 NA
TRY2 %>% filter(grepl("Thesium", SpeciesName)) %>% pull(Plant_growth_form) %>% table()
TRY3 %>% filter(grepl("Thesium", SpeciesName))
BBAFTG2 %>% filter(is.na(BGO)) # Thesium
table(BBAFTG2$BBB)

BBAFTG2 <- BBAFTG2 %>% 
    dplyr::rename(CGO = BGO) %>%
    mutate(BPI =
           case_when(
             CGO == "Clonal herb" ~ 3, 
             CGO == "Lignotuber" ~ 5, 
             CGO == "root crown" ~ 5, 
             CGO == "woody rhizome" ~ 6, 
             CGO == "Xylopodium" ~ 5,
             TRUE ~ NA)
  ) %>%
  mutate(CSI =
           case_when(
             CGO == "Clonal herb" ~ NA, # ATTENTION: THIS NEEDS CHECKING FROM ALE 
             CGO == "Lignotuber" ~ 0,
             CGO == "root crown" ~ 0,
             CGO == "woody rhizome" ~ 2, 
             CGO == "Xylopodium" ~ 0,
             TRUE ~ NA)
  ) %>% 
  mutate(is.woody=if_else(CGO=="Clonal herb",F,T)) %>% 
  mutate(is.woody=if_else(Resolved_name=="Thesium",F,is.woody)) %>% 
  mutate(BPI=if_else(Resolved_name=="Thesium",2,BPI)) %>% 
  mutate(is.clonal=if_else(CGO %in% c("Clonal herb","woody rhizome"),T,F)) %>% 
  mutate(is.resprouting=if_else(CGO %in% c("Lignotuber","Xylopodium","root crown"),T,F)) %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
              filter(!is.na(Plant_growth_form)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  mutate(BPT=BPI,
         is.woody.below=is.woody) %>% 
  mutate(is.woody.above=case_when(
    Plant_growth_form=="herb" | Plant_growth_form=="Herb" | Plant_growth_form=="herb/shrub"~ F,
    Plant_growth_form=="tree" | Plant_growth_form=="woody" | Plant_growth_form=="shrub"| 
    Plant_growth_form=="Shrub" | Plant_growth_form=="shrub/tree" | Plant_growth_form=="shrub/tree/woody" ~ T,
    Resolved_name %in% c("Acalypha dumetorum", "Alectra","Aspilia angolensis","Brachystegia russelliae",
                           "Chamaecrista newtonii",
                           "Clematis chrysocarpa", "Dolichos cardiophyllus","Dolichos dongaluta",
                           "Eriosema gossweileri",
                           "Fadogia chrysantha", "Fadogia graminea", "Helichrysum candolleanum",
                           "Helichrysum mechowianum", "Kalanchoe lindmanii",
                           "Landolphia gossweileri", "Lannea rubra",
                           "Lasiosiphon kraussianus","Leptactina prostrata", "Lopholaena decurrens",
                           "Oxygonum pachybasis","Protea ongotium", "Protea micans", "Protea paludosa",
                           "Protea poggei", "Rhynchotropis poggei","Stomatanthes tundavalaensis",
                           "Tephrosia acaciifolia", 
                           "Thesium atrum","Thesium lycopodioides", "Thesium triste", 
                           "Triumfetta geoides", "Triumfetta gossweileri","Vigna antunesii") ~ F,
    Resolved_name %in% c("Eriosema cyclophyllum","Gardenia brachythamnus","Oxygonum fruticosum", 
                           "Ozoroa xylophylla", "Strychnos gossweileri") ~ T,
    TRUE~NA)) %>% 
  mutate(BPT = case_when(
    BPI == 5 & !is.woody.above ~ 5.1,
    BPI == 5 & is.woody.above ~ 5.2,
    BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    BPI == 6 & is.woody.above ~ 6.2,
    TRUE~BPT) ) %>% 
  mutate(is.woody.above = if_else(BPT==2 | BPT==3,F, is.woody.above)) %>% 
  dplyr::select(-c("Plant_growth_form", "Plant_woodiness")) 

table(BBAFTG2$is.woody, exclude=NULL)
table(BBAFTG2$is.woody.above, exclude=NULL)
table(BBAFTG2$is.clonal, exclude=NULL)
table(BBAFTG2$is.resprouting, exclude=NULL)
table(BBAFTG2$BPT, BBAFTG2$is.woody.below, exclude=NULL)
'     
      FALSE TRUE
  2       2    0
  3       4    0
  5       0    4
  5.1     0   42
  5.2     0   20
  6       0    7
  6.1     0   31
  6.2     0   42 '
table(BBAFTG2$BPT, BBAFTG2$is.woody.above, exclude=NULL)

BBAFTG2 %>% filter(is.na(is.woody.above))
BBAFTG2 %>% filter(BPT==5)
BBAFTG2 %>% filter(BPT==6)

# BBAFTG2a <- BBAFTG2 %>% left_join(TRY2 %>% dplyr::select(Resolved_name,OrigValueStr), by="Resolved_name", multiple="first")
# table(BBAFTG2a$OrigValueStr, exclude=NULL)
table(BBAFTG2$BPI,BBAFTG2$is.woody, exclude=NULL)
# table(BBAFTG2a$OrigValueStr,BBAFTG2$BPI,exclude=NULL)


write.csv(BBAFTG2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                        "20210830_Species_list_BG-groups_Harmonized6_CGO.csv"), 
          row.names = F, , fileEncoding = "latin1")

### 8. Bud bank Brazilian Asteraceae
# from: https://link.springer.com/article/10.1007/s12224-016-9266-8/tables/1
# from the publication:
# Filartiga, A.L., Klimešová, J., Appezzato-da-Glória, B., 2017. 
# Underground organs of Brazilian Asteraceae: testing the CLO-PLA 
# database traits. Folia Geobot 52, 367–385. 
# https://doi.org/10.1007/s12224-016-9266-8

BBBA2 <- read.csv("1_Harmonized_Taxa/Budbank_Brazilian_Asteraceae_Table1_Harmonized6.csv",  
                  encoding = "latin1")
str(BBBA2)
table(BBBA2$Original.classification)
BBBA2 %>% filter(Original.classification=="tree") # only Baccharis dracunculifolia
table(BBBA2$Classification.according.to.CLO.PLA.traits)
BBBA2 <- BBBA2 %>% 
  dplyr::rename(CGO = Original.classification) %>%
  mutate(BPI =
           case_when(
             CGO == "tree" ~ 5,# only Baccharis dracunculifolia, see  Tropical Ecology 55(2): 167-176, 2014
             CGO == "perennial herb non clonal" ~ 2,
             CGO == "clonal herb" ~ 3, 
             CGO == "xylopodium" ~ 5, 
             TRUE ~ NA)
  ) %>%
  mutate(CSI =
           case_when(
             Classification.according.to.CLO.PLA.traits == "hypogeogenous rhizome" ~ 2,
             Classification.according.to.CLO.PLA.traits == "epigeogenous rhizome" ~ 2,
             CGO == "tree" ~ 0,# # only Baccharis dracunculifolia, see  Tropical Ecology 55(2): 167-176, 2014
             CGO == "perennial herb non clonal" ~ 0,
             CGO == "xylopodium" ~ 0, 
             #CGO == "clonal herb" ~ NA, # ATTENTION: THIS NEEDS CHECKING FROM ALE  #excluded because no unique CSI and the 
                                                                                    # 'Classification.according.to.CLO.PLA.traits' was used
             Classification.according.to.CLO.PLA.traits == "hypogeogenous rhizome" ~ 2,
             Classification.according.to.CLO.PLA.traits == "epigeogenous rhizome" ~ 2,
             TRUE ~ NA)
  ) %>% 
  mutate(is.woody=if_else(CGO=="tree" | CGO=="xylopodium" ,T,F)) %>% 
  mutate(is.clonal=if_else(CGO %in% c("clonal herb"),T,F)) %>% 
  mutate(is.resprouting=if_else(CGO %in% c("xylopodium"),T,F)) %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
              filter(!is.na(Plant_growth_form)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY4 %>% dplyr::select(Resolved_name,Plant_resprouting_capacity) %>% 
              filter(!is.na(Plant_resprouting_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  mutate(BPT=BPI,
         is.woody.below=is.woody) %>% 
  mutate(is.woody.above=case_when(
    Plant_growth_form %in% c("herb", "Herb", "herb/shrub", "herb/non-woody", "herbaceous") ~ F,
    Plant_growth_form=="tree" | Plant_growth_form=="woody" | Plant_growth_form=="shrub"| 
      Plant_growth_form=="Shrub" | Plant_growth_form=="shrub/tree" | Plant_growth_form=="shrub/tree/woody" ~ T,
    Resolved_name=="Baccharis dracunculifolia" ~ T,
    grepl("Aldama",Resolved_name) ~ F,
    grepl("Calea",Resolved_name) ~ F,
    grepl("Chromolaena",Resolved_name) ~ F,
    grepl("Dimerostemma",Resolved_name) ~ F,
    grepl("Mikania",Resolved_name) ~ F,
    grepl("Trichocline",Resolved_name) ~ F,
    grepl("Wedelia",Resolved_name) ~ F,
    Resolved_name %in% c("Baccharis crispa","Baccharis pentodonta", "Calea abbreviata",
                           "Chrysolaena nicolackii", "Elephantopus racemosus",
                           "Gyptis commersonii","Stomatanthes corumbensis",
                           "Eupatorium xylorhizum", "Lepidaploa almasensis",
                           "Lessingianthus elegans",
                           "Lessingianthus exiguus","Lessingianthus reitzianus",
                           "Lessingianthus warmingianus",
                           "Lucilia lycopodioides", "Minasia ramosa","Panphalea commersonii",
                           "Porophyllum bahiense","Pterocaulon polypterum", "Richterago riparia",
                           "Lepidaploa psilostachya") ~ F,
    Resolved_name %in% c("Baccharis curitybensis","Baccharis pentaptera",
                           "Baccharis subdentata", "Campuloclinium chlorolepis",
                           "Gochnatia barrosoae","Lessingianthus brevipetiolatus",
                           "Lessingianthus zuccarinianus","Moquiniastrum pulchrum") ~ T)) %>% 
  mutate(BPT = case_when(
    BPI == 5 & !is.woody.above ~ 5.1,
    BPI == 5 & is.woody.above ~ 5.2,
    BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    BPI == 6 & is.woody.above ~ 6.2,
    TRUE~BPT) ) %>% 
  mutate(is.woody.above = if_else(BPT==2 | BPT==3,F, is.woody.above))

table(BBBA2$is.woody, exclude=NULL)
table(BBBA2$is.clonal, exclude=NULL)
table(BBBA2$is.resprouting, exclude=NULL)
BBBA2 %>% filter(CGO=="tree")
table(BBBA2$CGO, exclude=NULL)
table(BBBA2$BPT, BBBA2$is.woody.below, exclude=NULL)
table(BBBA2$BPT, BBBA2$is.woody.above, exclude=NULL)
BBBA2 %>% filter(BPT==5)
BBBA2 %>% filter(grepl("Baccharis",Resolved_name))

write.csv(BBBA2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                      "Budbank_Brazilian_Asteraceae_Table1_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "latin1")

### 3. CLO-PLA ###
# from https://clopla.butbn.cas.cz/index.php?page=intro
# joined from several sub-databases by Tomas Herben Dec 2023
CLOPLA2 <- read.csv("1_Harmonized_Taxa/sumfile-merged-all_Harmonized6.csv",  
                  encoding = "latin1")

str(CLOPLA2)
table(CLOPLA2$GF, exclude=NULL)
table(CLOPLA2$BGF, exclude=NULL)
table(CLOPLA2$spread, exclude=NULL)
CLOPLA2 <- CLOPLA2 %>%
  dplyr::rename(CGO = BGF) %>%
  mutate(CGO=if_else(Resolved_name=="Euphorbia segetalis","annual",CGO),
         GF=if_else(Resolved_name=="Euphorbia segetalis","annual",GF),
         GF=if_else(Resolved_name=="Brachypodium retusum","clonal perennial",GF)) %>%   
  mutate(BPI= case_when( 
    GF == "annual" ~ 1,
    GF == "nonclonal perennial" ~ 2,
    GF == "clonal perennial"~ 3,
    GF == "nonclonal woody"  ~ 4, 
    GF == "clonal woody" ~ 6,
    #ATTENTION: There are species in here that need to moved to BPI= 5 
    # (resprouting trees), 
    # this needs manual separation by species knowledge (postponed to after workshop)
    TRUE ~ NA)
  ) %>%
  mutate(CSI= case_when (
    GF == "annual"  ~ 0,
    #clonal == 0 ~ 0,
    GF == "nonclonal perennial"  ~ 0,
    GF == "nonclonal woody"  ~ 0,
    GF == "clonal perennial" & spread <= 5 ~ 1, 
              #the CSI in non woody species depends on how far they spread per year, we set threshold at 5cm;
              # but this means species without spread data get CSI = NA
    GF == "clonal perennial" & spread > 5 ~ 2,
    GF == "clonal woody"  ~ 2
    ))%>%
  mutate(ClonalGenetExtent = spread * persistence) %>% 
  mutate(is.woody=if_else(GF=="clonal woody" | GF=="nonclonal woody" ,T,F)) %>% 
  mutate(is.clonal=if_else(GF %in% c("clonal perennial","clonal woody"),T,F)) %>% 
  mutate(is.resprouting=if_else(CGO %in% c("root sprouting"),T,NA)) %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
              filter(!is.na(Plant_growth_form)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY4 %>% dplyr::select(Resolved_name,Plant_resprouting_capacity) %>% 
              filter(!is.na(Plant_resprouting_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  mutate(is.woody=if_else(Resolved_name=="Botrychium biternatum",F,is.woody),
         is.woody=if_else(Resolved_name=="Limonium carolinianum",F,is.woody),
         is.woody=if_else(Resolved_name == "Krascheninnikovia ceratoides",T,is.woody)) %>% 
  mutate(BPI=if_else(Resolved_name=="Krascheninnikovia ceratoides",4,BPI),
         BPI=if_else(Resolved_name=="Limonium carolinianum",2,BPI),
         BPI=if_else(Resolved_name=="Botrychium biternatum",2,BPI)) %>% 
  mutate(BPI=if_else(Resolved_name %in% c("Crucianella maritima","Gutierrezia sarothrae",
                                            "Thylacospermum caespitosum"),5,BPI)) %>% 
  mutate(BPT=BPI,
         is.woody.below=is.woody) %>% 
  mutate(is.woody.above=case_when(
    !is.woody.below ~ F,
    BPI %in% c(1,2,3) ~ F,
    BPI %in% c(5) ~ T,
    Plant_growth_form %in% c("herb", "Herb", "herb/shrub", "herb/non-woody", "herbaceous",
                             "herb.","Subshrub, Forb/herb") ~ F,
    Plant_growth_form %in% c("tree", "Tree","woody", "shrub", "Shrub", "shrub/tree", "tree / shrub",
                            "shrub/tree/woody","shrub/woody") ~ T,
    Plant_woodiness %in% c("woody", "Woody","W","w") ~ T,
    Plant_woodiness %in% c("non-woody","semi-woody") ~ F,
    Resolved_name %in% c("Caragana brachypoda", "Carpinus betulus","Clematis vitalba","Cornus mas",
                           "Corylus avellana","Dasiphora arbuscula","Dasiphora dryadanthoides",
                           "Hypericum tenuifolium","Krascheninnikovia ceratoides","Opuntia","Smilax auriculata",
                           "Suaeda vera","Ulmus minor",
                           "Wisteria frutescens") ~ T,
    Resolved_name %in% c("Euphorbia spinosa","Jacobaea maritima","Santolina etrusca",
                           "Thymus linearis") ~ F)) %>% 
  mutate(is.woody.above=if_else(BPI==4 & is.na(is.woody.above) & Plant_growth_form=="conifer",T,is.woody.above)) %>% 
  mutate(is.resprouting=case_when(
    Plant_resprouting_capacity %in% c("Yes","yes","high") ~ T,
    Plant_resprouting_capacity %in% c("No","no","Not Resprouting") ~ F,
    is.woody.below & !is.woody.above ~ T,
  )) %>% 
  mutate(BPT = case_when(
    BPI == 4 & !is.woody.above & is.resprouting ~ 5.1,
    BPI == 4 & is.na(is.woody.above) & is.resprouting ~ 5.2,
    BPI == 4 & is.woody.above & is.resprouting ~ 5.2,
    BPI == 5 & !is.woody.above ~ 5.1,
    BPI == 5 & is.woody.above ~ 5.2,
    BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    BPI == 6 & is.woody.above ~ 6.2,
    TRUE~BPT) )  %>% 
  mutate(is.woody.above = if_else(BPT==5.2,T, is.woody.above),
         is.woody.above = if_else(BPT==5.1,F, is.woody.above))
  

table(CLOPLA2$is.woody, exclude=NULL)
table(CLOPLA2$is.clonal, exclude=NULL)
table(CLOPLA2$is.resprouting, exclude=NULL)
CLOPLA2 %>% filter(BPT==4 & !is.woody.above)
CLOPLA2 %>% filter(BPT==4 & is.na(is.woody.above))
CLOPLA2 %>% filter(BPT==6)
CLOPLA2 %>% filter(BPT==6.1 & !is.woody.above)
CLOPLA2 %>% filter(is.na(BPT))
table(CLOPLA2$BPT, CLOPLA2$is.woody.above, exclude=NULL)
table(CLOPLA2$BPT, CLOPLA2$is.woody.below, exclude=NULL)
'      FALSE TRUE
  1     830    0
  2    1166    0
  3    2449    0
  4       0  201
  5.1     0   11
  5.2     0   54
  6.1     0    6
  6.2     0  100'
CLOPLA2 %>%  filter(Resolved_name=="Krascheninnikovia ceratoides")

write.csv(CLOPLA2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                        "sumfile-merged-all_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "latin1")

### 5. BBBnew: A global Belowground Bud Bank database
# This is BBB from 4 expanded by Alessandra Fidelis
# version from sUnderfoot workshop 13.11.2023

BBBnew2  <- read.csv("1_Harmonized_Taxa/iDivTable_BBB_Harmonized6.csv",  
                     encoding = "UTF-8")
table(BBBnew2$type.of.BBB)
table(BBBnew2$growth.form)
table(BBBnew2$type.of.BBB,BBBnew2$growth.form)
BBBnew2 <- BBBnew2 %>%
  dplyr::rename(CGO = type.of.BBB) %>%
  mutate(Growthform=if_else(growth.form == "Shrub","shrub",growth.form)) %>% 
  mutate(BPI =
           case_when(
             CGO == "bulb" ~ 3, 
             CGO == "xylopodium" ~ 5, 
             CGO == "rhizome" ~ 3, #ATTENTION: is this always fleshy because woody rhizome is separately listed? CHECK!!
             CGO == "root crown" ~ 5, 
             CGO == "herbaceous perennial non-clonal" ~ 2, 
             CGO == "woody rhizome" ~ 6,
             TRUE ~ NA)
  ) %>%
  mutate(CSI =
           case_when(
             CGO == "bulb" ~ 1, 
             CGO == "xylopodium" ~ 0, 
             CGO == "rhizome" ~ 1, #ATTENTION: is this always fleshy because woody is rhizome is separately listed? CHECK!!
             CGO == "root crown" ~ 0, 
             CGO == "herbaceous perennial non-clonal" ~ 0, 
             CGO == "woody rhizome" ~ 2,
             TRUE ~ NA)
  ) %>% 
  mutate(is.woody=if_else(CGO=="xylopodium" | CGO=="woody rhizome" ,T,F)) %>% 
  mutate(is.clonal=if_else(CGO %in% c("rhizome","woody rhizome"),T,F)) %>% 
  mutate(is.resprouting=if_else(CGO %in% c("xylopodium"),T,NA)) %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
              filter(!is.na(Plant_growth_form)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY4 %>% dplyr::select(Resolved_name,Plant_resprouting_capacity) %>% 
              filter(!is.na(Plant_resprouting_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  mutate(BPT=BPI,
         is.woody.below=is.woody) %>% 
  mutate(is.woody.below=case_when(
    Growthform == "sub-shrub" ~ T,
    Resolved_name %in% c("Evolvulus sericeus","Lucilia nitens","Glechon ciliata") ~ F,
    Resolved_name %in% c("Richardia grandiflora","Galianthe fastigiata") ~ T,
    TRUE ~ is.woody)) %>% 
  mutate(is.woody.above=case_when(
    BPI %in% c(1,2,3) ~ F,

    Plant_growth_form %in% c("herb", "Herb", "herb/shrub", "herb/non-woody", "herbaceous",
                             "herb.","Subshrub, Forb/herb") ~ F,
    Plant_growth_form %in% c("tree", "Tree","woody", "shrub", "Shrub", "shrub/tree", "tree / shrub",
                             "shrub/tree/woody","shrub/woody") ~ T,
    Plant_woodiness %in% c("woody", "Woody","W","w") ~ T,
    Plant_woodiness %in% c("non-woody","semi-woody") ~ F,
    Resolved_name %in% c("Eugenia arenosa") ~ T,
    Resolved_name %in% c("Buchnera lavandulacea","Camarea hirsuta") ~ F,
    Growthform %in% c("shrub", "Shrub") ~ T,
    Growthform %in% c("sub-shrub", "forb") ~ F)) %>% 
  mutate(BPT = case_when(
   is.na(BPI) & is.woody.above  ~ 4,
   is.na(BPI) & !is.woody.below & !is.clonal ~ 2,
   is.na(BPI) & is.woody.below & !is.clonal & !is.woody.above ~ 5,
   # BPI == 4 & is.na(is.woody.above) & is.resprouting ~ 5.2,
    # BPI == 4 & is.woody.above & is.resprouting ~ 5.2,
    BPI == 5 & !is.woody.below & !is.woody.above ~ 2,
    BPI == 5 & is.woody.below & !is.woody.above ~ 5.1,
    BPI == 5 & is.woody.below  & is.woody.above ~ 5.2,
    BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    BPI == 6 & is.woody.above ~ 6.2,
    TRUE~BPT) ) %>% 
  mutate(is.woody.below=if_else(is.woody.above,T,is.woody.below))
  
    

table(BBBnew2$is.woody, exclude=NULL)
table(BBBnew2$is.clonal, exclude=NULL)
table(BBBnew2$is.resprouting, exclude=NULL)
table(BBBnew2$BPI, BBBnew2$is.woody, exclude=NULL)
'      FALSE TRUE <NA>
  2       11    0    0
  3       63    0    0
  5      109  153    0
  6        0    1    0
  <NA>     0    0   13'
table(BBBnew2$BPT, BBBnew2$is.woody.below, exclude=NULL)
'      FALSE TRUE
  2      86    0
  3      63    0
  4       0    3
  5       0   41
  5.1     0  115
  5.2     0   41
  6.2     0    1'
BBBnew2 %>% filter(BPT==3 & is.na(is.woody.below))
BBBnew2 %>% filter(is.na(BPT))
table(BBBnew2$BPT, BBBnew2$is.woody.above, exclude=NULL)
'       FALSE TRUE
  2      86    0
  3      63    0
  4       0    3
  5       2   39
  5.1   115    0
  5.2     0   41
  6.2     0    1'
BBBnew2 %>% filter(BPI==5 & !is.woody)
BBBnew2 %>% filter(BPT==2 & !is.woody.below)
BBBnew2 %>% filter(Resolved_name=="Anemopaegma arvense")

write.csv(BBBnew2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                        "iDivTable_BBB_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "UTF-8")

### 6. BBBSouthAfrica
# This has been Resolved by Frances 
# CGO column was modified by Frances Siebert in the raw table in google drive
# version from sUnderfoot workshop 15.11.2023
BBBSouthAfrica2 <- read.csv("1_Harmonized_Taxa/iDivTable_BBB_South_Africa_data_Harmonized6.csv", 
                            encoding = "UTF-8")
str(BBBSouthAfrica2)
table(BBBSouthAfrica2$type.of.BBB, exclude = NULL)
# xylopodium does not occur in the data
table(BBBSouthAfrica2$growth.form, exclude = NULL)
BBBSouthAfrica2 <- BBBSouthAfrica2 %>%
  dplyr::rename(CGO = type.of.BBB) %>%
  mutate(CGO=if_else(CGO=="root crown","Root crown",CGO)) %>% 
  mutate(BPI =
           case_when(
             CGO == "xylopodium" ~ 5, 
             CGO == "Non-woody rhizome" ~ 3, 
             CGO == "Root crown" ~ 5, 
             CGO == "Perennial herbaceous non-clonal" ~ 2, 
             CGO == "woody rhizome" ~ 6,
             TRUE ~ NA)
  ) %>%
  mutate(CSI =
           case_when(
             CGO == "xylopodium" ~ 0, 
             CGO == "Non-woody rhizome" ~ 1, 
             CGO == "Root crown" ~ 0, 
             CGO == "Perennial herbaceous non-clonal" ~ 0, 
             CGO == "woody rhizome" ~ 2,
             TRUE ~ NA)
  ) %>% 
  mutate(is.woody=if_else(CGO=="woody rhizome" ,T,NA)) %>% 
  mutate(is.woody=if_else(CGO %in% c("Non-woody rhizome","Perennial herbaceous non-clonal") ,F,is.woody)) %>% 
  mutate(is.clonal=if_else(CGO %in% c("Non-woody rhizome","woody rhizome"),T,F)) %>% 
  mutate(is.resprouting=if_else(CGO %in% c("Root crown"),T,NA)) %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
              filter(!is.na(Plant_growth_form)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY4 %>% dplyr::select(Resolved_name,Plant_resprouting_capacity) %>% 
              filter(!is.na(Plant_resprouting_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  mutate(BPT=BPI,
         is.woody.below=is.woody) %>% 
  mutate(is.woody.below=case_when(
    growth.form %in% c("shrub","Forb/shrub","Forb/Shrub","shrub(creeper)") ~ T,
    CGO == "Root crown" ~ T,
    TRUE ~ is.woody.below)) %>% 
  mutate(is.woody.above=case_when(
    BPI %in% c(1,2,3) ~ F,
    growth.form %in% c("shrub","shrub(creeper)") ~ T,
    Plant_growth_form %in% c("herb", "Herb", "herb/shrub", "herb/non-woody", "herbaceous",
                             "herb.","Subshrub, Forb/herb") ~ F,
    Plant_growth_form %in% c("tree", "Tree","woody", "shrub", "Shrub", "shrub/tree", "tree / shrub",
                             "shrub/tree/woody","shrub/woody") ~ T,
    Plant_woodiness %in% c("woody", "Woody","W","w") ~ T,
    Plant_woodiness %in% c("non-woody","semi-woody") ~ F,
    Resolved_name %in% c("Solanum campylacanthum") ~ T,
    Resolved_name %in% c("Afroaster comptonii",
                           "Crabbea cirsioides","Helichrysum nudifolium var. oxyphyllum",
                           "Helichrysum platypterum",
                           "Senecio venosus","Justicia flava") ~ F)) %>% #,
  #   Growthform %in% c("shrub") ~ T,
  #   Growthform %in% c("sub-shrub") ~ F)) %>% 
  mutate(BPT = case_when(
    is.na(BPI) & is.woody.above  ~ 4,
    # is.na(BPI) & !is.woody.above & !is.clonal ~ 3,
    # BPI == 4 & is.na(is.woody.above) & is.resprouting ~ 5.2,
    # BPI == 4 & is.woody.above & is.resprouting ~ 5.2,
    BPI == 5 & !is.woody.below & !is.woody.above ~ 2,
    BPI == 5 & is.woody.below & !is.woody.above ~ 5.1,
    BPI == 5 & is.woody.below  & is.woody.above ~ 5.2,
    BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    BPI == 6 & is.woody.above ~ 6.2,
    TRUE~BPT) ) #%>% 
  #mutate(is.woody.below=if_else(is.woody.above,T,is.woody.below))

table(BBBSouthAfrica2$is.woody, exclude=NULL)
table(BBBSouthAfrica2$is.clonal, exclude=NULL)
table(BBBSouthAfrica2$is.resprouting, exclude=NULL)
table(BBBSouthAfrica2$CGO, exclude = NULL)
table(BBBSouthAfrica2$BPT,BBBSouthAfrica2$is.woody.below, exclude = NULL)
table(BBBSouthAfrica2$BPT,BBBSouthAfrica2$is.woody.above, exclude = NULL)
BBBSouthAfrica2 %>% filter(BPT==6)
  
write.csv(BBBSouthAfrica2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                        "iDivTable_BBB_South_Africa_data_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "UTF-8")


#10. AlineAlessandra = DataBaseNew_AlineAlessandra_Dec2023.xlsx
AlineAlessandra2 <- read.csv("1_Harmonized_Taxa/DataBaseNew_AlineAlessandra_Dec2023_Harmonized6.csv",
                             fileEncoding = "latin1")
str(AlineAlessandra2)
table(AlineAlessandra2$BBB.or.CGO)
table(AlineAlessandra2$woodiness.aboveground, exclude=NULL)
table(AlineAlessandra2$aboveground.growth.form, exclude=NULL)
table(AlineAlessandra2$clonality, exclude=NULL)
table(AlineAlessandra2$resprouting.capability, exclude=NULL)
table(AlineAlessandra2$BPI, exclude=NULL)

table(AlineAlessandra2$fine.root.N, exclude=NULL)
table(AlineAlessandra2$rooting.depth_cm , exclude=NULL)

AlineAlessandra2 <- AlineAlessandra2 %>% 
  dplyr::rename(CGO = BBB.or.CGO) %>%
  mutate(BPI = as.numeric(substr(BPI,1,1)),
         CSI=as.numeric(substr(mobility.potential_CSI,1,1))) %>% 
  mutate(is.woody=if_else(CGO %in% c("xylopodium","woody rhizome"),T,NA)) %>% 
  mutate(is.clonal=if_else(clonality %in% c("Clonal"),T,F)) %>% 
  mutate(is.resprouting=if_else(resprouting.capability %in% c("Resprouting"),T,F)) %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
              filter(!is.na(Plant_growth_form)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY4 %>% dplyr::select(Resolved_name,Plant_resprouting_capacity) %>% 
              filter(!is.na(Plant_resprouting_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  mutate(BPT=BPI,
         is.woody.below=is.woody) %>% 
  mutate(is.woody.below=case_when(
    BPI %in% c(1,2,3) ~ F,
    BPI %in% c(4,5,6) ~ T,
    TRUE ~ is.woody.below)) %>% 
  mutate(is.woody.above=case_when(
    aboveground.growth.form %in% c("Shrub","Tree") ~ T,
    aboveground.growth.form %in% c("Forb","Graminoid","Sub-shrub") ~ F,
    woodiness.aboveground == "Woody" ~ T,
    woodiness.aboveground == "Herbaceous" ~ F,
    BPI %in% c(1,2,3) ~ F)) %>% 
  mutate(BPT = case_when(
    # is.na(BPI) & is.woody.above  ~ 4,
    # is.na(BPI) & !is.woody.above & !is.clonal ~ 3,
    # BPI == 4 & is.na(is.woody.above) & is.resprouting ~ 5.2,
    # BPI == 4 & is.woody.above & is.resprouting ~ 5.2,
    is.woody.below & is.woody.above & !is.clonal & !is.resprouting ~ 4,
    BPI == 5 & !is.woody.below & !is.woody.above ~ 2,
    BPI == 5 & is.woody.below & !is.woody.above ~ 5.1,
    BPI == 5 & is.woody.below  & is.woody.above ~ 5.2,
    BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    BPI == 6 & is.woody.above ~ 6.2,
    TRUE~BPT) ) #%>% 


table(AlineAlessandra2$is.woody, exclude=NULL)
table(AlineAlessandra2$is.clonal, exclude=NULL)
table(AlineAlessandra2$is.resprouting, exclude=NULL)
table(AlineAlessandra2$BPT, AlineAlessandra2$is.woody.below, exclude=NULL)
table(AlineAlessandra2$BPT, AlineAlessandra2$is.woody.above, exclude=NULL)
table(AlineAlessandra2$BPT, AlineAlessandra2$woodiness.belowground, exclude=NULL)
table(AlineAlessandra2$BPT, AlineAlessandra2$woodiness.aboveground, exclude=NULL)
table(AlineAlessandra2$woodiness.belowground, AlineAlessandra2$woodiness.aboveground, exclude=NULL)
AlineAlessandra2 %>% filter(BPT==4 & is.na(is.woody.below))
AlineAlessandra2 %>% filter(grepl("Casearia sylvestris",Resolved_name))

write.csv(AlineAlessandra2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                         "DataBaseNew_AlineAlessandra_Dec2023_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "latin1")


#11. Siebert = Siebert_Frances_More data from Africa_Nov2023.xlsx
Siebert2 <- read.csv("1_Harmonized_Taxa/Siebert_Frances_More data from Africa_Nov2023_Harmonized6.csv",  
                     fileEncoding = "latin1")
str(Siebert2)
table(Siebert2$BBB.or.CGO, exclude=NULL)
table(Siebert2$clonality, exclude=NULL)
table(Siebert2$resprouting.capability, exclude=NULL)
table(Siebert2$BPI, exclude=NULL)
table(Siebert2$woodiness.belowground, exclude=NULL)
table(Siebert2$woodiness.aboveground, exclude=NULL)

table(Siebert2$fine.root.N, exclude=NULL)
table(Siebert2$rooting.depth_cm , exclude=NULL)

Siebert2 <- Siebert2 %>% 
  dplyr::rename(CGO = BBB.or.CGO) %>%
  mutate(BPI = as.numeric(substr(BPI,1,1)),
         CSI=as.numeric(substr(mobility.potential_CSI,1,1))) %>% 
  ## HB changes made 26.11.2024
  mutate(is.woody=if_else(woodiness.belowground %in% c("Woody"),T,F)) %>% 
  #mutate(is.woody=if_else(woodiness.aboveground %in% c("Woody"),T,F)) %>% 
  mutate(is.clonal=if_else(clonality %in% c("Clonal"),T,F)) %>% 
  mutate(is.resprouting=if_else(resprouting.capability %in% c("Resprouting"),T,F)) %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
            filter(!is.na(Plant_growth_form)), 
          by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY4 %>% dplyr::select(Resolved_name,Plant_resprouting_capacity) %>% 
              filter(!is.na(Plant_resprouting_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  mutate(BPT=BPI,
         is.woody.below=is.woody,
         is.woody.above=if_else(woodiness.aboveground=="Woody",T,F)) %>% 
  mutate(BPT = case_when(
    # is.na(BPI) & is.woody.above  ~ 4,
    # is.na(BPI) & !is.woody.above & !is.clonal ~ 3,
    # BPI == 2 & is.woody.below & is.resprouting ~ 5.2,
    # BPI == 4 & is.woody.above & is.resprouting ~ 5.2,
    BPI == 5 & !is.woody.below & !is.woody.above ~ 2,
    BPI == 2 & is.woody.below & !is.woody.above & is.resprouting ~ 5.1,
    BPI == 5 & is.woody.below & !is.woody.above ~ 5.1,
    BPI == 5 & is.woody.below  & is.woody.above ~ 5.2,
    BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    BPI == 6 & is.woody.above ~ 6.2,
    BPI == 3 & is.woody.below & is.woody.above & is.clonal ~ 6.2,
    TRUE~BPT) ) #%>% 

table(Siebert2$is.woody, exclude=NULL)
table(Siebert2$is.clonal, exclude=NULL)
table(Siebert2$is.resprouting, exclude=NULL)
table(Siebert2$BPI, Siebert2$is.woody, exclude=NULL)
table(Siebert2$BPI, Siebert2$woodiness.aboveground, exclude=NULL)
table(Siebert2$BPI, Siebert2$woodiness.belowground, exclude=NULL)
table(Siebert2$BPT, Siebert2$is.woody.below, exclude=NULL)
table(Siebert2$BPT, Siebert2$is.woody.above, exclude=NULL)
Siebert2 %>% filter(BPT==3 & is.woody.below)

write.csv(Siebert2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                      "Siebert_Frances_More data from Africa_Nov2023_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "latin1")

# 1.12. Laughlin_additions = laughlin_sUnderfoot_2023_additions.xlsx
Laughlin_additions2 <- read.csv("1_Harmonized_Taxa/laughlin_sUnderfoot_2023_additions_Harmonized6.csv",  
                                fileEncoding = "latin1")
str(Laughlin_additions2) # 259 obs. of  16  variables:

table(Laughlin_additions2$lifeForm, exclude=NULL)
table(Laughlin_additions2$lifeHistory, exclude=NULL)
table(Laughlin_additions2$BudBankDensity_numberPerTiller, exclude=NULL)


Laughlin_additions2 <- Laughlin_additions2 %>% 
  mutate(BPI=case_when(
    lifeHistory %in% c("Annual","Annual/Perennial") ~ 1,
    TRUE ~ NA), 
         CSI=NA) %>% 
  mutate(is.woody=if_else(lifeForm %in% c("Shrub","Subshrub"),T,NA),
         is.woody=if_else(lifeHistory %in% c("Annual"),F,is.woody)) %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
              filter(!is.na(Plant_growth_form)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY4 %>% dplyr::select(Resolved_name,Plant_resprouting_capacity) %>% 
              filter(!is.na(Plant_resprouting_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY5 %>% dplyr::select(Resolved_name,Plant_vegetative_regeneration_capacity) %>% 
              filter(!is.na(Plant_vegetative_regeneration_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY6 %>% dplyr::select(Resolved_name,Plant_lifespan) %>% 
              filter(!is.na(Plant_lifespan)), 
            by="Resolved_name", multiple="first") %>% 
  mutate(is.resprouting=if_else(Plant_resprouting_capacity %in% c("Resprouting","yes"),T,NA),
         is.resprouting=if_else(Resolved_name %in% c("Tetradymia canescens"),T,is.resprouting)) %>% 
  mutate(is.clonal=case_when(
    Plant_vegetative_regeneration_capacity %in% 
      c("above ground stolon (long runner)","Rhizome far-creeping",
        "rhizomes or stolons","running rhizome") ~ T,
    Resolved_name %in% c("Artemisia cana","Artemisia frigida","Artemisia tridentata",
                           "Artemisia tripartita","Ribes montigenum","Vaccinium caespitosum",
                           "Vaccinium scoparium") ~ T,
    Plant_vegetative_regeneration_capacity %in% 
      c("Little or no vegetative spread","no","none") ~ F,
    Resolved_name %in% c("Atriplex canescens","Ericameria nauseosa",
                           "Eriogonum effusum","Gutierrezia sarothrae",
                           "Krascheninnikovia ceratoides  subsp. lanata",
                           "Tetradymia canescens") ~ F,
    TRUE~NA)) %>% 
  mutate(is.annual = if_else(Plant_lifespan %in% c("always annual","always annual, always biennial",
                                                   "annual","annual/biennial"),T,NA),
         is.annual = if_else(Plant_lifespan %in% c(">10","2","always pluriennial-pollakanthic",
                                                   "annual","biannual","biennial","biennial/perennial",
                                                   "per","perennial"),F,is.annual)) %>% 
  mutate(BPT=BPI,
         is.woody.below=is.woody,
         is.woody.below=if_else(lifeHistory %in% c("Annual","Annual/Perennial"),F,is.woody.below), 
         is.woody.below=if_else(family %in% c("Poaceae"),F,is.woody.below), 
         is.woody.below=if_else(Plant_growth_form %in% c("herbaceous monocotyl","Grass","grass"),F,is.woody.below)) %>% 
  mutate(is.woody.above=case_when(
    BPI %in% c(1,2,3) ~ F,
    !is.woody.below ~ F,
    lifeForm %in% c("Forb","Graminoid","Subshrub") ~ F,
    lifeForm %in% c("Shrub") ~ T,
    family %in% c("Poaceae") ~ F,
    TRUE ~ NA)) %>% 
  mutate(BPT = case_when(
    !is.woody.below & !is.woody.above & is.annual ~ 1,
    !is.woody.below & !is.woody.above & !is.annual & !is.clonal ~ 1,
    !is.woody.below & !is.woody.above & is.resprouting ~ 2,
    !is.woody.below & !is.woody.above & !is.clonal ~ 2,
    !is.woody.below & !is.woody.above & is.clonal ~ 3,
    is.woody.below & !is.woody.above & is.resprouting ~ 5.1,
    is.woody.below & !is.woody.above & !is.clonal ~ 5.1,
    is.woody.below & is.woody.above & !is.clonal ~ 5.2,
    is.woody.below & !is.woody.above & is.clonal ~ 6.1,
    is.woody.below & is.woody.above & is.clonal ~ 6.2,
    # is.na(BPI) & !is.woody.above & !is.clonal ~ 3,
    # BPI == 2 & is.woody.below & is.resprouting ~ 5.2,
    # BPI == 4 & is.woody.above & is.resprouting ~ 5.2,
    # BPI == 5 & !is.woody.below & !is.woody.above ~ 2,
    # BPI == 5 & is.woody.below & !is.woody.above ~ 5.1,
    # BPI == 5 & is.woody.below  & is.woody.above ~ 5.2,
    # BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    # BPI == 6 & is.woody.above ~ 6.2,
    TRUE~BPT) ) #%>% 


table(Laughlin_additions2$is.woody.below, exclude=NULL)
table(Laughlin_additions2$is.clonal, exclude=NULL)
table(Laughlin_additions2$Plant_resprouting_capacity, exclude=NULL)
table(Laughlin_additions2$Plant_lifespan, exclude=NULL)
table(Laughlin_additions2$Plant_vegetative_regeneration_capacity, exclude=NULL)
table(Laughlin_additions2$BPT, Laughlin_additions2$is.woody.below, exclude=NULL)
table(Laughlin_additions2$BPT, Laughlin_additions2$is.woody.above, exclude=NULL)
Laughlin_additions2 %>% filter(BPT==1 & is.na(is.woody.above))
Laughlin_additions2 %>% filter(is.na(BPT) & !is.woody.below)
Laughlin_additions2 %>% filter(is.na(BPT) & is.clonal)
Laughlin_additions2 %>% filter(Resolved_name=="Artemisia tripartita")
Laughlin_additions2 %>% filter(!is.woody.below & is.clonal)

write.csv(Laughlin_additions2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                         "laughlin_sUnderfoot_2023_additions_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "latin1")

#14. ITEX = TVC_SPP_20120502.csv
# cleaned by Xin Jing 23.10.2023
ITEX2 <- read.csv("1_Harmonized_Taxa/itex_spp_clonal_traits_Harmonized6.csv")
str(ITEX2) 
table(ITEX2$is.woody_itex, exclude=NULL)
'FALSE  TRUE  <NA> 
  904   115    33'
table(ITEX2$is.clonal_itex, exclude=NULL)
'FALSE  TRUE  <NA> 
  105   251   696 '
ITEX2 <- ITEX2 %>% 
  left_join(TRY2 %>% dplyr::select(Resolved_name,Plant_growth_form) %>% 
              filter(!is.na(Plant_growth_form)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY3 %>% dplyr::select(Resolved_name,Plant_woodiness) %>% 
              filter(!is.na(Plant_woodiness)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY4 %>% dplyr::select(Resolved_name,Plant_resprouting_capacity) %>% 
              filter(!is.na(Plant_resprouting_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY5 %>% dplyr::select(Resolved_name,Plant_vegetative_regeneration_capacity) %>% 
              filter(!is.na(Plant_vegetative_regeneration_capacity)), 
            by="Resolved_name", multiple="first") %>% 
  left_join(TRY6 %>% dplyr::select(Resolved_name,Plant_lifespan) %>% 
              filter(!is.na(Plant_lifespan)), 
            by="Resolved_name", multiple="first") %>%
  mutate(is.annual = if_else(Plant_lifespan %in% c("1","always annual",
                                                   "annual","Annual","annual/biennial"),T,NA), 
         is.annual = if_else((!is.annual | is.na(is.annual)) & !is.na(Plant_lifespan),F,is.annual)) %>%
  mutate(is.resprouting=if_else(Plant_resprouting_capacity %in% c("Resprouting","yes","Yes"),T,NA),
       is.resprouting=if_else(Plant_resprouting_capacity %in% c("0","no","No","Not Resprouting",
                                                                "not resprouting after defoliation"),F,is.resprouting)) %>% 
  mutate(is.clonal=case_when(
    is.clonal_itex ~ T,
    Plant_vegetative_regeneration_capacity %in% 
      c("above ground stolon (long runner)","Rhizome far-creeping","Rhizome shortly creeping",
        "Extensively creeping and rooting at nodes","Shortly creeping and rooting at nodes",
        "rhizomes or stolons","running rhizome","Shortly creeping, stolons in illuminated medium",
        "spontaneous, connection long","spontaneous, connection short","yes") ~ T,
    # Resolved_name %in% c("Artemisia cana","Artemisia frigida","Artemisia tridentata",
    #                        "Artemisia tripartita","Ribes montigenum","Vaccinium caespitosum",
    #                        "Vaccinium scoparium") ~ T,
    Plant_vegetative_regeneration_capacity %in% 
      c("Little or no vegetative spread","no","none") ~ F,
    # Resolved_name %in% c("Eriogonum effusum","Atriplex canescens","Ericameria nauseosa",
    #                        "Eriogonum effusum","Gutierrezia sarothrae",
    #                        "Krascheninnikovia ceratoides  subsp. lanata",
    #                        "Tetradymia canescens") ~ F,
    TRUE~NA)) %>% 
  mutate(is.woody.above=case_when(
    is.woody_itex ~ T,
    is.annual ~ F,
    Plant_woodiness %in% c("0","Grass&Sedges","h","H","Herbaceous","non-woody",
                           "semi-woody","Semi-woody") ~ F,
    Plant_woodiness %in% c("w","W","woody","Woody") ~ T,
    TRUE ~ NA)) %>%
  mutate(is.woody.below=case_when(
    is.woody.above ~ T,
    is.annual ~ F,
    TRUE ~ NA)) %>%
  mutate(BPT = case_when(
    is.annual ~ 1,
    !is.woody.below & !is.woody.above & !is.annual & !is.clonal ~ 1,
    !is.woody.below & !is.woody.above & is.resprouting ~ 2,
    !is.woody.below & !is.woody.above & !is.clonal ~ 2,
    !is.woody.below & !is.woody.above & is.clonal ~ 3,
    is.woody.below & is.woody.above & !is.resprouting & !is.clonal ~ 4,
    is.woody.below & !is.woody.above & is.resprouting ~ 5.1,
    is.woody.below & is.woody.above & is.resprouting  ~ 5.2, #& !is.clonal
    is.woody.below & is.woody.above & !is.clonal ~ 5.2,
    is.woody.below & !is.woody.above & is.clonal ~ 6.1,
    is.woody.below & is.woody.above & is.clonal ~ 6.2,
    # is.na(BPI) & !is.woody.above & !is.clonal ~ 3,
    # BPI == 2 & is.woody.below & is.resprouting ~ 5.2,
    # BPI == 4 & is.woody.above & is.resprouting ~ 5.2,
    # BPI == 5 & !is.woody.below & !is.woody.above ~ 2,
    # BPI == 5 & is.woody.below & !is.woody.above ~ 5.1,
    # BPI == 5 & is.woody.below  & is.woody.above ~ 5.2,
    # BPI == 6 & is.woody.below & !is.woody.above ~ 6.1,
    # BPI == 6 & is.woody.above ~ 6.2,
    TRUE~NA) ) #%>% 


  

table(ITEX2$Plant_growth_form)
table(ITEX2$Plant_woodiness)
table(ITEX2$Plant_vegetative_regeneration_capacity, exclude=NULL)
table(ITEX2$Plant_lifespan, exclude=NULL)
table(ITEX2$Plant_resprouting_capacity, exclude=NULL)
table(ITEX2$BPT, ITEX2$is.woody.below, exclude=NULL)
'        FALSE TRUE <NA>
  1       20    0    0
  4        0    2    0
  5.2      0    9    0
  6.2      0   59    0
  <NA>     0   58  904'
table(ITEX2$BPT, ITEX2$is.woody.above, exclude=NULL)
table(ITEX2$BPT, ITEX2$is.woody.above, ITEX2$is.annual, exclude=NULL)
table(ITEX2$is.annual, exclude=NULL)
table(ITEX2$is.resprouting, ITEX2$is.woody.below, exclude=NULL)

ITEX2 %>% filter(BPT==1 & is.woody.above)
ITEX2 %>% filter(is.resprouting)

write.csv(ITEX2, here("2_Harmonized_Taxa_and_reclassified_CGO", 
                                    "itex_spp_clonal_traits_Harmonized6_CGO.csv"), 
          row.names = F, fileEncoding = "latin1")
