# sUnderfoot pipeline to harmonize the following databases
# 1. GRoot = GRooTFullVersion.csv
# 2. RSIP = nph18031-sup-0001-datasets1.xlsx
# 3. CloPla = sumfile-merged-all.txt
# 4. BBB Budbank = BBBdb_2017.11.xlsx
# 5. BBBnew = iDivTable_BBB_copy
# 6. BBBSouthAfrica = iDivTable_BBB_South Africa data.xlsx
# 7. Bud bank Afrotropical grasslands = 20210830 Species list + BG-groups update USE THIS FOR ANALYSES NEW EDITED 11.14.23.xlsx
# 8. Budbank Brazilian Asteraceae = Budbank_Brazilian_Asteraceae_Table1_harmonized.xlsx
# 9. Root traits tropical savanna = belowground_traits.csv
#    revised by Alessandra Fidelis 20.03.2024 = root_traits_revised.csv
#10. AlineAlessandra = DataBaseNew_AlineAlessandra_Dec2023.xlsx
#11. Siebert = Siebert_Frances_More data from Africa_Nov2023.xlsx
#12. Laughlin_additions = laughlin_sUnderfoot_2023_additions.xlsx
#13. Sandhills_roots = Sandhills_roottraits.xlsx
#14. ITEX Data, harmonized by Xin Jing
#15. Additional traits compiled from internet resources

# all datasets produced are uploaded to Google Drive > sUnderfoot > Data > 
# 1. Harmonized Taxa

library(readxl)
library(tidyverse)
library(vegdata)
library(data.table)
library(WorldFlora)

# Functions
# returns string w/o trailing whitespace, minus or numerical
trim.trailing <- function (x) sub("\\s+$|\\s+\\d$|\\s+\\-\\s+\\d$", "", x) 

setwd("c:/Daten/iDiv4/sDiv/2023sUnderfoot/Databases/")

### sPlot backbone ###
# on the iDiv RStudio Server in the sPlot folder under:
# share/groups/sPlot/users/Gabriella/sPlot4/building/_outputs/new_backbone_clean_4_1_wip.RDS 

#load("c:/Daten/iDiv4/sPlot/sPlot4.1/Data/new_backbone_clean_4_1_wip.RDS")
#str(backbone_sPlot_TRY) # 222,653 × 15
#Backbone <- backbone_sPlot_TRY

# backbone_sPlot_clean <- read_rds("c:/Daten/iDiv4/sPlot/sPlot4.1/Data/new_backbone_clean_4_1_wip.RDS")
# backbone_sPlot_clean[grep("Anemone sylvestris", backbone_sPlot_clean$Original_name),]
# backbone_sPlot_clean$Harmonized_name_wfo[grep("Anemone sylvestris", backbone_sPlot_clean$Original_name)]

backbone_sPlot <- read_rds("c:/Daten/iDiv4/sPlot/sPlot4.1/Data/backbone_sPlot4.1.RDS")
str(backbone_sPlot) # 413,663 × 25
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

### World Flora Online Backbone
# from www.worldfloraonline.org/downloadData
# downloaded 15.11.2025, version 2025.06	Jun. 21, 2025
wfo.backbone <- read_delim("c:/Daten/iDiv4/sPlot/sPlot4.1/Data/classification.csv")
#Rows: 1648360 Columns: 29
wfo.backbone$scientificName[grep("  var",wfo.backbone$scientificName, fixed=T)]
wfo.backbone$scientificName[grep("  subsp",wfo.backbone$scientificName, fixed=T)]
# the mistakes in the WFO backbone, with two "  " before "var." have been removed in the 
# June 2025 version
# However, this is still  in the sPlot backbone
WFO.match(spec.data="Stipa comata", WFO.data=wfo.backbone)

### 1. GRooT ###
# from https://github.com/GRooT-Database/GRooT-Data/find/master
GRooT1 <- read.csv("GRooT/GRooTFullVersion.csv", 
                   encoding="latin1")
str(GRooT1) # 114222 obs. of  73 variables
# species is only the epitheton
length(unique(GRooT1$species)) # 3942

# Make a full species name, including ssp.
GRooT1$Edited_name <- paste(GRooT1$genusTNRS," ", 
                         GRooT1$speciesTNRS,
                         ifelse(nchar(GRooT1$infraspecificTNRS)==0,"",
                          paste(" ssp. ", GRooT1$infraspecificTNRS, sep="")),sep="")
# Show all subspecies
GRooT1 %>% filter(grepl("ssp",Edited_name, fixed=T)) %>% 
  dplyr::select(Edited_name) %>% distinct()
# Hybrids were coded as "infraspecficic" and thus, above receveived a "ssp." addition, which is now amended 
GRooT1 <- GRooT1 %>% mutate(Edited_name=gsub(" × ssp. "," x ",Edited_name, fixed=T),
                            Edited_name=gsub(" x ssp. "," x ",Edited_name, fixed=T),
                            Edited_name=gsub("x Bolboschoenoplectus ssp. mariqueter","x Bolboschoenoplectus mariqueter",Edited_name, fixed=T))

# Make a species while ignoring ssp.
GRooT1$Parsed_name <- paste(GRooT1$genusTNRS, 
                         GRooT1$speciesTNRS,
                         ifelse(GRooT1$speciesTNRS=="×" | GRooT1$speciesTNRS=="x",
                                GRooT1$infraspecificTNRS,""),sep=" ")
# Remove trailing spaces
GRooT1$Parsed_name <- trim.trailing(GRooT1$Parsed_name)

GRooT1 %>% filter(grepl("Solidago",Edited_name, fixed=T)) %>% 
  dplyr::select(Edited_name, Parsed_name) %>% 
  distinct()
GRooT1 %>% filter(grepl("Vignea",Edited_name, fixed=T)) %>% 
  dplyr::select(Edited_name, Parsed_name) %>% 
  distinct()

# Show single genera
GRooT1 %>% filter(grepl("Hel", genusTNRS, fixed=T)) %>% 
  dplyr::select(genusTNRS, speciesTNRS, infraspecificTNRS, Edited_name, Parsed_name) %>% 
  distinct()

# Make some manual changes
GRooT1$Parsed_name[GRooT1$Parsed_name=="Photinia ×fraseri"] <- "Photinia fraseri"
GRooT1$Parsed_name[GRooT1$Parsed_name=="Symphyotrichum ×salignum"] <- "Symphyotrichum salignum"
GRooT1$Parsed_name[GRooT1$Parsed_name=="Larix ×marschlinsii"] <- "Larix marschlinsii"
GRooT1$Parsed_name[GRooT1$Parsed_name=="Helychrisum"] <- "Helichrysum"
GRooT1$Parsed_name[GRooT1$Parsed_name=="Carex Vignea"] <- "Vignea"
GRooT1$Parsed_name[GRooT1$Parsed_name=="virgaurea caucasica"] <- "Virgaurea caucasica"
GRooT1$Parsed_name[GRooT1$Parsed_name=="x Bolboschoenoplectus"] <- "x Bolboschoenoplectus mariqueter"
GRooT1$Parsed_name[GRooT1$Edited_name=="Prestoea acuminata ssp. montana"] <- "Prestoea montana"
GRooT1$Parsed_name[GRooT1$Edited_name=="Stipa comata"] <- "Hesperostipa comata"

# how many var. and ssp. are there?
GRooT1$Parsed_name[grep(" var.",GRooT1$Parsed_name, fixed=T)] # none
GRooT1$Parsed_name[grep(" ssp.",GRooT1$Parsed_name, fixed=T)] # none

# Match the binary species name (Parsed_name) on the names in the sPlot 4 Backbone
index1 <- match(backbone_sPlot$Resolved_name, GRooT1$Parsed_name)
length(index1) # 413663
length(index1[!is.na(index1)]) # 17038

# create the sUnderfoot backbone
Backbone_sUnderfoot <- backbone_sPlot[!is.na(index1),] %>% 
  distinct()
str(Backbone_sUnderfoot)
# 17,0038 × 26]
max(Backbone_sUnderfoot$seqnum) # 413662
length(unique(Backbone_sUnderfoot$Resolved_name))# 5667

# Match the sPlot4 Backbone to the binary species name (Parsed_name)
index2 <- match(GRooT1$Parsed_name, backbone_sPlot$Resolved_name)
length(index2) # 114222
length(index2[!is.na(index2)]) #   108302
sort(unique(GRooT1$Parsed_name[is.na(index2)])) # 684
GRooT1 %>% filter(Parsed_name=="Inga")
backbone_sPlot$Original_name[grep("Inula hirta", backbone_sPlot$Original_name, fixed=T)]
backbone_sPlot$Harmonized_name_wfo[grep("Inula hirta", backbone_sPlot$Original_name, fixed=T)]
backbone_sPlot$Resolved_name[grep("Inula hirta", backbone_sPlot$Original_name, fixed=T)]
backbone_sPlot[grep("Inula hirta", backbone_sPlot$Original_name, fixed=T),]
backbone_sPlot[grep("Prestoea", backbone_sPlot$Original_name, fixed=T),]

# Take over name from the sPlot Backbone 
GRooT1$Resolved_name <- backbone_sPlot$Resolved_name[index2]
sort(unique(GRooT1$Parsed_name[is.na(GRooT1$Resolved_name)])) #684
# there was a mistake in the sPlot backbone with removing the species epitheton
# which is reverted here
GRooT1 %>% filter(grepl("Prestoea",Edited_name, fixed=T)) %>% 
  dplyr::select(Edited_name, Parsed_name, Resolved_name) %>% 
  distinct()
GRooT1$Resolved_name[GRooT1$Edited_name=="Prestoea acuminata ssp. montana"] <- "Prestoea montana"

# list of non-matching species
GRooT_species_list <- GRooT1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange()
str(GRooT_species_list) # 683 obs. of  1 variable

# match against sPlot backbone Edited_names
any(is.na(GRooT1$Parsed_name)) #F
any(is.na(backbone_sPlot$Edited_name)) #F
any(is.na(backbone_sPlot$Resolved_name)) #F
index3 <- match(GRooT1$Parsed_name[is.na(GRooT1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 5897
length(index3[!is.na(index3)]) #  5764
GRooT1$Parsed_name[is.na(GRooT1$Resolved_name)][is.na(index3)] #133
GRooT1$Resolved_name[is.na(GRooT1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot <- rbind(Backbone_sUnderfoot,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot) #17,687 × 26                           
length(unique(Backbone_sUnderfoot$Resolved_name))# 6199

# match against sPlot backbone Original_names
index4 <- match(GRooT1$Parsed_name[is.na(GRooT1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 133
length(index4[!is.na(index4)]) #  0
# no new species to be added

# list of non-matching species
GRooT_species_list <- GRooT1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange()
GRooT_species_list # 34 obs. of  1 variable

# GRooT_species_list <- GRooT_species_list %>% 
#   mutate(Parsed_name=if_else(Parsed_name=="Anemone sylvestris", "Anemone sylvestris L.", Parsed_name))

backbone_sPlot$Original_name[grep("Anemone sylvestris", backbone_sPlot$Original_name, fixed=T)]
backbone_sPlot[grep("Anemone sylvestris", backbone_sPlot$Original_name, fixed=T),] %>% as.data.frame()
backbone_sPlot[grep("Orchis morio", backbone_sPlot$Original_name, fixed=T),]
backbone_sPlot$Edited_name[grep("Listera ovata", backbone_sPlot$Edited_name, fixed=T)]
backbone_sPlot$Resolved_name[grep("Orchis morio", backbone_sPlot$Resolved_name, fixed=T)]
# WFO.one(WFO.match(spec.data="Anemonoides sylvestris",
                  # WFO.data=wfo.backbone))

# Run GRooT_species_list against wfo.backbone
GRooT_harmonized_WFO <- WFO.one(WFO.match(spec.data=GRooT_species_list$Parsed_name,
                                         WFO.data=wfo.backbone))
str(GRooT_harmonized_WFO)
# 34 obs. of  47
# GRooT_harmonized_WFO_added <- WFO.one(WFO.match(spec.data="Anemonoides sylvestris",
#                   WFO.data=wfo.backbone))
# GRooT_harmonized_WFO_added$spec.name <- "Anemone sylvestris"
# GRooT_harmonized_WFO <- GRooT_harmonized_WFO %>% 
#   filter(spec.name!="Anemone sylvestris") %>% 
#   rbind(GRooT_harmonized_WFO_added)
  
save(GRooT_harmonized_WFO, file="WFO_Harmonized/GRooT_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/GRooT_harmonized_WFO4.RData")

GRooT_harmonized_WFO$scientificName
GRooT_harmonized_WFO[GRooT_harmonized_WFO$scientificName=="Grafia",]
# GRooT_harmonized_WFO[GRooT_harmonized_WFO$spec.name=="Anemone sylvestris",]
GRooT_harmonized_WFO$scientificName[GRooT_harmonized_WFO$scientificName=="Solidago virgaurea  subsp. virgaurea"] <- "Solidago virgaurea"

GRooT2 <- GRooT1 %>% left_join(GRooT_harmonized_WFO %>% 
                      dplyr::select(spec.name , scientificName),
                               by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
GRooT2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Edited_name, Parsed_name) %>% 
  distinct() %>% 
  arrange()
# no NAs in Resolved_name

length(unique(GRooT2$Resolved_name)) #6227
GRooT2 %>% filter(grepl("Vignea", Parsed_name, fixed=T)) %>% 
  #dplyr::select(Species1, Species2, Species3) %>% 
  distinct()

GRooT2 %>% filter(grepl("Ptarmica vulgaris", Edited_name, fixed=T)) %>% 
  #dplyr::select(Species1, Species2, Species3) %>% 
  distinct()

GRooT2 %>% filter(grepl("Vignea appropinquata", Parsed_name, fixed=T)) %>% 
  dplyr::select(Edited_name, Parsed_name, Resolved_name) %>% 
  distinct() %>% 
  arrange()

GRooT2 %>% filter(grepl("subsp.", Resolved_name, fixed=T)) %>% 
  dplyr::select(Edited_name, Parsed_name, Resolved_name) %>% 
  distinct() %>% 
  arrange()
# 80

backbone_sPlot %>% filter(grepl("Senna auriculata", Original_name, fixed=T)) %>% 
  dplyr::select(Original_name, Resolved_name) %>% print(n=100)
'Backbone %>% filter(grepl("Tragopogon d", Name_short, fixed=T)) %>% 
  dplyr::select(Name_submitted, Name_correct, Name_short) %>% print(n=100)'


GRooT2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(genusTNRS, speciesTNRS, infraspecificTNRS, Edited_name, Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
# 0 

# Add species from WFO to the Backbone_sUnderfoot 
names(Backbone_sUnderfoot)
head(Backbone_sUnderfoot)
max(backbone_sPlot$seqnum) # 413663
names(GRooT_harmonized_WFO)
head(GRooT_harmonized_WFO$spec.name)
head(GRooT_harmonized_WFO)
index6 <- match(GRooT_harmonized_WFO$spec.name, GRooT2$Parsed_name)

GRooT_harmonized_WFO2 <- GRooT_harmonized_WFO %>% 
  mutate(seqnum=seq(500000+1,500000+nrow(GRooT_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=GRooT2$Edited_name[index6],
         Edited_name=GRooT2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot2 <- rbind(Backbone_sUnderfoot,
                GRooT_harmonized_WFO2 %>% 
         dplyr::select(names(Backbone_sUnderfoot)))
nrow(Backbone_sUnderfoot) # 17,687
nrow(Backbone_sUnderfoot2) #  17,721
max(Backbone_sUnderfoot2$seqnum) #500034

# write.csv(Backbone_sUnderfoot2, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", row.names = F)

backbone_sPlot %>% filter(grepl("comata", Original_name, fixed=T)) %>% as.data.frame()

names(GRooT2)
GRooT2 <- GRooT2 %>% dplyr::select(-c("scientificName"))

write.csv(GRooT2, file="GRooT/GRooTFullVersion_harmonized6.csv",  
          row.names = F, fileEncoding = "UTF-8", quote=T)

### 2. RSIP ###
# https://nph.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fnph.18031&file=nph18031-sup-0001-DatasetS1.xlsx
RSIP1 <- read_excel("RSIP/nph18031-sup-0001-datasets1.xlsx", sheet="RSIP")
str(RSIP1) # 5,654 × 70
length(unique(RSIP1$Species)) #2966
RSIP1 %>% filter(Species=="Prestoea montana") %>%  as.data.frame()
RSIP1 %>% filter(Species=="Stipa comata") %>%  as.data.frame() #6
RSIP1 %>% filter(Species=="Hesperostipa comata") %>%  as.data.frame() #0
RSIP1 <- RSIP1 %>% mutate(Edited_name=tstrsplit(Species, " ssp. ", fixed=T)[[1]]) %>% 
  # remove variants and forms
  mutate(Edited_name=tstrsplit(Edited_name, " var. ", fixed=T)[[1]]) %>% 
  mutate(Edited_name=tstrsplit(Edited_name, " fo. ", fixed=T)[[1]]) %>% 
  # keep subspecies
  #mutate(Edited_name=tstrsplit(Edited_name, " ssp ", fixed=T)[[1]]) %>% 
  #mutate(Edited_name=tstrsplit(Edited_name, " subsp. ", fixed=T)[[1]]) %>% 
  mutate(Edited_name=gsub(" ssp ", " subsp. ",Edited_name, fixed=T)) %>% 
  # remove sp. and spp. from genus names
  mutate(Edited_name=gsub(" sp.","",Edited_name, fixed=T)) %>% 
  mutate(Edited_name=gsub(" spp.","",Edited_name, fixed=T)) %>% 
  mutate(Edited_name=gsub(" spp","",Edited_name, fixed=T)) %>% 
  # remove authorities
  mutate(Edited_name=paste(tstrsplit(Edited_name, " ", fixed=T)[[1]],
                        tstrsplit(Edited_name, " ", fixed=T)[[2]],
                        sep=" ")) %>% 
  mutate(Edited_name=gsub(" NA","",Edited_name, fixed=T)) %>% 
  # turn upper case epithetons into lower case
  mutate(Edited_name=if_else(substr(tstrsplit(Edited_name," ", fixed=T)[[2]],1,1) %in% LETTERS,
          paste(tstrsplit(Edited_name," ", fixed=T)[[1]],
                tolower(tstrsplit(Edited_name," ", fixed=T)[[2]]), sep=" "),
          Edited_name)) %>% 
  # Standardisation of taxonomic names, especially taxon rank indicators and hybrid signs 
  # using vegdata package (does not work outside GermanSL)
  mutate(Parsed_name=taxname.abbr(Edited_name)) 

# remove nonsense names
RSIP1 <- RSIP1[!is.na(RSIP1$Species),]
RSIP1 <- RSIP1[RSIP1$Species!="X",]
RSIP1 <- RSIP1[RSIP1$Parsed_name!="Mix of",]
RSIP1 <- RSIP1[RSIP1$Parsed_name!="Caatinga palms",]
RSIP1 <- RSIP1[RSIP1$Parsed_name!="secondary evergreen",]
RSIP1 <- RSIP1[RSIP1$Parsed_name!="semi-closed canopy",]

# change specific names
RSIP1$Parsed_name[RSIP1$Parsed_name=="Terenna"] <- "Tarenna"
RSIP1$Parsed_name[RSIP1$Species=="Opuntia x vivipara"] <- "Opuntia vivipara"
RSIP1$Parsed_name[RSIP1$Species=="Populus x canadensis"] <- "Populus x canadensis"
RSIP1$Parsed_name[RSIP1$Species=="Eucalyptus PF1"] <- "Eucalyptus"
RSIP1$Parsed_name[RSIP1$Species=="Diospyro spp."] <- "Diospyros"
RSIP1$Parsed_name[RSIP1$Species=="Dipterocarp spp."] <- "Dipterocarpaceae"
RSIP1$Parsed_name[RSIP1$Species=="Beilschmeidia sp."] <- "Beilschmiedia"
RSIP1$Parsed_name[RSIP1$Species=="Simarouba versicolorA.St.-Hil."] <- "Simarouba versicolor"
RSIP1$Parsed_name[RSIP1$Parsed_name=="Suaeda vera."] <- "Suaeda vera"
RSIP1$Parsed_name[RSIP1$Species=="Silene_vulgaris"] <- "Silene vulgaris"
RSIP1$Parsed_name[RSIP1$Parsed_name=="Acacia tortillis"] <- "Acacia tortilis"
RSIP1$Parsed_name[RSIP1$Species=="Agropyron repen"] <- "Agropyron repens"
RSIP1$Parsed_name[RSIP1$Species=="Anagallis femina"] <- "Anagallis foemina"
RSIP1$Parsed_name[RSIP1$Species=="Eterolobium cyclocarpum"] <- "Enterolobium cyclocarpum"
RSIP1$Parsed_name[RSIP1$Species=="Nuphar alba"] <- "Nymphaea alba"
RSIP1$Parsed_name[RSIP1$Species=="Rhinathus minor"] <- "Rhinanthus minor"
RSIP1$Parsed_name[RSIP1$Parsed_name=="Scandix pecten"] <- "Scandix pecten-veneris"
RSIP1$Parsed_name[RSIP1$Species=="Syringia vulgaris"] <- "Syringa vulgaris"
RSIP1$Parsed_name[RSIP1$Parsed_name=="triticosecale"] <- "Triticosecale"
RSIP1$Parsed_name[RSIP1$Species=="Urtica diocia"] <- "Urtica dioca"
RSIP1$Parsed_name[RSIP1$Species=="Veronica hederaefolia"] <- "Veronica hederifolia"
RSIP1$Parsed_name[RSIP1$Species=="Grawia sp."] <- "Grewia"
RSIP1$Parsed_name[RSIP1$Species=="Stipa comata"] <- "Hesperostipa comata"

RSIP1 %>% dplyr::select(Species, Edited_name, Parsed_name) %>% print(n=100) %>% distinct() %>% 
  arrange(Species)

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(RSIP1$Parsed_name, Backbone_sUnderfoot2$Resolved_name)
length(index1) # 5634
length(index1[!is.na(index1)]) #   3622

# Take over name from the sUnderfoot Backbone
RSIP1$Resolved_name <- Backbone_sUnderfoot2$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(RSIP1$Parsed_name[is.na(RSIP1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 2012
length(index2[!is.na(index2)]) #  1020
RSIP1$Parsed_name[is.na(RSIP1$Resolved_name)][is.na(index2)] #992
RSIP1$Resolved_name[is.na(RSIP1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot3 <- rbind(Backbone_sUnderfoot2,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot3) # 18,508 × 25                     
max(Backbone_sUnderfoot3$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(RSIP1$Parsed_name[is.na(RSIP1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 992
length(index3[!is.na(index3)]) #  756
RSIP1$Parsed_name[is.na(RSIP1$Resolved_name)][is.na(index3)] #236
RSIP1$Resolved_name[is.na(RSIP1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot3 <- rbind(Backbone_sUnderfoot3,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot3) # 18,915 × 25                   
max(Backbone_sUnderfoot3$seqnum)

# Match the sPlot4 Backbone to the binary species name (Original_name)
index4 <- match(RSIP1$Parsed_name[is.na(RSIP1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 236
length(index4[!is.na(index4)]) #  0

RSIP1 %>% filter(Species=="Prestoea montana") %>%  as.data.frame()
RSIP1 <- RSIP1 %>% mutate(Resolved_name = if_else(Species=="Prestoea montana", "Prestoea montana", Resolved_name))
#backbone_sPlot$Resolved_name[grep("Tarenna",backbone_sPlot$Resolved_name)]

RSIP_species_list <- RSIP1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
RSIP_species_list %>% print(n=400)
# 183 species

# list of non-matching species

RSIP_harmonized_WFO <- WFO.one(WFO.match(spec.data=RSIP_species_list$Parsed_name,
                                    WFO.data=wfo.backbone))
str(RSIP_harmonized_WFO)
#183 obs. of  47
save(RSIP_harmonized_WFO, file="WFO_Harmonized/RSIP_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/RSIP_harmonized_WFO4.RData")
# RSIP_harmonized_WFO$scientificName

RSIP2 <- RSIP1 %>% left_join(RSIP_harmonized_WFO %>% 
                  dplyr::select(spec.name , scientificName),
                               by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
RSIP2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Edited_name, Parsed_name) %>% 
  distinct() %>% 
  arrange()
# 0

length(unique(RSIP2$Resolved_name)) #2749


# Add species from WFO to the Backbone_sUnderfoot 
max(Backbone_sUnderfoot3$seqnum) # 500034
index6 <- match(RSIP_harmonized_WFO$spec.name, RSIP2$Parsed_name)

RSIP_harmonized_WFO2 <- RSIP_harmonized_WFO %>% 
  mutate(seqnum=seq(500034+1,500034+nrow(RSIP_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=RSIP2$Edited_name[index6],
         Edited_name=RSIP2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot3 <- rbind(Backbone_sUnderfoot3,
                              RSIP_harmonized_WFO2 %>% 
                                dplyr::select(names(Backbone_sUnderfoot3)))

nrow(Backbone_sUnderfoot3) # 19,098
max(Backbone_sUnderfoot3$seqnum) #500217
# write.csv(Backbone_sUnderfoot3, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", row.names = F)

RSIP2 <- RSIP2 %>% dplyr::select(-c("scientificName"))
write.csv(RSIP2, file="RSIP/nph18031-sup-0001-datasets1_harmonized6.csv",  
          row.names = F, fileEncoding = "UTF-8")

### 3. CLO-PLA ###
# from https://clopla.butbn.cas.cz/index.php?page=intro
# joined from several sub-databases by Tomas Herben Dec 2023
CLOPLA1 <- fread("CLOPLA/sumfile-merged-all.txt", encoding="Latin-1")
str(CLOPLA1) # 4818 obs. of  9 variables:
head(CLOPLA1)
#sort(unique(CLOPLA1$Species_name))[1100:1400]

length(unique(CLOPLA1$spname)) # 4818
CLOPLA1 <- as_tibble(CLOPLA1)

CLOPLA1$spname[grep("ë", CLOPLA1$spname, fixed=T)]
CLOPLA1$spname[grep(" x ", CLOPLA1$spname, fixed=T)]
Backbone_sUnderfoot3[grep("ë", Backbone_sUnderfoot3$Resolved_name, fixed=T)]
# empty
CLOPLA1 <- CLOPLA1 %>%
  # remove quotes in Species_name
  #mutate(Species_name=gsub("\"", "", Species_name)) %>% 
  
  # replace Hierochloë with e
  mutate(Edited_name=gsub("ë", "e", spname, fixed=T)) %>% 
  # remove hybrid "x"
  mutate(Edited_name=gsub(" x ", " ", Edited_name, fixed=T)) %>% 
  # replace strange blank
  mutate(Edited_name=gsub(" ", " ", Edited_name, fixed=T)) %>% 
  # remove cf., gr., sp., NA
  mutate(Edited_name=gsub(" cf.", "", Edited_name, fixed=T)) %>% 
  mutate(Edited_name=gsub(" gr.", "", Edited_name, fixed=T)) %>% 
  mutate(Edited_name=gsub(" sp.", "", Edited_name, fixed=T)) %>% 
  mutate(Edited_name=gsub(" NA", "", Edited_name, fixed=T)) #%>%   
  #mutate(Species2=paste(tstrsplit(Species_name, " ", fixed=T)[[1]],
  #                      tstrsplit(Species_name, " ", fixed=T)[[2]],
  #                      sep=" ")) 
CLOPLA1 %>% dplyr::select(spname, Edited_name) %>% 
  print(n=100)
sort(unique(CLOPLA1$Edited_name))[1100:1400]

CLOPLA1$Parsed_name <- CLOPLA1$Edited_name
# some manual changes
CLOPLA1$Parsed_name[CLOPLA1$Parsed_name=="Micranthes hieraciifolia "] <- "Micranthes hieraciifolia"
CLOPLA1$Parsed_name[CLOPLA1$Parsed_name=="Schizarium scoparium"] <- "Schizachyrium scoparium"
# CLOPLA1$Parsed_name[CLOPLA1$Parsed_name=="Crocus albiflorus"]
# CLOPLA1[grepl("Crocus",CLOPLA1$Parsed_name),] %>% as.data.frame()
# backbone_sPlot %>% filter(Resolved_name=="Crocus albiflorus") %>% as.data.frame()
# backbone_sPlot %>% filter(Resolved_name=="Crocus vernus") %>% as.data.frame()
CLOPLA1$Parsed_name[CLOPLA1$Parsed_name=="Crocus albiflorus"] <- "Crocus vernus"
CLOPLA1$Parsed_name[CLOPLA1$Parsed_name=="Hieracium gracile"]
CLOPLA1[CLOPLA1$Parsed_name=="Senecio incanus",] %>% as.data.frame()
# backbone_sPlot %>% filter(Resolved_name=="Senecio incanus") %>% as.data.frame()
# backbone_sPlot %>% filter(Resolved_name=="Jacobaea incana") %>% as.data.frame()
CLOPLA1$Parsed_name[CLOPLA1$Parsed_name=="Senecio incanus"] <- "Jacobaea incana"
backbone_sPlot %>% filter(Resolved_name=="Juncus albescens") %>% as.data.frame()
CLOPLA1$Parsed_name[grepl("comata",CLOPLA1$Parsed_name)]
CLOPLA1$Parsed_name[CLOPLA1$Parsed_name=="Hesterostipa comata"] <- "Hesperostipa comata"

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(CLOPLA1$Parsed_name, Backbone_sUnderfoot3$Resolved_name)
length(index1) # 4818
length(index1[!is.na(index1)]) #   2376

# Take over name from the sUnderfoot Backbone
CLOPLA1$Resolved_name <- Backbone_sUnderfoot3$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(CLOPLA1$Parsed_name[is.na(CLOPLA1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 2442
length(index2[!is.na(index2)]) #  1864
CLOPLA1$Parsed_name[is.na(CLOPLA1$Resolved_name)][is.na(index2)] #578
CLOPLA1$Resolved_name[is.na(CLOPLA1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot4 <- rbind(Backbone_sUnderfoot3,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot4) #20,960 × 25                     
max(Backbone_sUnderfoot4$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(CLOPLA1$Parsed_name[is.na(CLOPLA1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 578
length(index3[!is.na(index3)]) #  459
CLOPLA1$Parsed_name[is.na(CLOPLA1$Resolved_name)][is.na(index3)] # 119
CLOPLA1$Resolved_name[is.na(CLOPLA1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot4 <- rbind(Backbone_sUnderfoot4,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot4) # 21,416 × 25                   
max(Backbone_sUnderfoot4$seqnum)

# Match the sPlot4 Backbone to the binary species name (Original_name)
index4 <- match(CLOPLA1$Parsed_name[is.na(CLOPLA1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 119
length(index4[!is.na(index4)]) #  2

Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot4 <- rbind(Backbone_sUnderfoot4,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot4) # 21,418 × 25                   
max(Backbone_sUnderfoot4$seqnum)

CLOPLA_species_list <- CLOPLA1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
CLOPLA_species_list %>% print(n=300)
# 119 species

backbone_sPlot %>% filter(Original_name=="Anemone nemorosa") %>% as.data.frame()
backbone_sPlot %>% filter(Original_name=="Inula salicina") %>% as.data.frame()

CLOPLA_harmonized_WFO <- WFO.one(WFO.match(spec.data=CLOPLA_species_list$Parsed_name,
                                         WFO.data=wfo.backbone))
str(CLOPLA_harmonized_WFO)
# 119 obs. of  47 
save(CLOPLA_harmonized_WFO, file="WFO_Harmonized/CLOPLA_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/CLOPLA_harmonized_WFO4.RData")

CLOPLA_harmonized_WFO$scientificName

CLOPLA2 <- CLOPLA1 %>% left_join(CLOPLA_harmonized_WFO %>% 
                               dplyr::select(spec.name , scientificName),
                             by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
CLOPLA2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Edited_name, Parsed_name) %>% 
  distinct() %>% 
  arrange()
# no NAs in Resolved_name

backbone_sPlot$Original_name[grep("Schizarium",backbone_sPlot$Original_name)]
CLOPLA1[grep("Cerastium arv",CLOPLA1$spname),]
CLOPLA1[grep("Cerastium arvense NA",CLOPLA1$spname),]
CLOPLA2[grep("Cerastium arv",CLOPLA2$spname),]
CLOPLA2[grep("Cerastium arv",CLOPLA2$Edited_name),]
CLOPLA_harmonized_WFO[grep("Cerastium arv",CLOPLA_harmonized_WFO$spec.name),]

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(CLOPLA_harmonized_WFO$spec.name, CLOPLA2$Parsed_name)
max(Backbone_sUnderfoot4$seqnum) #500217
CLOPLA_harmonized_WFO2 <- CLOPLA_harmonized_WFO %>% 
  mutate(seqnum=seq(500217+1,500217+nrow(CLOPLA_harmonized_WFO)),
       sPlot=F,
       TRY=F,
       Fantasy_name=NA,
       Original_name=CLOPLA2$Edited_name[index6],
       Edited_name=CLOPLA2$Edited_name[index6],
       Harmonized_name_wfo=scientificName,
       Resolved_name=scientificName,
       multiple_matches=NA,
       correct_wfo_output=NA,
       modified_info=NA,
       explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot4 <- rbind(Backbone_sUnderfoot4,
                              CLOPLA_harmonized_WFO2 %>% 
                                dplyr::select(names(Backbone_sUnderfoot4)))
nrow(Backbone_sUnderfoot4) # 21,537

# write.csv(Backbone_sUnderfoot4, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", row.names = F)

CLOPLA2 <- CLOPLA2 %>% dplyr::select(-c("scientificName"))
write.csv(CLOPLA2, file="CLOPLA/sumfile-merged-all_harmonized6.csv",  row.names = F, fileEncoding = "latin1")

### 4. BBB: A global Belowground Bud Bank database
# from: https://www.uv.es/jgpausas/bbb.htm
# Pausas, J.G., Lamont, B.B., Paula, S., Appezzato‐da‐Glória, B.,
# Fidelis, A., 2018. Unearthing belowground bud banks in 
# fire‐prone ecosystems. New Phytol 217, 1435–1448.
# https://doi.org/10.1111/nph.14982

#BBB1 <- read.csv("BBB/BBBdb_2017.11.csv", encoding="latin1")
BBB1 <- read_excel("BBB/BBBdb_2017.11.xlsx", sheet="Data")
str(BBB1) # 2114 obs. of  9 variables:
head(BBB1)
length(unique(BBB1$Taxa)) # 2114
sort(unique(BBB1$BBB))
BBB1$Edited_name <- BBB1$Taxa
BBB1$Parsed_name <- BBB1$Taxa

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(BBB1$Parsed_name, Backbone_sUnderfoot4$Resolved_name)
length(index1) # 2114
length(index1[!is.na(index1)]) #   414

# Take over name from the sUnderfoot Backbone
BBB1$Resolved_name <- Backbone_sUnderfoot4$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(BBB1$Parsed_name[is.na(BBB1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 1700
length(index2[!is.na(index2)]) #  1527
BBB1$Parsed_name[is.na(BBB1$Resolved_name)][is.na(index2)] # 173
BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot5 <- rbind(Backbone_sUnderfoot4,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot5) #23,064 × 25                     
max(Backbone_sUnderfoot5$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(BBB1$Parsed_name[is.na(BBB1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 173
length(index3[!is.na(index3)]) #  161
BBB1$Parsed_name[is.na(BBB1$Resolved_name)][is.na(index3)] # 12
BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot5 <- rbind(Backbone_sUnderfoot5,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot5) # 23,225 × 26                           

# match against sPlot backbone Original_names
index4 <- match(BBB1$Parsed_name[is.na(BBB1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 12
length(index4[!is.na(index4)]) # 1
BBB1$Parsed_name[is.na(BBB1$Resolved_name)][is.na(index4)] # 11
BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot5 <- rbind(Backbone_sUnderfoot5,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot5) #23,226 × 26                         

#backbone_sPlot$Original_name[grep("Populus alba",backbone_sPlot$Resolved_name)]

BBB_species_list <- BBB1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
BBB_species_list %>% print(n=200)
# 11 species

BBB_harmonized_WFO <- WFO.one(WFO.match(spec.data=BBB_species_list$Parsed_name,
                                           WFO.data=wfo.backbone))
str(BBB_harmonized_WFO)
# 11 obs. of  47 
save(BBB_harmonized_WFO, file="WFO_Harmonized/BBB_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/BBB_harmonized_WFO4.RData")
BBB_harmonized_WFO$scientificName

BBB2 <- BBB1 %>% left_join(BBB_harmonized_WFO %>% 
                  dplyr::select(spec.name , scientificName),
                  by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
BBB2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Taxa, Parsed_name) %>% 
  distinct() %>% 
  arrange()
# no NAs in Resolved_name

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(BBB_harmonized_WFO$spec.name, BBB2$Parsed_name)
max(Backbone_sUnderfoot5$seqnum) #500336
BBB_harmonized_WFO2 <- BBB_harmonized_WFO %>% 
  mutate(seqnum=seq(500336+1,500336+nrow(BBB_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=BBB2$Edited_name[index6],
         Edited_name=BBB2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot5 <- rbind(Backbone_sUnderfoot5,
                              BBB_harmonized_WFO2 %>% 
                                dplyr::select(names(Backbone_sUnderfoot5)))
nrow(Backbone_sUnderfoot5) # 23237

# write.csv(Backbone_sUnderfoot5, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", row.names = F)

BBB2 <- BBB2 %>% dplyr::select(-c("scientificName"))
write.csv(BBB2, file="BBB/BBBdb_2017.11_harmonized6.csv",  row.names = F, fileEncoding = "latin1")


### 5. BBBnew: A global Belowground Bud Bank database
# This is BBB from 4 expanded by Alessandra Fidelis
# version from sUnderfoot workshop 13.11.2023

BBBnew1 <- read_excel("BBBnew/iDivTable_BBB.xlsx", sheet="Sheet1")
# CGO column was modified by Alessandra Fidelis in the raw table in google drive
# Attention: there is also a Sheet2 in the data, which seems to contain
# exactly the same data
# tibble [377 × 16]

str(BBBnew1) # 350 × 16
head(BBBnew1)
length(unique(BBBnew1$Species)) # 59
sort(unique(BBBnew1$'type of BBB'))
BBBnew1$Edited_name <- BBBnew1$Species
BBBnew1$Parsed_name <- BBBnew1$Species

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(BBBnew1$Parsed_name, Backbone_sUnderfoot5$Resolved_name)
length(index1) # 350
length(index1[!is.na(index1)]) #   110

# Take over name from the sUnderfoot Backbone
BBBnew1$Resolved_name <- Backbone_sUnderfoot5$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(BBBnew1$Parsed_name[is.na(BBBnew1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 240
length(index2[!is.na(index2)]) #  151
BBBnew1$Parsed_name[is.na(BBBnew1$Resolved_name)][is.na(index2)] # 89
BBBnew1$Resolved_name[is.na(BBBnew1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot6 <- rbind(Backbone_sUnderfoot5,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot6) #23,263 × 25                     
max(Backbone_sUnderfoot6$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(BBBnew1$Parsed_name[is.na(BBBnew1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 89
length(index3[!is.na(index3)]) #  40
BBBnew1$Parsed_name[is.na(BBBnew1$Resolved_name)][is.na(index3)] # 49
BBBnew1$Resolved_name[is.na(BBBnew1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot6 <- rbind(Backbone_sUnderfoot6,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot6) # 23,272 × 26                           

# match against sPlot backbone Original_names
index4 <- match(BBBnew1$Parsed_name[is.na(BBBnew1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 49
length(index4[!is.na(index4)]) # 0
BBBnew1$Parsed_name[is.na(BBBnew1$Resolved_name)][is.na(index4)] # 49
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot6 <- rbind(Backbone_sUnderfoot6,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot6) #28,895 × 26                         

BBBnew_species_list <- BBBnew1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
BBBnew_species_list %>% print(n=200)
# 3 species

BBBnew_harmonized_WFO <- WFO.one(WFO.match(spec.data=BBBnew_species_list$Parsed_name,
                                        WFO.data=wfo.backbone))
str(BBBnew_harmonized_WFO)
# 3 obs. of  47 
# save(BBBnew_harmonized_WFO, file="WFO_Harmonized/BBBnew_harmonized_WFO4.RData")
load(file="WFO_Harmonized/BBBnew_harmonized_WFO4.RData")
BBBnew_harmonized_WFO$scientificName

BBBnew2 <- BBBnew1 %>% left_join(BBBnew_harmonized_WFO %>% 
                             dplyr::select(spec.name , scientificName),
                           by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
BBBnew2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Species, Parsed_name) %>% 
  distinct() %>% 
  arrange()
# no NAs in Resolved_name

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(BBBnew_harmonized_WFO$spec.name, BBBnew2$Parsed_name)
max(Backbone_sUnderfoot6$seqnum) #500347
BBBnew_harmonized_WFO2 <- BBBnew_harmonized_WFO %>% 
  mutate(seqnum=seq(500347+1,500347+nrow(BBBnew_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=BBBnew2$Edited_name[index6],
         Edited_name=BBBnew2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot6 <- rbind(Backbone_sUnderfoot6,
                              BBBnew_harmonized_WFO2 %>% 
                                dplyr::select(names(Backbone_sUnderfoot6)))
nrow(Backbone_sUnderfoot6) # 23,275

# write.csv(Backbone_sUnderfoot6, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", row.names = F)

BBBnew2 <- BBBnew2 %>% dplyr::select(-c("scientificName"))
write.csv(BBBnew2, file="BBBnew/iDivTable_BBB_harmonized6.csv",  row.names = F, fileEncoding = "UTF-8")

### 6. BBBSouthAfrica
# This has been harmonized by Frances 
# CGO column was modified by Frances Siebert in the raw table in google drive
# version from sUnderfoot workshop 15.11.2023

BBBSouthAfrica1 <- read_excel("BBBSouthAfrica/iDivTable_BBB_South Africa data.xlsx", 
                              sheet="Sheet1", skip=1)
# Attention: there is also a Sheet2 in the data, which seems to contain
# other data

str(BBBSouthAfrica1) # 410 × 25
head(BBBSouthAfrica1)
length(unique(BBBSouthAfrica1$Species)) # 36
sort(unique(BBBSouthAfrica1$Species))
sort(unique(BBBSouthAfrica1$`type of BBB`))
'[1] "Non-woody rhizome"               "Perennial herbaceous non-clonal"
[3] "root crown"                      "Root crown"                     
[5] "woody rhizome"  '
BBBSouthAfrica1$Edited_name <- BBBSouthAfrica1$Species
BBBSouthAfrica1$Parsed_name <- BBBSouthAfrica1$Species

#BBBSouthAfrica1$Parsed_name[BBBSouthAfrica1$Species=="Axonopus sp / Elionurus multicus"] <- "Axonopus"
#BBBSouthAfrica1$Parsed_name[BBBSouthAfrica1$Species=="Molinha não identificada"] <- "Molinia"

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(BBBSouthAfrica1$Parsed_name, Backbone_sUnderfoot6$Resolved_name)
length(index1) # 410
length(index1[!is.na(index1)]) #   80

# Take over name from the sUnderfoot Backbone
BBBSouthAfrica1$Resolved_name <- Backbone_sUnderfoot6$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(BBBSouthAfrica1$Parsed_name[is.na(BBBSouthAfrica1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 330
length(index2[!is.na(index2)]) #  265
BBBSouthAfrica1$Parsed_name[is.na(BBBSouthAfrica1$Resolved_name)][is.na(index2)] # 65
BBBSouthAfrica1$Resolved_name[is.na(BBBSouthAfrica1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot7 <- rbind(Backbone_sUnderfoot6,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot7) #23,191 × 25                     
max(Backbone_sUnderfoot7$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(BBBSouthAfrica1$Parsed_name[is.na(BBBSouthAfrica1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 65
length(index3[!is.na(index3)]) #  41
BBBSouthAfrica1$Parsed_name[is.na(BBBSouthAfrica1$Resolved_name)][is.na(index3)] # 24
BBBSouthAfrica1$Resolved_name[is.na(BBBSouthAfrica1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot7 <- rbind(Backbone_sUnderfoot7,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot7) # 23,295 × 26                           

# match against sPlot backbone Original_names
index4 <- match(BBBSouthAfrica1$Parsed_name[is.na(BBBSouthAfrica1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 24
length(index4[!is.na(index4)]) # 0
BBBSouthAfrica1$Parsed_name[is.na(BBBSouthAfrica1$Resolved_name)][is.na(index4)] # 24
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot7 <- rbind(Backbone_sUnderfoot7,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot7) #28,895 × 26                         

BBBSouthAfrica_species_list <- BBBSouthAfrica1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
BBBSouthAfrica_species_list %>% print(n=200)
# 7 species

BBBSouthAfrica_harmonized_WFO <- WFO.one(WFO.match(spec.data=BBBSouthAfrica_species_list$Parsed_name,
                                           WFO.data=wfo.backbone))
str(BBBSouthAfrica_harmonized_WFO)
# 7 obs. of  47 
save(BBBSouthAfrica_harmonized_WFO, file="WFO_Harmonized/BBBSouthAfrica_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/BBBSouthAfrica_harmonized_WFO4.RData")
BBBSouthAfrica_harmonized_WFO$scientificName

BBBSouthAfrica2 <- BBBSouthAfrica1 %>% left_join(BBBSouthAfrica_harmonized_WFO %>% 
                                   dplyr::select(spec.name , scientificName),
                                 by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
BBBSouthAfrica2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Species, Parsed_name) %>% 
  distinct() %>% 
  arrange()
# no NAs in Resolved_name

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(BBBSouthAfrica_harmonized_WFO$spec.name, BBBSouthAfrica2$Parsed_name)
max(Backbone_sUnderfoot7$seqnum) #500350
BBBSouthAfrica_harmonized_WFO2 <- BBBSouthAfrica_harmonized_WFO %>% 
  mutate(seqnum=seq(500350+1,500350+nrow(BBBSouthAfrica_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=BBBSouthAfrica2$Edited_name[index6],
         Edited_name=BBBSouthAfrica2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)



Backbone_sUnderfoot7 <- rbind(Backbone_sUnderfoot7,
                              BBBSouthAfrica_harmonized_WFO2 %>% 
                                dplyr::select(names(Backbone_sUnderfoot7)))
nrow(Backbone_sUnderfoot7) # 23302

# write.csv(Backbone_sUnderfoot7, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", row.names = F)

BBBSouthAfrica2 <- BBBSouthAfrica2 %>% dplyr::select(-c("scientificName"))
write.csv(BBBSouthAfrica2, file="BBBSouthAfrica/iDivTable_BBB_South_Africa_data_harmonized6.csv",  row.names = F, fileEncoding = "UTF-8")


### 7. Bud bank Afrotropical grasslands
# from: https://zenodo.org/record/5521402
# from the publication:
# Meller, P., Stellmes, M., Fidelis, A., Finckh, M., 2022. 
# Correlates of geoxyle diversity in Afrotropical grasslands. 
# Journal of Biogeography 49, 339–352. https://doi.org/10.1111/jbi.14305

#BBAFTG1 <- read.csv("Budbank_afrotropical_grasslands/20210830 Species list + BG-groups update USE THIS FOR ANALYSES.csv", sep=",",encoding="latin1")
BBAFTG1 <- read_excel("Budbank_afrotropical_grasslands/20210830 Species list + BG-groups update USE THIS FOR ANALYSES NEW EDITED 11.14.23.xlsx", 
                                  sheet="20210830 Species list + BG-grou")
str(BBAFTG1) # [152 × 15]
head(BBAFTG1)
length(unique(BBAFTG1$species)) # 118
sort(unique(BBAFTG1$BBB))
#  "Li" "RC" "Ro" "WR" "Xy"
sort(unique(BBAFTG1$BBB.2))
'[1] ""            "Li"          "Li/Tuber"    "Pfahlwurzel" "Ro/RC"       "ST"         
[7] "WR"          "Xy"   '

BBAFTG1 <- BBAFTG1 %>% mutate(Edited_name=tstrsplit(species, " ssp ", fixed=T)[[1]]) %>% 
  # remove variants and forms
  mutate(Edited_name=tstrsplit(Edited_name, " var ", fixed=T)[[1]]) %>% 
  # remove sp. and spp. from genus names
  mutate(Edited_name=gsub(" sp1","",Edited_name, fixed=T)) %>% 
  mutate(Edited_name=gsub(" sp2","",Edited_name, fixed=T)) %>%
  mutate(Edited_name=gsub(" sp","",Edited_name, fixed=T)) %>% 
  mutate(Edited_name=gsub("aff ","",Edited_name, fixed=T)) 

BBAFTG1$Parsed_name <- BBAFTG1$Edited_name

#BBBnew1$Parsed_name[BBBnew1$Species=="Axonopus sp / Elionurus multicus"] <- "Axonopus"
#BBBnew1$Parsed_name[BBBnew1$Species=="Molinha não identificada"] <- "Molinia"

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(BBAFTG1$Parsed_name, Backbone_sUnderfoot7$Resolved_name)
length(index1) # 152
length(index1[!is.na(index1)]) #   12

# Take over name from the sUnderfoot Backbone
BBAFTG1$Resolved_name <- Backbone_sUnderfoot7$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(BBAFTG1$Parsed_name[is.na(BBAFTG1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 140
length(index2[!is.na(index2)]) #  114
BBAFTG1$Parsed_name[is.na(BBAFTG1$Resolved_name)][is.na(index2)] # 26
BBAFTG1$Resolved_name[is.na(BBAFTG1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot8 <- rbind(Backbone_sUnderfoot7,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot8) #23,390 × 25                     
max(Backbone_sUnderfoot8$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(BBAFTG1$Parsed_name[is.na(BBAFTG1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 26
length(index3[!is.na(index3)]) #  10
BBAFTG1$Parsed_name[is.na(BBAFTG1$Resolved_name)][is.na(index3)] # 16
BBAFTG1$Resolved_name[is.na(BBAFTG1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot8 <- rbind(Backbone_sUnderfoot8,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot8) # 23,397 × 26                           

# match against sPlot backbone Original_names
index4 <- match(BBAFTG1$Parsed_name[is.na(BBAFTG1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 16
length(index4[!is.na(index4)]) # 0
BBAFTG1$Parsed_name[is.na(BBAFTG1$Resolved_name)][is.na(index4)] # 16
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot8 <- rbind(Backbone_sUnderfoot8,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot8) #28,895 × 26                         

BBAFTG_species_list <- BBAFTG1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
BBAFTG_species_list %>% print(n=200)
# 13 species

BBAFTG_harmonized_WFO <- WFO.one(WFO.match(spec.data=BBAFTG_species_list$Parsed_name,
                                           WFO.data=wfo.backbone))
str(BBAFTG_harmonized_WFO)
# 13 obs. of  47 
save(BBAFTG_harmonized_WFO, file="WFO_Harmonized/BBAFTG_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/BBAFTG_harmonized_WFO4.RData")
BBAFTG_harmonized_WFO$scientificName

BBAFTG2 <- BBAFTG1 %>% left_join(BBAFTG_harmonized_WFO %>% 
                                   dplyr::select(spec.name , scientificName),
                                 by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
BBAFTG2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(species, Parsed_name) %>% 
  distinct() %>% 
  arrange()
# no NAs in Resolved_name

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(BBAFTG_harmonized_WFO$spec.name, BBAFTG2$Parsed_name)
max(Backbone_sUnderfoot8$seqnum) #500357
BBAFTG_harmonized_WFO2 <- BBAFTG_harmonized_WFO %>% 
  mutate(seqnum=seq(500357+1,500357+nrow(BBAFTG_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=BBAFTG2$Edited_name[index6],
         Edited_name=BBAFTG2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot8 <- rbind(Backbone_sUnderfoot8,
                              BBAFTG_harmonized_WFO2 %>% 
                                dplyr::select(names(Backbone_sUnderfoot8)))
nrow(Backbone_sUnderfoot8) # 23,410
# write.csv(Backbone_sUnderfoot8, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", row.names = F)

BBAFTG2 <- BBAFTG2 %>% dplyr::select(-c("scientificName"))
write.csv(BBAFTG2, file="Budbank_afrotropical_grasslands/20210830_Species_list_BG-groups_harmonized6.csv",  row.names = F, fileEncoding = "latin1")

### 8. Bud bank Brazilian Asteraceae
# from: https://link.springer.com/article/10.1007/s12224-016-9266-8/tables/1
# from the publication:
# Filartiga, A.L., Klimešová, J., Appezzato-da-Glória, B., 2017. 
# Underground organs of Brazilian Asteraceae: testing the CLO-PLA 
# database traits. Folia Geobot 52, 367–385. 
# https://doi.org/10.1007/s12224-016-9266-8

#BBBA1 <- read.csv("Budbank_Brazilian_Asteraceae/Budbank_Brazilian_Asteraceae_Table1.csv", sep=",",encoding="latin1")
BBBA1 <- read_excel("Budbank_Brazilian_Asteraceae/Budbank_Brazilian_Asteraceae_Table1_harmonized.xlsx", 
                    sheet="Budbank_Brazilian_Asteraceae_Ta")
str(BBBA1) # 165 obs. of  5 variables:
BBBA1 <- BBBA1 %>% dplyr::select(-Species_name_harmonized)
# Species_name_harmonized was th old outdated harmonization
head(BBBA1)
length(unique(BBBA1$Species)) # 165
sort(unique(BBBA1$Original.classification))
'[1] "clonal herb"               "perennial herb non clonal" "tree"                     
[4] "xylopodium"  '

BBBA1 <- BBBA1 %>% mutate(Edited_name=tstrsplit(Species, " subs ", fixed=T)[[1]]) %>% 
  # remove subspecies
  # remove accents
  mutate(Edited_name=gsub("á","a",Edited_name, fixed=T)) 

BBBA1$Parsed_name <- BBBA1$Edited_name

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(BBBA1$Parsed_name, Backbone_sUnderfoot8$Resolved_name)
length(index1) # 165
length(index1[!is.na(index1)]) #   34

# Take over name from the sUnderfoot Backbone
BBBA1$Resolved_name <- Backbone_sUnderfoot8$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(BBBA1$Parsed_name[is.na(BBBA1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 131
length(index2[!is.na(index2)]) #  83
BBBA1$Parsed_name[is.na(BBBA1$Resolved_name)][is.na(index2)] # 48
BBBA1$Resolved_name[is.na(BBBA1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot9 <- rbind(Backbone_sUnderfoot8,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot9) #23,492 × 25                     
max(Backbone_sUnderfoot9$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(BBBA1$Parsed_name[is.na(BBBA1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 48
length(index3[!is.na(index3)]) #  15
BBBA1$Parsed_name[is.na(BBBA1$Resolved_name)][is.na(index3)] # 33
BBBA1$Resolved_name[is.na(BBBA1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot9 <- rbind(Backbone_sUnderfoot9,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot9) # 23,507 × 26                           

# match against sPlot backbone Original_names
index4 <- match(BBBA1$Parsed_name[is.na(BBBA1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 33
length(index4[!is.na(index4)]) # 0
BBBA1$Parsed_name[is.na(BBBA1$Resolved_name)][is.na(index4)] # 33
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot9 <- rbind(Backbone_sUnderfoot9,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot9) #28,895 × 26                         

BBBA_species_list <- BBBA1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
BBBA_species_list %>% print()
# 33 species

BBBA_harmonized_WFO <- WFO.one(WFO.match(spec.data=BBBA_species_list$Parsed_name,
                                           WFO.data=wfo.backbone))
str(BBBA_harmonized_WFO)
# 33 obs. of  47 
save(BBBA_harmonized_WFO, file="WFO_Harmonized/BBBA_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/BBBA_harmonized_WFO4.RData")
BBBA_harmonized_WFO$scientificName

BBBA2 <- BBBA1 %>% left_join(BBBA_harmonized_WFO %>% 
                                   dplyr::select(spec.name , scientificName),
                                 by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
BBBA2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Species, Parsed_name) %>% 
  distinct() %>% 
  arrange()
# no NAs in Resolved_name

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(BBBA_harmonized_WFO$spec.name, BBBA2$Parsed_name)
max(Backbone_sUnderfoot9$seqnum) #500370
BBBA_harmonized_WFO2 <- BBBA_harmonized_WFO %>% 
  mutate(seqnum=seq(500370+1,500370+nrow(BBBA_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=BBBA2$Edited_name[index6],
         Edited_name=BBBA2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot9 <- rbind(Backbone_sUnderfoot9,
                              BBBA_harmonized_WFO2 %>% 
                                dplyr::select(names(Backbone_sUnderfoot9)))
nrow(Backbone_sUnderfoot9) # 23540
# write.csv(Backbone_sUnderfoot9, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", row.names = F)
# Backbone_sUnderfoot9 <- read.csv("Backbone_sUnderfoot.csv")

BBBA2 <- BBBA2 %>% dplyr::select(-c("scientificName"))
write.csv(BBBA2, file="Budbank_Brazilian_Asteraceae/Budbank_Brazilian_Asteraceae_Table1_harmonized6.csv",  row.names = F, fileEncoding = "latin1")


### 9. Root traits tropical savanna
#from: Teixeira, J., Souza, L., Le Stradic, S., Fidelis, A., 2022. 
# Fire promotes functional plant diversity and modifies soil carbon 
# dynamics in tropical savanna. Science of The Total Environment 812, 152317. 
# https://doi.org/10.1016/j.scitotenv.2021.152317
# files are version 2 from:
# https://zenodo.org/record/6525627
# Please note that "BBTS" is a misspelling but kept for historic reasons
#BBTS1 <- fread("Budbank_trop_savanna/belowground_traits.csv", quote = "", encoding="Latin-1")
BBTS1 <- fread("Budbank_trop_savanna/root_traits_revised.csv", quote = "\"", encoding="Latin-1")
str(BBTS1) # 372 obs. of  20 variables:
head(BBTS1)
sort(unique(BBTS1$specie))

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x) 

names(BBTS1)
names(BBTS1) <- trim.leading(gsub("\"", "", names(BBTS1)))
names(BBTS1)

BBTS1 <- BBTS1 %>%
  mutate(root_type=gsub("\"", "", root_type)) %>% 
  mutate(root_type=trim.leading(root_type))
# remove quotes in this field
length(unique(BBTS1$specie)) # 38
unique(BBTS1$root_type) #" fine"  " trans"
colnames(BBTS1)
' [1] "season"                   "cerrado_phytophysiognomy" "site"                    
 [4] "treatment"                "family"                   "specie"                  
 [7] "codesp"                   "individual"               "root_type"               
[10] "freshweight.g."           "dryweight.g."             "Length.cm."              
[13] "ProjArea.cm2."            "SurfArea.cm2."            "AvgDiam.mm."             
[16] "RootVolume.cm3."          "Length.m."                "SRL_m.g"                 
[19] "RTD_g.cm3"                "RDMC_mg.g"   '

unique(BBTS1$specie)
BBTS1 <- BBTS1 %>%
  mutate(specie=gsub("\"", "", specie)) %>% 
  mutate(specie=trim.leading(specie))
# remove quotes in this field
unique(BBTS1$specie)
BBTS1 <- BBTS1 %>% 
  # remove authorities
  mutate(Edited_name=paste(tstrsplit(specie, " ", fixed=T)[[1]],
                            tstrsplit(specie, " ", fixed=T)[[2]],
                            sep=" ")) %>% 
  # remove sp. and cf.
  mutate(Edited_name=gsub(" sp.", "", Edited_name, fixed=T)) %>% 
  mutate(Edited_name=gsub(" cf.", "", Edited_name, fixed=T)) %>% 
  mutate(Edited_name=if_else(Edited_name=="Serreada Discolor","Serreada discolor",Edited_name))

unique(BBTS1$Edited_name)
BBTS1$Parsed_name <- BBTS1$Edited_name

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(BBTS1$Parsed_name, Backbone_sUnderfoot9$Resolved_name)
length(index1) # 372
length(index1[!is.na(index1)]) #   168

# Take over name from the sUnderfoot Backbone
BBTS1$Resolved_name <- Backbone_sUnderfoot9$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(BBTS1$Parsed_name[is.na(BBTS1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 204
length(index2[!is.na(index2)]) #  150
BBTS1$Parsed_name[is.na(BBTS1$Resolved_name)][is.na(index2)] # 54
BBTS1$Resolved_name[is.na(BBTS1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot10 <- rbind(Backbone_sUnderfoot9,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot10) #23,558 × 25                     
max(Backbone_sUnderfoot10$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(BBTS1$Parsed_name[is.na(BBTS1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 54
length(index3[!is.na(index3)]) #  18
BBTS1$Parsed_name[is.na(BBTS1$Resolved_name)][is.na(index3)] # 36
BBTS1$Resolved_name[is.na(BBTS1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot10 <- rbind(Backbone_sUnderfoot10,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot10) # 23,560 × 26                           

# match against sPlot backbone Original_names
index4 <- match(BBTS1$Parsed_name[is.na(BBTS1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 36
length(index4[!is.na(index4)]) # 0
BBTS1$Parsed_name[is.na(BBTS1$Resolved_name)][is.na(index4)] # 36
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot10 <- rbind(Backbone_sUnderfoot10,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot9) #28,895 × 26                         

BBTS_species_list <- BBTS1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
BBTS_species_list %>% print()
# 3 species

BBTS_harmonized_WFO <- WFO.one(WFO.match(spec.data=BBTS_species_list$Parsed_name,
                                         WFO.data=wfo.backbone))
str(BBTS_harmonized_WFO)
# 3 obs. of  47 
save(BBTS_harmonized_WFO, file="WFO_Harmonized/BBTS_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/BBTS_harmonized_WFO4.RData")
BBTS_harmonized_WFO$scientificName

BBTS2 <- BBTS1 %>% left_join(BBTS_harmonized_WFO %>% 
                               dplyr::select(spec.name , scientificName),
                             by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
BBTS2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(specie, Parsed_name) %>% 
  distinct() %>% 
  arrange()
'0'

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(BBTS_harmonized_WFO$spec.name, BBTS2$Parsed_name)
max(Backbone_sUnderfoot10$seqnum) #500403
BBTS_harmonized_WFO2 <- BBTS_harmonized_WFO %>% 
  mutate(seqnum=seq(500403+1,500403+nrow(BBTS_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=BBTS2$Edited_name[index6],
         Edited_name=BBTS2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot10 <- rbind(Backbone_sUnderfoot10,
                              BBTS_harmonized_WFO2 %>% 
                                dplyr::select(names(Backbone_sUnderfoot10)))
nrow(Backbone_sUnderfoot10) # 23563
# write.csv(Backbone_sUnderfoot10, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", 
#           row.names = F, fileEncoding = "UTF-8")

BBTS2 <- BBTS2 %>% dplyr::select(-c("scientificName"))
write.csv(BBTS2, file="Budbank_trop_savanna/belowground_traits_harmonized6.csv",  row.names = F, fileEncoding = "latin1")

#10. AlineAlessandra = DataBaseNew_AlineAlessandra_Dec2023.xlsx
AlineAlessandra1 <- read_excel("AlineAlessandra/DataBaseNew_AlineAlessandra_Dec2023.xlsx", 
        sheet="AlessandraAlineNewData")
str(AlineAlessandra1) # 225 × 26
sort(unique(AlineAlessandra1$Species))
AlineAlessandra1$Edited_name <- AlineAlessandra1$Species
AlineAlessandra1$Parsed_name <- AlineAlessandra1$Species

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(AlineAlessandra1$Parsed_name, Backbone_sUnderfoot10$Resolved_name)
length(index1) # 225
length(index1[!is.na(index1)]) #   86

# Take over name from the sUnderfoot Backbone
AlineAlessandra1$Resolved_name <- Backbone_sUnderfoot10$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(AlineAlessandra1$Parsed_name[is.na(AlineAlessandra1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 139
length(index2[!is.na(index2)]) #  116
AlineAlessandra1$Parsed_name[is.na(AlineAlessandra1$Resolved_name)][is.na(index2)] # 23
AlineAlessandra1$Resolved_name[is.na(AlineAlessandra1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot11 <- rbind(Backbone_sUnderfoot10,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot11) #23,679 × 25                     
max(Backbone_sUnderfoot11$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(AlineAlessandra1$Parsed_name[is.na(AlineAlessandra1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 23
length(index3[!is.na(index3)]) #  10
AlineAlessandra1$Parsed_name[is.na(AlineAlessandra1$Resolved_name)][is.na(index3)] # 13
AlineAlessandra1$Resolved_name[is.na(AlineAlessandra1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot11 <- rbind(Backbone_sUnderfoot11,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot11) # 23,689 × 26                           

# match against sPlot backbone Original_names
index4 <- match(AlineAlessandra1$Parsed_name[is.na(AlineAlessandra1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 13
length(index4[!is.na(index4)]) # 0
AlineAlessandra1$Parsed_name[is.na(AlineAlessandra1$Resolved_name)][is.na(index4)] # 13
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot11 <- rbind(Backbone_sUnderfoot11,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot11) #28,895 × 26                         

AlineAlessandra_species_list <- AlineAlessandra1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
AlineAlessandra_species_list %>% print()
# 13 species

AlineAlessandra_harmonized_WFO <- WFO.one(WFO.match(spec.data=AlineAlessandra_species_list$Parsed_name,
                                         WFO.data=wfo.backbone))
str(AlineAlessandra_harmonized_WFO)
# 13 obs. of  47 
save(AlineAlessandra_harmonized_WFO, file="WFO_Harmonized/AlineAlessandra_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/AlineAlessandra_harmonized_WFO4.RData")
AlineAlessandra_harmonized_WFO$scientificName

AlineAlessandra2 <- AlineAlessandra1 %>% left_join(AlineAlessandra_harmonized_WFO %>% 
                               dplyr::select(spec.name , scientificName),
                             by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
AlineAlessandra2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Species, Parsed_name) %>% 
  distinct() %>% 
  arrange()

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(AlineAlessandra_harmonized_WFO$spec.name, AlineAlessandra2$Parsed_name)
max(Backbone_sUnderfoot11$seqnum) #500406
AlineAlessandra_harmonized_WFO2 <- AlineAlessandra_harmonized_WFO %>% 
  mutate(seqnum=seq(500406+1,500406+nrow(AlineAlessandra_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=AlineAlessandra2$Edited_name[index6],
         Edited_name=AlineAlessandra2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot11 <- rbind(Backbone_sUnderfoot11,
                               AlineAlessandra_harmonized_WFO2 %>% 
                                 dplyr::select(names(Backbone_sUnderfoot11)))
nrow(Backbone_sUnderfoot11) # 23702
# write.csv(Backbone_sUnderfoot11, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", 
#           row.names = F, fileEncoding = "UTF-8")

AlineAlessandra2 <- AlineAlessandra2 %>% dplyr::select(-c("scientificName"))
write.csv(AlineAlessandra2, file="AlineAlessandra/DataBaseNew_AlineAlessandra_Dec2023_harmonized6.csv",  row.names = F, fileEncoding = "latin1")

#11. Siebert = Siebert_Frances_More data from Africa_Nov2023.xlsx
Siebert1 <- read_excel("FrancesSiebert/Siebert_Frances_More data from Africa_Nov2023.xlsx", 
                               sheet="Data from Africa")
str(Siebert1) # 127 × 28]
sort(unique(Siebert1$Species)) #123
Siebert1$Parsed_name <- Siebert1$Species

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(Siebert1$Parsed_name, Backbone_sUnderfoot11$Resolved_name)
length(index1) # 127
length(index1[!is.na(index1)]) #   40

# Take over name from the sUnderfoot Backbone
Siebert1$Resolved_name <- Backbone_sUnderfoot11$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(Siebert1$Parsed_name[is.na(Siebert1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 87
length(index2[!is.na(index2)]) #  75
Siebert1$Parsed_name[is.na(Siebert1$Resolved_name)][is.na(index2)] # 12
Siebert1$Resolved_name[is.na(Siebert1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot12 <- rbind(Backbone_sUnderfoot11,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot12) # 23,776 × 25                     
max(Backbone_sUnderfoot12$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(Siebert1$Parsed_name[is.na(Siebert1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 12
length(index3[!is.na(index3)]) #  7
Siebert1$Parsed_name[is.na(Siebert1$Resolved_name)][is.na(index3)] # 5
Siebert1$Resolved_name[is.na(Siebert1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot12 <- rbind(Backbone_sUnderfoot12,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot12) # 23,783 × 26                           

# match against sPlot backbone Original_names
index4 <- match(Siebert1$Parsed_name[is.na(Siebert1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 5
length(index4[!is.na(index4)]) # 0
Siebert1$Parsed_name[is.na(Siebert1$Resolved_name)][is.na(index4)] # 5
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot12 <- rbind(Backbone_sUnderfoot12,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot11) #28,895 × 26                         

Siebert_species_list <- Siebert1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
Siebert_species_list %>% print()
# 4 species

Siebert_harmonized_WFO <- WFO.one(WFO.match(spec.data=Siebert_species_list$Parsed_name,
                                                    WFO.data=wfo.backbone))
str(Siebert_harmonized_WFO)
# 4 obs. of  47 
save(Siebert_harmonized_WFO, file="WFO_Harmonized/Siebert_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/Siebert_harmonized_WFO4.RData")
Siebert_harmonized_WFO$scientificName

Siebert2 <- Siebert1 %>% left_join(Siebert_harmonized_WFO %>% 
                      dplyr::select(spec.name , scientificName),
                      by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
Siebert2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Species, Parsed_name) %>% 
  distinct() %>% 
  arrange()

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(Siebert_harmonized_WFO$spec.name, Siebert2$Parsed_name)
max(Backbone_sUnderfoot12$seqnum) #500419
Siebert_harmonized_WFO2 <- Siebert_harmonized_WFO %>% 
  mutate(seqnum=seq(500419+1,500419+nrow(Siebert_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=Siebert2$Parsed_name[index6],
         Edited_name=Siebert2$Parsed_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot12 <- rbind(Backbone_sUnderfoot12,
                               Siebert_harmonized_WFO2 %>% 
                                 dplyr::select(names(Backbone_sUnderfoot12)))
nrow(Backbone_sUnderfoot12) # 23787
# write.csv(Backbone_sUnderfoot12, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", 
#           row.names = F, fileEncoding = "UTF-8")

Siebert2 <- Siebert2 %>% dplyr::select(-c("scientificName"))
write.csv(Siebert2, file="FrancesSiebert/Siebert_Frances_More data from Africa_Nov2023_harmonized6.csv",  row.names = F, fileEncoding = "latin1")

#12. Laughlin_additions = laughlin_sUnderfoot_2023_additions.xlsx
Laughlin_additions1 <- read_excel("Laughlin_additions/laughlin_sUnderfoot_2023_additions.xlsx", 
                       sheet="sUnderfoot_2023_additions")
str(Laughlin_additions1) # 259 × 13
sort(unique(Laughlin_additions1$species)) #194
Laughlin_additions1$Edited_name <- Laughlin_additions1$species
Laughlin_additions1$Edited_name[Laughlin_additions1$Edited_name=="Andropon Gerardii"] <- "Andropon gerardii"
Laughlin_additions1$Parsed_name <- Laughlin_additions1$Edited_name

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(Laughlin_additions1$Parsed_name, Backbone_sUnderfoot12$Resolved_name)
length(index1) # 259
length(index1[!is.na(index1)]) #   183

# Take over name from the sUnderfoot Backbone
Laughlin_additions1$Resolved_name <- Backbone_sUnderfoot12$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(Laughlin_additions1$Parsed_name[is.na(Laughlin_additions1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 81
length(index2[!is.na(index2)]) #  43
Laughlin_additions1$Parsed_name[is.na(Laughlin_additions1$Resolved_name)][is.na(index2)] # 33
Laughlin_additions1$Resolved_name[is.na(Laughlin_additions1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot13 <- rbind(Backbone_sUnderfoot12,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot13) # 23,829 × 25                     
max(Backbone_sUnderfoot13$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(Laughlin_additions1$Parsed_name[is.na(Laughlin_additions1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 38
length(index3[!is.na(index3)]) #  28
Laughlin_additions1$Parsed_name[is.na(Laughlin_additions1$Resolved_name)][is.na(index3)] # 10
Laughlin_additions1$Resolved_name[is.na(Laughlin_additions1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot13 <- rbind(Backbone_sUnderfoot13,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot13) # 23846 × 26                           

# match against sPlot backbone Original_names
index4 <- match(Laughlin_additions1$Parsed_name[is.na(Laughlin_additions1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 10
length(index4[!is.na(index4)]) # 0
Laughlin_additions1$Parsed_name[is.na(Laughlin_additions1$Resolved_name)][is.na(index4)] # 10
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot13 <- rbind(Backbone_sUnderfoot13,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot13) #28,895 × 26                         

Laughlin_additions_species_list <- Laughlin_additions1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
Laughlin_additions_species_list %>% print()
# 9 species

Laughlin_additions_harmonized_WFO <- WFO.one(WFO.match(spec.data=Laughlin_additions_species_list$Parsed_name,
                                            WFO.data=wfo.backbone))
str(Laughlin_additions_harmonized_WFO)
# 9 obs. of  47 
Laughlin_additions_harmonized_WFO$scientificName
Laughlin_additions_harmonized_WFO[Laughlin_additions_harmonized_WFO$spec.name=="Andropogon gerardii var. Paucipilus",]
Laughlin_additions_harmonized_WFO[4,]

save(Laughlin_additions_harmonized_WFO, file="WFO_Harmonized/Laughlin_additions_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/Laughlin_additions_harmonized_WFO4.RData")

Laughlin_additions2 <- Laughlin_additions1 %>% left_join(Laughlin_additions_harmonized_WFO %>% 
                                     dplyr::select(spec.name , scientificName),
                                   by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
Laughlin_additions2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(species, Parsed_name) %>% 
  distinct() %>% 
  arrange()

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(Laughlin_additions_harmonized_WFO$spec.name, Laughlin_additions2$Parsed_name)
max(Backbone_sUnderfoot13$seqnum) #500423
Laughlin_additions_harmonized_WFO2 <- Laughlin_additions_harmonized_WFO %>% 
  mutate(seqnum=seq(500423+1,500423+nrow(Laughlin_additions_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=Laughlin_additions2$Edited_name[index6],
         Edited_name=Laughlin_additions2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot13 <- rbind(Backbone_sUnderfoot13,
                               Laughlin_additions_harmonized_WFO2 %>% 
                                 dplyr::select(names(Backbone_sUnderfoot13)))
nrow(Backbone_sUnderfoot13) # 23855
# write.csv(Backbone_sUnderfoot13, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", 
#           row.names = F, fileEncoding = "UTF-8")

Laughlin_additions2 <- Laughlin_additions2 %>% dplyr::select(-c("scientificName"))
write.csv(Laughlin_additions2, file="Laughlin_additions/laughlin_sUnderfoot_2023_additions_harmonized6.csv",  row.names = F, fileEncoding = "latin1")


#13. Sandhills_roots = Sandhills_roottraits.xlsx
Sandhills_roots1 <- read_excel("Sandhills_roottraits/Sandhills_roottraits.xlsx", 
                                  sheet="Sheet1")
str(Sandhills_roots1) # 63 × 10
sort(unique(Sandhills_roots1$Species)) #63
Sandhills_roots1$Edited_name <- Sandhills_roots1$Species
Sandhills_roots1$Parsed_name <- Sandhills_roots1$Edited_name

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(Sandhills_roots1$Parsed_name, Backbone_sUnderfoot13$Resolved_name)
length(index1) # 63
length(index1[!is.na(index1)]) #   54

# Take over name from the sUnderfoot Backbone
Sandhills_roots1$Resolved_name <- Backbone_sUnderfoot13$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(Sandhills_roots1$Parsed_name[is.na(Sandhills_roots1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 9
length(index2[!is.na(index2)]) #  0
Sandhills_roots1$Parsed_name[is.na(Sandhills_roots1$Resolved_name)][is.na(index2)] # 8
# Sandhills_roots1$Resolved_name[is.na(Sandhills_roots1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>% 
#   filter(!is.na(Resolved_name))
# 
# Backbone_sUnderfoot14 <- rbind(Backbone_sUnderfoot13,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot14) #30,122 × 25                     
# max(Backbone_sUnderfoot14$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(Sandhills_roots1$Parsed_name[is.na(Sandhills_roots1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 9
length(index3[!is.na(index3)]) #  7
Sandhills_roots1$Parsed_name[is.na(Sandhills_roots1$Resolved_name)][is.na(index3)] # 2
Sandhills_roots1$Resolved_name[is.na(Sandhills_roots1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>% 
  filter(!is.na(Resolved_name)) %>% 
  distinct()
Backbone_sUnderfoot14 <- rbind(Backbone_sUnderfoot13,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot14) # 23.862 × 26                           

# match against sPlot backbone Original_names
index4 <- match(Sandhills_roots1$Parsed_name[is.na(Sandhills_roots1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 2
length(index4[!is.na(index4)]) # 0
Sandhills_roots1$Parsed_name[is.na(Sandhills_roots1$Resolved_name)][is.na(index4)] # 2
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot14 <- rbind(Backbone_sUnderfoot14,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot14) #28,895 × 26                         

Sandhills_roots_species_list <- Sandhills_roots1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
Sandhills_roots_species_list %>% print()
# 2 species

Sandhills_roots_harmonized_WFO <- WFO.one(WFO.match(spec.data=Sandhills_roots_species_list$Parsed_name,
                                                       WFO.data=wfo.backbone))
str(Sandhills_roots_harmonized_WFO)
# 2 obs. of  47 
Sandhills_roots_harmonized_WFO$scientificName

save(Sandhills_roots_harmonized_WFO, file="WFO_Harmonized/Sandhills_roots_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/Sandhills_roots_harmonized_WFO4.RData")

Sandhills_roots2 <- Sandhills_roots1 %>% left_join(Sandhills_roots_harmonized_WFO %>% 
                                                           dplyr::select(spec.name , scientificName),
                                                         by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
Sandhills_roots2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Species, Parsed_name) %>% 
  distinct() %>% 
  arrange()

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(Sandhills_roots_harmonized_WFO$spec.name, Sandhills_roots2$Parsed_name)
max(Backbone_sUnderfoot14$seqnum) #500432
Sandhills_roots_harmonized_WFO2 <- Sandhills_roots_harmonized_WFO %>% 
  mutate(seqnum=seq(500432+1,500432+nrow(Sandhills_roots_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=Sandhills_roots2$Edited_name[index6],
         Edited_name=Sandhills_roots2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot14 <- rbind(Backbone_sUnderfoot14,
                               Sandhills_roots_harmonized_WFO2 %>% 
                                 dplyr::select(names(Backbone_sUnderfoot14)))
nrow(Backbone_sUnderfoot14) # 23864
# write.csv(Backbone_sUnderfoot14, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv", 
#           row.names = F, fileEncoding = "UTF-8")

Sandhills_roots2 <- Sandhills_roots2 %>% dplyr::select(-c("scientificName"))
write.csv(Sandhills_roots2, file="Sandhills_roottraits/Sandhills_roottraits_harmonized6.csv",  row.names = F, fileEncoding = "latin1")

#14. ITEX = TVC_SPP_20120502.csv
# cleaned by Xin Jing 23.10.2024
# ITEX1 <- read.csv("c:/Daten/FundivEurope3/XinJing/DAAD/Data/outputs/itex_spp_clonal_traits.csv")
ITEX1 <- read.csv("Itex/itex_spp_clonal_traits.csv")
str(ITEX1) # 1052 obs. of  5 variables:
sort(unique(ITEX1$scientificName_wfo)) #994
ITEX1$Edited_name <- ITEX1$spp_name_itex
ITEX1$Parsed_name <- ITEX1$scientificName_wfo

ITEX1$Parsed_name <- gsub("  var."," var.",ITEX1$Parsed_name, fixed=T)
# there was a mistake in wfo, with two "  " before "subsp."
ITEX1$Parsed_name <- gsub("  subsp."," subsp.",ITEX1$Parsed_name, fixed=T)

# backbone_sPlot[grep("Crocus caeruleus", backbone_sPlot$Original_name),] %>% as.data.frame() 
# wfo.backbone[grep("Crocus caeruleus",wfo.backbone$scientificName, fixed=T),] %>% as.data.frame() 
wfo.backbone[grep("Crocus albiflorus",wfo.backbone$scientificName, fixed=T),] %>% as.data.frame()
wfo.backbone[grep("Crocus vernus",wfo.backbone$scientificName, fixed=T),] %>% as.data.frame()
wfo.backbone[grep("Juncus albescens",wfo.backbone$scientificName, fixed=T),] %>% as.data.frame()


sort(unique(ITEX1$Edited_name[is.na(ITEX1$Parsed_name)])) # 6
ITEX1 <- ITEX1 %>% 
  mutate(Parsed_name = if_else(Edited_name=="Anemone drummondii ",
                                   "Anemone drummondii", Parsed_name),
         Edited_name = if_else(Edited_name=="Anemone drummondii ",
                                          "Anemone drummondii", Edited_name),
         Parsed_name = if_else(Edited_name=="Athyrium  filix-femina",
                                   "Athyrium filix-femina", Parsed_name),      
         Edited_name = if_else(Edited_name=="Athyrium  filix-femina",
                                   "Athyrium filix-femina", Edited_name),    
         Parsed_name = if_else(Edited_name=="Hieracium spp.",
                                   "Hieracium", Parsed_name), 
         Edited_name = if_else(Edited_name=="Hieracium spp.",
                                   "Hieracium", Edited_name), 
         Parsed_name = if_else(Edited_name=="Ranunculus trichophyllus ",
                                   "Ranunculus trichophyllus", Parsed_name),
         Edited_name = if_else(Edited_name=="Ranunculus trichophyllus ",
                                   "Ranunculus trichophyllus", Edited_name),
         Parsed_name = if_else(Edited_name=="Saxifraga spp.",
                                   "Saxifraga", Parsed_name), 
         Edited_name = if_else(Edited_name=="Saxifraga spp.",
                                   "Saxifraga", Edited_name), 
         Parsed_name = if_else(Edited_name=="Taraxacum spp.",
                                   "Taraxacum", Parsed_name),
         Edited_name = if_else(Edited_name=="Taraxacum spp.",
                                   "Taraxacum", Edited_name),
         Parsed_name = if_else(Edited_name=="Crocus albiflorus",
                               "Crocus vernus", Parsed_name),
         Parsed_name = if_else(Edited_name=="Hieracium gracile",
                               "Hieracium gracile", Parsed_name),
         Parsed_name = if_else(Edited_name=="Juncus albescens",
                               "Juncus albescens", Parsed_name),
         Parsed_name = if_else(Edited_name=="Koeleria cristata",
                               "Koeleria cristata", Parsed_name),
         Parsed_name = if_else(Edited_name=="Polygala chamaebuxus",
                               "Polygala chamaebuxus", Parsed_name),
         Parsed_name = if_else(Edited_name=="Ranunculus nemorosus",
                               "Ranunculus nemorosus", Parsed_name),
         Parsed_name = if_else(Edited_name=="Ranunculus oreophilus",
                               "Ranunculus oreophilus", Parsed_name))
# Backbone_sUnderfoot14 <- read.csv(file="Backbone_sUnderfoot.csv")
# 12285 lines
# Match the binary species name (Parsed_name) on the names in the sPlot 4 Backbone

# Match the binary species name (Parsed_name) on the names in the sUnderfoot Backbone
index1 <- match(ITEX1$Parsed_name, Backbone_sUnderfoot14$Resolved_name)
length(index1) # 1052
length(index1[!is.na(index1)]) #   652

# Take over name from the sUnderfoot Backbone
ITEX1$Resolved_name <- Backbone_sUnderfoot14$Resolved_name[index1]

# Match the remaining binary species name (Parsed_name) on the names in the sPlot Backbone
index2 <- match(ITEX1$Parsed_name[is.na(ITEX1$Resolved_name)], backbone_sPlot$Resolved_name)
length(index2) # 400
length(index2[!is.na(index2)]) #  397
ITEX1$Parsed_name[is.na(ITEX1$Resolved_name)][is.na(index2)] # 3
ITEX1$Resolved_name[is.na(ITEX1$Resolved_name)] <- backbone_sPlot$Resolved_name[index2]

Backbone_sUnderfoot_inter <- backbone_sPlot[index2,] %>%
  filter(!is.na(Resolved_name)) %>% 
  distinct()

Backbone_sUnderfoot15 <- rbind(Backbone_sUnderfoot14,Backbone_sUnderfoot_inter)
str(Backbone_sUnderfoot15) # 24,250 × 25
max(Backbone_sUnderfoot15$seqnum)

# match against sPlot backbone Edited_names
index3 <- match(ITEX1$Parsed_name[is.na(ITEX1$Resolved_name)], backbone_sPlot$Edited_name)
length(index3) # 3
length(index3[!is.na(index3)]) #  0
ITEX1$Parsed_name[is.na(ITEX1$Resolved_name)][is.na(index3)] # 3
# ITEX1$Resolved_name[is.na(ITEX1$Resolved_name)] <- backbone_sPlot$Resolved_name[index3]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index3,] %>%
#   filter(!is.na(Resolved_name)) %>% 
#   distinct()
# Backbone_sUnderfoot15 <- rbind(Backbone_sUnderfoot14,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot15) # 30,171 × 26

# match against sPlot backbone Original_names
index4 <- match(ITEX1$Parsed_name[is.na(ITEX1$Resolved_name)], backbone_sPlot$Original_name)
length(index4) # 3
length(index4[!is.na(index4)]) # 0
ITEX1$Parsed_name[is.na(ITEX1$Resolved_name)][is.na(index4)] # 3
# BBB1$Resolved_name[is.na(BBB1$Resolved_name)] <- backbone_sPlot$Resolved_name[index4]

# Backbone_sUnderfoot_inter <- backbone_sPlot[index4,] %>% 
#   filter(!is.na(Edited_name))
# 
# Backbone_sUnderfoot15 <- rbind(Backbone_sUnderfoot15,Backbone_sUnderfoot_inter)
# str(Backbone_sUnderfoot15) #28,895 × 26                         

ITEX_species_list <- ITEX1 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(Parsed_name) %>% 
  distinct() %>% 
  arrange(Parsed_name)
ITEX_species_list %>% print()
# 3 species
# WFO.one(WFO.match(spec.data="Crocus albiflorus", WFO.data=wfo.backbone))

ITEX_harmonized_WFO <- WFO.one(WFO.match(spec.data=ITEX_species_list$Parsed_name,
                                                    WFO.data=wfo.backbone))
str(ITEX_harmonized_WFO)
# 3 obs. of  47 
ITEX_harmonized_WFO$scientificName

save(ITEX_harmonized_WFO, file="WFO_Harmonized/ITEX_harmonized_WFO4.RData")
# load(file="WFO_Harmonized/ITEX_harmonized_WFO4.RData")

ITEX2 <- ITEX1 %>% left_join(ITEX_harmonized_WFO %>% 
                                                     dplyr::select(spec.name , scientificName),
                                                   by=c("Parsed_name"="spec.name")) %>% 
  mutate(Resolved_name = if_else(is.na(Resolved_name), scientificName, Resolved_name)) 
ITEX2 %>% filter(is.na(Resolved_name)) %>% 
  dplyr::select(spp_name_itex, Parsed_name) %>% 
  distinct() %>% 
  arrange()

# Add species from WFO to the Backbone_sUnderfoot 
index6 <- match(ITEX_harmonized_WFO$spec.name, ITEX2$Parsed_name)
max(Backbone_sUnderfoot15$seqnum) #500434
ITEX_harmonized_WFO2 <- ITEX_harmonized_WFO %>% 
  mutate(seqnum=seq(500434+1,500434+nrow(ITEX_harmonized_WFO)),
         sPlot=F,
         TRY=F,
         Fantasy_name=NA,
         Original_name=ITEX2$Edited_name[index6],
         Edited_name=ITEX2$Edited_name[index6],
         Harmonized_name_wfo=scientificName,
         Resolved_name=scientificName,
         multiple_matches=NA,
         correct_wfo_output=NA,
         modified_info=NA,
         explanation=NA) %>% 
  rename(Parsed_name=spec.name)

Backbone_sUnderfoot15 <- rbind(Backbone_sUnderfoot15,
                               ITEX_harmonized_WFO2 %>% 
                                 dplyr::select(names(Backbone_sUnderfoot15)))
nrow(Backbone_sUnderfoot15) # 24253
write.csv(Backbone_sUnderfoot15, file="sUnderfoot_Backbone/Backbone_sUnderfoot.csv",
          row.names = F, fileEncoding = "UTF-8")

ITEX2 <- ITEX2 %>% dplyr::select(-c("scientificName"))
write.csv(ITEX2, "Itex/itex_spp_clonal_traits_harmonized6.csv")
# write.csv(ITEX2, file="1_Harmonized_Taxa/ITEX3.csv",  row.names = F, fileEncoding = "latin1")
