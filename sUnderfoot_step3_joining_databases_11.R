# Step 3: Join Databases
# Combine taxonomically harmonized data from step 1 
# and harmonized traits from step 2

library(tidyverse)
library(readxl)
library(data.table)

setwd("c:/Daten/iDiv4/sDiv/2023sUnderfoot/Databases/")
# All databases are all linked by Harmonized_name

# 1. Load databases
# 1.1 GRooT
GRooT2 <- read.csv("1_Harmonized_Taxa/GRooTFullVersion_harmonized6.csv",  
                   encoding = "UTF-8")
str(GRooT2) # 114222 obs. of  76 variables:

# 1.2 RSIP
RSIP2 <- read.csv("1_Harmonized_Taxa/nph18031-sup-0001-datasets1_harmonized6.csv", 
                  encoding="UTF-8")
str(RSIP2) # 5634 obs. of  73 variables:

# 1.3 CLOPLA
# from https://clopla.butbn.cas.cz/index.php?page=intro
CLOPLA2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/sumfile-merged-all_harmonized6_CGO.csv", 
                    encoding="latin1")
str(CLOPLA2) # 4818 obs. of  24 variables:

# 1.4. BBB: A global Belowground Bud Bank database
BBB2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/BBBdb_2017.11_harmonized6_CGO.csv", encoding="latin1")
str(BBB2) # 2114 obs. of  15 variables:

# 1.5. BBBnew: A global Belowground Bud Bank database
# This is BBB from 4 expanded by Alessandra Fidelis
# version from sUnderfoot workshop 13.11.2023
BBBnew2  <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/iDivTable_BBB_harmonized6_CGO.csv",  
                     encoding = "UTF-8")
str(BBBnew2)
# 350 obs. of  31

# 1.6. BBBSouthAfrica
# This has been harmonized by Frances 
# CGO column was modified by Frances Siebert in the raw table in google drive
# version from sUnderfoot workshop 15.11.2023
BBBSouthAfrica2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/iDivTable_BBB_South_Africa_data_harmonized6_CGO.csv", 
                            encoding = "UTF-8")
str(BBBSouthAfrica2) #410 obs. of  30

# 1.7. Bud bank Afrotropical grasslands
BBAFTG2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/20210830_Species_list_BG-groups_harmonized6_CGO.csv", sep=",",encoding="latin1")
str(BBAFTG2) # 152 obs. of  26 variables:
table(BBAFTG2$BBB)

# 1.8 Bud bank Brazilian Asteraceae
BBBA2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/Budbank_Brazilian_Asteraceae_Table1_harmonized6_CGO.csv", sep=",",encoding="latin1")
str(BBBA2) # 165 obs. of  9 variables:

# 1.9. Root traits tropical savanna
#      revised by Alessandra Fidelis 07.03.2024 = root_traits_revised.csv
BBTS2 <- read.csv("1_Harmonized_Taxa/belowground_traits_harmonized6.csv", sep=",",encoding="latin1")
str(BBTS2) # 372 obs. of  23

# 1.10. AlineAlessandra = DataBaseNew_AlineAlessandra_Dec2023.xlsx
AlineAlessandra2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/DataBaseNew_AlineAlessandra_Dec2023_harmonized6_CGO.csv",  
                             encoding = "latin1")
str(AlineAlessandra2) # 225 obs. of  30 variables:

# 1.11. Siebert = Siebert_Frances_More data from Africa_Nov2023.xlsx
Siebert2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/Siebert_Frances_More data from Africa_Nov2023_harmonized6_CGO.csv",  
                             fileEncoding = "latin1")
str(Siebert2) # 127 obs. of  31  variables:

# 1.12. Laughlin_additions = laughlin_sUnderfoot_2023_additions.xlsx
Laughlin_additions2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/laughlin_sUnderfoot_2023_additions_harmonized6_CGO.csv",  
                              fileEncoding = "latin1")
str(Laughlin_additions2) # 259 obs. of  21  variables:

# 1.13. Sandhills_roots = Sandhills_roottraits.xlsx
Sandhills_roots2 <- read.csv("1_Harmonized_Taxa/Sandhills_roottraits_harmonized6.csv",  
                             fileEncoding = "latin1")
str(Sandhills_roots2) # 63 obs. of  13 variables

# 1.14. ITEX, from Xin Jing
ITEX2 <- read.csv("2_Harmonized_Taxa_and_reclassified_CGO/itex_spp_clonal_traits_harmonized6_CGO.csv",  
                             fileEncoding = "latin1")
str(ITEX2) # 1052 obs. of  20  variables

# 2. Harmonize by taxon name
# 2.1 Database with root traits
# 2.1.1 GRooT
# traits needed from this database 
sort(unique(GRooT2$traitName))
trait.list.GRooT <- c("Root_N_concentration","Mean_Root_diameter",
                      "Root_tissue_density","Coarse_root_fine_root_mass_ratio",
                      "Specific_root_length")

GRooT2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 6227
any(is.na(GRooT2$Resolved_name))
# F
unique(GRooT2$Resolved_name[grep(" var.",GRooT2$Resolved_name,fixed=T)]) #23
unique(GRooT2$Resolved_name[grep("  var.",GRooT2$Resolved_name,fixed=T)]) #0
unique(GRooT2$Resolved_name[grep(" subsp.",GRooT2$Resolved_name,fixed=T)]) #78
unique(GRooT2$Resolved_name[grep("  subsp.",GRooT2$Resolved_name,fixed=T)]) #0

# check for duplicate and missing taxon names 
# remove subspecies and variants
GRooT2 <- GRooT2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, " var.", fixed=T)[[1]])
GRooT2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 6165

GRooT3 <- GRooT2 %>% filter(traitName %in% trait.list.GRooT) %>% 
  dplyr::select(Edited_name, Parsed_name, Resolved_name, Aggregated_name, traitName, traitValue) 
str(GRooT3) #45535 obs. of  6 variables


# 2.1.2 (# 9.) Root traits tropical savanna
str(BBTS2) # 372 obs. of  23
table(BBTS2$root_type, exclude = NULL)
BBTS3 <- BBTS2 %>% 
  filter(root_type=="fine") %>% 
  rename(Mean_Root_diameter=AvgDiam.mm.,
         Root_tissue_density=RTD_g.cm3,
         Specific_root_length = SRL_m.g) %>% 
  mutate(Root_N_concentration=NA,
         Coarse_root_fine_root_mass_ratio=NA) %>% 
  dplyr::select(all_of(c("Edited_name", "Parsed_name","Resolved_name", trait.list.GRooT)))
str(BBTS3) # 186 obs. of  8 variables:
BBTS3_species_list <- sort(unique(BBTS3$Resolved_name))
index1 <- match(BBTS3_species_list,GRooT3$Resolved_name)
index1
# only 2 matches
BBTS3_species_list[!is.na(index1)]
#"Aristida"          "Elionurus muticus"
as.data.frame(GRooT3[GRooT3$Resolved_name=="Aristida",])

hist(GRooT3 %>% filter(traitName=="Mean_Root_diameter") %>% dplyr::select(traitValue) %>% unlist(.))
hist(BBTS3$Mean_Root_diameter)
hist(GRooT3 %>% filter(traitName=="Root_tissue_density") %>% dplyr::select(traitValue) %>% unlist(.))
hist(BBTS3$Root_tissue_density)
hist(GRooT3 %>% filter(traitName=="Specific_root_length") %>% dplyr::select(traitValue) %>% unlist(.))
hist(BBTS3$Specific_root_length)

summary(BBTS3$Root_tissue_density)
# would have to be excluded here. However, RTD does not match
# has to be integrated later, for the time being, we delete the data
BBTS3$Root_tissue_density[BBTS3$Root_tissue_density>1]

BBTS3$Root_tissue_density <- NA

#BBTS3[BBTS3$Resolved_name=="Aristida",]
#as.data.frame(GRooT3[GRooT3$Resolved_name=="Elionurus muticus",])

unique(BBTS3$Resolved_name[grep(" var.",BBTS3$Resolved_name,fixed=T)]) #0
unique(BBTS3$Resolved_name[grep("  var.",BBTS3$Resolved_name,fixed=T)]) #0
unique(BBTS3$Resolved_name[grep(" subsp.",BBTS3$Resolved_name,fixed=T)]) #0
unique(BBTS3$Resolved_name[grep("  subsp.",BBTS3$Resolved_name,fixed=T)]) #0
# no subspecies or variants
BBTS3$Aggregated_name <- BBTS3$Resolved_name
# make long format, for rbind with GRooT3
BBTS4 <- BBTS3 %>%  
  pivot_longer(cols=all_of(trait.list.GRooT), names_to = "traitName", values_to = "traitValue")
str(BBTS4) # 930 × 6


# 2.1.3 (# 12.) Laughlin_additions = laughlin_sUnderfoot_2023_additions.xlsx
str(Laughlin_additions2) # 259 obs. of  21 variables:
table(Laughlin_additions2$BudBankDensity_numberPerTiller, exclude = NULL)
Laughlin_additions3 <- Laughlin_additions2 %>% 
  rename(Original_name=species,
         Mean_Root_diameter=RD_mm,
         Root_tissue_density=RTD_g_cm3,
         Specific_root_length = SRL_m_g,
         Root_N_concentration = RN_perc,
         BBsize = BudBankDensity_numberPerTiller) %>% 
  mutate(Coarse_root_fine_root_mass_ratio=NA,
         Root_N_concentration = Root_N_concentration * 10) %>% # in mg/g in GRoot
  dplyr::select(all_of(c("Original_name","Edited_name", "Parsed_name","Resolved_name", trait.list.GRooT,
                         "BBsize","BPI","BPT","CSI","is.woody","is.woody.below","is.woody.above","is.clonal","is.resprouting")))
str(Laughlin_additions3) # 259 obs. of 18 variables:
Laughlin_additions3_species_list <- sort(unique(Laughlin_additions3$Resolved_name))
index1 <- match(Laughlin_additions3_species_list,GRooT3$Resolved_name)
length(index1[!is.na(index1)])
# 81 matches
Laughlin_additions3_species_list[!is.na(index1)]
unique(Laughlin_additions3$Resolved_name[grep(" var.",Laughlin_additions3$Resolved_name,fixed=T)]) #0
unique(Laughlin_additions3$Resolved_name[grep("  var.",Laughlin_additions3$Resolved_name,fixed=T)]) #0
unique(Laughlin_additions3$Resolved_name[grep(" subsp.",Laughlin_additions3$Resolved_name,fixed=T)]) #2
unique(Laughlin_additions3$Resolved_name[grep("  subsp.",Laughlin_additions3$Resolved_name,fixed=T)]) #0
# 2 subspecies, no variants
Laughlin_additions3 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 190
Laughlin_additions3 <- Laughlin_additions3 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, " var.", fixed=T)[[1]])
Laughlin_additions3 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 190

#Backbone_sUnderfoot14[grep("subsp.",Backbone_sUnderfoot13$Resolved_name)]

hist(GRooT3 %>% filter(traitName=="Mean_Root_diameter") %>% dplyr::select(traitValue) %>% unlist(.))
hist(Laughlin_additions3$Mean_Root_diameter)
hist(GRooT3 %>% filter(traitName=="Root_tissue_density") %>% dplyr::select(traitValue) %>% unlist(.))
hist(Laughlin_additions3$Root_tissue_density)
hist(GRooT3 %>% filter(traitName=="Specific_root_length") %>% dplyr::select(traitValue) %>% unlist(.))
hist(Laughlin_additions3$Specific_root_length)
hist(GRooT3 %>% filter(traitName=="Root_N_concentration") %>% dplyr::select(traitValue) %>% unlist(.))
hist(Laughlin_additions3$Root_N_concentration)
# all in the same range

# make long format, for rbind with GRooT3
Laughlin_additions4 <- Laughlin_additions3 %>%  
  pivot_longer(cols=all_of(trait.list.GRooT), names_to = "traitName", values_to = "traitValue") %>% 
  dplyr::select(-c("Original_name","BBsize","BPI","BPT","CSI","is.woody","is.woody.below","is.woody.above","is.clonal","is.resprouting"))
str(Laughlin_additions4) # 1,295 × 6

# 2.1.4 (#13) Sandhills_roots = Sandhills_roottraits.xlsx
str(Sandhills_roots2) # 63 obs. of  13 variables:
Sandhills_roots3 <- Sandhills_roots2 %>% 
  rename(Mean_Root_diameter=RD_mm,
         Root_tissue_density=RTD_g_m3,
         Specific_root_length = SRL_m_g) %>% 
  mutate(Coarse_root_fine_root_mass_ratio=NA,
         Root_N_concentration = NA) %>% 
  dplyr::select(all_of(c("Edited_name", "Parsed_name","Resolved_name", trait.list.GRooT)))
str(Sandhills_roots3) # 63 obs. of  8 variables
Sandhills_roots3_species_list <- sort(unique(Sandhills_roots3$Resolved_name))
index1 <- match(Sandhills_roots3_species_list,GRooT3$Resolved_name)
length(index1[!is.na(index1)])
# 29 matches
Sandhills_roots3_species_list[!is.na(index1)]

unique(Sandhills_roots3$Resolved_name[grep(" var.",Sandhills_roots3$Resolved_name,fixed=T)])
unique(Sandhills_roots3$Resolved_name[grep("  var.",Sandhills_roots3$Resolved_name,fixed=T)])
unique(Sandhills_roots3$Resolved_name[grep(" subsp.",Sandhills_roots3$Resolved_name,fixed=T)])
unique(Sandhills_roots3$Resolved_name[grep("  subsp.",Sandhills_roots3$Resolved_name,fixed=T)])
# 1 subspecies, no variants
Sandhills_roots3 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 63
Sandhills_roots3 <- Sandhills_roots3 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, "  var.", fixed=T)[[1]])
Sandhills_roots3 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 63


hist(GRooT3 %>% filter(traitName=="Mean_Root_diameter") %>% dplyr::select(traitValue) %>% unlist(.))
hist(Sandhills_roots3$Mean_Root_diameter)
hist(GRooT3 %>% filter(traitName=="Root_tissue_density") %>% dplyr::select(traitValue) %>% unlist(.))
hist(Sandhills_roots3$Root_tissue_density)
hist(GRooT3 %>% filter(traitName=="Specific_root_length") %>% dplyr::select(traitValue) %>% unlist(.))
hist(Laughlin_additions3$Specific_root_length)
# all in the same range

# make long format, for rbind with GRooT3
Sandhills_roots4 <- Sandhills_roots3 %>%  
  pivot_longer(cols=all_of(trait.list.GRooT), names_to = "traitName", values_to = "traitValue")
str(Sandhills_roots4) # 315 × 6

# 2.1.5 join databases
GRooT4 <- GRooT3 %>% rbind(BBTS4,
                        Laughlin_additions4,
                        Sandhills_roots4)
str(GRooT4) #48075 obs. of  6 variables

# check for Root_tissue_density
GRooT4 %>% filter(traitName == "Root_tissue_density") %>% 
  dplyr::select(traitValue) %>% 
  unlist() %>% 
  hist()
# set all Root_tissue_density > 1 to NA
GRooT4$traitValue[GRooT4$traitName=="Root_tissue_density" & GRooT4$traitValue>1 & !is.na(GRooT4$traitValue)] <- NA

# 2.1.6 Calculate species mean values 
# calculate number of observations per trait
GRooT5a <- GRooT4 %>% filter(traitName %in% trait.list.GRooT) %>% 
  dplyr::select(Aggregated_name, traitName, traitValue) %>% 
  pivot_wider(names_from = traitName, values_from = traitValue, 
              values_fn = function(x) length(x[!is.na(x)])) %>% 
  rename_with(~ paste0("n_",.), .cols=where(is.numeric))
str(GRooT5a)
# same number of rows as number of species, i.e. 2801

# combine trait data with number of observations
GRooT5 <- GRooT4 %>% filter(traitName %in% trait.list.GRooT) %>% 
  dplyr::select(Aggregated_name, traitName, traitValue) %>% 
  pivot_wider(names_from = traitName, values_from = traitValue, 
              values_fn = function(x) mean(x, na.rm = TRUE)) %>% 
  # add n from GRooT5a
  left_join(GRooT5a, by="Aggregated_name") %>% 
  # combine with one name of the original and cleaned names
  # ATTENTION: it is arbitrary which of the original names was used
  left_join(GRooT4 %>% dplyr::select(Aggregated_name,
                Edited_name,Parsed_name,Resolved_name) %>% 
              group_by(Aggregated_name) %>% 
              slice(1),
            by="Aggregated_name") %>% 
  left_join(GRooT2 %>% dplyr::select(Aggregated_name,abilityToGrownClonallyCloPla) %>% 
              mutate(GRooT_clonal=ifelse(abilityToGrownClonallyCloPla=="",NA,
                        ifelse(abilityToGrownClonallyCloPla=="present",1,0))) %>% 
              dplyr::select(-abilityToGrownClonallyCloPla) %>% 
              group_by(Aggregated_name) %>% 
              slice(1),
            by="Aggregated_name") %>% 
  rename(GRooT_Edited_name = Edited_name,
         GRooT_Parsed_name=Parsed_name,
         GRooT_Resolved_name=Resolved_name)
str(GRooT5) # [2,801 × 15] 

table(GRooT5$GRooT_clonal, exclude=NULL)
'  0    1 <NA> 
 255  281 2265  '

length(unique(GRooT5$Aggregated_name)) #2802

# 2.2 Database with rooting depths
# 2.2.1 RSIP
# traits needed from this database 
str(RSIP2) # 5634 obs. of  72 variables:
trait.list.RSIP <- c("Dr","Lr","Growth_form","Life_span","Tissue")
# Lr = Maximum lateral root spread/one-sided (radius) linear distance from stem reached by roots [m] (see Table S2)
# Dr = Maximum rooting depth of plant [m]

# check for duplicate and missing taxon names
unique(RSIP2$Resolved_name[grep(" var.",RSIP2$Resolved_name,fixed=T)])
unique(RSIP2$Resolved_name[grep("  var.",RSIP2$Resolved_name,fixed=T)])
unique(RSIP2$Resolved_name[grep(" subsp.",RSIP2$Resolved_name,fixed=T)])
unique(RSIP2$Resolved_name[grep("  subsp.",RSIP2$Resolved_name,fixed=T)])
# 42 subspecies, 10 variants
RSIP2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 2749
RSIP2 <- RSIP2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, " var.", fixed=T)[[1]])
RSIP2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 2725

any(duplicated(RSIP2$Aggregated_name)) #T
length(RSIP2$Aggregated_name[duplicated(RSIP2$Aggregated_name)])
# 2909

RSIP2 %>% dplyr::select(all_of(c("Edited_name", "Parsed_name","Resolved_name","Aggregated_name", 
                                 trait.list.RSIP))) %>%
  distinct(Aggregated_name) %>% 
  nrow()
# 2725

length(unique(RSIP2$Aggregated_name)) # 2725
length(RSIP2$Species[RSIP2$Aggregated_name=="NA"]) #0
length(RSIP2$Species[is.na(RSIP2$Aggregated_name)]) #0

RSIP3 <- RSIP2 %>% dplyr::select(all_of(c("Species","Edited_name", 
                                          "Parsed_name","Resolved_name","Aggregated_name",
                                          trait.list.RSIP))) %>% 
  rename(Original_name=Species, 
         Lateral_root_spread=Lr,
         Rooting_depth=Dr,
         RSIP_Growthform=Growth_form,
         RSIP_Lifespan=Life_span,
         RSIP_is.woody=Tissue)
str(RSIP3) # 5634 obs. of  10 variables

# 2.2.2 (#13) Sandhills_roots = Sandhills_roottraits.xlsx
str(Sandhills_roots2) # 63 obs. of  13 variables:
unique(Sandhills_roots2$Resolved_name[grep(" var.",Sandhills_roots2$Resolved_name,fixed=T)])
unique(Sandhills_roots2$Resolved_name[grep("  var.",Sandhills_roots2$Resolved_name,fixed=T)])
unique(Sandhills_roots2$Resolved_name[grep(" subsp.",Sandhills_roots2$Resolved_name,fixed=T)])
unique(Sandhills_roots2$Resolved_name[grep("  subsp.",Sandhills_roots2$Resolved_name,fixed=T)])
# 1 subspecies, 0 variants
Sandhills_roots2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 63
Sandhills_roots2 <- Sandhills_roots2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, " var.", fixed=T)[[1]])
Sandhills_roots2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 63


Sandhills_roots3a <- Sandhills_roots2 %>% 
  rename(Rooting_depth=RootingDepth.cm.) %>% 
  mutate(Rooting_depth=Rooting_depth/100,  # in RSIP it is measured in m
         Lateral_root_spread = NA,
         Original_name=Species) %>% 
  dplyr::select(all_of(c("Original_name","Edited_name", "Parsed_name","Resolved_name", "Aggregated_name", 
                         "Rooting_depth","Lateral_root_spread"))) %>% 
  mutate(RSIP_Growthform=NA,
         RSIP_Lifespan=NA,
         RSIP_is.woody=NA)
str(Sandhills_roots3a) # 63 obs. of  10 variables

# 2.2.3 join databases
RSIP4 <- RSIP3 %>% rbind(Sandhills_roots3a)
str(RSIP4) # 5,697 × 10

# 2.1.6 Calculate species mean values 
RSIP5a <- RSIP4 %>% dplyr::select(all_of(c("Aggregated_name",
                                           "Lateral_root_spread","Rooting_depth"))) %>% 
  group_by(Aggregated_name) %>% 
  summarize_at(.vars=vars(Lateral_root_spread:Rooting_depth),
               .funs=list(~length(.[!is.na(.)]))) %>% 
  # calculate means across the traits                                               
  rename_with(~ paste0("n_",.), .cols=where(is.numeric))

RSIP5 <- RSIP4 %>% dplyr::select(all_of(c("Aggregated_name",
                              "Lateral_root_spread","Rooting_depth"))) %>% 
  group_by(Aggregated_name) %>% 
  summarize_at(.vars=vars(Lateral_root_spread:Rooting_depth),
               .funs=list(~mean(., na.rm=T))) %>% 
  # add n from RSIP5a
  left_join(RSIP5a, by="Aggregated_name") %>% 
  # combine with one name of the original and cleaned names
  # ATTENTION: it is arbitrary which of the original names was used
  left_join(RSIP4 %>% dplyr::select(Original_name,Edited_name, 
                    Parsed_name,Resolved_name,Aggregated_name,
                    RSIP_Growthform,RSIP_Lifespan,
                    RSIP_is.woody) %>% 
              group_by(Aggregated_name) %>% 
              slice(1),
            by="Aggregated_name") %>% 
  rename(RSIP_Original_name = Original_name,
         RSIP_Edited_name=Edited_name,
         RSIP_Parsed_name=Parsed_name,
         RSIP_Resolved_name=Resolved_name)

str(RSIP5) # [2,742 × 12]

# 2.3 Database with clonal traits
# CLOPLA, BBB, BBBnew, BBBSouthAfrica, BBAFTG, BBBA, AlineAlessandra, Siebert
# 2.3.1 join all databases 
name.list.CLOPLA <- c("Original_name",
                      "Edited_name","Parsed_name",
                      "Resolved_name","Aggregated_name")
trait.list.CLOPLA <- c("CGO","Growthform","BPI","BPT","CSI","is.woody","is.woody.below","is.woody.above","is.clonal","is.resprouting")
# categorical traits CLOPLA: CGO, Growthform (=GF)
trait.list.further.CLOPLA <- c("BBdepth","BBsize","multiplication",
                               "spread", "persistence", "ClonalGenetExtent")
# all numeric

# check all databases for var. and subsp.
unique(CLOPLA2$Resolved_name[grep(" var.",CLOPLA2$Resolved_name,fixed=T)])
unique(CLOPLA2$Resolved_name[grep("  var.",CLOPLA2$Resolved_name,fixed=T)])
unique(CLOPLA2$Resolved_name[grep(" subsp.",CLOPLA2$Resolved_name,fixed=T)])
unique(CLOPLA2$Resolved_name[grep("  subsp.",CLOPLA2$Resolved_name,fixed=T)])
# 55 subspecies, 15 variants
CLOPLA2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 4724
CLOPLA2 <- CLOPLA2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, " var.", fixed=T)[[1]])
CLOPLA2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 4684

unique(BBB2$Resolved_name[grep(" var.",BBB2$Resolved_name,fixed=T)])
unique(BBB2$Resolved_name[grep("  var.",BBB2$Resolved_name,fixed=T)])
unique(BBB2$Resolved_name[grep(" subsp.",BBB2$Resolved_name,fixed=T)])
unique(BBB2$Resolved_name[grep("  subsp.",BBB2$Resolved_name,fixed=T)])
# 40 subspecies, 42 variants
BBB2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 2095
BBB2 <- BBB2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, "  subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, " subsp.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, "  var.", fixed=T)[[1]]) %>% 
  mutate(Aggregated_name=tstrsplit(Aggregated_name, " var.", fixed=T)[[1]])
BBB2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 2056

unique(BBBnew2$Resolved_name[grep(" var.",BBBnew2$Resolved_name,fixed=T)])
unique(BBBnew2$Resolved_name[grep("  var.",BBBnew2$Resolved_name,fixed=T)])
unique(BBBnew2$Resolved_name[grep(" subsp.",BBBnew2$Resolved_name,fixed=T)])
unique(BBBnew2$Resolved_name[grep("  subsp.",BBBnew2$Resolved_name,fixed=T)])
# 0 subspecies, 1 variants
BBBnew2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 59
BBBnew2 <- BBBnew2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]]) 
BBBnew2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 59

unique(BBBSouthAfrica2$Resolved_name[grep(" var.",BBBSouthAfrica2$Resolved_name,fixed=T)])
unique(BBBSouthAfrica2$Resolved_name[grep("  var.",BBBSouthAfrica2$Resolved_name,fixed=T)])
unique(BBBSouthAfrica2$Resolved_name[grep(" subsp.",BBBSouthAfrica2$Resolved_name,fixed=T)])
unique(BBBSouthAfrica2$Resolved_name[grep("  subsp.",BBBSouthAfrica2$Resolved_name,fixed=T)])
# 0 subspecies, 1 variants
BBBSouthAfrica2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 35
BBBSouthAfrica2 <- BBBSouthAfrica2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])
BBBSouthAfrica2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 35

unique(BBAFTG2$Resolved_name[grep(" var.",BBAFTG2$Resolved_name,fixed=T)])
unique(BBAFTG2$Resolved_name[grep("  var.",BBAFTG2$Resolved_name,fixed=T)])
unique(BBAFTG2$Resolved_name[grep(" subsp.",BBAFTG2$Resolved_name,fixed=T)])
unique(BBAFTG2$Resolved_name[grep("  subsp.",BBAFTG2$Resolved_name,fixed=T)])
# 0 subspecies, 1 variants
BBAFTG2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 113
BBAFTG2 <- BBAFTG2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])
BBAFTG2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 113

unique(BBBA2$Resolved_name[grep(" var.",BBBA2$Resolved_name,fixed=T)])
unique(BBBA2$Resolved_name[grep("  var.",BBBA2$Resolved_name,fixed=T)])
unique(BBBA2$Resolved_name[grep(" subsp.",BBBA2$Resolved_name,fixed=T)])
unique(BBBA2$Resolved_name[grep("  subsp.",BBBA2$Resolved_name,fixed=T)])
# 1 subspecies, 0 variants
BBBA2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 158
BBBA2 <- BBBA2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " subsp.", fixed=T)[[1]])
BBBA2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 158

unique(AlineAlessandra2$Resolved_name[grep(" var.",AlineAlessandra2$Resolved_name,fixed=T)])
unique(AlineAlessandra2$Resolved_name[grep("  var.",AlineAlessandra2$Resolved_name,fixed=T)])
unique(AlineAlessandra2$Resolved_name[grep(" subsp.",AlineAlessandra2$Resolved_name,fixed=T)])
unique(AlineAlessandra2$Resolved_name[grep("  subsp.",AlineAlessandra2$Resolved_name,fixed=T)])
# 0 subspecies, 2 variants
AlineAlessandra2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 222
AlineAlessandra2 <- AlineAlessandra2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]])
AlineAlessandra2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 222

unique(Siebert2$Resolved_name[grep(" var.",Siebert2$Resolved_name,fixed=T)])
unique(Siebert2$Resolved_name[grep("  var.",Siebert2$Resolved_name,fixed=T)])
unique(Siebert2$Resolved_name[grep(" subsp.",Siebert2$Resolved_name,fixed=T)])
unique(Siebert2$Resolved_name[grep("  subsp.",Siebert2$Resolved_name,fixed=T)])
# 0 subspecies, 0 variants
Siebert2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 123
Siebert2 <- Siebert2 %>% 
  mutate(Aggregated_name=Resolved_name)
Siebert2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 123
table(Siebert2$BPT, Siebert2$is.woody.below, exclude=NULL)
'      FALSE TRUE
  1      18    0
  2      23    0
  3      29    0
  4       0    4
  5.1     0   17
  5.2     0   19
  6.1     0    4
  6.2     0   13'

unique(ITEX2$Resolved_name[grep(" var.",ITEX2$Resolved_name,fixed=T)])
unique(ITEX2$Resolved_name[grep("  var.",ITEX2$Resolved_name,fixed=T)])
unique(ITEX2$Resolved_name[grep(" subsp.",ITEX2$Resolved_name,fixed=T)])
unique(ITEX2$Resolved_name[grep("  subsp.",ITEX2$Resolved_name,fixed=T)])
# 26 subspecies, 9 variants
ITEX2 %>% 
  distinct(Resolved_name) %>% 
  nrow()
# 1000
ITEX2 <- ITEX2 %>% 
  mutate(Aggregated_name=tstrsplit(Resolved_name, " var.", fixed=T)[[1]],
         Aggregated_name=tstrsplit(Aggregated_name, " subsp.", fixed=T)[[1]])
ITEX2 %>% 
  distinct(Aggregated_name) %>% 
  nrow()
# 984

# 2.3.1 join all numerical traits 
new_cols <- data.frame(matrix(ncol=6,nrow=1, dimnames=list(NULL, trait.list.further.CLOPLA)))

CLOPLA3 <- CLOPLA2 %>% 
  rename(Original_name = spname, Growthform = GF) %>% 
  dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
  mutate(db = "CLOPLA") %>% 
  bind_rows(BBB2 %>% 
        rename(Original_name=Taxa) %>%
          mutate(!!!new_cols) %>% 
        dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
        mutate(db = "BBB")) %>% 
  bind_rows(BBBnew2 %>% 
          rename(Original_name=Species) %>%
          mutate(!!!new_cols) %>% 
          # mutate(BBsize=BBB.density) %>% changed 23.12.2025
          mutate(BBsize=buds.shoot) %>% 
          dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
          mutate(db = "BBBnew")) %>% 
  bind_rows(BBBSouthAfrica2 %>% 
          rename(Original_name=Species, Growthform =growth.form) %>%
          mutate(!!!new_cols) %>% 
          mutate(BBsize=BBB.density.Total) %>% 
          # here BBB.density.Total is correct, as buds/shoot is NA
          dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
          mutate(db = "BBBSouthAfrica")) %>% 
  bind_rows(BBAFTG2 %>% 
          rename(Original_name=species) %>%
          mutate(Growthform=NA) %>% 
          mutate(!!!new_cols) %>% 
          dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
          mutate(db = "BBAFTG")) %>% 
  bind_rows(BBBA2 %>% 
          rename(Original_name=Species) %>% 
          mutate(Growthform=NA) %>% 
          mutate(!!!new_cols) %>% 
          dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
          mutate(db = "BBBA")) %>% 
  bind_rows(AlineAlessandra2 %>% 
          rename(Original_name=Species) %>% 
          mutate(Growthform=aboveground.growth.form) %>% 
          mutate(!!!new_cols) %>% 
          dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
          mutate(db = "AlineAlessandra")) %>% 
  bind_rows(Siebert2 %>% 
        rename(Original_name=Species) %>% 
        mutate(Growthform=aboveground.growth.form,
               Edited_name=Parsed_name) %>% 
          mutate(!!!new_cols) %>% 
          dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
          mutate(db = "Siebert")) %>% 
  bind_rows(Laughlin_additions3 %>% 
              mutate(CGO=NA, Growthform=NA, BBsize2=BBsize) %>% 
              mutate(!!!new_cols) %>% 
              mutate(BBsize=BBsize2) %>% 
              dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
              mutate(db = "Laughlin_additions")) %>% 
  bind_rows(ITEX2 %>% 
              rename(Original_name=spp_name_itex,
                     is.woody=is.woody_itex) %>% 
              mutate(Growthform=Plant_growth_form,
                     CGO=NA, BPI=NA, CSI=NA) %>% 
              mutate(!!!new_cols) %>% 
              dplyr::select(all_of(c(name.list.CLOPLA,trait.list.CLOPLA,trait.list.further.CLOPLA))) %>% 
              mutate(db = "ITEX")) %>% 
  mutate(n=row_number())
str(CLOPLA3) #9672 obs. of  23 variables:
# Note: still contains multiple rows per species

# 2.3.2 Check for inconsistencies in BPI and CSI
table(CLOPLA3$BPI, CLOPLA3$is.woody, exclude=NULL)
table(CLOPLA3$BPT, CLOPLA3$is.woody.below, exclude=NULL)
'       FALSE TRUE <NA>
  1      920    0    0
  2     1426    0    0
  3     2985    0    0
  4        0  252    0
  5        0   49    0
  5.1      0  735    0
  5.2      0 1423    0
  6        0    8    0
  6.1      0  196    0
  6.2      0  532    0
  <NA>    59   59 1028'
table(CLOPLA3$BPT, CLOPLA3$is.woody.above, exclude=NULL)
table(CLOPLA3$CSI, CLOPLA3$is.clonal, exclude=NULL)

table(CLOPLA3$db, CLOPLA3$BPT, exclude=NULL)
'                        1    2    3    4    5  5.1  5.2    6  6.1  6.2 <NA>
  AlineAlessandra      15    1   43    3    0   87   45    0   17   14    0
  BBAFTG                0    2    4    0    4   42   20    7   31   42    0
  BBB                   0  125  257   39    0  203 1177    0   26  287    0
  BBBA                  0    9   27    0    2  102   25    0    0    0    0
  BBBnew                0   86   63    3   41  115   41    0    0    1    0
  BBBSouthAfrica        0    2  102    0    2  155   30    0  109   10    0
  CLOPLA              830 1166 2449  201    0   11   54    1    6  100    0
  ITEX                 20    0    0    2    0    0    9    0    0   59  962
  Laughlin_additions   37   12   11    0    0    3    3    0    3    6  184
  Siebert              18   23   29    4    0   17   19    0    4   13    0
'

species.list.all.data <- sort(unique(CLOPLA3$Aggregated_name))
str(species.list.all.data) #7641
CLOPLA3 %>% filter(Aggregated_name=="Aldama arenaria")
BPT_inconsistencies <- NULL
for(i in 1:length(species.list.all.data)){
  x <- CLOPLA3 %>% filter(Aggregated_name==species.list.all.data[i]) 
  if(length(x$BPT)>1){
    if(dim(table(x$BPT))>1){
      #x %>% dplyr::select(-CSI)
      # if("CLOPLA" %in% x$db & "BBB" %in% x$db & 4 %in% x$BPT & (5 %in% x$BPT | 5.1 %in% x$BPT | 5.2 %in% x$BPT)){
      #   CLOPLA3$BPT[CLOPLA3$Aggregated_name==species.list.all.data[i] & CLOPLA3$db=="CLOPLA"] <-
      #     CLOPLA3$BPT[CLOPLA3$Aggregated_name==species.list.all.data[i] & CLOPLA3$db=="BBB"][1]
      #   # Attention: this changes already CLOPLA assignments! is.woody.below etc. have to be changed, too!!!
      #   x <- CLOPLA3 %>% filter(Aggregated_name==species.list.all.data[i]) 
      #   if(dim(table(x$BPT))>1) {
      #     BPT_inconsistencies <- rbind(BPT_inconsistencies,x %>% dplyr::select(-CSI))
      #   }
      # } else {
        BPT_inconsistencies <- rbind(BPT_inconsistencies,x)
      # }
    }
  }
}
dim(BPT_inconsistencies) # 567 22
BPT_inconsistencies[c(1:10),]
# write.csv(BPT_inconsistencies, file = "BPT_inconsistencies8.csv", row.names = F)


BPT_inconsistencies_HB <- read.csv("BPT_inconsistencies8_HB.csv", 
                                      sep=",", na.strings = c("","NA"))
str(BPT_inconsistencies_HB) #587 obs. of  24
table(BPT_inconsistencies_HB$revised, exclude=NULL)
'  0   1 
376 211 '
CLOPLA4 <- CLOPLA3 %>% left_join(BPT_inconsistencies_HB %>% dplyr::select(n,revised),
                            by="n")
# CLOPLA4 %>% dplyr::select(Aggregated_name,n,revised) %>% print(n=40)
dim(CLOPLA4) #9672 24

table(CLOPLA4$revised, exclude=NULL)
'   0    1 <NA> 
 376  211 9085 
'
table(CLOPLA3$db, CLOPLA3$CSI, exclude=NULL)
'                        0    1    2 <NA>
  AlineAlessandra     150   41   34    0
  BBAFTG               67    0   80    5
  BBB                1424   91  152  447
  BBBA                124    0   39    2
  BBBnew              273   63    1   13
  BBBSouthAfrica      189  102  119    0
  CLOPLA             2259 1023 1096  440
  ITEX                  0    0    0 1052
  Laughlin_additions    0    0    0  259
  Siebert               0    0    0  127
'
CSI_inconsistencies <- NULL
for(i in 1:length(species.list.all.data)){
  x <- CLOPLA3 %>% filter(Aggregated_name==species.list.all.data[i]) 
  if(length(x$CSI)>1){
    if(dim(table(x$CSI))>1){
      CSI_inconsistencies <- rbind(CSI_inconsistencies,x %>% dplyr::select(-BPI))
    }
  }
}
CSI_inconsistencies
#write.csv(CSI_inconsistencies, file = "CSI_inconsistencies6.csv", row.names = F)

CSI_inconsistencies_HB <- read.csv("CSI_inconsistencies6_HB.csv", 
                                      sep=",", na.strings = c("","NA"))
str(CSI_inconsistencies_HB)
CLOPLA4 <- CLOPLA4 %>% left_join(CSI_inconsistencies_HB %>% dplyr::select(n,revised.CSI),
                                 by="n")
# CLOPLA4 %>% dplyr::select(Aggregated_name,n,revised,revised.CSI) %>% print(n=40)
dim(CLOPLA4) #9672 25
table(CLOPLA4$revised,CLOPLA4$revised.CSI, exclude=NULL)
'          0    1 <NA>
  0      65    7  304
  1       7   38  166
  <NA>   24   19 9042'
# Take is.woody.below etc from revised==1, and only CSI from revised:CSI==1
# check is.woody etc. later


# 2.3.3 aggregate by Resolved_name
str(CLOPLA4) #9672 obs. of  25 variables:

# check for duplicate and missing taxon names 
CLOPLA4$Original_name[is.na(CLOPLA4$Resolved_name)]
# 0
CLOPLA4$Original_name[is.na(CLOPLA4$Aggregated_name)]

length(unique(CLOPLA4$Resolved_name)) #  7755
length(CLOPLA4$Resolved_name[duplicated(CLOPLA4$Resolved_name)])
# 1917
length(CLOPLA4$Aggregated_name[duplicated(CLOPLA4$Aggregated_name)])
# 2031

# categorical traits CLOPLA: CGO, Growthform (=GF)
table(CLOPLA3$Growthform, exclude=NULL)

#CLOPLA4$Growthform[CLOPLA4$Growthform=="Fibrous"] <- NA
CLOPLA4$Growthform[CLOPLA4$Growthform=="T"] <- "annual"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Forb"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Forb/shrub"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Forb/Shrub"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="grass"] <-"graminoid"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Grass"] <-"graminoid"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Graminoid"] <-"graminoid"
CLOPLA4$Growthform[CLOPLA4$Growthform=="GRAMINOID"] <-"graminoid"
CLOPLA4$Growthform[CLOPLA4$Growthform=="graminoid/non-woody"] <-"graminoid"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Graminoids"] <-"graminoid"
CLOPLA4$Growthform[CLOPLA4$Growthform=="SEDGE"] <-"graminoid"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Herb"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="herb"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="herb."] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="herbs"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Forbs"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="H"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Herb (H)"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="herb/non-woody"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Herbaceous dicots"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Herbaceous Dicot"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="perennial herb"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="leguminous forb"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="nonclonal perennial"] <- "non-clonal perennial"
CLOPLA4$Growthform[CLOPLA4$Growthform=="non clonal woody"] <- "non-clonal woody"
CLOPLA4$Growthform[CLOPLA4$Growthform=="nonclonal woody"] <- "non-clonal woody"
CLOPLA4$Growthform[CLOPLA4$Growthform=="shrub(creeper)"] <- "liana" # Tylosema fassoglense
CLOPLA4$Growthform[CLOPLA4$Growthform=="Shrub"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Shrub/Tree intermediate"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="shrub/woody"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="tree / shrub"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Tree, Shrub"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="S"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="sub-shrub"] <- "subshrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Sub-shrub"] <- "subshrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Suffrutex"] <- "subshrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Forb/herb, Subshrub"] <- "subshrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Subshrub, Forb/herb"] <- "subshrub"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Variable"] <- NA
CLOPLA4$Growthform[CLOPLA4$Growthform=="Tree"] <- "tree"
CLOPLA4$Growthform[CLOPLA4$Growthform=="conifer"] <- "tree"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Ranunculus gmelinii"] <- "clonal perennial"
CLOPLA4$Growthform[CLOPLA4$Growthform=="b H"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Forb/herb"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Pinguicula vulgaris"] <- "forb"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Rhododendron camtschaticum"] <- "shrub"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Eleocharis acicularis"] <- "clonal perennial"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Eleocharis uniglumis"] <- "clonal perennial"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Lycopodium fastigiatum"] <- "clonal perennial"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Lycopodium annotinum"] <- "clonal perennial"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Utricularia vulgaris" & CLOPLA4$db=="ITEX"] <- "clonal perennial"
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Pedicularis racemosa" & CLOPLA4$db=="ITEX"] <- "non-clonal perennial"
CLOPLA4$BPT[CLOPLA4$Aggregated_name=="Pedicularis racemosa" & CLOPLA4$db=="ITEX"] <- 5.1
CLOPLA4$is.woody.below[CLOPLA4$Aggregated_name=="Pedicularis racemosa" & CLOPLA4$db=="ITEX"] <- T
CLOPLA4$is.clonal[CLOPLA4$Aggregated_name=="Pedicularis racemosa" & CLOPLA4$db=="ITEX"] <- F
CLOPLA4$Growthform[CLOPLA4$Aggregated_name=="Lycopodium annotinum"] <- "clonal perennial"
CLOPLA4$Growthform[CLOPLA4$Growthform=="Woody"] <- NA
CLOPLA4$Growthform[CLOPLA4$Growthform=="woody"] <- NA
CLOPLA4$Growthform[CLOPLA4$Growthform=="Free-standing"] <- NA
CLOPLA4$Growthform[CLOPLA4$Growthform=="G"] <- NA
CLOPLA4$Growthform[CLOPLA4$Growthform=="herb/shrub/non-woody/woody"] <- NA
table(CLOPLA4$Growthform,  exclude = NULL)

'                      annual             clonal perennial 
                         831                         2455 
                clonal woody                        erect 
                         107                            1 
                     Fibrous                         forb 
                          19                         1494 
                   graminoid                HEMI-PARASITE 
                         306                            6 
herb/hemiparasitic/non-woody                   herb/shrub 
                           4                            2 
    herb/succulent/non-woody                   herbaceous 
                           3                            2 
          herbaceous dicotyl           Herbaceous Monocot 
                          18                           20 
        herbaceous monocotyl                        liana 
                           9                           10 
                          No         non-clonal perennial 
                           5                         1166 
            non-clonal woody                non-succulent 
                         264                           18 
                    Parasite                        shrub 
                           2                          223 
 Stem ascending to prostrate                   Stem erect 
                          41                          183 
              Stem prostrate                     subshrub 
                          13                          203 
                   succulent                         tree 
                           2                           60 
                        <NA> 
                        2205 '
CLOPLA4 %>% filter(Growthform == "HEMI-PARASITE")
CLOPLA4 %>% filter(Aggregated_name == "Pedicularis racemosa")
CLOPLA4 %>% filter(grepl("Pedicularis",Aggregated_name))
#CLOPLA4a <- CLOPLA4
#CLOPLA4 <- CLOPLA4a
table(CLOPLA4$CGO, exclude=NULL)

CLOPLA4[CLOPLA4$CGO=="tuber" & !is.na(CLOPLA4$CGO=="tuber"),]

CLOPLA4$CGO[CLOPLA4$CGO==" "] <- NA
CLOPLA4$CGO[CLOPLA4$CGO=="Bulb"] <- "bulb"
CLOPLA4$CGO[CLOPLA4$CGO=="Clonal herb"] <- "clonal herb"
CLOPLA4$CGO[CLOPLA4$CGO=="Corm"] <- "corm"
CLOPLA4$CGO[CLOPLA4$CGO=="Lignotuber"] <- "lignotuber"
CLOPLA4$CGO[CLOPLA4$CGO=="herbaceous perennial non-clonal"] <- "herbaceous perennial non clonal"
CLOPLA4$CGO[CLOPLA4$CGO=="perennial herb non clonal"] <- "herbaceous perennial non clonal"
CLOPLA4$CGO[CLOPLA4$CGO=="Perennial herbaceous non clonal"] <- "herbaceous perennial non clonal"
CLOPLA4$CGO[CLOPLA4$CGO=="Perennial herbaceous non-clonal"] <- "herbaceous perennial non clonal"
CLOPLA4$CGO[CLOPLA4$CGO=="Root crown"] <- "root crown"
CLOPLA4$CGO[CLOPLA4$CGO=="Roots- clonal"] <- "roots-clonal"
CLOPLA4$CGO[CLOPLA4$CGO=="Stem tubers"] <- "stem tuber"
CLOPLA4$CGO[CLOPLA4$CGO=="Taproot tuber- non clonal"] <- "taproot tuber-non clonal"
CLOPLA4$CGO[CLOPLA4$CGO=="tuber"] <- "root tuber"
CLOPLA4$CGO[CLOPLA4$CGO=="Woody rhizomes"] <- "woody rhizome"
CLOPLA4$CGO[CLOPLA4$CGO=="Xylopodium"] <- "xylopodium"
table(CLOPLA4$CGO, exclude=NULL)
'                          annual                bud-bearing root                            bulb 
                            830                               6                             167 
                    clonal herb                            corm            epigeogenous rhizome 
                            187                              12                            1030 
herbaceous perennial non clonal                 hypocotyl tuber           hypogeogenous rhizome 
                             22                               6                             851 
                     lignotuber                       main root            no belowground roots 
                            704                            1182                              35 
              Non-woody rhizome                         rhizome                      root crown 
                            102                              96                             775 
                 root sprouting                      root tuber                    roots-clonal 
                            209                              46                             171 
                     stem tuber                          stolon        taproot tuber-non clonal 
                             66                             180                              19 
                           tree                woody non-clonal                   woody rhizome 
                              1                              25                             383 
                     xylopodium                            <NA> 
                            677                            1890  '
# check some names
CLOPLA4 %>% filter(Aggregated_name=="Gyptis lanigera")
CLOPLA4 %>% filter(Aggregated_name=="Hypericum mutilum")
CLOPLA4 %>% filter(Aggregated_name=="Ptilotrichum canescens")
CLOPLA4 %>% filter(Aggregated_name=="Protea gaguedi")
CLOPLA4 %>% filter(Aggregated_name=="Gyptis pinnatifida")
CLOPLA4 %>% filter(Aggregated_name=="Lessingianthus elegans")
CLOPLA4 %>% filter(Aggregated_name=="Aldama arenaria")
CLOPLA4 %>% filter(Aggregated_name=="Chrysolaena flexuosa")
CLOPLA4 %>% filter(Aggregated_name=="Protea gaguedi")

CLOPLA4[CLOPLA4$CGO=="tuber" & !is.na(CLOPLA4$CGO=="tuber"),]


CLOPLA5a <- CLOPLA4 %>%  dplyr::select(all_of(c(name.list.CLOPLA, "BPT", 
                                                "is.woody.below","is.woody.above", "is.clonal", "is.resprouting",
                                                "revised"))) %>% 
  group_by(Aggregated_name) %>% 
    filter(revised==1 | is.na(revised)) %>% 
    slice(1) %>% 
  rename(BBB_Original_name = Original_name,
         BBB_Edited_name = Edited_name,
         BBB_Parsed_name = Parsed_name,
         BBB_Resolved_name = Resolved_name)
# ATTENTION: it is arbitrary which of the original data rows is used
str(CLOPLA5a) # 7,633 × 11

CLOPLA5b <- CLOPLA4 %>%  dplyr::select(all_of(c("Aggregated_name", "CSI", "revised.CSI"))) %>% 
  group_by(Aggregated_name) %>% 
  filter(revised.CSI==1 | is.na(revised.CSI)) %>% 
  slice(1)
str(CLOPLA5b)
# 7,634 × 3

#CLOPLA5a$Aggregated_name[!CLOPLA5a$Aggregated_name %in% CLOPLA5b$Aggregated_name]

CLOPLA5c <- CLOPLA4 %>% 
  dplyr::select(all_of(c("Aggregated_name", trait.list.further.CLOPLA))) %>% 
  group_by(Aggregated_name) %>% 
  summarize_at(.vars=vars(all_of(trait.list.further.CLOPLA)),
               .funs=list(~length(.[!is.na(.)]))) %>% 
  rename_with(~ paste0("n_",.), .cols=where(is.numeric))
str(CLOPLA5c) # [7,641 × 7]

CLOPLA5d <- CLOPLA4 %>%  dplyr::select(all_of(c("Aggregated_name", trait.list.further.CLOPLA))) %>% 
  group_by(Aggregated_name) %>% 
      summarize_at(.vars=vars(all_of(trait.list.further.CLOPLA)),
                   .funs=list(~mean(., na.rm=T))) 
str(CLOPLA5d) # [7,641 × 7]

CLOPLA5e <- CLOPLA4 %>%  dplyr::select(all_of(c("Aggregated_name", "CGO","Growthform","db"))) %>% 
  group_by(Aggregated_name) %>% 
  # paste together all categories of the categorical fields
  summarize(CGO=paste(sort(unique(CGO[!is.na(CGO)])), collapse=", "),
            Growthform=paste(sort(unique(Growthform[!is.na(Growthform)])), collapse=", "),
            db=paste(sort(unique(db[!is.na(db)])), collapse=", ") ) %>% 
  rename(BBB_CGO= CGO,
         BBB_Growthform=Growthform)

str(CLOPLA5e) # [7,641 × 4]

CLOPLA5 <- CLOPLA5a %>% 
# combine 
  left_join(CLOPLA5e, by="Aggregated_name") %>% 
  left_join(CLOPLA5b, by="Aggregated_name") %>% 
  left_join(CLOPLA5c, by="Aggregated_name") %>% 
  left_join(CLOPLA5d, by="Aggregated_name") 

  
str(CLOPLA5) 
# 7,633 × 28
# one species per row
table(CLOPLA5$BPT, exclude=NULL)
'   1    2    3    4    5  5.1  5.2    6  6.1  6.2 <NA> 
 848 1278 2665  193    8  374 1228    7   74  446  512 '

table(CLOPLA5$BBB_Growthform, exclude=NULL)

table(CLOPLA5$BPT, CLOPLA5$is.woody.below, exclude=NULL)
'       FALSE TRUE <NA>
  1      848    0    0
  2     1278    0    0
  3     2665    0    0
  4        0  193    0
  5        0    8    0
  5.1      0  374    0
  5.2      0 1228    0
  6        0    7    0
  6.1      0   74    0
  6.2      0  446    0
  <NA>    30   41  441'

table(CLOPLA5$BPT, CLOPLA5$is.woody.above, exclude=NULL)
'      FALSE TRUE <NA>
  1      848    0    0
  2     1278    0    0
  3     2663    2    0
  4        2  190    1
  5        2    1    5
  5.1    374    0    0
  5.2      0 1228    0
  6        0    0    7
  6.1     74    0    0
  6.2      0  446    0
  <NA>   316   41  155
'

# 3. Make joint file of GRooT, RSIP, BBBall
all.data1 <- GRooT5 %>% 
  full_join(RSIP5, by="Aggregated_name") %>% 
  full_join(CLOPLA5, by="Aggregated_name")
str(all.data1)
# [10,453 × 53]

# check for duplicate species introduced with full_join!!!
length(unique(all.data1$Aggregated_name)) #10453 -> o.k.

names(all.data1)


all.data2 <- all.data1 %>% 
  dplyr::select(where(is.numeric)) %>% 
  dplyr::select(-starts_with("n_"))
all.data2[all.data2>0] <- 1
str(all.data2)
all.data2 <- all.data2 %>%
  mutate(n_numeric_entries_per_row=rowSums(., na.rm=T))
all.data3 <- cbind(all.data1,all.data2 %>% 
                     dplyr::select(n_numeric_entries_per_row))
str(all.data3) # 10454 obs. of  54
names(all.data3)
all.data3 <- all.data3 %>% 
  dplyr::select(Aggregated_name,GRooT_Edited_name, GRooT_Parsed_name, GRooT_Resolved_name, 
                GRooT_clonal,Root_N_concentration, Coarse_root_fine_root_mass_ratio,
                Mean_Root_diameter,  Root_tissue_density, Specific_root_length,
                n_Root_N_concentration, n_Coarse_root_fine_root_mass_ratio,
                n_Mean_Root_diameter, n_Root_tissue_density,              
                n_Specific_root_length, 
                RSIP_Original_name, RSIP_Edited_name, RSIP_Parsed_name,RSIP_Resolved_name,
                Rooting_depth, Lateral_root_spread, n_Rooting_depth, n_Lateral_root_spread,
                RSIP_Growthform, RSIP_Lifespan, RSIP_is.woody,
                BBB_Original_name, BBB_Edited_name, BBB_Parsed_name,BBB_Resolved_name,
                BBB_CGO, BBB_Growthform,
                BBdepth, BBsize, multiplication, spread, persistence,
                ClonalGenetExtent, BPT, CSI, 
                is.woody.below, is.woody.above, is.clonal, is.resprouting,
                n_BBdepth, n_BBsize, n_multiplication, n_spread,
                n_persistence, n_ClonalGenetExtent, n_numeric_entries_per_row) %>% 
  arrange(Aggregated_name,desc(n_numeric_entries_per_row)) 
str(all.data3) #10453 obs. of  51 variables:
unique(all.data3$Aggregated_name[grep(" var.",all.data3$Resolved_name,fixed=T)]) #0
unique(all.data3$Aggregated_name[grep(" subsp.",all.data3$Resolved_name,fixed=T)]) #0
unique(all.data3$Aggregated_name[grep(" var.",all.data3$Aggregated_name,fixed=T)]) #0
unique(all.data3$Aggregated_name[grep(" subsp.",all.data3$Aggregated_name,fixed=T)]) #0
write.csv(all.data3, file="3_All/All_databases11.csv",  
          row.names = F, fileEncoding = "UTF-8")

# check against backbone
Backbone_sUnderfoot <- read.csv("sUnderfoot_Backbone/Backbone_sUnderfoot.csv", fileEncoding = "UTF-8")
table(all.data3$GRooT_Resolved_name[!is.na(all.data3$GRooT_Resolved_name)] %in% Backbone_sUnderfoot$Original_name)
'FALSE  TRUE 
  318  2483 '
x1 <- all.data3$GRooT_Resolved_name[!all.data3$GRooT_Resolved_name %in% Backbone_sUnderfoot$Original_name &
                              !is.na(all.data3$GRooT_Resolved_name)]
x1
x1[!x1 %in% Backbone_sUnderfoot10$Edited_name]
# "Lophozonia" "Augusta"    "Carria"     "Swinglea"   "Vignea"
x1[!x1 %in% Backbone_sUnderfoot10$Resolved_name]
# "" 
all.data3$GRooT_Resolved_name[!(all.data3$GRooT_Resolved_name %in% Backbone_sUnderfoot$Original_name |
                              all.data3$GRooT_Resolved_name %in% Backbone_sUnderfoot$Resolved_name) &
                             !is.na(all.data3$GRooT_Resolved_name)]
# 0

table(all.data3$RSIP_Resolved_name[!is.na(all.data3$RSIP_Resolved_name)] %in% Backbone_sUnderfoot$Original_name)
'FALSE  TRUE 
  503  2239  '
x2 <- all.data3$RSIP_Resolved_name[!all.data3$RSIP_Resolved_name %in% Backbone_sUnderfoot$Original_name &
                                    !is.na(all.data3$RSIP_Resolved_name)]
x2
# "Dipterocarpaceae"   "Juglans regianigra"

x2[!x2 %in% Backbone_sUnderfoot$Edited_name] 
"Dipterocarpaceae"   "Juglans regianigra"
x2[!x2 %in% Backbone_sUnderfoot10$Resolved_name]
# 0
all.data3$RSIP_Resolved_name[!(all.data3$RSIP_Resolved_name %in% Backbone_sUnderfoot$Original_name |
                                all.data3$RSIP_Resolved_name %in% Backbone_sUnderfoot$Resolved_name) &
                              !is.na(all.data3$RSIP_Resolved_name)]
# 0

table(all.data3$BBB_Resolved_name[!is.na(all.data3$BBB_Resolved_name)] %in% Backbone_sUnderfoot$Original_name)
'FALSE  TRUE 
 1241  6392  '
x3 <- all.data3$BBB_Resolved_name[!all.data3$BBB_Resolved_name %in% Backbone_sUnderfoot$Original_name &
                                   !is.na(all.data3$BBB_Resolved_name)]
x3

x3[!x3 %in% Backbone_sUnderfoot10$Edited_name]

x3[!x3 %in% Backbone_sUnderfoot10$Resolved_name]

all.data3$BBB_Resolved_name[!(all.data3$BBB_Resolved_name %in% Backbone_sUnderfoot$Original_name |
                               all.data3$BBB_Resolved_name %in% Backbone_sUnderfoot$Resolved_name) &
                             !is.na(all.data3$BBB_Resolved_name)]
# 0

# 4.Harmonize Growthform
# to be done
table(all.data3$BPT, exclude=NULL)
'   1    2    3    4    5  5.1  5.2    6  6.1  6.2 <NA> 
 848 1278 2665  193    8  374 1228    7   74  446 3332   '
table(all.data3$CSI, exclude=NULL)
'    0    1    2 <NA> 
3715 1114 1325 4299 '
