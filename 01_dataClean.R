#
# ------------------------------------------------------------------------------------------------------
#
#
# Spatio-temporal patterns in crops and agrochemicals in Canada over 35 years
# Egina Malaj, Levi Freistadt, Christy A. Morrissey
#
# Published in Frontiers in Environmental Science
# 
# Last Update: 27-August-2020 
#
# Author: Egina Malaj,PhD
# contact: eginamalaj@gmail.com
#
# ------------------------------------------------------------------------------------------------------
# Census of Agriculture Data Collection
# ------------------------------------------------------------------------------------------------------
#
#
require(tidyverse)
options(scipen=999)
#
#
### 1981
#
files81 <- list.files("rawData/1981", full.names=T, pattern="csv")
files81
#
# Read and join
tbl81 <- files81%>%
  map(read_csv)%>%
  map(~slice(.,-2)) # remove 000000 (data for Canada) for all dataframes - problem when joining as the number of 0 per file changes
#
#
tbl81_1<-tbl81%>%
  reduce(full_join, by = c("AREAID","SGC","PROV","CAR","CD","CCS"))
#
nrow(tbl81_1)==nrow(tbl81[[1]]) # join works well
#
#
tbl81_1[,8:654] <- sapply(tbl81_1[,8:654], readr::parse_number)
#
# There are no "x" in 1981 & 86 only zeros & no tillage data & expense data
#
#
db81<- tbl81_1[-c(1:2),] %>%
  select(AREAID,SGC,PROV,CAR,CD,CCS,AREANAME.x,
         TFAREA.x.x,NTFAREA.y.y, CRPLND, NCRPLND,
         TOTFER,NTOTFER, HERBCI, NHERBCI, INSECTI, NINSECTI, # no fungicide data FUNGIC, NFUNGIC,
         TSMFRTC, OTHBERC,# total berries and grapes under cultivations
         TOTVEG,TOTOVEG, OTHVEG, BUCWHT, SUGARB, POTATS, # vegetables
         TOTTFT, # total tree fruit and other tree fruit
         WHTSPG,WHTDUR,WHTWIN,WHTUTL, OATS,BARLEY, MXDGRN,CORNGR,CORNEN, RYEFAL,RYESPG,CANRSD, # grain. corn for sillage =CORNEN
         CANOLA,SOYBNS,FLAXSD,MUSTSD,SUNFLS, # oilseeds
         DFPEAS,LENTIL,FABBNS, # pulses - no data for cheakpeas
         SALES95) # expenses
#
#
db81_2<- db81%>%
  mutate(grain = WHTSPG + WHTDUR + WHTWIN + WHTUTL+ OATS + BARLEY + 
           MXDGRN + CORNGR + CORNEN + RYEFAL + RYESPG +CANRSD,
         oilsd= CANOLA + SOYBNS + FLAXSD + MUSTSD + SUNFLS,
         plses = DFPEAS  + LENTIL + FABBNS, 
         veg = TOTVEG + TOTOVEG+ OTHVEG + BUCWHT + SUGARB + POTATS,
         vegNoPotat=TOTVEG + TOTOVEG+ OTHVEG + BUCWHT + SUGARB,
         corn=CORNGR + CORNEN,
         wheat=WHTSPG + WHTDUR + WHTWIN + WHTUTL,
         frt = TSMFRTC + OTHBERC + TOTTFT, # berries+ tree fruits + other
         NTFAREA=NTFAREA.y.y,
         TFAREA =TFAREA.x.x,
         AREANAME=AREANAME.x,
         FERTIL = TOTFER,
         NFERTIL = NTOTFER)%>%
  select(AREAID,SGC,PROV,CAR,CD,CCS,AREANAME,
         TFAREA,NTFAREA, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI,
         grain,oilsd,plses, veg, frt, CANOLA,OATS, BARLEY,SOYBNS,POTATS,vegNoPotat,corn,wheat,
         SALES95)%>%
  add_column(TOTEXP=NA,TILCONV=NA, TILCONS=NA, TILLNO=NA,
             FUNGIC=NA,NFUNGIC=NA, C_YEAR="1981") # no data on expenses & tillage & fungicide - add empty column so all years can join
#
#
### 1986
#
files86 <- list.files("rawData/1986", full.names=T, pattern="csv")
files86
#
# Read and join
tbl86 <- files86%>%
  map(read_csv)%>%
  map(~slice(.,-2)) 
#
# Add in one dataset
tbl86_1<-tbl86%>%
  reduce(full_join, by = "AREAID")
# ! Always check nr of rows!
nrow(tbl86_1)==nrow(tbl86[[1]]) # ok
#
#
# There are no "x" in 1986 only zeros & no tillage data
#
db86<- tbl86_1[-1,]  %>%
  select(AREAID,SGC.x,PROV.x,CAR.x,CD.x,CCS.x,AREANAME.x,
         TFAREA.y,NTFAREA, CRPLND, NCRPLND,
         FERTIL, NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI, # no fungicide data FUNGIC, NFUNGIC,
         TSMFRTC, OTHBERC,# total berries and grapes under cultivations
         TOTVEG,OTHVEG, BUCWHT, SUGARB, POTATS, # vegetables
         TOTTFT, OTREETA, # total tree fruit and other tree fruit
         WHTSPG,WHTDUR,WHTWIN,WHTUTL,WHTHSPRG,WHTSSPRG,WHTSMDF, OATS,BARLEY, FDOATS, FDBARL, MXDGRN,CORNGR,CORNEN, RYEFAL,RYESPG,CANARY, # grain. corn for sillage =CORNEN
         CANOLA,SOYBNS,FLAXSD,MUSTSD,SUNFLS, # oilseeds
         DFPEAS,LENTIL,WHITBN,ODFBNS,FABABN, # pulses - no data for cheakpeas
         SALES95,TOTEXP)%>% # expenses
  rename(SGC=SGC.x,PROV=PROV.x,CAR=CAR.x,CD=CD.x,CCS=CCS.x,AREANAME=AREANAME.x,TFAREA=TFAREA.y) 
#
# Transform to numeric
#
db86[,8:55] <- sapply(db86[,8:55], readr::parse_number)
#
#
db86_2<- db86%>%
  mutate(grain = WHTSPG + WHTDUR + WHTWIN + WHTUTL+ WHTHSPRG + WHTSSPRG + WHTSMDF+
           OATS + BARLEY + FDOATS + FDBARL +
           MXDGRN + CORNGR + CORNEN + RYEFAL + RYESPG + CANARY,
         oilsd= CANOLA + SOYBNS + FLAXSD + MUSTSD + SUNFLS,
         plses = DFPEAS  + LENTIL + FABABN, 
         vegNoPotat=TOTVEG + OTHVEG + BUCWHT + SUGARB,
         corn=CORNGR + CORNEN,
         wheat= WHTSPG + WHTDUR + WHTWIN + WHTUTL+ WHTHSPRG + WHTSSPRG + WHTSMDF,
         veg = TOTVEG + OTHVEG + BUCWHT + SUGARB + POTATS,
         frt = TSMFRTC + OTHBERC + TOTTFT+OTREETA)%>% # berries+ tree fruits + other
  select(AREAID,SGC,PROV,CAR,CD,CCS,AREANAME,
         TFAREA,NTFAREA, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI,
         grain,oilsd,plses, veg, frt, CANOLA,OATS, BARLEY,SOYBNS,POTATS,vegNoPotat,corn,wheat,
         SALES95,TOTEXP)%>%
  add_column(TILCONV=NA, TILCONS=NA, TILLNO=NA,
             FUNGIC=NA,NFUNGIC=NA, C_YEAR="1986") # no data on tillage & fung - add empty column so all years can join
#
#
#
### 1991
#
files91 <- list.files("rawData/1991", full.names=T, pattern="csv")
files91
#
# Read and join
tbl91 <- files91%>%
  map(read_csv)%>%
  map(~slice(.,-2)) 
#
#
tbl91_1<-tbl91%>%
  reduce(full_join, by = "AREAID")
#
nrow(tbl91_1)==nrow(tbl91[[1]])# True - Ok
#
colnm91<-
        tbl91_1[1,]%>%
          gather()
#
db91<- tbl91_1[-1,] %>%
  select(AREAID,SGC.x,PROV.x,CAR.x,CD.x,CCS.x,AREANAME.x,
         TFAREA,NTFAREA.x, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI, # no fungicide data FUNGIC, NFUNGIC,
         TILCONV, TILCONS, TILLNO, # tillage
         TSMFRTC, OTHBERC,# total berries and grapes under cultivations
         TOTVEG,OTHVEG, BUCWHT, SUGARB, POTATS, # vegetables
         TOTTFT,OTREETA, # total tree fruit and other tree fruit
         WHTSPG,WHTDUR,WHTWIN,OATS,BARLEY, MXDGRN,CORNGR,CORNEN, RYEFAL,RYESPG,CANARY, # grain. corn for sillage =CORNEN
         CANOLA,SOYBNS,FLAXSD,MUSTSD,SUNFLS, # oilseeds
         DFPEAS,LENTIL,WHITBN,ODFBNS,FABABN, # pulses - no data for cheakpeas
         SALES95, TOTEXP)%>% # expenses
  rename(SGC=SGC.x,PROV=PROV.x,CAR=CAR.x,CD=CD.x,CCS=CCS.x,AREANAME=AREANAME.x,NTFAREA=NTFAREA.x) 
#
# Transform to numeric
#
db91[,8:52] <- sapply(db91[,8:52], readr::parse_number)
#
# All NA for crops to 0 - we need for summing them up
db91[,21:50][is.na(db91[,21:50])] <-0
#
#
db91_2<- db91%>%
  mutate(grain = WHTSPG + WHTDUR + WHTWIN + OATS + BARLEY + 
           MXDGRN + CORNGR + CORNEN + RYEFAL + RYESPG +CANARY,
         oilsd= CANOLA + SOYBNS + FLAXSD + MUSTSD + SUNFLS,
         plses = DFPEAS  + LENTIL + WHITBN + ODFBNS + FABABN, 
         veg = TOTVEG + OTHVEG + BUCWHT + SUGARB + POTATS,
         vegNoPotat=TOTVEG + OTHVEG + BUCWHT + SUGARB,
         corn=CORNGR + CORNEN,
         wheat=WHTSPG + WHTDUR + WHTWIN,
         frt = TSMFRTC+TOTTFT + OTREETA+ OTHBERC)%>% # berries+ tree fruits + other
  select(AREAID,SGC,PROV,CAR,CD,CCS,AREANAME,
         TFAREA,NTFAREA, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI,
         TILCONV, TILCONS, TILLNO,
         grain,oilsd,plses, veg, frt, CANOLA,OATS, BARLEY,SOYBNS,POTATS,vegNoPotat,corn,wheat,
         SALES95, TOTEXP)%>%
  add_column(FUNGIC=NA, NFUNGIC=NA, C_YEAR="1991") # no for fung - add empty column so all years can join
#
#
#
### 1996
#
files96 <- list.files("rawData/1996", full.names=T, pattern="csv")
files96
#
# Read and join
tbl96 <- files96%>%
  map(read_csv)%>%
  map(~slice(.,-2))%>%
  map(~filter(.,!AREAID==0000000)) # still areaid with 000000 remove area with zero otherwise join below goes crazy
#
#
tbl96_1<-tbl96%>%
  reduce(full_join, by = "AREAID")
#
nrow(tbl96_1)==nrow(tbl96[[1]]) # 2315 entries
#
#
db96<- tbl96_1[-1,] %>%
  select(AREAID,SGC.x,PROV.x,CAR.x,CD.x,CCS.x,AREANAME.x,
         TFAREA.y.y.y.y,NTFAREA, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI, FUNGIC, NFUNGIC,
         TILCONV, TILCONS, TILLNO, # tillage
         TSMFRTC,OTHBERC, # total berries and grapes under cultivations
         TOTVEG,OTHVEG, BUCWHT, SUGARB, POTATS, # vegetables
         TOTTFT,OTREETA, # total tree fruit and other tree fruit
         WHTSPG,WHTDUR,WHTWIN,OATS,BARLEY, MXDGRN,CORNGR,CORNEN, RYEFAL,RYESPG,CANARY, # grain. corn for sillage =CORNEN
         CANOLA,SOYBNS,FLAXSD,MUSTSD,SUNFLS, # oilseeds
         DFPEAS,LENTIL,WHITBN,ODFBNS,FABABN, # pulses - no data for cheakpeas
         SALES95, TOTEXP) %>% # expenses
  rename(SGC=SGC.x,PROV=PROV.x,CAR=CAR.x,CD=CD.x,CCS=CCS.x,AREANAME=AREANAME.x,TFAREA=TFAREA.y.y.y.y) 
#
# Transform to numeric
#
db96[,8:54] <- sapply(db96[,8:54], readr::parse_number)
#
# All NA for crops to 0 - we need for summing them up
db96[,23:52][is.na(db96[,23:52])] <-0
#
db96_2<- db96%>%
   mutate(grain = WHTSPG + WHTDUR + WHTWIN + OATS + BARLEY + 
                    MXDGRN + CORNGR + CORNEN + RYEFAL + RYESPG +CANARY,
         oilsd= CANOLA + SOYBNS + FLAXSD + MUSTSD + SUNFLS,
         plses = DFPEAS  + LENTIL + WHITBN + ODFBNS + FABABN, 
         veg = TOTVEG + OTHVEG + BUCWHT + SUGARB + POTATS,
         vegNoPotat=TOTVEG + OTHVEG + BUCWHT + SUGARB,
         corn=CORNGR + CORNEN,
         wheat=WHTSPG + WHTDUR + WHTWIN,
         frt = TSMFRTC+TOTTFT + OTREETA+ OTHBERC# berries+ tree fruits + other
         )%>%
  select(AREAID,SGC,PROV,CAR,CD,CCS,AREANAME,
         TFAREA,NTFAREA, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI, FUNGIC, NFUNGIC,
         TILCONV, TILCONS, TILLNO,
         grain,oilsd,plses, veg, frt,CANOLA,OATS, BARLEY,SOYBNS,POTATS,vegNoPotat,corn,wheat, 
         SALES95, TOTEXP)%>%
  add_column (C_YEAR="1996")
#
#
# 2001
#
files01 <- list.files("rawData/2001", full.names=T, pattern="csv")
files01
#
# Read and join
tbl01 <- files01%>%
  map(read_csv)%>%
  map(~slice(.,-2))
#  
tbl01_1<-tbl01%>%
  reduce(left_join, by = c("PROV","CAR","CD","CCS","GEO NAMES"))
#  
nrow(tbl01_1)==nrow(tbl01[[1]])
#
names(tbl01_1) <- toupper(names(tbl01_1)) # all upper case same as the 
#
# Column names
colnm01<-
  tbl01_1[1,]%>%
  gather()
#
db01<- tbl01_1[-1,] %>%
  select(PROV,CAR,CD,CCS,`GEO NAMES`,
         TFAREA.X,NTFAREA_1, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI, FUNGIC, NFUNGIC,
         TILCONV, TILCONS, TILLNO, # tillage
         TOTVEG,OTHVEG, BUCWHT, SUGARB, POTATS, # vegetables
         TOTFRT,OTFRTTA, # total fruits, berries and nuts (producing and not producing) and other
         WHTSPG,WHTDUR,WHTWIN,OATS,BARLEY, MXDGRN,CORNGR,CORNSI, RYEFAL,RYESPG,CANARY, # grain. corn for sillage =CORNSI
         CANOLA,SOYBNS,FLAXSD,MUSTSD,SUNFLS, # oilseeds
         DFPEAS,LENTIL,WHITBN,CHICPEA, ODFBNS, # pulses - no data for fava beans
         SALESXFP, TOTEXP) # expenses
#
# To numeric - all 'x' to NA
#
db01[,6:50] <- sapply(db01[,6:50], readr::parse_number)
#
# All NA for crops to 0 - we need for summing them up
db01[,21:48][is.na(db01[,21:48])] <-0
#
db01_2<- db01%>%
  mutate(grain = WHTSPG + WHTDUR + WHTWIN + OATS + BARLEY + 
           MXDGRN + CORNGR + CORNSI + RYEFAL + RYESPG +CANARY,
         oilsd= CANOLA + SOYBNS + FLAXSD + MUSTSD + SUNFLS,
         plses = DFPEAS  + LENTIL + WHITBN +CHICPEA+ ODFBNS, 
         veg = TOTVEG + OTHVEG + BUCWHT + SUGARB + POTATS,
         frt = TOTFRT + OTFRTTA, # berries+ tree fruits + other
         vegNoPotat=TOTVEG + OTHVEG + BUCWHT + SUGARB,
         corn=CORNGR + CORNSI,
         wheat=WHTSPG + WHTDUR + WHTWIN,
         TFAREA=TFAREA.X, # renaming
         NTFAREA=NTFAREA_1,
         AREANAME=`GEO NAMES`,
         SALES95=SALESXFP)%>%
  select(PROV,CAR,CD,CCS,AREANAME,
         TFAREA,NTFAREA, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI, FUNGIC, NFUNGIC,
         TILCONV, TILCONS, TILLNO,
         grain,oilsd,plses, veg, frt,CANOLA,OATS, BARLEY,SOYBNS,POTATS,vegNoPotat,corn,wheat, 
         SALES95, TOTEXP)%>%
  add_column(C_YEAR="2001")%>%
  mutate(CD=as.character( # add 0 to make the GEO_CD all with 2 numbers and all characters
          str_pad(CD, width = 2, pad = "0")),
          CAR=as.character( # add 0 to make the GEO_CD all with 2 numbers and all characters
            str_pad(CAR, width = 2, pad = "0")),
          CCS=as.character( # add 0 to make the GEO_CD all with 3 numbers and all characters
            str_pad(CCS, width = 3, pad = "0")),
         AREAID = paste(PROV, CD, CAR ,CCS, sep = ""),# put together Codes
         SGC = paste(PROV, CD, CCS, sep = ""))
#
#
### 2006
#
files06 <- list.files("rawData/2006", full.names=T, pattern="csv")
files06
#
# Read and join
tbl06 <- files06%>%
  map(read_csv)%>%
  map(~slice(.,-2))
#
#
tbl06_1<-tbl06%>%
  reduce(left_join, by = c("HDPROV","AGOPCAR","CENSUSD","CCS","AREANAME"))
#  
nrow(tbl06_1)==nrow(tbl06[[1]])# ok
#
# Column names
colnm06<-
  tbl06_1[1,]%>%
  gather()
#
db06<- tbl06_1[-1,] %>%
  select(HDPROV,AGOPCAR,CENSUSD,CCS,AREANAME,
         TFAREA,NTFAREA, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI, FUNGIC, NFUNGIC,
         TILCONV, TILCONS, TILLNO, # tillage
         TOTVEG,OTHVEG, BUCWHT, SUGARB, POTATS, # vegetables
         TOTFRT,OTFRTTA, # total fruits, berries and nuts (producing and not producing) and other
         WHTSPG,WHTDUR,WHTWIN,OATS,BARLEY, MXDGRN,CORNGR,CORNSI, RYEFAL,RYESPG,CANARY, # grain. corn for sillage =CORNSI
         CANOLA,SOYBNS,FLAXSD,MUSTSD,SUNFLS, # oilseeds
         DFPEAS,LENTIL,WHITBN,CHICPEA, ODFBNS, # pulses - no data for fava beans
         VAL_SALESXFP, TOTEXP) # expenses
#
# To numeric
#
db06[,6:50] <- sapply(db06[,6:50], readr::parse_number)
#
# All NA for crops to 0 - we need for summing them up
db06[,21:48][is.na(db06[,21:48])] <-0
#
#
db06_2<- db06%>%
  mutate(grain = WHTSPG + WHTDUR + WHTWIN + OATS + BARLEY + 
           MXDGRN + CORNGR + CORNSI + RYEFAL + RYESPG +CANARY,
         oilsd= CANOLA + SOYBNS + FLAXSD + MUSTSD + SUNFLS,
         plses = DFPEAS  + LENTIL + WHITBN +CHICPEA+ ODFBNS, 
         veg = TOTVEG + OTHVEG + BUCWHT + SUGARB + POTATS,
         vegNoPotat=TOTVEG + OTHVEG + BUCWHT + SUGARB,
         corn=CORNGR + CORNSI,
         wheat=WHTSPG + WHTDUR + WHTWIN,
         frt = TOTFRT + OTFRTTA)%>%
  select(HDPROV,AGOPCAR,CENSUSD,CCS,AREANAME,
         TFAREA,NTFAREA, CRPLND, NCRPLND,
         FERTIL,NFERTIL, HERBCI, NHERBCI, INSECTI, NINSECTI, FUNGIC, NFUNGIC,
         TILCONV, TILCONS, TILLNO,
         grain,oilsd,plses, veg, frt,CANOLA,OATS, BARLEY,SOYBNS,POTATS,vegNoPotat,corn,wheat, 
         VAL_SALESXFP, TOTEXP)%>%
  rename(PROV=HDPROV, CAR=AGOPCAR, CD=CENSUSD,SALES95=VAL_SALESXFP)%>%
  add_column(C_YEAR="2006")%>%
  mutate(CD=as.character( # add 0 to make the GEO_CD all with 2 numbers and all characters
    str_pad(CD, width = 2, pad = "0")),
    CAR=as.character( # add 0 to make the GEO_CD all with 2 numbers and all characters
      str_pad(CAR, width = 2, pad = "0")),
    CCS=as.character( # add 0 to make the GEO_CD all with 3 numbers and all characters
      str_pad(CCS, width = 3, pad = "0")),
    AREAID = paste(PROV, CD, CAR ,CCS, sep = ""),# put together Codes
    SGC = paste(PROV, CD, CCS, sep = ""))
#
#
#####
#
# Data format for 2011 & 2016 is different
#
#
################# 2011 & 2016 
#
#
files1116 <- list.files("rawData/2011_16", full.names=T, pattern="csv")
files1116
#
# Read and join
tbl1116 <- files1116%>%
  map(read_csv)%>%
  map(~rename(.,param= 3))%>% # remname parameters
  reduce(rbind)%>%
  filter(!UOM=="Hectares")# remove hectars - only keep acres
#
colnm1116<-unique(tbl1116[3:4])
#
# Values to numbers
tbl1116[7] <- sapply(tbl1116[7], readr::parse_number)
#
# Function to gathers and spreads at the same time
# Source: https://community.rstudio.com/t/spread-with-multiple-value-columns/5378
#
myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}
#
# Tidying dataset
spr1116<-  tbl1116[-(5:6)] %>%
  filter(param %in% c("Total farm area" ,"Land in crops (excluding Christmas tree area)" ,"Tillage retaining most crop residue on the surface",
           "No-till seeding or zero-till seeding","Tillage incorporating most crop residue into soil","Herbicides","Insecticides",
           "Fungicides","Insecticides","Fungicides","Commercial fertilizer","Total wheat","Oats" ,"Barley","Mixed grains","Total corn" ,
           "Total rye","Canola (rapeseed)","Soybeans","Flaxseed","Dry field peas","Lentils","Chick peas","Dry white beans","Other dry beans",
           "Potatoes","Mustard seed","Sunflowers","Canary seed","Buckwheat","Sugar beets",
           "Total area of fruits, berries and nuts (producing and non-producing)",
           "Other fruits, berries and nuts total area","Total vegetables (excluding greenhouse vegetables)","Other vegetables",
            "Total gross farm receipts (excluding forest products sold) (dollars)","Total farm business operating expenses"))%>% # selec tall parameters
  spread(param,Value) # from long wide to sum them
#
#
# All NA for crops to 0 - we need for summing them up
spr1116[,c(4:8,10:12,17:19,21:28,31:32,36:38)][is.na(spr1116[,c(4:8,10:12,17:19,21:28,31:32,36:38)])] <-0
#
#
cln1116<-spr1116%>%
  mutate(grain=`Total wheat`+Oats+Barley+`Mixed grains`+ `Total corn`+ `Total rye`+ `Canary seed`,
         oilsd= `Canola (rapeseed)`+ Soybeans+ Flaxseed+ `Mustard seed` + Sunflowers,
         plses= `Dry field peas`+ Lentils+ `Chick peas`+`Dry white beans`+`Other dry beans`,
         veg= `Total vegetables (excluding greenhouse vegetables)`+`Other vegetables` +Buckwheat+ `Sugar beets`+Potatoes,
         vegNoPotat= `Total vegetables (excluding greenhouse vegetables)`+`Other vegetables` +Buckwheat+ `Sugar beets`,
         frt= `Total area of fruits, berries and nuts (producing and non-producing)`+ `Other fruits, berries and nuts total area`
           )%>% # add up crops
  rename(TFAREA= "Total farm area",
         CRPLND= "Land in crops (excluding Christmas tree area)",
         corn= `Total corn`,
         wheat=`Total wheat`,
         CANOLA=`Canola (rapeseed)`,
         OATS=`Oats`,
         BARLEY=`Barley`,
         SOYBNS=`Soybeans`,
         POTATS=`Potatoes`,
         INSECTI= "Insecticides",
         HERBCI= "Herbicides",
         FUNGIC= "Fungicides",
         FERTIL= "Commercial fertilizer",
         TILCONV= "Tillage incorporating most crop residue into soil",
         TILCONS= "Tillage retaining most crop residue on the surface",
         TILLNO= "No-till seeding or zero-till seeding",
         SALES95= "Total gross farm receipts (excluding forest products sold) (dollars)",
         TOTEXP= "Total farm business operating expenses",
         AREANAME= "GEO",
         C_YEAR= "Ref_Date")%>% # rename
  select(C_YEAR,AREANAME,UOM,TFAREA,CRPLND,INSECTI,HERBCI,FUNGIC,FERTIL,TILCONV,TILCONS,TILLNO,SALES95,TOTEXP, grain,oilsd,plses,veg,frt,
         CANOLA,OATS, BARLEY,SOYBNS,POTATS,vegNoPotat,corn,wheat)%>% # parameters interested
  myspread(UOM,4:27)%>% # spread with multiple columns as value
  select("C_YEAR","AREANAME","Acres_BARLEY","Acres_CANOLA","Acres_corn","Acres_CRPLND","Acres_FERTIL","Acres_frt",                        
         "Acres_FUNGIC","Acres_grain","Acres_HERBCI","Acres_INSECTI","Acres_OATS","Acres_oilsd","Acres_plses","Acres_POTATS",                     
         "Acres_SOYBNS","Acres_TFAREA","Acres_TILCONS","Acres_TILCONV","Acres_TILLNO","Acres_veg","Acres_vegNoPotat","Acres_wheat",                      
         "Dollars_SALES95","Dollars_TOTEXP","Number of farms reporting_CRPLND","Number of farms reporting_FERTIL","Number of farms reporting_FUNGIC",
         "Number of farms reporting_HERBCI","Number of farms reporting_INSECTI","Number of farms reporting_TFAREA")%>% 
  rename_at(vars(contains('Number of farms reporting_')), funs(sub('Number of farms reporting_', 'N', .)))%>% # rename parts of the colname 
  rename_at(vars(contains('Dollars_')), funs(sub('Dollars_', '', .)))%>%
  rename_at(vars(contains('Acres_')), funs(sub('Acres_', '', .)))%>%
  mutate(geo2=as.character(lapply(strsplit(as.character(AREANAME), split=" "),
                             tail, n=1)),
         geo2=sub("\\[|\\]", "", geo2),
         geo2=sub("\\]|\\]", "", geo2), # remove names and brackets from column
         type=substr(geo2, 1L, regexpr("[a-zA-Z][0-9]", geo2)), 
         CCSUID=substr(geo2, regexpr("[a-zA-Z][0-9]", geo2) + 1L, 1000L)) %>% # split names from type from number in two different cols
  separate(CCSUID, into = c('PROV', 'other'), sep = 2)%>%
  separate(other, into = c('CAR', 'other2'), sep = 2)%>%
  separate(other2, into = c('CD', 'CCS'), sep = 2)%>% # split them into different columns
  mutate(AREAID = paste(PROV, CD, CAR ,CCS, sep = ""),# put together Codes
         SGC = paste(PROV, CD, CCS, sep = ""))%>%
  select(-geo2,-type) # last select 
#
#
# Join tables together: 
# 542040 final entries from 80 raw datasets with a total of 17,782,907
#
cens<-as.data.frame(rbind(db81_2,db86_2,db91_2,db96_2,db01_2,db06_2,cln1116))
#
# Save it 
# save(cens, file="RData/censdat.RData")
#
# ------------------------------------------------------------------------------------------------------
#
# End
#
# ------------------------------------------------------------------------------------------------------

        
