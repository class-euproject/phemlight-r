
library(readr)
library(plyr)
library(dplyr)
library(data.table)


setwd("/esarchive/scratch/drodrig1/experiments/camp_nou_Jaime/fleet_compo/")
output <- "/esarchive/scratch/drodrig1/experiments/camp_nou_Jaime/"
agg_vehicle_categories <- function(names_to_look_H3, euro_category, names_to_add_PL) {
  bus_standart_H3 <- compo_full[grepl(names_to_look_H3, compo_full$Copert_V_name),]
  bus_standart_H3 <- bus_standart_H3[grepl(euro_category, bus_standart_H3$Copert_V_name),]
  compo_full[grepl(names_to_add_PL, compo_full$PHEMlight),8] <- sum(bus_standart_H3$share, na.rm = T)
  return(compo_full)
}

mapping <- read.csv("mapping_PHEM_HERMESv3.csv", stringsAsFactors = F)
copertVnames<- read.csv("COPERT_V_code_names.csv", stringsAsFactors = F)
fleet <- read.csv("compo_upperdiagonal_Z2_2015.csv", stringsAsFactors = F)
names(fleet)[1]<- "code_hermesV3"

fleet<- left_join(fleet, copertVnames, by = c("code_hermesV3" = "HERMESv3.0"))

compo_full <- full_join(fleet, mapping, by = c("code_hermesV3" = "HERMESv3.0_code"))
compo_full$PL_share <- NA

# Assaign the HERMES composition to the already matched PHEMLight categories
for (i in (1:nrow(compo_full))){
  if (is.na(compo_full$share[i]) == F & is.na(compo_full$PHEMlight[i]) == F)  {
    compo_full$PL_share[i] <- compo_full$share[i]
  }
}

### Aggregation of vehicle categories from HERMESv3 to PHEMLight ###

## BUS ##
compo_full <- agg_vehicle_categories("CNG_Urban CNG Buses", "Euro VI", "HDV_CB_CNG_EU6$")
#CNG from COPERTV had much higher emissions than CNG PL, closest vehicle is CB Eu 5.
# compo_full <- agg_vehicle_categories("Buses_CNG_Urban CNG Buses","EEV", "HDV_CB_D_EU5") 


# euros<- c("Conventional$", "Euro I$", "Euro II$", "Euro III$", "Euro IV$", "Euro V$", "Euro VI$")
# cats<- c("HDV_CB_D_EU0$", "HDV_CB_D_EU1$", "HDV_CB_D_EU2$", "HDV_CB_D_EU3$", "HDV_CB_D_EU4$", "HDV_CB_D_EU5$", "HDV_CB_D_EU6$")
# x<- data.frame(euros, cats)
# 
# for (i in (1:nrow(x))) {
#   agg_vehicle_categories("Urban Buses Standard|Coaches Standard|Urban Buses Midi", x[i,1], x[i, 2] )
# }

compo_full<- agg_vehicle_categories("Urban Buses Standard|Coaches Standard|Urban Buses Midi", "Conventional$", "HDV_CB_D_EU0$")
compo_full<- agg_vehicle_categories("Urban Buses Standard|Coaches Standard|Urban Buses Midi", "Euro I$", "HDV_CB_D_EU1$")
compo_full<- agg_vehicle_categories("Urban Buses Standard|Coaches Standard|Urban Buses Midi", "Euro II$", "HDV_CB_D_EU2$")
compo_full<- agg_vehicle_categories("Urban Buses Standard|Coaches Standard|Urban Buses Midi", "Euro III$", "HDV_CB_D_EU3$")
compo_full<- agg_vehicle_categories("Urban Buses Standard|Coaches Standard|Urban Buses Midi", "Euro IV$", "HDV_CB_D_EU4$")
compo_full<- agg_vehicle_categories("Urban Buses Standard|Coaches Standard|Urban Buses Midi|Buses_CNG_Urban CNG Buses_EEV", "Euro V$|EEV$", "HDV_CB_D_EU5$")
compo_full<- agg_vehicle_categories("Urban Buses Standard|Coaches Standard|Urban Buses Midi", "Euro VI$", "HDV_CB_D_EU6$")

# Para el BUS hibrido que no tiene nombre en COPERT V, caso especial. El BH1 equivale a un CB_Eu5, y el BH2 al Hibrido de PL 
compo_full[grepl("HDV_CB_D_HEV_EU6", compo_full$PHEMlight),8] <- compo_full[grepl("BH_2", compo_full$code_hermesV3),3]
compo_full[grepl("HDV_CB_D_EU5", compo_full$PHEMlight),8] <- compo_full[grepl("HDV_CB_D_EU5", compo_full$PHEMlight),8] + compo_full[grepl("BH_1", compo_full$code_hermesV3),3]

compo_full<- agg_vehicle_categories("Coaches Articulated|Buses Articulated", "Conventional$", "HDV_CO_D_EU0$")
compo_full<- agg_vehicle_categories("Coaches Articulated|Buses Articulated", "Euro I$", "HDV_CO_D_EU1$")
compo_full<- agg_vehicle_categories("Coaches Articulated|Buses Articulated", "Euro II$", "HDV_CO_D_EU2$")
compo_full<- agg_vehicle_categories("Coaches Articulated|Buses Articulated", "Euro III$", "HDV_CO_D_EU3$")
compo_full<- agg_vehicle_categories("Coaches Articulated|Buses Articulated", "Euro IV$", "HDV_CO_D_EU4$")
compo_full<- agg_vehicle_categories("Coaches Articulated|Buses Articulated", "Euro V$", "HDV_CO_D_EU5$")
compo_full<- agg_vehicle_categories("Coaches Articulated|Buses Articulated", "Euro VI$", "HDV_CO_D_EU6$")

## HDV ##
compo_full<- agg_vehicle_categories("Diesel_Rigid <=7,5|Diesel_Rigid 7,5 - 12|Diesel_Rigid 12 - 14", "Conventional$", "HDV_RT_I_D_EU0$")
compo_full<- agg_vehicle_categories("Diesel_Rigid <=7,5|Diesel_Rigid 7,5 - 12|Diesel_Rigid 12 - 14", "Euro I$", "HDV_RT_I_D_EU1$")
compo_full<- agg_vehicle_categories("Diesel_Rigid <=7,5|Diesel_Rigid 7,5 - 12|Diesel_Rigid 12 - 14", "Euro II$", "HDV_RT_I_D_EU2$")
compo_full<- agg_vehicle_categories("Diesel_Rigid <=7,5|Diesel_Rigid 7,5 - 12|Diesel_Rigid 12 - 14", "Euro III$", "HDV_RT_I_D_EU3$")
compo_full<- agg_vehicle_categories("Diesel_Rigid <=7,5|Diesel_Rigid 7,5 - 12|Diesel_Rigid 12 - 14", "Euro IV$", "HDV_RT_I_D_EU4$")
compo_full<- agg_vehicle_categories("Diesel_Rigid <=7,5|Diesel_Rigid 7,5 - 12|Diesel_Rigid 12 - 14", "Euro V$", "HDV_RT_I_D_EU5$")
compo_full<- agg_vehicle_categories("Diesel_Rigid <=7,5|Diesel_Rigid 7,5 - 12|Diesel_Rigid 12 - 14", "Euro VI$", "HDV_RT_I_D_EU6$")

compo_full<- agg_vehicle_categories("Diesel_Rigid 14 - 20|Diesel_Rigid 20 - 26|Diesel_Rigid 26 - 28|Diesel_Rigid 28 - 32|Diesel_Rigid >32", "Conventional$", "HDV_RT_II_D_EU0$")
compo_full<- agg_vehicle_categories("Diesel_Rigid 14 - 20|Diesel_Rigid 20 - 26|Diesel_Rigid 26 - 28|Diesel_Rigid 28 - 32|Diesel_Rigid >32", "Euro I$", "HDV_RT_II_D_EU1$")
compo_full<- agg_vehicle_categories("Diesel_Rigid 14 - 20|Diesel_Rigid 20 - 26|Diesel_Rigid 26 - 28|Diesel_Rigid 28 - 32|Diesel_Rigid >32", "Euro II$", "HDV_RT_II_D_EU2$")
compo_full<- agg_vehicle_categories("Diesel_Rigid 14 - 20|Diesel_Rigid 20 - 26|Diesel_Rigid 26 - 28|Diesel_Rigid 28 - 32|Diesel_Rigid >32", "Euro III$", "HDV_RT_II_D_EU3$")
compo_full<- agg_vehicle_categories("Diesel_Rigid 14 - 20|Diesel_Rigid 20 - 26|Diesel_Rigid 26 - 28|Diesel_Rigid 28 - 32|Diesel_Rigid >32", "Euro IV$", "HDV_RT_II_D_EU4$")
compo_full<- agg_vehicle_categories("Diesel_Rigid 14 - 20|Diesel_Rigid 20 - 26|Diesel_Rigid 26 - 28|Diesel_Rigid 28 - 32|Diesel_Rigid >32", "Euro V$", "HDV_RT_II_D_EU5$")
compo_full<- agg_vehicle_categories("Diesel_Rigid 14 - 20|Diesel_Rigid 20 - 26|Diesel_Rigid 26 - 28|Diesel_Rigid 28 - 32|Diesel_Rigid >32", "Euro VI$", "HDV_RT_II_D_EU6$")

compo_full<- agg_vehicle_categories("Trucks_Diesel_Articulated", "Conventional$", "HDV_TT_D_EU0$")
compo_full<- agg_vehicle_categories("Trucks_Diesel_Articulated", "Euro I$", "HDV_TT_D_EU1$")
compo_full<- agg_vehicle_categories("Trucks_Diesel_Articulated", "Euro II$", "HDV_TT_D_EU2$")
compo_full<- agg_vehicle_categories("Trucks_Diesel_Articulated", "Euro III$", "HDV_TT_D_EU3$")
compo_full<- agg_vehicle_categories("Trucks_Diesel_Articulated", "Euro IV$", "HDV_TT_D_EU4$")
compo_full<- agg_vehicle_categories("Trucks_Diesel_Articulated", "Euro V$", "HDV_TT_D_EU5$")
compo_full<- agg_vehicle_categories("Trucks_Diesel_Articulated", "Euro VI$", "HDV_TT_D_EU6$")


## PC and LDV ##
compo_full<- agg_vehicle_categories("Petrol_Small", "Euro 6", "LCV_I_G_EU6c$")

compo_full<- agg_vehicle_categories("Diesel_Large-SUV-Executive|Diesel_Medium", "Conventional$", "PC_D_EU0$")
compo_full<- agg_vehicle_categories("Diesel_Large-SUV-Executive|Diesel_Medium", "Euro 1$", "PC_D_EU1$")
compo_full<- agg_vehicle_categories("Diesel_Large-SUV-Executive|Diesel_Medium", "Euro 2$", "PC_D_EU2$")
compo_full<- agg_vehicle_categories("Diesel_Large-SUV-Executive|Diesel_Medium", "Euro 3$", "PC_D_EU3$")
compo_full<- agg_vehicle_categories("Diesel_Large-SUV-Executive|Diesel_Medium", "Euro 4$", "PC_D_EU4$")
compo_full<- agg_vehicle_categories("Diesel_Large-SUV-Executive|Diesel_Medium", "Euro 5$", "PC_D_EU5$")
compo_full<- agg_vehicle_categories("Diesel_Large-SUV-Executive|Diesel_Medium", "Euro 6", "PC_D_EU6$")

compo_full<- agg_vehicle_categories("Petrol_Large-SUV-Executive|Petrol_Medium", "Conventional$", "PC_D_EU0$")
compo_full<- agg_vehicle_categories("Petrol_Large-SUV-Executive|Petrol_Medium", "Euro 1$", "PC_G_EU1$")
compo_full<- agg_vehicle_categories("Petrol_Large-SUV-Executive|Petrol_Medium", "Euro 2$", "PC_G_EU2$")
compo_full<- agg_vehicle_categories("Petrol_Large-SUV-Executive|Petrol_Medium", "Euro 3$", "PC_G_EU3$")
compo_full<- agg_vehicle_categories("Petrol_Large-SUV-Executive|Petrol_Medium", "Euro 4$", "PC_G_EU4$")
compo_full<- agg_vehicle_categories("Petrol_Large-SUV-Executive|Petrol_Medium", "Euro 5$", "PC_G_EU5$")
compo_full<- agg_vehicle_categories("Petrol_Large-SUV-Executive|Petrol_Medium", "Euro 6", "PC_G_EU6$")

# It is necessary to change the normalisation of the different groups, since they differ:
## HERMES             # PHEMLight
# BUS + HDV = 1         BUS = 1
# PC + LDV = 1          HDV = 1
# Mopeds = 1            PC + LDV = 1
# Motos = 1             Motos (todas) = 1

#Hence
xBus<- sum(select(filter(compo_full, group == "BUS"), PL_share), na.rm = T) 
xHDV<- sum(select(filter(compo_full, group == "HDV"), PL_share), na.rm = T)

want<- which(compo_full$group == "BUS")
compo_full$PL_share[want] <- compo_full$PL_share[want] / xBus

want<- which(compo_full$group == "HDV")
compo_full$PL_share[want] <- compo_full$PL_share[want] / xHDV

# Give the PHEMLight shape
header_fleetshare <- rbind(c("c Fleet composition for link of PHEMLight with NITRA", "", ""), 
           c("c", "", ""), 
           c("c fleet segment",	"vehicle segment",	"share"))

fleetshare <- select(compo_full, group, PHEMlight, PL_share)
fleetshare <- na.omit(fleetshare)

# Save
fwrite(data.frame(header_fleetshare), file = paste0(output, "fleetshare_Z2_new.csv"), append= T, col.names = F, row.names = F, quote = F)
fwrite(fleetshare, file = paste0(output, "fleetshare_Z2_new.csv"), append = T, col.names = F, row.names = F, quote = F)

# -------------------------
# test
# sum(select(filter(compo_full, group %in% c("HDV", "BUS")), PL_share), na.rm = T)
# sum(select(filter(compo_full, group == "HDV"), PL_share), na.rm = T)
# sum(select(filter(compo_full, group == "BUS"), PL_share), na.rm = T)
# sum(select(filter(compo_full, group == "PC"), PL_share), na.rm = T)
# sum(select(filter(compo_full, Class == "light_veh"), share), na.rm = T)
