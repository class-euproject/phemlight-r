#!/usr/bin/env Rscript
######################################################################################################
#                                    PHEMlight: Advanced Mode 5
######################################################################################################
rm(list=ls())
print("PHEMLIGHT!")

suppressWarnings(library(readr))
suppressWarnings(library(sf))
suppressWarnings(library(plyr))
suppressWarnings(library(dplyr))
suppressWarnings(library(data.table))
suppressWarnings(library(units))
suppressWarnings(library(tidyr))
suppressWarnings(library(PHEMLight))

args <- commandArgs(trailingOnly=T)
# if (length(args) != 3) {
#   quit(save="no", status=1)
# }
input_path <- args[1]
drive_cycle_data_path <- args[2]
output_file <- args[3]

# Configuration Data ------------------------------------------------------
general_path <- "/home/esabate/pollutionMap/phemlight-r/"
input_path <- paste0(general_path, "in/")
drive_cycle_data_path <- paste0(input_path, args[2])
# file_roads <- "shapefile of the road network, if emissions need to be georeferenciated"
#drive_cycle_data_path <- paste0(general_path, "in/pollution_c694aee0f606.csv")
#print(drive_cycle_data_path)
drive_cycle_data <- drive_cycle_data_path
output_path <- paste0(general_path, "out/")
# file_resuspension_ef <- "resuspension emission factors" # <- You will not use it
path_vehicles <- paste0(input_path, "V4_no_elect/")
fleetShare_data <- paste0(input_path, "fleetshare.csv")

# Aggregated False : Individual vehicle emissions calculation: g/veh/h
# Aggregated True: Vehicle emissions aggregated per vehicle group and time-step: g/h
aggregated <- F
non_exhaust <- F
intervals <- T
interval_time <- 3600 # seconds
ta = 10 #Temp. Ambiente
ltrip <- 6.47 # Average distance in Barcelona [km] (obs. mobilitat 2013)

#--
tunning_idle <- 1.1 # No tocar (!!)
tunning_critP <- 1.15 # No tocar (!!)
tunning_VSP <-0.9 # No tocar (!!)
# End configuration data #

# setwd(input_path)


############### ============== EMISSION FACTORS ============== ###############

# roads <- sf::read_sf(file_roads)
# roads <- subset(roads, select = c("id", "name"))


## Lists of files needed ##
files_emissions <- list.files(path_vehicles)
files_emissions <- files_emissions[!grepl("*FC.csv|*.veh|^Fleet", files_emissions)]
header<- def_header(paste0(path_vehicles, files_emissions[1]))
tot_emissions_EF <- ldply(files_emissions, read_csv_emissions)

## Drive cyle data ##
drive_cycles_ss <- read_csv(drive_cycle_data)
drive_cycles_ss$Av_link_speed <- drive_cycles_ss$Av_link_speed/3.6 # To m/s
drive_cycles_ss$Vehicle_type<- as.factor(drive_cycles_ss$Vehicle_type)
drive_cycles_ss <- arrange(drive_cycles_ss, LinkID, Vehicle_type)
drive_cycles_ss <- mutate(drive_cycles_ss, acc = ifelse(lead(Time) - Time != 1, 0, ((lead(Av_link_speed) - Av_link_speed)/(lead(Time) - Time)))) # Acceleration, when time gap between vehicles, acc = 0

## Fleetshares ##
fleetshare<- read.csv(fleetShare_data, skip = 2, col.names = c("veh_group", "veh_category", "share"))
fleetshare$veh_category<- as.character(fleetshare$veh_category)

## Vehicle Data ##
files_vehicles <- list.files(path_vehicles)
files_vehicles <- files_vehicles[grepl("*.veh", files_vehicles)]
tot_vehicles_data <- llply(files_vehicles, read_veh_data)
names(tot_vehicles_data)<- c(files_vehicles)

## Prated/Pcritical
veh_critial_power<- ldply(files_emissions, read_critical_power)
veh_critial_power$source <- sub(".csv", "", veh_critial_power$source)
criticP_share<- left_join(veh_critial_power, fleetshare, by = c("source" = "veh_category"))
criticP_share$avg_crit_P <- criticP_share$crit_P*criticP_share$share
avg_critic_P<- ddply(criticP_share, ~veh_group, summarise, sum_avg_critP = sum(avg_crit_P))
avg_critic_P[c(1,2), 2] <- avg_critic_P[c(1,2), 2]*tunning_critP # Desfase de Pcrit respecto a la original


## Idle
idle <- ldply(files_emissions, read_idle)
idle <- calc_idles(idle, fleetshare)
idle[1,2:7] <- idle[1,2:7]*tunning_idle #Desfase de idle respecto al original

## Mileage correction factor (aging)
mcor_nox<- mileage_factor_calc(paste0(input_path, "aging/mcorr_nox.csv"))
mcor_co<- mileage_factor_calc(paste0(input_path, "aging/mcorr_co.csv"))
mcor_vox<- mileage_factor_calc(paste0(input_path, "aging/mcorr_voc.csv"))

## Power and pollutant product with fleet shares ###
tot_emissions_EF$Source <- sub(".csv", "", tot_emissions_EF$Source)
tot_emissions_EF <- left_join(tot_emissions_EF, fleetshare, by = c("Source" = "veh_category"))
tot_emissions_EF <- transform(tot_emissions_EF,
                             NOx = NOx*share,
                             HC = HC*share,
                             CO = CO*share,
                             PM = PM*share,
                             PN = PN*share,
                             NO = NO*share)

## PC, Bus and HDV .veh lists
PC_phemlight <- create_PC_list()
BUS_phemlight <- create_bus_list()
HDV_phemlight <- create_HDV_list()

## Average Real Power - Pollutant per vehicle group
P_PC <- ddply(filter(tot_emissions_EF, veh_group == "PC"), ~cp_norm.rated., summarise,
              NOx_normalised = sum(NOx),
              HC_normalised = sum(HC),
              CO_normalised = sum(CO),
              PM_normalised = sum(PM),
              PN_normalised = sum(PN),
              NO_normalised = sum(NO))
P_BUS <- ddply(filter(tot_emissions_EF, veh_group == "BUS"), ~cp_norm.rated., summarise,
               NOx_normalised = sum(NOx),
               HC_normalised = sum(HC),
               CO_normalised = sum(CO),
               PM_normalised = sum(PM),
               PN_normalised = sum(PN),
               NO_normalised = sum(NO))
P_HDV <- ddply(filter(tot_emissions_EF, veh_group == "HDV"), ~cp_norm.rated., summarise,
               NOx_normalised = sum(NOx),
               HC_normalised = sum(HC),
               CO_normalised = sum(CO),
               PM_normalised = sum(PM),
               PN_normalised = sum(PN),
               NO_normalised = sum(NO))

# Desnormalizamos pollutants EF para Bus y HDV (que esta normalizado)
P_BUS[,2:7] <- P_BUS[,2:7]*avg_critic_P[1,2]
P_HDV[,2:7] <- P_HDV[,2:7]*avg_critic_P[2,2]


############### ============== VSP FROM DRIVE CYCLES (normalised) ============== ###############

# Lectura de todos los RMF
rot_mass_factors<- ldply(files_vehicles, data_rot_mass_factor)
names(rot_mass_factors)<- c("speed_m/s", "gear_ratio", "rot_mass_factor", "source")
rot_mass_factors$`speed_m/s`<- rot_mass_factors$`speed_m/s`/3.6

# Rot Mass factor optimisation
#1
rot_mass_factors1<- rot_mass_factors
rot_mass_factors1$source <- sub(".PHEMLight.veh", "", rot_mass_factors1$source)
rot_mass_factors1 <- left_join(rot_mass_factors1, fleetshare, by = c("source" = "veh_category"))
rot_mass_factors1 <- as.data.table(rot_mass_factors1)
max_rot_share<- rot_mass_factors1[rot_mass_factors1[, .I[which.max(share)], by=veh_group]$V1]
max_car<- as.character(filter(max_rot_share, veh_group == "PC") %>% select(source))
max_bus<- as.character(filter(max_rot_share, veh_group == "BUS") %>% select(source))
max_hdv<- as.character(filter(max_rot_share, veh_group == "HDV") %>% select(source))
#2
rot_mass_factors$source <- sub(".PHEMLight.veh", "", rot_mass_factors$source)
rot_mass_factors <- left_join(rot_mass_factors, fleetshare, by = c("source" = "veh_category"))
rot_mass_factors <- ddply(rot_mass_factors, ~source, summarise, avg_rot_mass = mean(rot_mass_factor), share = unique(share), veh_group = unique(veh_group))
rot_mass_factors$share.rotmass<- rot_mass_factors$avg_rot_mass*rot_mass_factors$share
rot_mass_factors <- ddply(rot_mass_factors, ~veh_group, summarise, avg_rot_m = sum(share.rotmass))


# Calculo de la media de las variables específicas de vehículos
sum_veh_specific_var<- calc_sum_specific_var(tot_vehicles_data, fleetshare)


# Calculo VSPs
drive_cycles_ss$Time <- as.integer(drive_cycles_ss$Time/1000)
drive_cycles_ss_dt_car <- data.table(filter(drive_cycles_ss, Vehicle_type == "Car"))
drive_cycles_ss_dt_car[,rot_mass_f:= (calc_rot_mass_factor(max_car, Av_link_speed))]
drive_cycles_ss_dt_car[, avg_VSP:= (calc_power_PC(Av_link_speed, acc, rot_mass_f))]
drive_cycles_ss_dt_car[, rot_mass_f:= NULL]

drive_cycles_ss_dt_bus <- data.table(filter(drive_cycles_ss, Vehicle_type == "Bus"))
drive_cycles_ss_dt_bus[, dec_coast:= (calc_Decel_coast_BUS(Av_link_speed))] # Al bus se le añade el decel_coast
drive_cycles_ss_dt_bus[, avg_VSP:= (calc_power_BUS(Av_link_speed, acc))]
drive_cycles_ss_dt_bus$avg_VSP<- drive_cycles_ss_dt_bus$avg_VSP * tunning_VSP # Desfase respecto a VSP original


drive_cycles_ss_dt_hdv <- data.table(filter(drive_cycles_ss, Vehicle_type == "HDV"))
drive_cycles_ss_dt_hdv[, avg_VSP:= (calc_power_HDV(Av_link_speed, acc))]
drive_cycles_ss_dt_hdv$avg_VSP <- drive_cycles_ss_dt_hdv$avg_VSP * tunning_VSP # Desfase respecto a VSP original

# Normalizar VSP drive cycles
drive_cycles_ss_dt_car[,norm_VSP:= (avg_VSP/avg_critic_P[3,2])]
drive_cycles_ss_dt_bus[,norm_VSP:= (avg_VSP/avg_critic_P[1,2])]
drive_cycles_ss_dt_hdv[,norm_VSP:= (avg_VSP/avg_critic_P[2,2])]

## Final interpolation
interpolated_value_PC <- drive_cycles_ss_dt_car[, c("NOx", "HC", "CO", "PM", "PN", "NO"):= (approx_vsp(drive_cycles_ss_dt_car, "Car"))]
interpolated_value_Bus <- drive_cycles_ss_dt_bus[, c("NOx", "HC", "CO", "PM", "PN", "NO"):= (approx_vsp(drive_cycles_ss_dt_bus, "Bus"))]
interpolated_value_HDV <- drive_cycles_ss_dt_hdv[, c("NOx", "HC", "CO", "PM", "PN", "NO"):= (approx_vsp(drive_cycles_ss_dt_hdv, "HDV"))]


# Coasting and idle corrections
want = which(interpolated_value_PC$Av_link_speed < 0.5)
  interpolated_value_PC$NOx[want] = idle[3,2]
  interpolated_value_PC$HC[want] = idle[3,3]
  interpolated_value_PC$CO[want] = idle[3,4]
  interpolated_value_PC$PM[want] = idle[3,5]
  interpolated_value_PC$PN[want] = idle[3,6]
  interpolated_value_PC$NO[want] = idle[3,7]

want = which(interpolated_value_PC$acc < -0.28)
  interpolated_value_PC[want, 9:14] = 0


want = which(interpolated_value_HDV$Av_link_speed < 0.5)
  interpolated_value_HDV$NOx[want] = idle[2,2]
  interpolated_value_HDV$HC[want] = idle[2,3]
  interpolated_value_HDV$CO[want] = idle[2,4]
  interpolated_value_HDV$PM[want] = idle[2,5]
  interpolated_value_HDV$PN[want] = idle[2,6]
  interpolated_value_HDV$NO[want] = idle[2,7]

want = which(interpolated_value_HDV$acc < -0.28)
  interpolated_value_HDV[want, 9:14] = 0


want = which(interpolated_value_Bus$Av_link_speed < 0.5)
  interpolated_value_Bus$NOx[want] = idle[1,2]
  interpolated_value_Bus$HC[want] = idle[1,3]
  interpolated_value_Bus$CO[want] = idle[1,4]
  interpolated_value_Bus$PM[want] = idle[1,5]
  interpolated_value_Bus$PN[want] = idle[1,6]
  interpolated_value_Bus$NO[want] = idle[1,7]

want = which(interpolated_value_Bus$Av_link_speed > 2.78 & interpolated_value_Bus$acc < interpolated_value_Bus$dec_coast)
  interpolated_value_Bus[want, 10:15] = 0

# Mileage factor corrections

want = which(interpolated_value_PC$Av_link_speed <= 5.277778) # 19km/h in m/s
  interpolated_value_PC[want, NOx:= (NOx*mcor_nox[1,3])]

want = which(interpolated_value_PC$Av_link_speed >= 17.5) # 63km/h in m/s
  interpolated_value_PC[want, NOx:= (NOx*mcor_nox[1,5])]

want = which(interpolated_value_PC$Av_link_speed < 17.5 & interpolated_value_PC$Av_link_speed > 5.277778)
  interpolated_value_PC[want, NOx:= (NOx*(mcor_nox[1,3] + (((Av_link_speed-19)*(mcor_nox[1,5]-mcor_nox[1,3]))/44)))]


# If aggregated = False -> Each vehicle of the link it is considered. Passing from g/veh/h => g/veh/s
# If aggregated = True -> Vehicles speed is averaged per time-step, then each time-step has to be multiplied per flow.
#   This should be done when aggregated aimsun results per time-step. g/link/s
interpolated_value_PC[,9:14]
interpolated_value_PC$Flow
if(aggregated) {
  interpolated_value_PC[,9:14] <- interpolated_value_PC[,9:14]*interpolated_value_PC$Flow/3600
  interpolated_value_Bus[,10:15] <- interpolated_value_Bus[,10:15]*interpolated_value_Bus$Flow/3600
  interpolated_value_HDV[,9:14] <- interpolated_value_HDV[,9:14]*interpolated_value_HDV$Flow/3600
  
  message1 <- "Vehicle emissions aggregated per vehicle group and time-step: g/link/s"
  
}  else {

  interpolated_value_PC[,9:14] <- interpolated_value_PC[,9:14]/3600
  interpolated_value_Bus[,10:15] <- interpolated_value_Bus[,10:15]/3600
  interpolated_value_HDV[,9:14] <- interpolated_value_HDV[,9:14]/3600

  # interpolated_value_PC<- filter(interpolated_value_PC, Time > 32400 & Time < 46800)
  # interpolated_value_Bus<- filter(interpolated_value_Bus, Time > 32400 & Time < 46800)
  # interpolated_value_HDV<- filter(interpolated_value_HDV, Time > 32400 & Time < 46800)
  
  message1 <- "Individual vehicle emissions calculation: g/veh/s"
}
byCol = c("LinkID")

if (intervals) {
    interpolated_value_PC$int <- as.integer(interpolated_value_PC$Time/interval_time)
    interpolated_value_HDV$int <- as.integer(interpolated_value_HDV$Time/interval_time)
    interpolated_value_Bus$int <- as.integer(interpolated_value_Bus$Time/interval_time)
    
    interpolated_value_PC$Hr <- as.integer(interpolated_value_PC$Time/3600.1)
    interpolated_value_HDV$Hr <- as.integer(interpolated_value_HDV$Time/3600.1)
    interpolated_value_Bus$Hr <- as.integer(interpolated_value_Bus$Time/3600.1)
    options(dplyr.summarise.inform = FALSE)
    interpolated_value_PC_splited <- interpolated_value_PC %>%
      group_by_at(c(byCol, 'Hr', 'int')) %>%
      summarise(link_speed_av = mean(Av_link_speed),
                NOx = sum(NOx, na.rm = T)*3600/interval_time, # This is to have the emissions of that interval for the whole hour
                HC = sum(HC, na.rm = T)*3600/interval_time,
                CO = sum(CO, na.rm = T)*3600/interval_time,
                PM = sum(PM, na.rm = T)*3600/interval_time,
                PN = sum(PN, na.rm = T)*3600/interval_time,
                NO = sum(NO, na.rm = T)*3600/interval_time)
    interpolated_value_PC_splited$veh_group<- "car"
    interpolated_value_HDV_splited <- interpolated_value_HDV %>%
      group_by_at(c(byCol, 'int', 'Hr')) %>%
      summarise(link_speed_av = mean(Av_link_speed),
                NOx = sum(NOx, na.rm = T)*3600/interval_time,
                HC = sum(HC, na.rm = T)*3600/interval_time,
                CO = sum(CO, na.rm = T)*3600/interval_time,
                PM = sum(PM, na.rm = T)*3600/interval_time,
                PN = sum(PN, na.rm = T)*3600/interval_time,
                NO = sum(NO, na.rm = T)*3600/interval_time)
    interpolated_value_HDV_splited$veh_group<- "hdv"
    interpolated_value_Bus_splited <- interpolated_value_Bus %>%
      group_by_at(c(byCol, 'int', 'Hr')) %>%
      summarise(link_speed_av = mean(Av_link_speed),
                NOx = sum(NOx, na.rm = T)*3600/interval_time,
                HC = sum(HC, na.rm = T)*3600/interval_time,
                CO = sum(CO, na.rm = T)*3600/interval_time,
                PM = sum(PM, na.rm = T)*3600/interval_time,
                PN = sum(PN, na.rm = T)*3600/interval_time,
                NO = sum(NO, na.rm = T)*3600/interval_time)
    interpolated_value_Bus_splited$veh_group<- "bus"
    message2 <- paste0("Aggregating results per link AND intervals of ", interval_time/60, " min")
    

} else {
  # ddply(interpolated_value_PC, ~VehID, summarise, 
  #                                        link_speed_av = mean(Av_link_speed),
  #                                        lat = lat[length(lat)],
  #                                        long = long[length(long)],
  #                                        NOx = sum(NOx, na.rm = T),
  #                                        HC = sum(HC, na.rm = T),
  #                                        CO = sum(CO, na.rm = T),
  #                                        PM = sum(PM, na.rm = T),
  #                                        PN = sum(PN, na.rm = T),
  #                                        NO = sum(NO, na.rm = T))
    interpolated_value_PC_splited <- interpolated_value_PC %>%
      group_by_at(c(byCol)) %>%
       summarise(link_speed_av = mean(Av_link_speed),
                 NOx = sum(NOx, na.rm = T),
                 HC = sum(HC, na.rm = T),
                 CO = sum(CO, na.rm = T),
                 PM = sum(PM, na.rm = T),
                 PN = sum(PN, na.rm = T),
                 NO = sum(NO, na.rm = T))
    interpolated_value_PC_splited$veh_group<- "car"

  # interpolated_value_HDV_splited <- ddply(interpolated_value_HDV, ~LinkID, summarise, 
  #                                         link_speed_av = mean(Av_link_speed),
  #                                         lat = lat[length(lat)],
  #                                         long = long[length(long)],
  #                                         NOx = sum(NOx, na.rm = T),
  #                                         HC = sum(HC, na.rm = T),
  #                                         CO = sum(CO, na.rm = T),
  #                                         PM = sum(PM, na.rm = T),
  #                                         PN = sum(PN, na.rm = T),
  #                                         NO = sum(NO, na.rm = T))
    interpolated_value_HDV_splited <- interpolated_value_HDV %>%
      group_by_at(c(byCol)) %>%
      summarise(link_speed_av = mean(Av_link_speed),
                 NOx = sum(NOx, na.rm = T),
                 HC = sum(HC, na.rm = T),
                 CO = sum(CO, na.rm = T),
                 PM = sum(PM, na.rm = T),
                 PN = sum(PN, na.rm = T),
                 NO = sum(NO, na.rm = T))  
  interpolated_value_HDV_splited$veh_group<- "hdv"

  # interpolated_value_Bus_splited <- ddply(interpolated_value_Bus, ~LinkID, summarise, 
  #                                         link_speed_av = mean(Av_link_speed),
  #                                         lat = lat[length(lat)],
  #                                         long = long[length(long)],
  #                                         NOx = sum(NOx, na.rm = T),
  #                                         HC = sum(HC, na.rm = T),
  #                                         CO = sum(CO, na.rm = T),
  #                                         PM = sum(PM, na.rm = T),
  #                                         PN = sum(PN, na.rm = T),
  #                                         NO = sum(NO, na.rm = T))
  interpolated_value_Bus_splited <- interpolated_value_Bus %>%
      group_by_at(c(byCol)) %>%
      summarise(link_speed_av = mean(Av_link_speed),
                 NOx = sum(NOx, na.rm = T),
                 HC = sum(HC, na.rm = T),
                 CO = sum(CO, na.rm = T),
                 PM = sum(PM, na.rm = T),
                 PN = sum(PN, na.rm = T),
                 NO = sum(NO, na.rm = T))  
  interpolated_value_Bus_splited$veh_group<- "bus"
  message2 <- "Caution: Aggregating ALL results per link, no time grouping"
}

rm(criticP_share, avg_critic_P, drive_cycles_ss, drive_cycles_ss_dt_bus, drive_cycles_ss_dt_car,
   drive_cycles_ss_dt_hdv, idle, max_rot_share, rot_mass_factors, rot_mass_factors1,
   sum_veh_specific_var, tot_emissions_EF, veh_critial_power, interpolated_value_Bus,
   interpolated_value_HDV, interpolated_value_PC)

emis_city<- bind_rows(interpolated_value_Bus_splited, interpolated_value_HDV_splited, interpolated_value_PC_splited)
emis_city$link_speed_av <- emis_city$link_speed_av*3.6 # Back to km/h, needed for non-exhaust

# Georeference
# emis_city <- full_join(emis_city, roads, by = c("LinkID" = "id")) %>%
#    replace_na(list(Hr = 0,
#                    link_speed_av = 0,
#                    NOx = 0,
#                    HC = 0,
#                    CO = 0,
#                    PM = 0,
#                    PN = 0,
#                    NO = 0,
#                    veh_group = "car"))  # To consider links with no traffic
# 
#  emis_city <- st_as_sf(emis_city, sf_column_name = "geometry")
#  emis_city <- st_set_crs(emis_city, "+proj=utm +zone=31 +datum=WGS84 +units=m + no_defs")
#cat(message1)
#cat(message2)
# saveRDS(emis_city, paste0(output_path, "emis_diagonal_24h_1hsec"))

write_csv(emis_city, append = TRUE, paste0(output_path, "history.csv") )
write_csv(emis_city, paste0(output_path,output_file))
#system("scp stream.csv vmasip@192.0.2.1:/mnt/data/bscuser/work/pollution-visualization/Data/stream.csv")
