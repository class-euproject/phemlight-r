read_csv_emissions <- function(filename){
  ret <- read.csv(paste0(path_vehicles,filename), skip = 3, col.names = header)
  ret$Source <- as.factor(filename)
  ret
}
read_veh_data <- function(filename){
  ret <- read.csv(paste0(path_vehicles,filename), sep = ",", header = F, colClasses = "character")
  #ret$Source <- as.factor(filename)
  ret
}
def_header <- function (file_path) {
  x <- read.csv(file_path)
  header<-colnames(x)
  return(header)
}
read_critical_power <- function(crit_Pname) {
  crit_P <- read.csv(paste0(path_vehicles,crit_Pname), skip = 2, nrows = 1, sep = ",", header = F)
  crit_P <- as.numeric(regmatches(crit_P[1,1], regexpr("[[:digit:].[:digit:]]+", crit_P[1,1])))
  crit_P<- data.frame(crit_P)
  crit_P$source<- crit_Pname
  return(crit_P)
}
read_idle<- function(file_emis){
  idle <- read.csv(paste0(path_vehicles,file_emis), skip = 3, nrows = 1, sep = ",", header = F)
  names(idle)<- c("x", "NOx_idle", "HC_idle", "CO_idle", "PM_idle", "PN_idle", "NO_idle")
  idle[1] <- NULL
  idle$source <- file_emis
  return(idle)
}
calc_idles <- function(idle, fleetshare) {
  idle$source <- sub(".csv", "", idle$source)
  idle <- left_join(idle, fleetshare, by = c("source" = "veh_category"))
  idle<- transform(idle, NOx_idle = NOx_idle*share,
                   HC_idle = HC_idle*share,
                   CO_idle = CO_idle*share,
                   PM_idle = PM_idle*share,
                   PN_idle = PN_idle*share,
                   NO_idle = NO_idle*share)
  idle <- ddply(idle, ~veh_group, summarise, NOx_idle = sum(NOx_idle),
                HC_idle = sum(HC_idle),
                CO_idle = sum(CO_idle),
                PM_idle = sum(PM_idle),
                PN_idle = sum(PN_idle),
                NO_idle = sum(NO_idle))
  idle <- idle[-4,]
  idle[1,2:7] <- idle[1,2:7]*avg_critic_P[1,2] # Denormalise idle for BUS
  idle[2,2:7] <- idle[2,2:7]*avg_critic_P[2,2] # Denormalise idle HDV
  return(idle)
}
mileage_factor_calc<- function(path_mileage_data) {
  mcor_poll <- read_csv(paste0(path_mileage_data))
  mcor_poll<- inner_join(mcor_poll, select(fleetshare, veh_category, share), by = c("PHEMlight" = "veh_category"))
  mcor_poll$sum_share <- sum(mcor_poll$share) 
  mcor_poll$norm_share <- mcor_poll$share / mcor_poll$sum_share
  mcor_poll<- ddply(mcor_poll, ~group, summarise, 
                    A_urban = sum(A_urban*norm_share),
                    B_urban = sum(B_urban*norm_share),
                    A_road = sum(A_road*norm_share),
                    B_road = sum(B_road*norm_share))
  return(mcor_poll)
}
create_PC_list <- function() {
  share_PC <- filter(fleetshare, veh_group == "PC")
  PC_list <- share_PC[2]
  PC_phemlight<- paste0(PC_list$veh_category, ".PHEMLight.veh")
  return(PC_phemlight)
}
create_bus_list <- function() {
  share_BUS<- filter(fleetshare, veh_group == "BUS")
  BUS_list <- share_BUS[2]
  BUS_phemlight<- paste0(BUS_list$veh_category, ".PHEMLight.veh")
  return(BUS_phemlight)
}
create_HDV_list <- function() {
  share_HDV<- filter(fleetshare, veh_group == "HDV")
  HDV_list <- share_HDV[2]
  HDV_phemlight<- paste0(HDV_list$veh_category, ".PHEMLight.veh")
  return(HDV_phemlight)
}
read_rot_mas_factor <- function(files_corrected) {
  rot_mass_factor <- read.csv(paste0(path_vehicles,files_corrected), sep = ",", skip = 109, nrows = 7)
  rot_mass_factor$source<- files_corrected
  return(rot_mass_factor)
}
read_rot_mas_factor_COTTRT <- function(files_corrected) {
  rot_mass_factor <- read.csv(paste0(path_vehicles,files_corrected), sep = ",", skip = 109, nrows = 14)
  rot_mass_factor$source<- files_corrected
  return(rot_mass_factor)
}
read_rot_mas_factor_RT_II<- function(files_corrected) {
  rot_mass_factor <- read.csv(paste0(path_vehicles,files_corrected), sep = ",", skip = 109, nrows = 12)
  rot_mass_factor$source<- files_corrected
  return(rot_mass_factor)
}
data_rot_mass_factor <- function(veh_files) {
  files_corrected <- veh_files[!grepl("CO|TT|RT", veh_files)]
  files_CO_RT_TT <- veh_files[grepl("CO|TT|RT", veh_files)]
  files_CO_RT_TT <- files_CO_RT_TT[!grepl("RT_II", files_CO_RT_TT)]
  files_RT_II<- veh_files[grepl("RT_II", veh_files)]
  rot_mass_factors_normal<- ldply(files_corrected, read_rot_mas_factor)
  rot_mass_factors_CORTTT<- ldply(files_CO_RT_TT, read_rot_mas_factor_COTTRT)
  rot_mass_factors_RT_II<- ldply(files_RT_II, read_rot_mas_factor_RT_II)
  rot_mass_factors<- rbind(rot_mass_factors_normal, rot_mass_factors_CORTTT, rot_mass_factors_RT_II)
  return(rot_mass_factors)
}
calc_rot_mass_factor<- function(veh_class, v){
  rot_mass_factor_wanted<- filter(rot_mass_factors1, source == veh_class)
  rot_mass_factor_wanted<- data.frame(approx(x = rot_mass_factor_wanted$`speed_m/s`, y = rot_mass_factor_wanted$rot_mass_factor, xout = v))
  return(rot_mass_factor_wanted$y)
}
join_veh_variables <- function(vehicle_data, fleetshares) {
  mveh<- laply(files_vehicles, function(x) {as.integer(vehicle_data[[x]][[1]][5])})
  mload <- laply(files_vehicles, function(x) {as.integer(vehicle_data[[x]][[1]][7])})
  Fr0 <- laply(files_vehicles, function(x) {as.numeric(vehicle_data[[x]][[1]][32])})
  Fr1 <- laply(files_vehicles, function(x) {as.numeric(vehicle_data[[x]][[1]][34])})
  Fr4 <- laply(files_vehicles, function(x) {as.numeric(vehicle_data[[x]][[1]][40])})
  Cd <- laply(files_vehicles, function(x) {as.numeric(vehicle_data[[x]][[1]][9])})
  A <- laply(files_vehicles, function(x) {as.numeric(vehicle_data[[x]][[1]][11])})
  mrot <- laply(files_vehicles, function(x) {as.numeric(vehicle_data[[x]][[1]][17])})
  veh_specifics<-data.frame(cbind(source = files_vehicles, mveh, mload, Fr0, Fr1, Fr4, Cd, A, mrot), row.names = files_vehicles)
  veh_specifics$source <- sub(".PHEMLight.veh", "", veh_specifics$source)
  names(fleetshares)[2] <- "source"
  veh_specifics<- join_all(list(veh_specifics,fleetshares), by='source', type='left')
  veh_specifics[,2:9] <- as.numeric(as.character(unlist(veh_specifics[,2:9])))
  return(veh_specifics)
}
calc_sum_specific_var <- function(vehicles_data, fleetshares) {
  veh_specific_var <- join_veh_variables(vehicles_data, fleetshares)
  avg_veh_specific_var <- veh_specific_var[,c(1,10)]
  avg_veh_specific_var[,3:10]<- veh_specific_var[,2:9]*veh_specific_var$share
  sum_veh_specific_var <- ddply(avg_veh_specific_var, ~veh_group, summarise,
                                smveh = sum(mveh, na.rm = T),
                                smload = sum(mload, na.rm = T),
                                sFr0 = sum(Fr0, na.rm = T),
                                sFr1 = sum(Fr1, na.rm = T),
                                sFr4 = sum(Fr4, na.rm = T),
                                sCd = sum(Cd, na.rm = T),
                                sA = sum(A, na.rm = T),
                                smrot = sum(mrot, na.rm = T))
  sum_veh_specific_var <- sum_veh_specific_var[-4,]
  return(sum_veh_specific_var)
}
calc_power_PC <- function(v, a, rot_mass, grad = 0) {
  # Veh specific data
  g <- 9.81
  sigma <- 1.225 # Air density
  PC_vars <- sum_veh_specific_var[3, 2:9]
  rot_mass_number<- rot_mass #calc_rot_mass_factor(max_car, v)
  # Power resistances to overcome
  Proll <- (PC_vars$smveh + PC_vars$smload) * g * (PC_vars$sFr0 + PC_vars$sFr1*v + PC_vars$sFr4*v**4)*v
  Pair<- PC_vars$sCd*PC_vars$sA*(sigma/2)*v**3
  Pacc<- (PC_vars$smveh*rot_mass_number+PC_vars$smload+PC_vars$smrot)*a*v
  Prgrad <- (PC_vars$smveh+PC_vars$smload)*grad*0.01*v*g
  ngear<- 0.9
  # Final engine power
  Pe <- (Proll + Pair + Pacc + Prgrad)/ngear
  Pe<- Pe/1000
  return(Pe)
}
calc_power_BUS <- function(v, a, grad = 0) {
  # Veh specific data
  g <- 9.81
  sigma <- 1.225 # Air density
  BUS_vars <- sum_veh_specific_var[1, 2:9]
  rot_mass_number<- rot_mass_factors[1,2]
  # Power resistances to overcome
  Proll <- (BUS_vars$smveh + BUS_vars$smload) * g * (BUS_vars$sFr0 + BUS_vars$sFr1*v + BUS_vars$sFr4*v**4)*v
  Pair<- BUS_vars$sCd*BUS_vars$sA*(sigma/2)*v**3
  Pacc<- (BUS_vars$smveh*rot_mass_number+BUS_vars$smload+BUS_vars$smrot)*a*v
  Prgrad <- (BUS_vars$smveh+BUS_vars$smload)*grad*0.01*v*g
  ngear<- 0.8
  # Final engine power
  Pe <- (Proll + Pair + Pacc + Prgrad)/ngear
  Pe<- Pe/1000
  return(Pe)
}
calc_power_HDV <- function(v, a, grad = 0) {
  # Veh specific data
  g <- 9.81
  sigma <- 1.225 # Air density
  HDV_vars <- sum_veh_specific_var[2, 2:9]
  rot_mass_number<- rot_mass_factors[2,2]  #calc_rot_mass_factor(max_hdv, v)
  # Power resistances to overcome
  Proll <- (HDV_vars$smveh + HDV_vars$smload) * g * (HDV_vars$sFr0 + HDV_vars$sFr1*v + HDV_vars$sFr4*v**4)*v
  Pair<- HDV_vars$sCd*HDV_vars$sA*(sigma/2)*v**3
  Pacc<- (HDV_vars$smveh*rot_mass_number+HDV_vars$smload+HDV_vars$smrot)*a*v
  Prgrad <- (HDV_vars$smveh+HDV_vars$smload)*grad*0.01*v*g
  ngear<- 0.9
  # Final engine power
  Pe <- (Proll + Pair + Pacc + Prgrad)/ngear
  Pe<- Pe/1000
  return(Pe)
}
final_power<- function(drive_cycles){
  if (drive_cycles$Vehicle_type == "Car"){
    drive_cycles$power <- calc_power_PC(drive_cycles$Av_link_speed, drive_cycles$acc)
    return(drive_cycles$power)
  } else if (drive_cycles$Vehicle_type == "Bus") {
    drive_cycles$power <- calc_power_BUS(drive_cycles$Av_link_speed, drive_cycles$acc)
    return(drive_cycles$power)
  } else if (drive_cycles$Vehicle_type == "HDV") {
    drive_cycles$power <- calc_power_HDV(drive_cycles$Av_link_speed, drive_cycles$acc)
    return(drive_cycles$power)
  }
}
approx_vsp<- function(drive_cycles_vsp, Veh_group) {
  selection<- filter(drive_cycles_vsp, Vehicle_type == Veh_group)
  if (Veh_group == "Car") {
    NOx <- data.frame(approx(x = P_PC$cp_norm.rated., y = P_PC$NOx_normalised, xout = selection$norm_VSP, rule = 2))
    HC <- data.frame(approx(x = P_PC$cp_norm.rated., y = P_PC$HC_normalised, xout = selection$norm_VSP, rule = 2))
    CO <- data.frame(approx(x = P_PC$cp_norm.rated., y = P_PC$CO_normalised, xout = selection$norm_VSP, rule = 2))
    PM <- data.frame(approx(x = P_PC$cp_norm.rated., y = P_PC$PM_normalised, xout = selection$norm_VSP, rule = 2))
    PN <- data.frame(approx(x = P_PC$cp_norm.rated., y = P_PC$PN_normalised, xout = selection$norm_VSP, rule = 2))
    NO <- data.frame(approx(x = P_PC$cp_norm.rated., y = P_PC$NO_normalised, xout = selection$norm_VSP, rule = 2))
    x <- data.frame(cbind(NOx$y, HC$y, CO$y, PM$y, PN$y, NO$y))
    
  } else if (Veh_group == "Bus") {
    NOx <- data.frame(approx(x = P_BUS$cp_norm.rated., y = P_BUS$NOx_normalised, xout = selection$norm_VSP, rule = 2))
    HC <- data.frame(approx(x = P_BUS$cp_norm.rated., y = P_BUS$HC_normalised, xout = selection$norm_VSP, rule = 2))
    CO <- data.frame(approx(x = P_BUS$cp_norm.rated., y = P_BUS$CO_normalised, xout = selection$norm_VSP, rule = 2))
    PM <- data.frame(approx(x = P_BUS$cp_norm.rated., y = P_BUS$PM_normalised, xout = selection$norm_VSP, rule = 2))
    PN <- data.frame(approx(x = P_BUS$cp_norm.rated., y = P_BUS$PN_normalised, xout = selection$norm_VSP, rule = 2))
    NO <- data.frame(approx(x = P_BUS$cp_norm.rated., y = P_BUS$NO_normalised, xout = selection$norm_VSP, rule = 2))
    x <- data.frame(cbind(NOx$y, HC$y, CO$y, PM$y, PN$y, NO$y))
    
  } else if (Veh_group == "HDV") {
    NOx <- data.frame(approx(x = P_HDV$cp_norm.rated., y = P_HDV$NOx_normalised, xout = selection$norm_VSP, rule = 2))
    HC <- data.frame(approx(x = P_HDV$cp_norm.rated., y = P_HDV$HC_normalised, xout = selection$norm_VSP, rule = 2))
    CO <- data.frame(approx(x = P_HDV$cp_norm.rated., y = P_HDV$CO_normalised, xout = selection$norm_VSP, rule = 2))
    PM <- data.frame(approx(x = P_HDV$cp_norm.rated., y = P_HDV$PM_normalised, xout = selection$norm_VSP, rule = 2))
    PN <- data.frame(approx(x = P_HDV$cp_norm.rated., y = P_HDV$PN_normalised, xout = selection$norm_VSP, rule = 2))
    NO <- data.frame(approx(x = P_HDV$cp_norm.rated., y = P_HDV$NO_normalised, xout = selection$norm_VSP, rule = 2))
    x <- data.frame(cbind(NOx$y, HC$y, CO$y, PM$y, PN$y, NO$y))
  }
  return(x)
}
calc_Decel_coast_BUS <- function(v, grad = 0) {
  # Veh specific data
  g <- 9.81
  sigma <- 1.225 # Air density
  BUS_vars <- sum_veh_specific_var[3, 2:9]
  rot_mass_number<- rot_mass_factors[1,2] #calc_rot_mass_factor(max_bus, v)
  # Power resistances to overcome
  Proll <- (BUS_vars$smveh + BUS_vars$smload) * g * (BUS_vars$sFr0 + BUS_vars$sFr1*v + BUS_vars$sFr4*v**4)*v
  Pair<- BUS_vars$sCd*BUS_vars$sA*(sigma/2)*v**3
  denom <- BUS_vars$smveh*rot_mass_number+BUS_vars$smload+BUS_vars$smrot
  Prgrad <- (BUS_vars$smveh+BUS_vars$smload)*grad*0.01*v
  Floss <- 0.1*Proll
  # Deceleration Coasting
  Decel_coast <- -(Proll + Pair + Prgrad + Floss)/denom
  return(Decel_coast)
}

# Non-exhaust
# Cold - Start
f_minone <- function(thelist) {
  for (i in (1:nrow(thelist))) {
    if (thelist$NOx[i] < 1) {
      thelist$NOx[i] = 1
    }
    if (thelist$HC[i] < 1){
      thelist$HC[i] = 1
    }
    if (length(thelist$PM) > 0) {
      if (thelist$PM[i] < 1){
        thelist$PM[i] = 1
      }
    }
  }
  return(thelist)
}
calc_Cold_Hot_P<- function(emissions) {
  A<- NULL; B<- NULL; C<- NULL
  for (i in (1:nrow(emissions))) {
    if (emissions$link_speed_av[i] >= 5 & emissions$link_speed_av[i] <= 25) {
      A[i]<- 0.0458; B[i] <- 0.00747; C[i] <- 0.764
    } else if (emissions$link_speed_av[i] > 25 & emissions$link_speed_av[i] <= 45) {
      A[i]<- 0.0484; B[i] <- 0.0228; C[i] <- 0.685
    } else {
      A[i] <- 0; B[i] <- 0; C[i] <- 1
    }
  }
  eCold_Hot_P <- data.frame(NOx = A*emissions$link_speed_av + B*ta + C)
  rm(A, B, C)
  A<- NULL; B<- NULL; C<- NULL
  for (i in (1:nrow(emissions))) {
    if (emissions$link_speed_av[i] >= 5 & emissions$link_speed_av[i] <= 45) {
      A[i]<- 0.0476; B[i] <- -0.477; C[i] <- 13.44
    } else {
      A[i] <- 0; B[i] <- 0; C[i] <- 1
    }
  }
  eCold_Hot_P$HC <- A*emissions$link_speed_av + B*ta + C
  rm(A, B, C)
  eCold_Hot_P <- f_minone(eCold_Hot_P) # If Ecold/Ehot < 1; 1 should be used (p.60)
  eCold_Hot_P <- eCold_Hot_P -1  # (eCold / eHot - 1)
  return(eCold_Hot_P)
}
calc_Cold_Hot_D <- function(){
  eCold_Hot_D <- data.frame(NOx = 1.3-0.013*ta, HC= 3.1-0.09*ta, PM = 3.1-0.1*ta) #table 3.41
  # eCold_Hot_D <- f_minone(eCold_Hot_D) En diesel no pone esa nota
  eCold_Hot_D <- eCold_Hot_D - 1 # (eCold / eHot - 1)
  return(eCold_Hot_D)
}
calc_cold_start<- function(eCold_Hot_P, eCold_Hot_D, emissions) {
  eCold_hot_D_G <- data.frame(NOx = gasoline * eCold_Hot_P$NOx + diesel*eCold_Hot_D$NOx)
  eCold_hot_D_G$HC <- (gasoline * eCold_Hot_P$HC + diesel*eCold_Hot_D$HC)
  eCold_hot_D_G$PM <- diesel*eCold_Hot_D$PM # Petrol doesn't emit PM cold start
  
  cold_start<- data.frame(beta, M, eHot_HC, eHot_NOx, eHot_PM, eCold_hot_D_G$NOx, eCold_hot_D_G$HC, eCold_hot_D_G$PM)
  cold_start <- transmute(cold_start,
                          NOx_cold = beta * M * eHot_NOx * eCold_hot_D_G.NOx,
                          HC_cold = beta * M * eHot_HC * eCold_hot_D_G.HC,
                          PM_cold = beta * M * eHot_PM * eCold_hot_D_G.PM)
  
  cold_start <- cbind(emissions$LinkID, emissions$Hr, emissions$veh_group, cold_start)
  names(cold_start)[1:3]<- c("LinkID", "Hr", "veh_group")
  return(cold_start)
}

# Resuspension
weight_LDV <- function(sharefile) {
  LDV<-  dplyr::filter(sharefile, grepl("LCV",vehicle_class))
  LDV<- sum(LDV$share)
}
weight_PC <- function(sharefile) {
  PC<-  dplyr::filter(sharefile, grepl("PC",vehicle_class))
  PC<- sum(PC$share)
}
ef_calc <- function(file_emissions, ef_constants){
  for (i in (1:nrow(file_emissions))) {
    if (file_emissions$veh_group[i] == "car"){
      ef[i] <- ef_constants[1,2]*PC + ef_constants[5,2]*LDV
    } else if (file_emissions$veh_group[i] == "bus" | file_emissions$veh_group[i] == "hdv") {
      ef[i] <- ef_constants[2,2]
    } else if (file_emissions$veh_group[i] == "motos"){
      ef[i] <- ef_constants[4,2]
    } else {ef[i] <- NA
    }
  }
  return(ef)
}
calc_resuspension <- function(file_emissions) {
  Eres<- as.vector(N * M * ef)
  resuspension <- data.frame(cbind(file_emissions$LinkID, file_emissions$Hr, file_emissions$veh_group, Eres))
  names(resuspension)[1:3] <- c("LinkID", "Hr", "veh_group")
  resuspension[1] <- as.integer(as.character(resuspension$LinkID))
  resuspension[2] <- as.integer(as.character(resuspension$Hr))
  resuspension[4] <- as.numeric(as.character(resuspension$Eres))
  return(resuspension)
}

# Wear
calc_tyre_wear <- function(final_t_wear) {
  if (final_t_wear$veh_group == "moto") {
    final_t_wear$Te_Tyre <- final_t_wear$Te_Tyre*final_t_wear$t_motos
    return(final_t_wear$Te_Tyre)
  } else if (final_t_wear$veh_group == "bus" | final_t_wear$veh_group == "hdv" ) {
    final_t_wear$Te_Tyre <- final_t_wear$Te_Tyre*final_t_wear$tHDV
    return(final_t_wear$Te_Tyre)
  } else {
    final_t_wear$Te_Tyre <- final_t_wear$Te_Tyre*final_t_wear$tPC_LDV
    return(final_t_wear$Te_Tyre)
  }
}
calc_break_wear <- function(final_b_wear) {
  if (final_b_wear$veh_group == "moto") {
    final_b_wear$Te_Break <- final_b_wear$Te_Break*final_b_wear$b_motos
  } else if (final_b_wear$veh_group == "bus" | final_b_wear$veh_group == "hdv" ) {
    final_b_wear$Te_Break <- final_b_wear$Te_Break*final_b_wear$bHDV
  } else {
    final_b_wear$Te_Break <- final_b_wear$Te_Break*final_b_wear$bPC_LDV
  }
}
calc_surface_wear <- function(final_s_wear){
  if (final_s_wear$veh_group == "moto") {
    final_s_wear$Te_surface <- final_s_wear$Te_surface*final_s_wear$s_motos
  } else if (final_s_wear$veh_group == "bus" | final_s_wear$veh_group == "hdv" ) {
    final_s_wear$Te_surface <- final_s_wear$Te_surface*final_s_wear$sHDV
  } else {
    final_s_wear$Te_surface <- final_s_wear$Te_surface*final_s_wear$sPC_LDV
  }
}

# No usadas
dec_coast_final<- function(drive_cycles){
  if (drive_cycles$Vehicle_type == "Car"){
    drive_cycles$decel_coast <- calc_Decel_coast_PC(drive_cycles$Av_link_speed)
    return(drive_cycles$decel_coast)
  } else if (drive_cycles$Vehicle_type == "Bus") {
    drive_cycles$decel_coast <- calc_Decel_coast_BUS(drive_cycles$Av_link_speed)
    return(drive_cycles$decel_coast)
  } else if (drive_cycles$Vehicle_type == "HDV") {
    drive_cycles$decel_coast <- calc_Decel_coast_HDV(drive_cycles$Av_link_speed)
    return(drive_cycles$decel_coast)
  }
}
calc_Decel_coast_PC <- function(v, grad = 0) {
  # Veh specific data
  g <- 9.81
  sigma <- 1.225 # Air density
  PC_vars <- sum_veh_specific_var[3, 2:9]
  rot_mass_number<- rot_mass_factors[3,2] #calc_rot_mass_factor(max_car, v)
  # Power resistances to overcome
  Proll <- (PC_vars$smveh + PC_vars$smload) * g * (PC_vars$sFr0 + PC_vars$sFr1*v + PC_vars$sFr4*v**4)*v
  Pair<- PC_vars$sCd*PC_vars$sA*(sigma/2)*v**3
  denom <- PC_vars$smveh*rot_mass_number+PC_vars$smload+PC_vars$smrot
  Prgrad <- (PC_vars$smveh+PC_vars$smload)*grad*0.01*v
  Floss <- 0  # 0.1*Proll
  # Deceleration Coasting
  Decel_coast <- -(Proll + Pair + Prgrad + Floss)/denom
  return(Decel_coast)
}
calc_Decel_coast_HDV <- function(v, grad = 0) {
  # Veh specific data
  g <- 9.81
  sigma <- 1.225 # Air density
  HDV_vars <- sum_veh_specific_var[3, 2:9]
  rot_mass_number<- rot_mass_factors[2,2]# calc_rot_mass_factor(max_hdv, v) ## ¡¡ TE QUEDA ESTO !!
  # Power resistances to overcome
  Proll <- (HDV_vars$smveh + HDV_vars$smload) * g * (HDV_vars$sFr0 + HDV_vars$sFr1*v + HDV_vars$sFr4*v**4)*v
  Pair<- HDV_vars$sCd*HDV_vars$sA*(sigma/2)*v**3
  denom <- HDV_vars$smveh*rot_mass_number+HDV_vars$smload+HDV_vars$smrot
  Prgrad <- (HDV_vars$smveh+HDV_vars$smload)*grad*0.01*v
  Floss <- 0.1*Proll
  # Deceleration Coasting
  Decel_coast <- -(Proll + Pair + Prgrad + Floss)/denom
  return(Decel_coast)
}
agg_vehicle_categories <- function(names_to_look_H3, euro_category, names_to_add_PL) {
  bus_standart_H3 <- compo_full[grepl(names_to_look_H3, compo_full$Copert_V_name),]
  bus_standart_H3 <- bus_standart_H3[grepl(euro_category, bus_standart_H3$Copert_V_name),]
  compo_full[grepl(names_to_add_PL, compo_full$PHEMlight),8] <- sum(bus_standart_H3$share, na.rm = T)
  return(compo_full)
}
save_emis_hermes<- function(df, poll){
  column_names<- data.frame(Year = sim_year, Mon = sim_month, Day = sim_day, JDay = sim_jday)
  desired_poll <- select(emis_totT, Hr, LinkID, poll)
  horizontal <- spread(desired_poll, LinkID, poll, fill = 0)
  total <- cbind(column_names, horizontal)
  write_csv(total, paste0(output_path, "rline_", sim_date, "_", poll, ".csv" ))
  print(paste0(output_path, "rline_", sim_date, "_", poll, ".csv" ))
}