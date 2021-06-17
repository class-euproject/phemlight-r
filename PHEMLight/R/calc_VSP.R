
#' Calculation of VSP for PC, BUS and HDV
#' @param v instantaneous vehicle speed
#' @param a instantaneous vehicle acceleration
#' @param rot_mass instantaneous rotational mass factor
#' @param grad road gradient of the time-step
#' @return vehicle power (VSP) of the vehicle group chosen

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

