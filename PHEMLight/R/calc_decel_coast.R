#' Deceleration coast calculation.
#' Currently only the deceleration coast for BUS group is needed, since it did not
#' had an effect on PC and HDV results, and cost computational time.
#' @param v instantaneous vehicle speed, gotten from global emissions file
#' @param grad road gradient, defaults to 0
#' @param drive_cycles drive cycle file of the vehicles simulated
#' @return Deceleration coast value, for each time-step for the vehicle group selected. Depending on the function used
#'

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
