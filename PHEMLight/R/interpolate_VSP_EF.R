
#' Interpolation of calculated VSP with emission factors, per vehicle group
#' @param drive_cycles_vsp vehicle specific power calculated for all drive cycles
#' @param Veh_group vehicle group (PC, BUS or HDV) to calculate its emissions
#' @return instantaneous emissions of NOx, HC, CO, PM, PN and NO for the specified vehicle group [g/h]

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
