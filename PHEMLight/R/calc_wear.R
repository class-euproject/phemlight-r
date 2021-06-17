
#' Calculate tyre, break and road wear emissions for all groups of vehicles
#'
#' @param final_t_wear Value of tyre wear that will previously be calculated
#' @param final_b_wear Value of break wear that will previously be calculated
#' @param final_s_wear Value of surface wear that will previously be calculated
#' @return tyre, road and surface wear emissions

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
