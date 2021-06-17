
#' Calculation of rotational mass factor
#' @param veh_class class of vehicle to be determined its rotational mass factor
#' @param v instantaneous vehicle speed
#' @return instantaneous rotational mass factor



calc_rot_mass_factor<- function(veh_class, v){
  rot_mass_factor_wanted<- filter(rot_mass_factors1, source == veh_class)
  rot_mass_factor_wanted<- data.frame(approx(x = rot_mass_factor_wanted$`speed_m/s`, y = rot_mass_factor_wanted$rot_mass_factor, xout = v))
  return(rot_mass_factor_wanted$y)
}
