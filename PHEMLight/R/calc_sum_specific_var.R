#' Calculation of the sum of the specific variables of vehicle class
#' @param fleetshare fleetshare of the vehicle fleet at the area of study
#' @param vehicles_data vehicles specific parameters
#' @return average of the vehicle specific parameters


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
