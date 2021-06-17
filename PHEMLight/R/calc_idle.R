
#' Calculation of idle
#' @param idle data frame with the idle emission factors for each vehicle group
#' @param fleetshare fleetshare of the vehicle fleet at the area of study
#' @return Average idle emission factors for each vehicle group

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




