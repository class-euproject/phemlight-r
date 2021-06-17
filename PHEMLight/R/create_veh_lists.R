
#' Create lists of PC, BUS and HDV vehicles from the database.
#' @param fleetshare fleetshare of the vehicles fleet of the area of study
#' @return list of all vehicles types for each vehicle group

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
