
#' Join all vehicle parameters in one data frame
#' @param vehicle_data vehicle parameters previously calculated
#' @param fleetshares vehicle fleetshare distribution of the area of study
#' @return all vehicle group parameters joint in a common data frame

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
