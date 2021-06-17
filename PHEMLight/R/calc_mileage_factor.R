
#' Function to to apply the mileage correction factor to vehicles
#' Only path to mileage data is neded.
#' @param path_mileage_data path of the mileage data file

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