
#' Calculate resuspension. For that it needs to calculate:
#' Weight of the LDV and PC fleet over the total (weight_LDV and weight_PC respectively)
#' The emission factors over the entire drive cycles (ef_calc)
#'
#' @param sharefile file with the fleet share of the city or area of study
#' @param file_emissions emission data frame
#' @param ef_constants file with the resuspension emission factors
#' @return Resuspension values for all vehicle groups

weight_LDV <- function(sharefile) {
  LDV<-  dplyr::filter(sharefile, grepl("LCV",vehicle_class))
  LDV<- sum(LDV$share)
}
weight_PC <- function(sharefile) {
  PC<-  dplyr::filter(sharefile, grepl("PC",vehicle_class))
  PC<- sum(PC$share)
}
ef_calc <- function(file_emissions, ef_constants){
  for (i in (1:nrow(file_emissions))) {
    if (file_emissions$veh_group[i] == "car"){
      ef[i] <- ef_constants[1,2]*PC + ef_constants[5,2]*LDV
    } else if (file_emissions$veh_group[i] == "bus" | file_emissions$veh_group[i] == "hdv") {
      ef[i] <- ef_constants[2,2]
    } else if (file_emissions$veh_group[i] == "motos"){
      ef[i] <- ef_constants[4,2]
    } else {ef[i] <- NA
    }
  }
  return(ef)
}
calc_resuspension <- function(file_emissions) {
  Eres<- as.vector(N * M * ef)
  resuspension <- data.frame(cbind(file_emissions$LinkID, file_emissions$Hr, file_emissions$veh_group, Eres))
  names(resuspension)[1:3] <- c("LinkID", "Hr", "veh_group")
  resuspension[1] <- as.integer(as.character(resuspension$LinkID))
  resuspension[2] <- as.integer(as.character(resuspension$Hr))
  resuspension[4] <- as.numeric(as.character(resuspension$Eres))
  return(resuspension)
}
