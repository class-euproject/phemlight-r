
#' Reading of the different values of Rotational Mass factor for the different vehicle classes.
#' @param path_vehicles Path were all vehicle data is. Normally within PHEMLight directories
#' @param files_corrected list of vehicle files with names already corrected
#' @return rotational mass factor for the different vehicle classes according to the shape of data of each one.

data_rot_mass_factor <- function(veh_files) {
  files_corrected <- veh_files[!grepl("CO|TT|RT", veh_files)]
  files_CO_RT_TT <- veh_files[grepl("CO|TT|RT", veh_files)]
  files_CO_RT_TT <- files_CO_RT_TT[!grepl("RT_II", files_CO_RT_TT)]
  files_RT_II<- veh_files[grepl("RT_II", veh_files)]
  rot_mass_factors_normal<- ldply(files_corrected, read_rot_mas_factor)
  rot_mass_factors_CORTTT<- ldply(files_CO_RT_TT, read_rot_mas_factor_COTTRT)
  rot_mass_factors_RT_II<- ldply(files_RT_II, read_rot_mas_factor_RT_II)
  rot_mass_factors<- rbind(rot_mass_factors_normal, rot_mass_factors_CORTTT, rot_mass_factors_RT_II)
  return(rot_mass_factors)
}

read_rot_mas_factor <- function(files_corrected) {
  rot_mass_factor <- read.csv(paste0(path_vehicles,files_corrected), sep = ",", skip = 109, nrows = 7)
  rot_mass_factor$source<- files_corrected
  return(rot_mass_factor)
}

read_rot_mas_factor_COTTRT <- function(files_corrected) {
  rot_mass_factor <- read.csv(paste0(path_vehicles,files_corrected), sep = ",", skip = 109, nrows = 14)
  rot_mass_factor$source<- files_corrected
  return(rot_mass_factor)
}

read_rot_mas_factor_RT_II<- function(files_corrected) {
  rot_mass_factor <- read.csv(paste0(path_vehicles,files_corrected), sep = ",", skip = 109, nrows = 12)
  rot_mass_factor$source<- files_corrected
  return(rot_mass_factor)
}

