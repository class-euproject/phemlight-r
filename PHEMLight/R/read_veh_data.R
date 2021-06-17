
#' These are different functions to read vehicle data needed by PHEMLight:
#' Emissions, vehicle parameters, critical power and idle.
#' @param file_path path of a file to read its header
#' @param path_vehicles Path were all vehicle data is. Normally within PHEMLight directories.
#' @param filename emissions filename to be read
#' @param crit_Pname critical power file to be read
#' @param file_emis file with all emission factors

def_header <- function (file_path) {
  x <- read.csv(file_path)
  header<-colnames(x)
  return(header)
}

read_csv_emissions <- function(filename){
  ret <- read.csv(paste0(path_vehicles,filename), skip = 3, col.names = header)
  ret$Source <- as.factor(filename)
  ret
}

read_veh_data <- function(filename){
  ret <- read.csv(paste0(path_vehicles,filename), sep = ",", header = F, colClasses = "character")
  #ret$Source <- as.factor(filename)
  ret
}

read_critical_power <- function(crit_Pname) {
  crit_P <- read.csv(paste0(path_vehicles,crit_Pname), skip = 2, nrows = 1, sep = ",", header = F)
  crit_P <- as.numeric(regmatches(crit_P[1,1], regexpr("[[:digit:].[:digit:]]+", crit_P[1,1])))
  crit_P<- data.frame(crit_P)
  crit_P$source<- crit_Pname
  return(crit_P)
}

read_idle<- function(file_emis){
  idle <- read.csv(paste0(path_vehicles,file_emis), skip = 3, nrows = 1, sep = ",", header = F)
  names(idle)<- c("x", "NOx_idle", "HC_idle", "CO_idle", "PM_idle", "PN_idle", "NO_idle")
  idle[1] <- NULL
  idle$source <- file_emis
  return(idle)
}
