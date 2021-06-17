#' Save the emissions obtained into R-Line shape.
#' @param df data.frame with emissions, in [g/m/s]
#' @param poll pollutant to be saved into R-Line format: NOx, NO, CO, HC, PM

save_emis_hermes<- function(df, poll){
  column_names<- data.frame(Year = sim_year, Mon = sim_month, Day = sim_day, JDay = sim_jday)
  desired_poll <- select(emis_totT, Hr, LinkID, poll)
  horizontal <- spread(desired_poll, LinkID, poll)
  total <- cbind(column_names, horizontal)
  write_csv(total, paste0(output_path, "rline_", sim_date, "_", poll, ".csv" ))
  print(paste0(output_path, "rline_", sim_date, "_", poll, ".csv" ))
}
