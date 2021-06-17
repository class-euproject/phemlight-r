
#' Mapping of the different vehicle category nomenclature between HERMESv3 and PHEMLight
#' @param names_to_look_H3 characters to search in the nomenclature to addapt to PHEMLight, can be more than one.
#' e.g.: "Urban Buses Standard|Coaches Standard|Urban Buses Midi"
#' @param euro_category Euro category of the vehicle type to look for. e.g.: Euro II
#' @param names_to_add_PL characters to map of PHEMLight nomenclature. e.g: HDV_CB_D_EU2$

agg_vehicle_categories <- function(names_to_look_H3, euro_category, names_to_add_PL) {
  bus_standart_H3 <- compo_full[grepl(names_to_look_H3, compo_full$Copert_V_name),]
  bus_standart_H3 <- bus_standart_H3[grepl(euro_category, bus_standart_H3$Copert_V_name),]
  compo_full[grepl(names_to_add_PL, compo_full$PHEMlight),8] <- sum(bus_standart_H3$share, na.rm = T)
  return(compo_full)
}
