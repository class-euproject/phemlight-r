
#' Calculate Cold-start emissions
#'
#' f_minone finds the values =< to 1, of NOx, HC and PM since COPERT V manual states that If Ecold/Ehot < 1;
#' 1 should be used (p.60). For petrol the cold start values depend on the speed of the vehicle,
#' for diesel it doesn't, hence calc_Cold_Hot_D doesn't requiere imputs
#' @param thelist list of files you want to test if they are minor than one
#' @param emissions general emission file to calculate cold start emissions
#' @param eCold_Hot_P Quotient emission factor of Cold/Hot start for Petrol previously calculated
#' @param eCold_Hot_D Quotient parameter of Cold/hot start for Diesel previously calculated
#' @return The cold start emissions for all vehicle groups, for Petrol and Diesel
#'
f_minone <- function(thelist) {
  for (i in (1:nrow(thelist))) {
    if (thelist$NOx[i] < 1) {
      thelist$NOx[i] = 1
    }
    if (thelist$HC[i] < 1){
      thelist$HC[i] = 1
    }
    if (length(thelist$PM) > 0) {
      if (thelist$PM[i] < 1){
        thelist$PM[i] = 1
      }
    }
  }
  return(thelist)
}
calc_Cold_Hot_P<- function(emissions) {
  A<- NULL; B<- NULL; C<- NULL
  for (i in (1:nrow(emissions))) {
    if (emissions$link_speed_av[i] >= 5 & emissions$link_speed_av[i] <= 25) {
      A[i]<- 0.0458; B[i] <- 0.00747; C[i] <- 0.764
    } else if (emissions$link_speed_av[i] > 25 & emissions$link_speed_av[i] <= 45) {
      A[i]<- 0.0484; B[i] <- 0.0228; C[i] <- 0.685
    } else {
      A[i] <- 0; B[i] <- 0; C[i] <- 1
    }
  }
  eCold_Hot_P <- data.frame(NOx = A*emissions$link_speed_av + B*ta + C)
  rm(A, B, C)
  A<- NULL; B<- NULL; C<- NULL
  for (i in (1:nrow(emissions))) {
    if (emissions$link_speed_av[i] >= 5 & emissions$link_speed_av[i] <= 45) {
      A[i]<- 0.0476; B[i] <- -0.477; C[i] <- 13.44
    } else {
      A[i] <- 0; B[i] <- 0; C[i] <- 1
    }
  }
  eCold_Hot_P$HC <- A*emissions$link_speed_av + B*ta + C
  rm(A, B, C)
  eCold_Hot_P <- f_minone(eCold_Hot_P) # If Ecold/Ehot < 1; 1 should be used (p.60)
  eCold_Hot_P <- eCold_Hot_P -1  # (eCold / eHot - 1)
  return(eCold_Hot_P)
}
calc_Cold_Hot_D <- function(){
  eCold_Hot_D <- data.frame(NOx = 1.3-0.013*ta, HC= 3.1-0.09*ta, PM = 3.1-0.1*ta) #table 3.41
  # eCold_Hot_D <- f_minone(eCold_Hot_D) En diesel no pone esa nota
  eCold_Hot_D <- eCold_Hot_D - 1 # (eCold / eHot - 1)
  return(eCold_Hot_D)
}
calc_cold_start<- function(eCold_Hot_P, eCold_Hot_D, emissions) {
  eCold_hot_D_G <- data.frame(NOx = gasoline * eCold_Hot_P$NOx + diesel*eCold_Hot_D$NOx)
  eCold_hot_D_G$HC <- (gasoline * eCold_Hot_P$HC + diesel*eCold_Hot_D$HC)
  eCold_hot_D_G$PM <- diesel*eCold_Hot_D$PM # Petrol doesn't emit PM cold start
  
  cold_start<- data.frame(beta, M, eHot_HC, eHot_NOx, eHot_PM, eCold_hot_D_G$NOx, eCold_hot_D_G$HC, eCold_hot_D_G$PM)
  cold_start <- transmute(cold_start,
                          NOx_cold = beta * M * eHot_NOx * eCold_hot_D_G.NOx,
                          HC_cold = beta * M * eHot_HC * eCold_hot_D_G.HC,
                          PM_cold = beta * M * eHot_PM * eCold_hot_D_G.PM)
  
  cold_start <- cbind(emissions$LinkID, emissions$Hr, emissions$veh_group, cold_start)
  names(cold_start)[1:3]<- c("LinkID", "Hr", "veh_group")
  return(cold_start)
}


