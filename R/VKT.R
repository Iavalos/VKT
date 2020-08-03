VKT <- function(population,employment, distance_per_trip,
                trips_per_person, trips_per_employee,
                selected_mode_fraction, selected_purpose_fraction){

  trips_all_modes <-
    (trips_per_person * population) +
    (trips_per_employee * employment)

  trips <- trips_all_modes * selected_mode_fraction*
    selected_purpose_fraction

  VKT <- trips * distance_per_trip
  return(VKT)
}



