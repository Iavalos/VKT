VKT <- function(population,employment, distance_per_trip,
                trips_per_person, trips_per_employee,
                selected_mode_fraction, selected_purpose_fraction,mode,policy_elasticity=NA,
                Npt = NA){

  if(is.na(policy_elasticity)){
    selected_mode_fraction = selected_mode_fraction
  } else { if(mode == "Car"){selected_mode_fraction = selected_mode_fraction*(1-(policy_elasticity/100))}else
  {if(mode != "Car"){selected_mode_fraction = selected_mode_fraction*(1+(abs(policy_elasticity/100)/Npt))}}
  }

  trips_all_modes <-
    (trips_per_person * population) +
    (trips_per_employee * employment)

  trips <- trips_all_modes * selected_mode_fraction*
    selected_purpose_fraction

  VKT <- trips * distance_per_trip
  return(VKT)
}



