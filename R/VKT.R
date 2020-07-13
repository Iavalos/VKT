VKT <- function(Id, Settlement, df.base ,df.assumptions, mode, purpose){
  pop = subset(df.assumptions, Category == "Trip_rates"& Subcategory=="Pop" & Location.ID == Settlement)$Value *
    subset(df.base, ID==Id & settlement == Settlement)$pop_point
  job = subset(df.assumptions, Category == "Trip_rates"& Subcategory=="Jobs"& Location.ID == Settlement)$Value *
    subset(df.base, ID==Id & settlement == Settlement)$jobs_perpo
  p.t = pop+job


  vehicle = subset(df.assumptions, Category == "Mode_split"& Subcategory==mode &
                 Location.ID==Settlement & Units == "per person")$Value


  distance = subset(df.assumptions, Category == "Travel_distance"& Subcategory==purpose &
                      Location.ID==Settlement)$Value
  mode.trips =  p.t * vehicle
  split = subset(df.assumptions, Category == "Trip_split"& Subcategory==purpose)$Value
  VKT = mode.trips *distance*split
  return(VKT)
}
