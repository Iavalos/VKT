VKT_table <- function(unique.ID, location.ID, df.base, df.assumptions, mode, purpose){
  pop = subset(df.assumptions, Category == "Trip_rates"& Subcategory=="Pop" & Location.ID == location.ID)$Value *
    subset(df.base, MMU.ID==unique.ID & location.ID == location.ID)$population
  job = subset(df.assumptions, Category == "Trip_rates"& Subcategory=="Jobs"& Location.ID == location.ID)$Value *
    subset(df.base, MMU.ID==unique.ID & location.ID == location.ID)$employment
  p.t = pop+job


  vehicle = subset(df.assumptions, Category == "Mode_split"& Subcategory==mode &
                 Location.ID==location.ID & Units == "per person")$Value


  distance = subset(df.assumptions, Category == "Travel_distance"& Subcategory==purpose &
                      Location.ID==location.ID)$Value
  mode.trips =  p.t * vehicle
  split = subset(df.assumptions, Category == "Trip_split"& Subcategory==purpose)$Value
  VKT = mode.trips *distance*split
  return(VKT)
}
