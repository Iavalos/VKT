Emissions_energy <- function(unique.ID, location.ID, df.base, df.assumptions){
  df.base[unique.ID,"Car.VKT.Wk"]<- VKT_table(unique.ID = unique.ID, location.ID = location.ID, df.base = df.base,
                              df.assumptions=df.assumptions, mode= "Car", purpose= "Work", policy=NA,share= NA, Npt=NA)
  df.base[unique.ID,"Bus.VKT.Wk"]<- VKT_table(unique.ID = unique.ID, location.ID = location.ID, df.base = df.base,
                              df.assumptions=df.assumptions, mode= "Bus", purpose= "Work", policy=NA,share= NA, Npt=NA)
  df.base[unique.ID,"Car.VKT.Sch"]<- VKT_table(unique.ID = unique.ID, location.ID = location.ID, df.base = df.base,
                               df.assumptions=df.assumptions, mode= "Car", purpose= "School", policy=NA,share= NA, Npt=NA)
  df.base[unique.ID,"Bus.VKT.Sch"]<- VKT_table(unique.ID = unique.ID, location.ID = location.ID, df.base = df.base,
                               df.assumptions=df.assumptions, mode= "Bus", purpose= "School", policy=NA,share= NA, Npt=NA)
  df.base["Car.VKT"] <- df.base["Car.VKT.Wk"] + df.base["Car.VKT.Sch"]
  df.base["Bus.VKT"] <-  df.base["Bus.VKT.Wk"] + df.base["Bus.VKT.Sch"]
  df.base["Daily.VKT"] <- df.base["Car.VKT"] + df.base["Bus.VKT"]

  energy <- function(VKT, mode, fuel){
    subcat = paste(mode,fuel,sep = "_")
    share = subset(df.assumptions, Category == "Mode_share"& Subcategory==subcat)$Value
    cu= subset(df.assumptions,  Category == "Density_weight"& Subcategory==fuel)$Value
    vol= subset(df.assumptions,  Category == "Density_volume"& Subcategory==fuel)$Value
    eff = subset(df.assumptions,  Category == "Mode_efficiency"& Subcategory==subcat)$Value
    energy = ((VKT*share*cu*vol)/eff)*0.01
    return(energy)
  }


  df.base["Car.energy.gasoline"] <- energy (df.base["Car.VKT"], "Car", "Gasoline")
  df.base["Car.energy.diesel"] <- energy (df.base["Car.VKT"], "Car", "Diesel")
  df.base["Bus.energy.gasoline"] <- energy (df.base["Bus.VKT"], "Bus", "Gasoline")
  df.base["Bus.energy.diesel"] <- energy (df.base["Bus.VKT"], "Bus", "Diesel")
  df.base[unique.ID,"Total.Energy"] <- sum(df.base[c("Car.energy.gasoline", "Car.energy.diesel",
                                   "Bus.energy.gasoline","Bus.energy.diesel")], na.rm = T)

  e.gasoline =sum(df.base[c("Car.energy.gasoline", "Bus.energy.gasoline")], na.rm = T)*
    subset(df.assumptions, Category == "Carbon_factor"& Subcategory=="Gasoline")$Value

  e.diesel = sum(df.base[c( "Car.energy.diesel","Bus.energy.diesel")], na.rm = T) *
    subset(df.assumptions, Category == "Carbon_factor"& Subcategory=="Diesel")$Value
  df.base[unique.ID,"Total.GHG"] <- sum(c(e.gasoline,e.diesel), na.rm = T)
  df.base[unique.ID,"MMU.ID"]<- unique.ID
  df.base[unique.ID,"location.ID"] <- location.ID
  return(df.base[unique.ID,])
}
