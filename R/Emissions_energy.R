Emissions_energy <- function(Id, settlement, df.base, df.assumptions){


  df.base["Car.VKT.Wk"]<- VKT(Id, Settlement, df.assumptions, mode= "Car", purpose= "Work")
  df.base["Bus.VKT.Wk"]<- VKT(Id, Settlement, df.assumptions, mode= "Bus", purpose= "Work")
  df.base["Car.VKT.Sch"]<- VKT(Id, Settlement, df.assumptions, mode= "Car", purpose= "School")
  df.base["Bus.VKT.Sch"]<- VKT(Id, Settlement, df.assumptions, mode= "Bus", purpose= "School")
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
  df.base["Total.Energy"] <- sum(df.base[c("Car.energy.gasoline", "Car.energy.diesel",
                                   "Bus.energy.gasoline","Bus.energy.diesel")])

  e.gasoline =sum(df.base[c("Car.energy.gasoline", "Bus.energy.gasoline")])*
    subset(df.assumptions, Category == "Carbon_factor"& Subcategory=="Gasoline")$Value

  e.diesel = sum(df.base[c( "Car.energy.diesel","Bus.energy.diesel")]) *
    subset(df.assumptions, Category == "Carbon_factor"& Subcategory=="Diesel")$Value
  df.base["Total.GHG"] <- e.gasoline+e.diesel
  return(df.base)
}
