Policies_elasticities <- function(unique.ID, location.ID, df.base, df.assumptions, policy, saturation_policy, Npt){

  df.base[unique.ID,"Car.VKT.Wk_policy"]<- VKT_table(unique.ID = unique.ID, location.ID = location.ID, 
                                                     df.base = df.base,df.assumptions = df.assumptions, 
                                                     mode = "Car", purpose = "Work",policy = policy, 
                                                     share = saturation_policy, Npt = NA)
  df.base[unique.ID,"Bus.VKT.Wk_policy"]<- VKT_table(unique.ID = unique.ID, location.ID = location.ID, 
                                                     df.base = df.base, df.assumptions=df.assumptions, 
                                                     mode= "Bus", purpose= "Work", policy=policy,
                                                     share = saturation_policy,Npt=Npt)
  df.base[unique.ID,"Car.VKT.Sch_policy"]<- VKT_table(unique.ID = unique.ID, location.ID = location.ID, 
                                                      df.base = df.base,df.assumptions=df.assumptions, 
                                                      mode= "Car", purpose= "School", policy=policy,
                                                      share = saturation_policy,Npt=NA)
  df.base[unique.ID,"Bus.VKT.Sch_policy"]<- VKT_table(unique.ID = unique.ID, location.ID = location.ID, 
                                                      df.base = df.base, df.assumptions=df.assumptions, 
                                                      mode= "Bus", purpose= "School", policy=policy,
                                                      share = saturation_policy, Npt=Npt)
  
  df.base["Car.VKT_policy"] <- df.base["Car.VKT.Wk_policy"] + df.base["Car.VKT.Sch_policy"]
  df.base["Bus.VKT_policy"] <-  df.base["Bus.VKT.Wk_policy"] + df.base["Bus.VKT.Sch_policy"]
  df.base["Daily.VKT_policy"] <- df.base["Car.VKT_policy"] + df.base["Bus.VKT_policy"]
  
  df.base[unique.ID,"MMU.ID"]<- unique.ID
  df.base[unique.ID,"location.ID"] <- location.ID
  df.base$population<- NULL
  df.base$employment <- NULL
  
  return(df.base[unique.ID,])
}

