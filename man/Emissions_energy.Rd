\name{Emissions_energy}
\alias{Emissions_energy}
\title{
 Estimation of GHG emissions and energy use.
}
\description{
A function to estimate the GHG emissions and energy consumption from specific VKT.
}
\usage{
Emissions_energy(unique.ID, location.ID, df.base, df.assumptions)
}

\arguments{
  \item{unique.ID}{
  Is the identifier of every MMU (numeric value).
}
  \item{location.ID}{
  Is the ID identification of every urban category (I.e. distinction between urban and rural, or districts). This should be in numeric value.
}
  \item{df.base}{
  Is the table reference that contains the ID and Settlement columns as well as population density and employment density information.
}

  \item{df.assumptions}{
  Is the table reference with all the assumption values.

}
}
\value{
Incorporate new columns on the table base (df.base).
The new columns are the following:
  \item{Car.VKT.Wk}{Total daily VKT from cars with work as purpose trip}
  \item{Bus.VKT.Wk}{Total daily VKT from public transport with work as purpose trip}
  \item{Car.VKT.Sch}{Total daily VKT from cars with School as purpose trip}
  \item{Bus.VKT.Sch}{Total daily VKT from public transport with School as purpose trip}
  \item{Car.VKT}{Sum of total daily VKT from cars}
  \item{Bus.VKT}{Sum of total daily VKT from public transport}
  \item{Daily.VKT}{Sum of total daily VKT from cars and public transport}
  \item{Car.energy.gasoline}{Energy consumption from cars that use gasoline}
  \item{Car.energy.diesel}{Energy consumption from cars that use diesel}
  \item{Bus.energy.gasoline}{Energy consumption from the share of public transport that use gasoline}
  \item{Bus.energy.diesel}{Energy consumption from the share of public transport  that use diesel}
  \item{Total.Energy}{Total energy consumption from both fuels and modes}
  \item{Total.GHG}{GHG emissions from the total energy consumption}
}

\author{
Ricardo Ochoa
}


\seealso{
 \code{\link{VKT_table}}
}
\examples{
data("assumptions_MX")
data("Base_table_weekday_CDMX")

GHG <- Emissions_energy(unique.ID= base_table_week[1,1], location.ID = base_table_week[1,2],
                 df.base = base_table_week, df.assumptions = assumptions_MX)

#Estimate the complete information, including VKT, energy consumption and GHG emissions in the complete dataset for Mexico City

new.table = data.frame(matrix(ncol=0,nrow=nrow(base_table_week)))
  for (i in 1:nrow(base_table_week)){
x <- Emissions_energy(unique.ID = base_table_week[i,1], location.ID = base_table_week[i,2],
                   df.base = base_table_week, df.assumptions = assumptions_MX)
new.table <- rbind(new.table,x)


  }
print (new.table)

}

\keyword{GHG}
\keyword{energy}
