\name{VKT_table}
\alias{VKT_table}
\title{
VKT estimation to a complete table
}
\description{
This function estimate the VKT from a specific area within a complete table.
}
\usage{
VKT_table(unique.ID, location.ID, df.base, df.assumptions, mode, purpose)
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
  \item{mode}{
  Is the travel modal where we want to estimate the VKT, this can be car or public.transport and goes to relationship with the table assumptions information.

}
  \item{Purpose}{
  The purpose of the trip we want to estimate the VKT, this could be work or school.

}
}
\details{
Before to run this function, the user has to load the two datasets needed here.
}
\value{
The daily VKT from a mode of travel and a trip purpose selected.
}
\references{

}
\author{
Ricardo Ochoa
}

\examples{
##----Load data tables need it to run the function ----
data("assumptions_MX")
data("Base_table_weekday_CDMX")

# Estimate the VKT on the complete dataset of Mexico City for cars and school as purpose trip

new.table = data.frame(matrix(ncol=0,nrow=nrow(base_table_week)))
for (i in 1:nrow(base_table_week)){
  x <- VKT_table(unique.ID = base_table_week[i,1], location.ID = base_table_week[i,4], df.base = base_table_week,
           df.assumptions = assumptions_MX, mode = "Car", purpose = "School")
  new.table <- rbind(new.table,x)
  names(new.table) <- c("VKT")


}
print(new.table)

}
\keyword{ VKT_table_function}
