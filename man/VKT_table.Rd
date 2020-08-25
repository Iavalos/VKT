\name{VKT_table}
\alias{VKT_table}
\title{
VKT estimation to a complete table
}
\description{
This function estimates the VKT from a specific area using data included in a table.
}
\usage{
VKT_table(unique.ID, location.ID, df.base, df.assumptions, mode, purpose)
}
\arguments{
  \item{unique.ID}{
  Is the identifier of every Minimal Mapping Unit (numeric value).

}
  \item{location.ID}{
  Is the ID of every urban category (I.e. distinction between urban and rural, or districts). This should be in numeric value.
}
  \item{df.base}{
  Is the table reference that contains the unique ID and location ID columns as well as population and employment information.
}

  \item{df.assumptions}{
  Is the table reference with all the assumption values.

}
  \item{mode}{
  Is the travel modal where we want to estimate the VKT, this can be a car or public transport. The modal selected should be included in the assumptions table.

}
  \item{Purpose}{
  The purpose of the trip where we want to estimate the VKTs, this could be work or school.
}
  \item{policy}{
Is the policy that will intervene in the final VKTs for each MMU. The elasticity value should be included on the assumptions table, incorporating a new category called scenarios and the name of the policy (i.e. BRT) as a subcategory.

}

  \item{Npt}{
    Is the new share of occupancy in a bus or the public transport mode modeled after the incorporation of the policy.

}
}
\details{
Before running this function, the user has to load the two datasets needed here.
}
\value{
The daily VKTs related to the mode of travel and a trip purpose selected.
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

# Estimate the VKTs for the complete dataset of Mexico City for cars, and using school as the purpose of the trip.

new.table = data.frame(matrix(ncol=0,nrow=nrow(base_table_week)))

for (i in 1:nrow(base_table_week)){
  x <- VKT_table(unique.ID = base_table_week[i,1], location.ID = base_table_week[i,2], df.base = base_table_week,
           df.assumptions = assumptions_MX, mode = "Car", purpose = "School", policy= NA, Npt= NA)
  new.table <- rbind(new.table,x)
  names(new.table) <- c("VKT")


}
print(new.table)

}
\keyword{ VKT_table_function}
