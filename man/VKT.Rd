\name{VKT}
\alias{VKT}
\title{
VKT estimation
}
\description{
This function estimate the VKT from a specific area.
}
\usage{
VKT(Id, Settlement, df.base, df.assumptions, mode, purpose)
}
\arguments{
  \item{Id}{
  Is the identifier of every MMU (numeric value).

}
  \item{Settlement}{
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
Before run this function, the user has to load the two datasets needed here.
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

VKT(Id = base_table_week[1,1], Settlement = base_table_week[1,2], df.base = base_table_week,
    df.assumptions = assumptions_MX,mode = "Car", purpose = "School")


{
  }
}
\keyword{ VKT_function}
