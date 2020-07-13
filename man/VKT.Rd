\name{VKT}
\alias{VKT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VKT estimation
}
\description{
This function estimate the VJT from a specific area .
}
\usage{
VKT(Id, Settlement, df.base, df.assumptions, mode, purpose)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Id}{
  Is the identifier of every MMU (numeric value).

}
  \item{Settlement}{
  Is the ID identification of every urban category (i.e. distiction between urban and rural, or districts). This should be in numeric value.
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
Before run this function, the user had to preload the two datasets needed here.
}
\value{
The daily VKT from a mode of travel and a trip purpose selected.
}
\references{

}
\author{
Ricardo Ochoa
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##----Load data tables need it to run the function ----
load("Base_table_weekday_CDMX")
assumption <- write.csv("assumptions_MX")

VKT(Id = base_table_mx$ID, Settlement = base_table_mx$Settlement, df.base=base_table_mx, df.assumptions=assumptions_mx, mode= "Car", purpose = "School")

VKT(mode.trips = 78, distance= 4.06, "Work")


{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ VKT }
