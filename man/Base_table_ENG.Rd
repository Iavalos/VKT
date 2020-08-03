\name{Base_table_ENG}
\alias{Base_table_ENG}
\docType{data}
\title{
Base table with information from England, UK divided by Regions and Local Authority Districts (LAD).
}
\description{
A database processed with the information required to estimate the VKT, GHG emissions and energy consumption.
}
\usage{data("Base_table_ENG")}
\format{
  A data frame with 317 observations on the following 6 variables.
  \describe{
    \item{\code{MMU.ID}}{a numeric vector to identify the specific MMU}
    \item{\code{REGION}}{English Region category where the MMU is located}
    \item{\code{LAD}}{The Local Authority District where the MMU is located}
    \item{\code{location.ID}}{An specific ID from the location where the minimal mapping unit is located}
    \item{\code{population}}{Population density of the MMU}
    \item{\code{employment}}{Number of jobs of the MMU}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
National Travel Survey of England, 2017
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Base_table_ENG)
## str(Base_table_ENG) ; head(Base_table_ENG)
}
\keyword{ENG_data}
