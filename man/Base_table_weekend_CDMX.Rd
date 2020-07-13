\name{Base_table_weekend_CDMX}
\alias{Base_table_weekend_CDMX}
\docType{data}
\title{
Base table with information from the Metropolitan Area of Mexico City divided by district on weekends.
}
\description{
A database processed with the information required to estimate the VKT, GHG emissions and energy consumption.
}
\usage{data("Base_table_weekend_CDMX")}
\format{
  A data frame with 194 observations on the following 4 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{settlement}}{The ID from the district analysed, which is the minimal mapping unit of this dataset}
    \item{\code{pop_point}}{Population density of the MMU}
    \item{\code{jobs_perpo}}{Number of jobs of the MMU}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Encuesta Origen Destino en Hogares de la Zona Metropolitana del Valle de México (EOD) 2017, INEGI (2018).
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Base_table_weekend_CDMX)
## maybe str(Base_table_weekend_CDMX) ; head(Base_table_weekday_CDMX)
}
\keyword{EOD_weekend}
