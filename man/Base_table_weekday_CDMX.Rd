\name{Base_table_weekday_CDMX}
\alias{Base_table_weekday_CDMX}
\docType{data}
\title{
Base table with information from the Metropolitan Area of Mexico City divided by district on weekdays.
}
\description{
A database processed with the information required to estimate the VKT, GHG emissions and energy consumption.
}
\usage{data("Base_table_weekday_CDMX")}
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
data(Base_table_weekday_CDMX)
## str(Base_table_weekday_CDMX) ; head(Base_table_weekday_CDMX)
}
\keyword{EOD_week}
