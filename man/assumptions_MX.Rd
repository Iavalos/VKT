\name{assumptions_MX}
\alias{assumptions_MX}
\docType{data}
\title{
Assumptions used to run the model in the ZMVM.
}
\description{
The assumptions table used to run the VKT function.
}
\usage{data("assumptions_MX")}
\format{
  A data frame with 7 observations on the following variable.
  \describe{
    \item{\code{Category}}{The feature that indicated a specific value i.e. mode travel}
    \item{\code{Subcategory}}{The subcategory from each features i.e. car or bus}
    \item{\code{Location}}{Name of the MMU}
    \item{\code{Location.ID}}{ID of the MMU, which has to be similar to the Settlement ID from the base table}
    \item{\code{Value}}{a numeric vector}
    \item{\code{Units}}{The units from the value}
    \item{\code{Notes}}{A specification of the data, here could be incorporated the source}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(assumptions_MX)
## str(assumptions_MX) ; head(assumptions_MX)
}
\keyword{datasets}
