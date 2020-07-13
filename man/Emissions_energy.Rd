\name{Emissions_energy}
\alias{Emissions_energy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Estimation of GHG emissions and energy use.
}
\description{
A funtion to estimate the GHG emissions and energy consumption from specific VKT.
}
\usage{
Emissions_energy(Id, settlement, df.base, df.assumptions)
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
