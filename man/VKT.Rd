\name{VKT}
\alias{single_VKT}
\title{
VKT estimation for a specific point
}
\description{
This function estimate the VKT from a specific mode, purpose and average travel distance.
}
\usage{
VKT(population, employment, distance_per_trip, trips_per_person,
    trips_per_employee,selected_mode_fraction,
    selected_purpose_fraction)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{population}{Population in selected location}
  \item{employment}{Number of jobs in selected location}
  \item{distance_per_trip}{Distance per trip per person for selected purpose}
  \item{trips_per_person}{Trips per person for selected location}
  \item{trips_per_employee}{Trips per employee for selected location}
  \item{selected_mode_fraction}{Fraction of trips for selected mode}
  \item{selected_purpose_fraction}{Fraction of trips for selected purpose}
  \item{mode}{Is the travel modal where we want to estimate the VKT, this can be a car or public transport.}
  \item{policy_elasticity}{Is the policy elasticity that will intervene in the VKTs estimation.}
  \item{Npt}{Fraction of trips for selected purpose}
}

\value{
a numerical value that represents the vehicle kilometre travelled based on the data included.
}

\author{
Ricardo Ochoa
}

\examples{
data("Base_table_ENG")
df <- Base_table_ENG[,c("population", "employment")]

#To estimate for all rows in a data frame
df$vkt <- VKT(population = df$population,
              employment = df$employment,
              distance_per_trip = 4.34,
              trips_per_person = 2.63,
              trips_per_employee = 0.5,
              selected_mode_fraction = 0.62183751,
              selected_purpose_fraction = 0.316)

#To estimate the VKT for a especific point
VKT(population = 93000, employment = 34000, distance_per_trip = 4.34, trips_per_person = 2.63,
    trips_per_employee = 0.5,selected_mode_fraction = 0.62183751,
    selected_purpose_fraction = 0.316)

#To estimate the VKT with the intervention of a policy
VKT(population = 1028,
      employment = 203,
      distance_per_trip = 4.50,
      trips_per_person = 3.9,
      trips_per_employee = 2,
      selected_mode_fraction = 0.0068,
      selected_purpose_fraction = 0.55,
      policy_elasticity = 3.5, #reduction with the new policy(\%)
      mode = "Bus", #Public transport or private vehicle
      Npt = 25) #New \% occupation


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Single_VKT }
