% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rphysunits.R
\name{convert}
\alias{convert}
\title{convert}
\usage{
convert(a, unit_from, unit_to)
}
\arguments{
\item{a}{: physical_quantity}

\item{unit_from}{: character with the original units}

\item{unit_to}{: character with the target units}
}
\value{
list
}
\description{
Function for unit conversion
}
\examples{
v1 <- 5.6*u_kg
v1a <- convert(v1, "gram", "pound_mass")
}
\author{
Oscar Garcia-Cabrejo \email{khaors@gmail.com}
}

