% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rphysunits.R
\name{lines.physical_quantity}
\alias{lines}
\alias{lines.physical_quantity}
\title{lines}
\usage{
\\method{lines}{physical_quantity}(x, y, ...)
}
\arguments{
\item{x, y}{: coordinate vectors of points to join}

\item{...}{: Further graphical parameters to be supplied to the base lines function}
}
\description{
A generic function taking coordinates given in various ways and joining the corresponding points with line segments.
}
\examples{
#Plot Force in function of time
time <- seq(0,100,1)*u_s
accel <- 9.8*u_m/(u_s^2)-(0.05*u_m/(u_s^3))*time
force <- 5.3*u_kg*accel
plot(time,force, type = "p")
force1 <- 2.3*u_kg*accel
lines(time, force1, col = "red")
}

