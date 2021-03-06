% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rphysunits.R
\name{plot.physical_quantity}
\alias{plot.physical_quantity}
\title{plot.physical_quantity}
\usage{
\method{plot}{physical_quantity}(x, y, xlab, ylab, ...)
}
\arguments{
\item{x}{: x coordinates in the plot. It is a vector of physical quantities.}

\item{y}{: y coordinates in the plot. It is a vector of physical quantities.}

\item{xlab}{: a title for the x axis}

\item{ylab}{: a title for the y axis}

\item{...}{: Arguments to be passed to the conventional plot function}
}
\description{
Function to plot a vector of physical_quantities
}
\examples{
#Plot Force in function of time
time <- seq(0,100,1)*u_s
accel <- 9.8*u_m/(u_s^2)-(0.05*u_m/(u_s^3))*time
force <- 5.3*u_kg*accel
plot(time,force, type = "p")
}
\author{
Oscar Garcia-Cabrejo \email{khaors@gmail.com}
}

