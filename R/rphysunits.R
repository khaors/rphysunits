#' rphysunits: A Package to define and operate physical quantities in the SI system within R.
#'
#' The rphysunits package includes functions to operate on:
#' - physical_quantities: A number with an associated set of units. These physical_quantity
#'   objects are defined using the prefix u_
#' - physical_constants: A specific constant with its corresponding units. These constants are
#'   defined using the prefix c_
#'
#' @section rphysunits functions:
#' The rphysunits functions are used to define and operate physical_quantities
#'
#' @docType package
#' @name rphysunits
#'
#require('stringi')
#require('graphics')
#' @title base
#' @name base
#' @description base variable
NULL
#' @title zero
#' @name zero
#' @description base variable
NULL
#' @title unit_labels
#' @name unit_labels
#' @description base variable
NULL
load("R/sysdata.rda")
#' @title
#' physical_quantity
#' @description
#' Define a physical quantity
#' @param value : The magnitude of the quantity
#' @param units : vector to define the specific dimensions of the quantity to create
#' @return A physical quantity with a given value and units
#' @usage physical_quantity(value, units)
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' length <- 1.5*u_m
physical_quantity <- function(value = 0.0, units = vector("numeric",length=8)){
  res <- list(value = value, units = units)
  attr(res, 'class') <- "physical_quantity"
  invisible(res)
  return(res)
}
#' @title
#' get_value
#' @description
#' Function to get the value from a physical_quantity object
#' @param q : A physical_quantity object
#' @return A numeric variable or vector
#' @usage get_value(q)
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' length <- 1.5*u_m
#' get_value(length)
get_value <- function(q){
  q$value
}
#' @title
#' get_units
#' @description
#' Function to get the units of a physical_quantity
#' @param q : A physical_quantity object
#' @return A character vector
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' a <- 5.6*u_m
#' get_units(a)
get_units <- function(q){
  q$units
}
#' @title
#' same_units
#' @description
#' Function to check if two physical_quantity objects have the same units
#' @param q1,q2 : Physical_quantity objects
#' @return A logical value
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' a1 <- 5.6*u_m
#' a2 <- 4.5*u_s
#' same_units(a1, a2)
same_units <- function(q1, q2){
  u1 <- get_units(q1)
  u2 <- get_units(q2)
  res <- FALSE
  if(sum(u1 == u2) == 8){
    res <- TRUE
  }
  return(res)
}
#' @title
#' *.physical_quantity
#' @description
#' Define the product operation of physical_quantity class
#' @param a,b : these can be a numeric object and/or a physical_quantity object
#' @return A physical quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' distance1 <- 1.5*u_m
#' var <- u_m*u_s
`*.physical_quantity` <- function(a, b) {
  if( class(a) == "numeric" & class(b) == "physical_quantity"){
    tmp <- physical_quantity(a*b$value, units = b$units)
  }
  if(class(a) == "physical_quantity" & class(b) == "numeric"){
    tmp <- physical_quantity(a$value*b, units = a$units)
  }
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    tmp <- physical_quantity(a$value * b$value, a$units + b$units)
  }
  res <- tmp
  return(res)
}
#' @title
#' /.physical_quantity
#' @description
#' Define the division operation of physical_quantity class
#' @param a,b : these can be a numeric object and/or a physical_quantity object
#' @return a physical_quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' vel <- 2.5*u_m/u_s
#' den <- 2.6*u_gram/u_cubic_centimeter
`/.physical_quantity` <- function(a, b) {
  # real/quantity
  if( class(a) == "numeric" & class(b) == "physical_quantity"){
    tmp <- physical_quantity(a/b$value, units = -b$units)
  }
  # quantity/real
  if(class(a) == "physical_quantity" & class(b) == "numeric"){
    tmp <- physical_quantity(a$value/b, units = a$units)
  }
  #quantity/quantity
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    tmp <- physical_quantity(a$value / b$value, a$units - b$units)
  }
  res <- tmp
  return(res)
}
#' @title
#' +.physical_quantity
#' @description
#' Define the sum operation of physical_quantity class
#' @param a,b : these can be a numeric object and/or a physical_quantity object
#' @return A physical quantity
#' @usage a + b
#' @aliases +
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' dist1 <- 3.4*u_m
#' dist2 <- 450*u_cm
#' dist3 <- dist1 + dist2
`+.physical_quantity` <- function(a,b){
  # real + quantity
  if(class(a) == "numeric" & class(b) == "physical_quantity"){
    stop("Attemp to add a real number and a physical quantity")
  }
  # quantity + real
  if(class(a) == "physical_quantity" & class(b) == "numeric") {
    stop("Attemp to add a physical quantity and a real number")
  }
  # quantity + quantity
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    delta_units <- max(abs(a$units - b$units))
    if(delta_units > 1.0e-16) {
      stop("Attemp to add inconsistent units")
    }
    tmp <- physical_quantity(a$value + b$value, a$units)
  }
  res <- tmp
  return(res)
}
# Here is the problem(-.physical_quantity)

#' @title
#' `-.physical_quantity`
#' @name minus.physical_quantity
#' @description
#' Define the substraction operation of physical_quantity class
#' @param a,b : these can be a numeric object and/or a physical_quantity object
#' @return A physical_quantity
#' @usage a-b
#' @aliases -
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' dist1 <- 5.7*u_m
#' dist2 <- 1.2*u_ft
#' dist3 <- dist1-dist2
`-.physical_quantity` <- function(a, b){
  # real - physical_quantity
  if(class(a) == "numeric" & class(b) == "physical_quantity"){
    stop("Attemp to substract a physical_quantity from a real number")
  }
  # physical_quantity - real
  if(class(a) == "physical_quantity" & class(b) == "numeric") {
    stop("Attemp to substract a real number from a physical quantity")
  }
  # physical_quantity - physical_quantity
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    delta_units <- max(abs(a$units - b$units))
    if(delta_units > 1.0e-16) {
      stop("Attemp to substract two physical quantities with inconsitent units")
    }
    tmp <- physical_quantity(a$value - b$value, units = a$units)
  }
  res <- tmp
  return(res)
}
#
#`-.physical_quantity` <- function(a){
#  tm <- physical_quantity(-a$value, a$units)
#}

#' @title
#' **.physical_quantity
#' @description
#' Define the power operation of physical_quantity class
#' @param a,b : these can be a numeric object and/or a physical_quantity object
#' @return A physical quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' dist <- 1.5*u_m
#' area <- dist**2
`**.physical_quantity` <- function(a, b){
  if(class(a) == "physical_quantity" & class(b) == "numeric"){
    tmp <- physical_quantity(a$value**b, units = b*a$units)
  }
  # real^quantity (quantity is dimensionless)
  if(class(a) == "numeric" & class(b) == "physical_quantity"){
    delta_units <- max(abs(b$units))
    if(delta_units > 1.0e-16){
      stop("Attemp to raise a number to a quantity with dimensions")
    }
    tmp <- physical_quantity(a**b$value, units = b$units)
  }
  #quantity ^ quantity
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    delta_units <- max(abs(b$units))
    if(delta_units > 1.0e-16){
      stop("Attemp to raise a quantity to a quantity with dimensions")
    }
    tmp <- physical_quantity(a$value**b$value, units = a$units)
  }
  res <- tmp
  return(res)
}
#' @title
#' exponentiation operator
#' @description
#' Function to apply the exponentiation operator to physical quantities
#' @param a : Physical quantity object
#' @param b : It can be a physical quantity object (dimensionless) or a number
#' @return:
#' The result of this function is a physical quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' m <- 1.5*u_kg
#' v <- 3.2*u_m/u_s
#' v2 <- v^2
#' energy <- 0.5*m*v2
#' energy
`^.physical_quantity` <- function(a, b){
  if(class(a) == "physical_quantity" & class(b) == "numeric"){
    tmp <- physical_quantity(a$value**b,b*a$units)
  }
  # real^quantity (quantity is dimensionless)
  if(class(a) == "numeric" & class(b) == "physical_quantity"){
    delta_units <- max(abs(b$units))
    if(delta_units > 1.0e-16){
      stop("Attemp to raise a number to a quantity with dimensions")
    }
    tmp <- physical_quantity(a**b$value, units = b$units)
  }
  #quantity ^ quantity
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    delta_units <- max(abs(b$units))
    if(delta_units > 1.0e-16){
      stop("Attemp to raise a quantity to a quantity with dimensions")
    }
    tmp <- physical_quantity(a$value**b$value,units=a$units)
  }
  res <- tmp
  return(res)
}
#' @title
#' abs.physical_quantity
#' @description
#' Absolute value of a physical quantity
#' @param x : physical quantity
#' @return
#' The absolute value of a physical quantity
#' @import devtools
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' a <- physical_quantity(-5.4)
#' a <- a * u_m
#' a <- abs(a)
#' a
abs.physical_quantity <- function(x){
  res <- physical_quantity(abs(x$value),x$units)
  return(res)
}
#' @title
#' sqrt
#' @description
#' Square root of a physical quantity
#' @param x : Physical quantity object
#' @return
#' the square root of a physical quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' area <- 1.7*u_m*u_m
#' length <- sqrt(area)
#' length
sqrt.physical_quantity <- function(x){
  res <- physical_quantity(sqrt(x$value),x$units/2)
  return(res)
}

#' @title
#' `>.physical_quantity`
#' @description
#' Operator to check if a physical quantity is greater than another
#' of the same units
#' @param a,b : physical quantities
#' @return A logical variable indicating if a is greater than b
#' @usage `>.physical_quantity(a,b)`
#' @aliases >.physical_quality
#' @examples
#' a <- 5.4 * u_m
#' b <- 3.2 * u_m
#' c <- 8.9 * u_s
#' a > b
#' a > c
`>.physical_quantity` <- function(a, b){
  res <- FALSE
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    check_value <- a$value > b$value
    check_units <- sum(a$units == b$units) == 8
    res <- check_value & check_units
  }
  if(class(a) == "physical_quantity" & class(b) == "numerical"){
    res <- a$value > b
  }
  if(class(a) == "numeric" & class(b) == "physical_quantity"){
    res <- a > b$value
  }
  return( res )
}
#' @title
#' <.physical_quantity
#' @description
#' Operator to  check if a physical quantity is less than another
#' of the same untis
#' @param a,b : numerical values and or physical quantities
#' @return A logical variable indicating if a is less than b
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' side1 <- 4.6 * u_m
#' side2 <- 3.4 * u_m
#' side2 < side1
#' side2 < 9.0
`<.physical_quantity` <- function(a, b){
  res <- FALSE
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    check_values <- a$value < b$value
    check_units <- sum(a$units == b$units) == 8
    res <- check_values & check_units
  }
  if(class(a) == "numeric" & class(b) == "physical_quantity"){
    res <- a < b$value
  }
  if(class(a) == "physical_quantity" & class(b) == "numeric"){
    res <- a$value < b
  }
  return(res)
}
#' @title
#' >=.physical_quantity
#' @description
#' Operator to check if a physical quantity is greater than or equal to
#' another physical quantity of the same units or a numeric value
#' @param a,b : physical quantities or numeric values
#' @return A logical variable specifying if a is greater than or equal to b
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' side1 <- 1.5 * u_m
#' side2 <- 3.4 * u_m
#' side1 >= side2
#' side1 >= 3.4
`>=.physical_quantity` <- function(a, b){
  res <- FALSE
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    check_values <- a$value > b$value
    check_values2 <- abs(a$value - b$value) < 1.0e-15
    check_units <- sum(a$units == b$units) == 8
    res <- (check_values | check_values2) & check_units
  }
  return(res)
}
#' @title
#' <=.physical_quantity
#' @description
#' Operator to check if a physical quantity is less or equal to another
#' physical quantity or numerical value
#' @param a,b : physical quantity or numerical value
#' @return A logical value specifying if a <= b
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' side1 <- 3.4 * u_m
#' side2 <- 6.7 *u_m
#' side1 <= side2
`<=.physical_quantity` <- function(a, b){
  return(a$value < b$value | abs(a$value - b$value) < 1.0e-15)
}
#' @title
#' ==.physical_quantity
#' @description
#' Function to compare equality of physical_quantities
#' @param a,b : A physical_quantity objects
#' @return Logical
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' a <- 3.4 * u_m
#' b <- 3.5 * u_m
#' a==b
`==.physical_quantity` <- function(a, b){
  res <- FALSE
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity"){
    check_values <- abs(a$value-b$value) < 1.0e-15
    check_units <- sum(a$units == b$units) == 8
    res <- check_values & check_units
  }
  if(class(a) == "physical_quantity" & class(b) == "numeric"){
    res <- abs(a$value-b) < 1.0e-15
  }
  if(class(a) == "numeric" & class(b) == "physical_quantity"){
    res <- abs(a-b$value) < 1.0e-15
  }
  return(res)
}
#' @title
#' is equal operator
#' @name is_equal
#' @description
#' Function to
#' @param a,b : Physical_quantity objects to be compared
#' @return Logical
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' a <- 3.4 * u_m
#' b <- 5.6 * u_m
#' a!=b
`!=.physical_quantity` <- function(a, b) {
  res <- FALSE
  if(class(a) == "physical_quantity" & class(b) == "physical_quantity") {
    res <- !(a==b)
  }
  if(class(a) == "numeric" & class(b) == "physical_quantity"){
    res <- !(a==b$value)
  }
  if(class(a) == "physical_quantity" & class(b) == "numeric"){
    res <- !(a$value==b)
  }
  return(res)
}
#' @title
#' print physical_quantity
#' @description
#' Function to display a physical_quantity object on the console
#' @param x : A physical_quantity
#' @param ... : Other parameters used in the underlying print function
#' @return Nothing
#' @usage  \\method{print}{physical_quantity}(x, ...)
#' @aliases print
#' @import stringi
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' dist <- 1*u_m
#' print(dist)
#'

print.physical_quantity <- function (x, ...) {
  if(class(x) != "physical_quantity"){
    stop("A physical_quantity is required as input")
  }
  cat("(Physical Quantity) \n")
  cat("Value = ")
  cat(signif(x$value))
  cat("\n")
  cat("Units= ")
  res.unit_label <- get_unit_label(x)
  res.unit_label <- as.character(res.unit_label)%s+%"\n"
  cat(res.unit_label);
}
#S3method(print, physical_quantity)
#' @title
#' get_unit_label
#' @description
#' Function to get the unit label
#' @param x : physical_quantity
#' @return list with the unit label
#' @usage get_unit_label(x)
#' @aliases get_unit_label
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' v <- 4.56 * u_m/u_s
#' v.label <- get_unit_label(v)
#' v[1]
get_unit_label <- function(x){
  if(class(x) != "physical_quantity"){
    stop("A physical_quantity is required as input")
  }
  current_units <-x$units/base
  pos_valid <- abs(current_units) > 0
  nvalid <- length(pos_valid)
  res_units <- ""
  for(i in 1:nvalid){
    if(pos_valid[i]){
      if(current_units[i] != 1){
        res_units <- res_units%s+%unit_labels[i]%s+%"^"%s+%current_units[i]%s+%" "
      }
      else{
        res_units <- res_units%s+%unit_labels[i]%s+%" "
      }
    }
  }
  return(res_units)
}
#' @title
#' as.physical_quantity
#' @description
#' Function to coerce an object into a physical_quantity
#' @param v : a vector or list with values
#' @return A physical_quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' a <- as.physical_quantity(0.5)
as.physical_quantity <- function(v){
  if(class(v) == "numeric"){
    q <- physical_quantity(v)
  }
  if(class(v) == "list"){
    if("Value" %in% names(v) & "Units" %in% names(v)){
      q <- v$Value*eval(parse(text = "u_"%s+%v$Units))
    }
  }
  return(q)
}

#as.physical_quantity <- function(v){
#  UseMethod("as.physical_quantity")
#}


#' @title
#' as.numeric
#' @description
#' Function to coerce a physical_quantity into a numeric
#' @param x : physical_quantity
#' @param ... : Other parameters
#' @return a numeric object
#' @usage \\method{as.numeric}{physical_quantity}(x, ...)
#' @aliases as.numeric.physical_quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' v1 <- 5.6*u_km
#' v1n <- as.numeric(v1)

as.numeric <- function(x, ...){
  UseMethod("as.numeric")
}

as.numeric.physical_quantity <- function(x, ...){
  if(class(x) == "physical_quantity"){
    v <- x$value
  }
  return(v)
}
#' @title
#' is.na
#' @description
#' A function to check if the value of a physical_quantity object is NA
#' @param x : A physical_quantity object
#' @return A logical value
#' @usage \\method{is.na}{physical_quantity}(x)
#' @aliases is.na
#' @author
#' Oscar Garcia-Cabrejo
#' @examples
#' a <- 4.3*u_m
#' is.na(a)
is.na.physical_quantity <- function(x) {
  is.na(x$value)
}
#' @title
#' is.physical_quantity
#' @description
#' A function to check if a given variable is
#' @name is.physical_quantity
#' @param q : A physical_quantity object
#' @return A logical value
#' @author
#' Oscar Garcia-Cabrejo
#' @examples
#' a <- 5.6*u_m
#' is.physical_quantity(a)
is.physical_quantity <- function(q){
  res <- FALSE
  if(class(q) == "physical_quantity"){
    res <- TRUE
  }
  return(res)
}
#' @title
#' convert.physical_quantity
#' @description
#' Function for unit conversion
#' @param a : physical_quantity
#' @param unit_from : character with the original units
#' @param unit_to : character with the target units
#' @return list
#' @usage \\method{convert}{physical_quantity}(a, unit_from, unit_to)
#' @aliases convert.physical_quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' v1 <- 5.6*u_kg
#' v1a <- convert(v1, "gram", "pound_mass")
convert.physical_quantity <- function(a, unit_from, unit_to){
  tmp <- a / eval(parse(text = "u_"%s+%unit_from))
  res <- tmp / eval(parse(text = "u_"%s+%unit_to))
  res1 <- list(Value = res$value, Units = unit_to)
  return(res1)
}
#' @title
#' convert
#' @description
#' Function for unit conversion
#' @param a : physical_quantity
#' @param unit_from : character with the original units
#' @param unit_to : character with the target units
#' @return list
#' @usage convert(a, unit_from, unit_to)
#' @aliases convert
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' v1 <- 5.6*u_kg
#' v1a <- convert(v1, "gram", "pound_mass")
convert <- function(a, unit_from, unit_to){
  UseMethod("convert")
}

#' @title
#' is_nondimensional
#' @description
#' Function to check if a physical quantity is nondimensional
#' @param q : physical_quantity
#' @return logical
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' v1 <- physical_quantity(5.8)
#' is_nondimensional(v1)
is_nondimensional <- function(q){
  res <- FALSE
  if(class(q) != "physical_quantity"){
    stop("The input variable is not a physical quantity")
  }
  delta_units <- max(abs(q$units))
  if(delta_units < 1.0e-16){
    res <- TRUE
  }
  return(res)
}
#' @title
#' set_units
#' @description
#' Function to set the units of a physical quantity
#' @param q : physical_quantity
#' @param units_label : character
#' @return A physical_quantity
#' @usage set_units(q, units_label)
#' @aliases set_unit
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' v <- physical_quantity(3.4)
#' set_units(v, "length")
set_units <- function(q, units_label){
  UseMethod("set_units")
}
#' @title
#' set_units.physical_quantity
#' @description
#' Function to set the units of a physical quantity
#' @param q : physical_quantity
#' @param units_label : character
#' @return A physical_quantity
#' @usage \\method{set_units}{physical_quantity}(q, units_label)
#' @aliases set_units.physical_quantity
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' v <- physical_quantity(3.4)
#' set_units(v, "length")
set_units.physical_quantity <- function(q, units_label){
  units <- vector("numeric", length = 8)
  units[1:8] <- 0.0
  if(is_nondimensional(q)){
    if(units_label == "length"){
      units[1] <- base
    }
    else if(units_label == "mass"){
      units[2] <- base
    }
    else if(units_label == "time"){
      units[3] <- base
    }
    else if(units_label == "temp"){
      units[4] <- base
    }
    else if(units_label == "intensity"){
      units[5] <- base
    }
    else if(units_label == "mol"){
      units[6] <- base
    }
    else if(units_label == "candela" | unit_labels == "cand" | units_label == "cd"){
      units[7] <- base
    }
    else if(units_label == "rad" | units_label == "angle" ) {
      units[8] <- base
    }
    q$units <- units
  }
  else{
    stop("Attemp to modify units in a physical_quantity that already has")
  }
  return(q)
}
#' @title
#' get_units.physical_quantity
#' @description
#' Function to get  the units of a physical quantity
#' @param q : physical_quantity
#' @usage get_units.physical_quantity(q)
#' @return character
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @examples
#' l <- 1.5*u_m
#'

get_units.physical_quantity <- function(q){
  if(class(q) == "physical quantity"){
    pos_units <- abs(q$units) != 0
  }
}
#' @title
#' plot.physical_quantity
#' @description
#' Function to plot a vector of physical_quantities
#' @param x : x coordinates in the plot. It is a vector of physical quantities.
#' @param y : y coordinates in the plot. It is a vector of physical quantities.
#' @param xlab : a title for the x axis
#' @param ylab : a title for the y axis
#' @param ... : Arguments to be passed to the conventional plot function
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @importFrom graphics plot
#' @import stringi
#' @examples
#' #Plot Force in function of time
#' time <- seq(0,100,1)*u_s
#' accel <- 9.8*u_m/(u_s^2)-(0.05*u_m/(u_s^3))*time
#' force <- 5.3*u_kg*accel
#' plot(time,force, type = "p")
plot.physical_quantity <- function(x, y, xlab, ylab, ...){
  if(class(x) != "physical_quantity" | class(y) != "physical_quantity" ){
    stop("Variables must be physical_quantities")
  }
  xv <- x$value
  yv <- y$value
  lx <- length(xv)
  ly <- length(yv)
  if(lx != ly){
    stop("Input vector must be of the same size")
  }
  if(!missing(x)){
    x.unit_label <- get_unit_label(x)
  }
  if(!missing(y)){
    y.unit_label <- get_unit_label(y)
  }
  if(missing(xlab)) {
    xlab <- deparse(substitute(x))
  }
  if(missing(ylab)){
    ylab <- deparse(substitute(y))
  }
  if(exists("x.unit_label")){
    xlab <- xlab %s+%"("%s+%x.unit_label%s+%")"
  }
  if(exists("y.unit_label")){
    ylab <- ylab %s+%"("%s+%y.unit_label%s+%")"
  }
  plot(xv, yv, xlab = xlab, ylab = ylab,...)
}
#' @title
#' lines
#' @description
#' A generic function taking coordinates given in various ways and joining the corresponding points with line segments.
#' @param x,y : coordinate vectors of points to join
#' @param ... : Further graphical parameters to be supplied to the base lines function
#' @usage \\method{lines}{physical_quantity}(x, y, ...)
#' @aliases lines
#' @importFrom graphics lines
#' @examples
#' #Plot Force in function of time
#' time <- seq(0,100,1)*u_s
#' accel <- 9.8*u_m/(u_s^2)-(0.05*u_m/(u_s^3))*time
#' force <- 5.3*u_kg*accel
#' plot(time,force, type = "p")
#' force1 <- 2.3*u_kg*accel
#' lines(time, force1, col = "red")
lines.physical_quantity <- function(x, y, ...){
  if(class(x) != "physical_quantity" | class(y) != "physical_quantity" ){
    stop("Variables must be physical_quantities")
  }
  xv <- x$value
  yv <- y$value
  lx <- length(xv)
  ly <- length(yv)
  if(lx != ly){
    stop("Input vector must be of the same size")
  }
  lines(xv,yv,...)
}
#' @section SI Units:
#' In this section, a set of objects defining units and constants
#'
#' @docType package
#' @name rphysunits

#' @title Basic SI units
#' @name basic_si
#' @description Basic units
#' @param 1.0,units : value and units
#'
NULL
#' @title u_meter
#' @name u_meter
#' @description  1 meter
#' @rdname basic_si
u_meter    <- physical_quantity(1.0, units = c(base,zero,zero,zero,zero,zero,zero,zero))
#' @rdname basic_si
u_kilogram <- physical_quantity(1.0, units = c(zero,base,zero,zero,zero,zero,zero,zero))
#' @rdname basic_si
u_second   <- physical_quantity(1.0, units = c(zero,zero,base,zero,zero,zero,zero,zero))
#' @rdname basic_si
u_kelvin   <- physical_quantity(1.0, units = c(zero,zero,zero,base,zero,zero,zero,zero))
#' @rdname basic_si
u_ampere   <- physical_quantity(1.0, units = c(zero,zero,zero,zero,base,zero,zero,zero))
#' @rdname basic_si
u_mole     <- physical_quantity(1.0, units = c(zero,zero,zero,zero,zero,base,zero,zero))
#' @rdname basic_si
u_candela  <- physical_quantity(1.0, units = c(zero,zero,zero,zero,zero,zero,base,zero))
#' @rdname basic_si
u_radian   <- physical_quantity(1.0, units = c(zero,zero,zero,zero,zero,zero,zero,zero))
#' @title SI_prefix
#' @name si_prefix
#' @description Prefixes of the SI units
NULL
#' @rdname si_prefix
u_yotta <- 1.0e+24
#' @rdname si_prefix
u_zetta <- 1.0e+21
#' @rdname si_prefix
u_exa   <- 1.0e+18
#' @rdname si_prefix
u_peta  <- 1.0e+15
#' @rdname si_prefix
u_tera  <- 1.0e+12
#' @rdname si_prefix
u_giga  <- 1.0e+09
#' @rdname si_prefix
u_mega  <- 1.0e+06
#' @rdname si_prefix
u_kilo  <- 1.0e+03
#' @rdname si_prefix
u_hecto <- 1.0e+02
#' @rdname si_prefix
u_deka  <- 1.0e+01
#' @rdname si_prefix
u_deca <- 1.0e+01
#' @rdname si_prefix
u_deci  <- 1.0e-01
#' @rdname si_prefix
u_centi <- 1.0e-02
#' @rdname si_prefix
u_milli <- 1.0e-03
#' @rdname si_prefix
u_micro <- 1.0e-06
#' @rdname si_prefix
u_nano  <- 1.0e-09
#' @rdname si_prefix
u_pico  <- 1.0e-12
#' @rdname si_prefix
u_femto <- 1.0e-15
#' @rdname si_prefix
u_atto  <- 1.0e-18
#' @rdname si_prefix
u_zepto <- 1.0e-21
#' @rdname si_prefix
u_zocto <- 1.0e-24
#' @rdname si_prefix

#' @title SI_basic_units_short
#' @name si_basic_units_short
NULL
#' @title u_m
#' @name u_m
#' @description Short version of basic SI units
#' @rdname si_basic_units_short
u_m   <- u_meter
#' @rdname si_basic_units_short
u_kg  <- u_kilogram
#' @rdname si_basic_units_short
u_s   <- u_second
#' @rdname si_basic_units_short
u_K   <- u_kelvin
#' @rdname si_basic_units_short
u_A   <- u_ampere
#' @rdname si_basic_units_short
u_mol <- u_mole
#' @rdname si_basic_units_short
u_cd  <- u_candela

#' @title SI_derived_units
#' @name si_derived_units1
#' @description First set of derived units
NULL
#' @rdname si_derived_units1
u_steradian <- u_radian ** 2
#' @rdname si_derived_units1
u_degree    <- u_radian * pi / 180.0
#' @rdname si_derived_units1
u_hertz     <- 2.0*pi * u_radian / u_second
#' @rdname si_derived_units1
u_newton    <- u_kilogram * u_meter / u_second ** 2
#' @rdname si_derived_units1
u_pascal    <- u_newton / u_meter ** 2
#' @rdname si_derived_units1
u_joule     <- u_newton * u_meter
#' @rdname si_derived_units1
u_watt      <- u_joule / u_second
#' @rdname si_derived_units1
u_coulomb   <- u_ampere * u_second  # = u_watt / u_ampere
#' @rdname si_derived_units1
u_volt      <- u_joule / u_coulomb
#' @rdname si_derived_units1
u_farad     <- u_coulomb / u_volt
#' @rdname si_derived_units1
u_ohm       <- u_volt / u_ampere
#' @rdname si_derived_units1
u_siemens   <- u_ampere / u_volt
#' @rdname si_derived_units1
u_weber     <- u_volt * u_second
#' @rdname si_derived_units1
u_tesla     <- u_weber / u_meter ** 2
#' @rdname si_derived_units1
u_henry     <- u_weber / u_ampere
#' @rdname si_derived_units1
u_lumen     <- u_candela * u_steradian
#' @rdname si_derived_units1
u_lux       <- u_lumen / u_meter ** 2
#' @title SI_derived_units1_short
#' @name si_derived_units1_short
#' @description First set of derived units (short)
NULL
#' @rdname si_derived_units1_short
u_sr  <- u_steradian
#' @rdname si_derived_units1_short
u_deg <- u_degree
#' @rdname si_derived_units1_short
u_Hz  <- u_hertz
#' @rdname si_derived_units1_short
u_N   <- u_newton
#' @rdname si_derived_units1_short
u_Pa  <- u_pascal
#' @rdname si_derived_units1_short
u_J   <- u_joule
#' @rdname si_derived_units1_short
u_W   <- u_watt
#' @rdname si_derived_units1_short
u_C   <- u_coulomb
#' @rdname si_derived_units1_short
u_V   <- u_volt
#' @rdname si_derived_units1_short
u_F   <- u_farad
#' @rdname si_derived_units1_short
u_Wb  <- u_weber
#' @rdname si_derived_units1_short
u_T   <- u_tesla
#!$    u_H   = u_henry
#' @rdname si_derived_units1_short
u_lm  <- u_lumen
#' @rdname si_derived_units1_short
u_lx  <- u_lux

#' @title SI_physical_constants
#' @name si_physical_constants
#' @description Physical constants
NULL
#' @rdname si_physical_constants
c_speed_of_light        <- 2.99792458e+8 * u_meter / u_second
#' @rdname si_physical_constants
c_magnetic              <- 4.0*pi * 1.0e-7 * u_newton / u_ampere**2
#                            1.2566370614e-6 * u_newton / u_ampere**2
#' @rdname si_physical_constants
c_electric              <- 1.0 / (c_magnetic * c_speed_of_light**2)
#                           = 8.854187817e-12 * u_farad / u_meter
#' @rdname si_physical_constants
c_planck                <- 6.62606876e-34 * u_joule * u_second
#' @rdname si_physical_constants
c_h_bar                 <- c_planck / (2.0*pi)
#' @rdname si_physical_constants
c_avogadro              <- 6.02214199e+23 / u_mole
#' @rdname si_physical_constants
c_universal_gas         <- 8.314472 * u_joule / (u_mole * u_kelvin)
#' @rdname si_physical_constants
c_molar_gas             <- c_universal_gas
#' @rdname si_physical_constants
c_standard_molar_volume <- 2.2413996e-2 * u_meter**3 / u_mole
#' @rdname si_physical_constants
c_boltzmann             <- c_molar_gas / c_avogadro
#                           = 1.3806503e-23 * u_joule / u_kelvin
#' @rdname si_physical_constants
c_electron_charge       <- 1.602176462e-19 * u_coulomb
#                           = 1.602176462e-19 * u_ampere * u_second
#' @rdname si_physical_constants
c_elementary_charge     <- c_electron_charge
#' @rdname si_physical_constants
c_e                     <- c_electron_charge
#' @rdname si_physical_constants
c_faraday               <- c_avogadro * c_electron_charge
#                           = 9.64853415e+4 * u_coulomb / u_mole
#                           = 9.64853415e+4 * u_ampere * u_second / u_mole
#' @rdname si_physical_constants
c_first_radiation       <- 2.0*pi * c_planck * c_speed_of_light**2
#                           = 3.74177107e-16 * u_watt / u_meter**2
#' @rdname si_physical_constants
c_second_radiation      <- c_planck * c_speed_of_light / c_boltzmann
#                           = 1.4387752e-2 * u_meter * u_kelvin
#' @rdname si_physical_constants
c_stefan_boltzmann      <- (pi**2 / 60.0) * c_boltzmann * (c_boltzmann / c_h_bar) * (c_boltzmann / (c_h_bar * c_speed_of_light))**2
#                           = 5.670400e-8 * u_watt / (u_meter**2 * u_kelvin**4)
#' @rdname si_physical_constants
c_wiens_radiation       <- 2.8977686e-3 * u_meter * u_kelvin
#' @rdname si_physical_constants
c_electron_rest_mass    <- 9.10938188e-31 * u_kilogram
#' @rdname si_physical_constants
c_proton_rest_mass      <- 1.67262158e-27 * u_kilogram
#' @rdname si_physical_constants
c_fine_structure        <- c_electron_charge**2  / (2.0 * c_electric * c_speed_of_light * c_planck)
#' @rdname si_physical_constants
c_bohr_magneton         <- c_electron_charge * (c_h_bar / (2.0 * c_electron_rest_mass))
#' @rdname si_physical_constants
c_nuclear_magneton      <- c_electron_charge * (c_h_bar / (2.0 * c_proton_rest_mass))
#' @rdname si_physical_constants
c_gravity               <- 6.673e-11 * u_meter**3 / (u_kilogram * u_second**2)
#' @rdname si_physical_constants
c_gravity_accel         <- 9.80665e+0 * u_meter / u_second**2
#' @rdname si_physical_constants
c_g_force               <- c_gravity_accel
#' @rdname si_physical_constants
c_ice_point             <- 273.15 * u_kelvin
#' @rdname si_physical_constants
c_water_triple_point    <- 273.16 * u_kelvin

#' @title NON_SI_units
#' @name non_si_units
#' @description Non-SI units
NULL
#' @rdname non_si_units
u_arc_minute <- u_degree / 60.0
#' @rdname non_si_units
u_arc_second <- u_arc_minute / 60.0
# Selected units of length
#' @rdname non_si_units
u_angstrom          <- 1.0e-10 * u_meter
#' @rdname non_si_units
u_micrometer        <- u_micro * u_meter
#' @rdname non_si_units
u_micron            <- u_micrometer
#' @rdname non_si_units
u_millimeter        <- u_milli * u_meter
#' @rdname non_si_units
u_mm                <- u_millimeter
#' @rdname non_si_units
u_centimeter        <- u_centi * u_meter
#' @rdname non_si_units
u_cm                <- u_centimeter
#' @rdname non_si_units
u_kilometer         <- u_kilo * u_meter
#' @rdname non_si_units
u_km                <- u_kilometer
#' @rdname non_si_units
u_nautical_mile     <- 1.852e+3 * u_meter
#' @rdname non_si_units
u_astronomical_unit <- 1.495979e+11 * u_meter
#' @rdname non_si_units
u_au                <- u_astronomical_unit
#' @rdname non_si_units
u_ua                <- u_astronomical_unit
#' @rdname non_si_units
u_mil               <- 2.54e-5 * u_meter
#' @rdname non_si_units
u_inch              <- 2.54 * u_centimeter
#' @rdname non_si_units
u_in                <- u_inch
#' @rdname non_si_units
u_foot              <- 12.0 * u_inch
#' @rdname non_si_units
u_ft                <- u_foot
#' @rdname non_si_units
u_yard              <- 3.0 * u_foot
#' @rdname non_si_units
u_yd                <- u_yard
#' @rdname non_si_units
u_statute_mile      <- 5280.0 * u_foot
#' @rdname non_si_units
u_mile              <- u_statute_mile
#' @rdname non_si_units
#u_light_year        = c_speed_of_light * u_year
#' @rdname non_si_units
u_parsec            <- 3.085678e+15 * u_meter
#' @rdname non_si_units
u_pc                <- u_parsec
# Selected units of area
#' @rdname non_si_units
u_square_millimeter <- u_millimeter ** 2
#' @rdname non_si_units
u_sqmm              <- u_square_millimeter
#' @rdname non_si_units
u_square_centimeter <- u_centimeter ** 2
#' @rdname non_si_units
u_sqcm              <- u_square_centimeter
#' @rdname non_si_units
u_square_meter      <- u_meter ** 2
#' @rdname non_si_units
u_sqm               <- u_square_meter
#' @rdname non_si_units
u_square_kilometer  <- u_kilometer ** 2
#' @rdname non_si_units
u_sqkm              <- u_square_kilometer
#' @rdname non_si_units
u_barn              <- 1.0e-28 * u_square_meter
#!$    u_b                 = u_barn
#' @rdname non_si_units
u_hectare           <- 1.0e+4 * u_square_meter
#' @rdname non_si_units
u_ha                <- u_hectare
#' @rdname non_si_units
u_square_inch       <- u_inch ** 2
#' @rdname non_si_units
u_sqin              <- u_square_inch
#' @rdname non_si_units
u_square_foot       <- u_foot ** 2
#' @rdname non_si_units
u_sqft              <- u_square_foot
#' @rdname non_si_units
u_square_yard       <- u_yard ** 2
#' @rdname non_si_units
u_sqyd              <- u_square_yard
#' @rdname non_si_units
u_square_mile       <- u_mile ** 2
#' @rdname non_si_units
u_sqmi              <- u_square_mile
#' @rdname non_si_units
u_acre              <- 43560.0 * u_square_foot
# Selected units of volume
#' @rdname non_si_units
u_cubic_meter        <- u_meter**3
#' @rdname non_si_units
u_liter              <- 1.0e-3 * u_meter ** 3
#' @rdname non_si_units
u_litre              <- u_liter
#' @rdname non_si_units
u_L                  <- u_liter
#' @rdname non_si_units
u_cubic_centimeter   <- u_centimeter ** 3
#' @rdname non_si_units
u_cc                 <- u_cubic_centimeter
#' @rdname non_si_units
u_milliliter         <- u_cubic_centimeter
#' @rdname non_si_units
u_mL                 <- u_milliliter
#' @rdname non_si_units
u_cubic_inch         <- u_inch ** 3
#' @rdname non_si_units
u_cubic_foot         <- u_foot ** 3
#' @rdname non_si_units
u_imperial_gallon_uk <- 4.54609 * u_liter
#' @rdname non_si_units
u_dry_gallon_usa     <- 4.404884 * u_liter
#' @rdname non_si_units
u_liquid_gallon_usa  <- 3.785412 * u_liter            # = 231.0 * u_cubicinch
#' @rdname non_si_units
u_quart              <- u_liquid_gallon_usa / 4.0
#' @rdname non_si_units
u_pint               <- u_liquid_gallon_usa / 8.0     #= u_quart / 2.0
#' @rdname non_si_units
u_cup                <- u_liquid_gallon_usa / 16.0    # = u_pint / 2.0
#' @rdname non_si_units
u_ounce_volume       <- u_liquid_gallon_usa / 128.0   # = u_cup / 8.0
#' @rdname non_si_units
u_tablespoon         <- u_liquid_gallon_usa / 256.0   # = u_ounce_volume / 2.0
#' @rdname non_si_units
u_teaspoon           <- u_liquid_gallon_usa / 768.0   # = u_tablespoon / 3.0
# Selected units of time
#' @rdname non_si_units
u_minute        <- 60.0 * u_second
#' @rdname non_si_units
u_min           <- u_minute
#' @rdname non_si_units
u_hour          <- 60.0 * u_minute
#' @rdname non_si_units
u_hr            <- u_hour
#' @rdname non_si_units
u_day           <- 24.0 * u_hour
#' @rdname non_si_units
u_d             <- u_day
#' @rdname non_si_units
u_year          <- 365.0 * u_day
#' @rdname non_si_units
u_yr            <- u_year
#' @rdname non_si_units
u_y             <- u_year
#' @rdname non_si_units
u_year_sidereal <- 3.155815e+15 * u_second
#' @rdname non_si_units
u_year_tropical <- 3.155693e+15 * u_second
# Selected units of linear velocity
#' @rdname non_si_units
u_kilometer_per_hour <- u_kilometer / u_hour
#' @rdname non_si_units
u_kph                <- u_kilometer_per_hour
#' @rdname non_si_units
u_knot               <- u_nautical_mile / u_hour
#' @rdname non_si_units
u_mile_per_hour      <- u_statute_mile / u_hour
#' @rdname non_si_units
u_mph                <- u_mile_per_hour
# Selected units of angular velocity
#' @rdname non_si_units
u_cycles_per_second    <- 2.0*pi * u_radian / u_second
#' @rdname non_si_units
u_rotations_per_minute <- 2.0*pi * u_radian / u_minute
#' @rdname non_si_units
u_rpm                  <- u_rotations_per_minute
# Selected units of mass
#' @rdname non_si_units
u_gram             <- u_kilogram / u_kilo
#' @rdname non_si_units
u_gramme           <- u_gram
#' @rdname non_si_units
u_g                <- u_gram
#' @rdname non_si_units
u_milligram        <- 1.0e-6 * u_kilogram
#' @rdname non_si_units
u_mg               <- u_milligram
#' @rdname non_si_units
u_tonne            <- 1.0e+3 * u_kilogram
#' @rdname non_si_units
u_metric_ton       <- 1.0e+3 * u_kilogram
#' @rdname non_si_units
u_atomic_mass_unit <- 1.0e-3 * u_kilogram / (c_avogadro * u_mole)
#' @rdname non_si_units
u_amu              <- u_atomic_mass_unit
#' @rdname non_si_units
u_u                <- u_atomic_mass_unit
#' @rdname non_si_units
u_pound_mass       <- 4.535924e-1 * u_kilogram
#' @rdname non_si_units
u_slug             <- 1.459390e+1 * u_kilogram
#' @rdname non_si_units
u_ton              <- 2000.0 * u_pound_mass
#' @rdname non_si_units
u_solar_mass       <- 1.989e+30 * u_kilogram
# Selected units of force
#' @rdname non_si_units
u_dyne        <- 1.0e-5 * u_newton
#' @rdname non_si_units
u_pound_force <- u_pound_mass * c_gravity_accel
#' @rdname non_si_units
u_lb          <- u_pound_force
# Selected non-SI units of pressure
#' @rdname non_si_units
u_hectopascal  <- u_hecto * u_pascal
#' @rdname non_si_units
u_bar          <- 1.0e+5 * u_pascal
#' @rdname non_si_units
u_millibar     <- u_milli * u_bar
#' @rdname non_si_units
u_mbar         <- u_millibar
#' @rdname non_si_units
u_mb           <- u_millibar
#' @rdname non_si_units
u_atmosphere   <- 1.01325e+5 * u_pascal
#' @rdname non_si_units
u_atm          <- u_atmosphere
#' @rdname non_si_units
u_inchH2O      <- 2.490889e+2 * u_pascal
#' @rdname non_si_units
u_inH2O        <- u_inchH2O
#' @rdname non_si_units
u_inchHg       <- 3.386389e+3 * u_pascal
#' @rdname non_si_units
u_inHg         <- u_inchHg
#' @rdname non_si_units
u_millimeterHg <- 1.333224e+2 * u_pascal
#' @rdname non_si_units
u_mmHg         <- u_millimeterHg
#' @rdname non_si_units
u_torricelli   <- u_millimeterHg
#' @rdname non_si_units
u_torr         <- u_torricelli
#' @rdname non_si_units
u_psi          <- u_pound_force / u_square_inch
# Selected units of energy
#' @rdname non_si_units
u_electronvolt <- c_electron_charge * u_volt
#' @rdname non_si_units
u_eV           <- u_electronvolt
#' @rdname non_si_units
u_erg          <- 1.0e-7 * u_joule
#' @rdname non_si_units
u_btu          <- 1.05505585262e+3 * u_joule  # International Table value
#' @rdname non_si_units
u_calorie      <- 4.1868 * u_joule            # International Table value
#' @rdname non_si_units
u_kilocalorie  <- u_kilo * u_calorie
#' @rdname non_si_units
u_therm        <- 1.054804e+8 * u_joule       # U.S.
#    u_therm        = 1.05506e+8 * u_Joule        ! EC
# Selected non-SI units of power
#' @rdname non_si_units
u_horsepower_electric <- 7.46e+2 * u_watt
#' @rdname non_si_units
u_horsepower_imperial <- 7.4570e+2 * u_watt
#' @rdname non_si_units
u_horsepower_metric   <- 7.354988e+2 * u_watt
#' @rdname non_si_units
u_horsepower_water    <- 7.46043e+2 * u_watt
# Selected units of temperature DIFFERENCE
# Note actual temperatures in Celsius or Fahrenheit cannot be expressed as a simple scaling of
# degrees Kelvin, because an offset is required as well.  Therefore, the following units
# should only be used to convert temperature DIFFERENCES, unless care is taken to supply
# the correct offset in the program performing the conversion.
#' @rdname non_si_units
u_celsius_degree    <- u_kelvin
#' @rdname non_si_units
u_fahrenheit_degree <- u_kelvin / 1.8
#' @rdname non_si_units
u_rankine           <- u_kelvin / 1.8
# Selected units of radioactivity and radiation
#' @rdname non_si_units
u_curie     <- 3.7e+10 / u_second
#' @rdname non_si_units
u_Ci        <- u_curie
#' @rdname non_si_units
u_becquerel <- 1.0 / u_second
#' @rdname non_si_units
u_Bq        <- u_becquerel
#' @rdname non_si_units
u_gray      <- u_joule / u_kilogram
#' @rdname non_si_units
u_Gy        <- u_gray
#' @rdname non_si_units
u_sievert   <- u_joule / u_kilogram
#' @rdname non_si_units
u_Sv        <- u_sievert
#' @rdname non_si_units
u_rem       <- 1.0e-2 * u_sievert
#' @rdname non_si_units
u_roentgen  <- 2.58e-4 * u_coulomb / u_kilogram
#' @rdname non_si_units
u_R         <- u_roentgen
# Selected units of viscosity
#' @rdname non_si_units
u_poiseuille <- u_kilogram / (u_meter * u_second)
#' @rdname non_si_units
u_pl         <- u_poiseuille
#' @rdname non_si_units
u_poise      <- 1.0e-1 * u_poiseuille
#' @rdname non_si_units
u_stokes     <- 1.0e-4 * u_meter**2 / u_second
#' @rdname non_si_units
u_sk         <- u_stokes
#' @rdname non_si_units
u_Pa_per_s   <- u_pascal / u_s
# Units of flow rate
#' @rdname non_si_units
u_cubic_meter_per_s <- u_cubic_meter / u_s
#' @rdname non_si_units
u_cubm_per_s        <- u_cubic_meter_per_s
#' @rdname non_si_units
u_L_per_s           <- u_L / u_s
#' @rdname non_si_units
u_gpm               <- u_liquid_gallon_usa / u_min
#' @rdname non_si_units
u_cfs               <- u_cubic_foot / u_s
# Units of hydraulic conductivity
#' @rdname non_si_units
u_m_per_day      <- u_m / u_day
#' @rdname non_si_units
u_cm_per_s       <- u_cm / u_s
#' @rdname non_si_units
u_gpd            <- u_liquid_gallon_usa / u_day
#' @rdname non_si_units
u_gpd_per_sqft   <- u_gpd /u_sqft
#' @rdname non_si_units
u_darcy          <- 0.8347245 * u_m_per_day
# Units of Transmissivity
#' @rdname non_si_units
u_sqm_per_day  <- u_sqm / u_day
#' @rdname non_si_units
u_sqft_per_s   <- u_sqft / u_s
#' @rdname non_si_units
u_gpd_per_ft   <- u_liquid_gallon_usa / (u_day*u_foot)
# Concentration
#' @rdname non_si_units
c_volume_solvent   <- 1.0 * u_litre
#' @rdname non_si_units
c_volume_solution  <- 1.0 * u_litre
#' @rdname non_si_units
c_mass_solvent     <- 1.0 * u_kg
#' @rdname non_si_units
c_mass_solution    <- 1.0 * u_kg
#' @rdname non_si_units
u_ppm   <- u_mg / u_L
#' @rdname non_si_units
u_Molar <- u_mol / u_L
#' @rdname non_si_units
u_M     <- u_Molar
#' @rdname non_si_units
u_molal <- u_mol / u_kg
# Electrical conductivity
#' @rdname non_si_units
u_microS_per_cm <- u_micro * u_siemens / u_cm
# Compressibility
#' @rdname non_si_units
u_sqft_per_pound <- u_sqft / u_pound_mass
#' @rdname non_si_units
u_sqm_per_N      <- u_sqm / u_N
#' @rdname non_si_units
u_per_bar        <- u_bar^-1
