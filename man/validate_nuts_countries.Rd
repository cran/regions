% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_nuts_countries.R
\name{validate_nuts_countries}
\alias{validate_nuts_countries}
\title{Validate Conformity with NUTS Country Codes}
\usage{
validate_nuts_countries(dat, geo_var = "geo")
}
\arguments{
\item{dat}{A data frame with a 2-character geo variable to be validated}

\item{geo_var}{Defaults to \code{"geo"}. The variable that contains the
2 character geo codes to be validated.}
}
\value{
The original data frame extended with the column \code{'typology'}.
This column states \code{'country'} for valid country typology coding, or
appropriate label for invalid ISO-3166-alpha-2 and ISO-3166-alpha-3 codes.
}
\description{
This function is mainly a wrapper around the well-known
\link[countrycode]{countrycode} function, with three exception that are
particular to the European Union statistical nomenclature.
\describe{
\item{EL}{Treated valid, because NUTS uses EL instead of GR for Greece since 2010.}
\item{UK}{Treated valid, because NUTS uses UK instead of GB for the United Kingdom.}
\item{XK}{XK is used for Kosovo, because Eurostat uses this code, too.}
}
All ISO-3166-1 country codes are validated, and also the
three exceptions.
}
\examples{
{
my_dat <- data.frame (
 geo = c("AL", "GR", "XK", "EL", "UK", "GB", "NLD", "ZZ" ),
 values = runif(8)
 )

 ## NLD is an ISO 3-character code and is not validated.
 validate_nuts_countries(my_dat)
}
}
\seealso{
Other validate functions: 
\code{\link{validate_nuts_regions}()}
}
\concept{validate functions}
