% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_nuts_regions.R
\name{validate_nuts_regions}
\alias{validate_nuts_regions}
\title{Validate Conformity With NUTS Geo Codes}
\usage{
validate_nuts_regions(dat, geo_var = "geo", nuts_year = 2016)
}
\arguments{
\item{dat}{A data frame with a 3-5 character \code{geo_var}
variable to be validated.}

\item{geo_var}{Defaults to \code{"geo"}. The variable that contains
the 3-5 character geo codes to be validated.}

\item{nuts_year}{The year of the NUTS typology to use.
Defaults to \code{2016}.  You can select any valid
NUTS definition, i.e. \code{1999}, \code{2003}, \code{2006},
\code{2010}, \code{2013}, the currently used \code{2016} and the
already announced and defined \code{2021}.}
}
\value{
Returns the original \code{dat} data frame with a column
that specifies the comformity with the NUTS definition of the year
\code{nuts_year}.
}
\description{
Validate that \code{geo_var} is conforming with the \code{NUTS1},
\code{NUTS2}, or \code{NUTS3} typologies.
While country codes are technically not part of the NUTS typologies,
Eurostat de facto uses a \code{NUTS0} typology to identify countries.
This de facto typology has three exception which are handled by the
\link[regions]{validate_nuts_countries} function.
}
\details{
NUTS typologies have different versions, therefore the conformity
is validated with one specific versions, which can be any of these:
\code{1999}, \code{2003}, \code{2006}, \code{2010},
\code{2013}, the currently used \code{2016} and the already
announced and defined \code{2021}.

The NUTS typology was codified with the \code{NUTS2003}, and the
pre-1999 NUTS typologies may confuse programmatic data processing,
given that some  NUTS1 regions were identified with country codes
in smaller countries that had no \code{NUTS1} divisions.

Currently the \code{2016} is used by Eurostat, but many datasets
still contain \code{2013} and sometimes earlier metadata.
}
\examples{
\donttest{
my_reg_data <- data.frame (
  geo = c("BE1", "HU102", "FR1",
          "DED", "FR7", "TR", "DED2",
          "EL", "XK", "GB"),
  values = runif(10))

validate_nuts_regions (my_reg_data)

validate_nuts_regions (my_reg_data, nuts_year = 2013)

validate_nuts_regions (my_reg_data, nuts_year = 2003)
}
}
\seealso{
Other validate functions: 
\code{\link{validate_nuts_countries}()}
}
\concept{validate functions}
