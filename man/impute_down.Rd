% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute_down.R
\name{impute_down}
\alias{impute_down}
\title{Imputing Data From Larger To Smaller Units}
\usage{
impute_down(
  upstream_data = NULL,
  downstream_data = NULL,
  country_var = "country_code",
  regional_code = "geo_code",
  values_var = "values",
  time_var = NULL,
  upstream_method_var = NULL,
  downstream_method_var = NULL
)
}
\arguments{
\item{upstream_data}{An upstream data frame to project on containing
smaller geographical units, for example, country-level data.}

\item{downstream_data}{A downstream data frame containing the
smaller level missing data observations. It must contain all the
necessary structural information for imputation.}

\item{country_var}{The geographical ID of the upstream data,
defaults to \code{"country_code"}.}

\item{regional_code}{The geographical ID of the downstream data,
defaults to \code{"geo_code"}.}

\item{values_var}{The variable that contains the upstream data to be
imputed to the downstream data, defaults to \code{"values"}.}

\item{time_var}{The time component, if present, defaults to
\code{"year"}.}

\item{upstream_method_var}{The name of the variable that contains the
potentially applied imputation methods. Defaults to \code{NULL}.}

\item{downstream_method_var}{The name of the variable that will contain
the metadata of the potentially applied imputation methods.
Defaults to \code{NULL} in which case a variable called \code{'method'}
will be created.  If possible, avoid using \code{upstream_data} or
\code{downstream_data} that contains a variable called \code{'method'}
for other purposes.}
}
\value{
The upstream data frame (containing data of a larger unit) and
the downstream data (containing data of smaller sub-divisional units) are
joined; whenever data is missing in the downstream sub-divisional column,
it is imputed with the corresponding values from the upstream data frame.
The \code{'method'} metadata column explains if the actual downstream
data or the imputed data can be found in the downstream value column.
}
\description{
Imputing Data From Larger To Smaller Units
}
\examples{
{
upstream <- data.frame ( country_code =  rep( "AU", 3),
                         year = c(2018:2020),
                         my_var  = c(10,12,11),
                         description = c("note1", NA_character_,
                         "note3")
                       )

downstream <- australia_states

impute_down ( upstream_data  = upstream,
              downstream_data = downstream,
              country_var = "country_code",
              regional_code = "geo_code",
              values_var = "my_var",
              time_var = "year" )
}
}
\seealso{
Other impute functions: 
\code{\link{impute_down_nuts}()}
}
\concept{impute functions}
