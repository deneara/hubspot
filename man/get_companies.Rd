% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_companies.R
\name{get_companies}
\alias{get_companies}
\title{Get a list of all companies, including a set of properties}
\usage{
get_companies(apikey = "demo",
  properties = get_company_properties(apikey),
  property_history = "true", max_iter = 10, max_properties = 100)
}
\arguments{
\item{apikey}{API key to work with Hubspot}

\item{properties}{Character vector of properties to request}

\item{property_history}{Whether version history for properties should be returned}

\item{max_iter}{The API is limited to 250 responses, use \code{max_iter} to limit how many pages of data will get returned}

\item{max_properties}{Avoid URLs that are too long, limit the number of properties returned, if required.}
}
\value{
List with company data
}
\description{
Get a list of all companies, including a set of properties
}
\examples{
companies <- get_companies(property_history = "false", max_iter = 1, max_properties = 10)
}
\seealso{
Other getters: \code{\link{get_company_properties}},
  \code{\link{get_contact_properties}},
  \code{\link{get_contacts}},
  \code{\link{get_deal_pipelines}},
  \code{\link{get_deal_properties}},
  \code{\link{get_deals}}, \code{\link{get_owners}}
}
\concept{getters}
