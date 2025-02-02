% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_deal_properties.R
\name{get_deal_properties}
\alias{get_deal_properties}
\title{Retrieve available properties for deals}
\usage{
get_deal_properties(apikey = "demo")
}
\arguments{
\item{apikey}{API key to work with Hubspot}
}
\value{
Character vector of properties
}
\description{
Retrieve available properties for deals
}
\examples{
properties <- get_deal_properties()
}
\seealso{
Other getters: \code{\link{get_companies}},
  \code{\link{get_company_properties}},
  \code{\link{get_contact_properties}},
  \code{\link{get_contacts}},
  \code{\link{get_deal_pipelines}},
  \code{\link{get_deals}}, \code{\link{get_owners}}
}
\concept{getters}
