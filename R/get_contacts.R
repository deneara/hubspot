#' Get a list of all contacts, including a set of properties
#'
#' @param apikey API key to work with Hubspot
#' @param properties Character vector of properties to request
#' @param property_history Whether version history for properties should be returned
#' @param form_submission_mode Which form submissions should be fetched
#' @param list_memberships Indicate whether current list memberships should be fetched
#' @param max_iter The API is limited to 100 responses, use `max_iter` to limit how many pages of data will get returned
#' @param max_properties Avoid URLs that are too long, limit the number of properties returned, if required.
#'
#' @return List with contact data
#' @export
#' @family getters
#' @examples
#' contacts <- get_contacts(property_history = "false", max_iter = 1, max_properties = 10)
get_contacts <- function(apikey = "demo",
                          properties = get_contact_properties(apikey),
                          property_history = "true",
                          form_submission_mode = "newest",
                          list_memberships = "false",
                          max_iter = 10,
                          max_properties = 100) {
  form_submission_mode <- match.arg(form_submission_mode,
                                    c("all", "none", "newest", "oldest"))
  base_url <- "https://api.hubapi.com"
  properties_url <- httr::modify_url(base_url, path = "/contacts/v1/lists/all/contacts/all")
  properties <- head(properties, max_properties)
  contacts <- list()
  n <- 0
  do <- TRUE
  offset <- 0

  while (do & n < max_iter) {
    res <- httr::GET(properties_url,
      query = c(
        list(
          vidOffset = offset,
          hapikey = apikey,
          count = 100,
          propertyMode = ifelse(property_history == "true", "value_and_history", "value_only"),
          formSubmissionMode = form_submission_mode,
          showListMemberships = list_memberships
        ),
        set_names(
          lapply(properties, function(x) {
            x
          }),
          rep("properties", length(properties))
        )
      )
    )
    n <- n + 1
    res_content <- httr::content(res)
    contacts[n] <- list(res_content$contacts)
    do <- res_content$`has-more`
    offset <- res_content$`vid-offset`
  }

  contacts <- flatten(contacts)
  contacts <- set_names(contacts, map_dbl(contacts, "vid"))
}
