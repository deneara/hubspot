#' Get a list of all engagements
#'
#' @param apikey API key to work with Hubspot
#' @param max_iter The API is limited to 250 responses, use `max_iter` to limit how many pages of data will get returned
#'
#' @return List with engagement data
#' @export
#' @family getters
#' @examples
#' engagements <- get_engagements(property_history = "false", max_iter = 1, max_properties = 10)
get_engagements <- function(apikey = "demo",
                          max_iter = 10) {
  base_url <- "https://api.hubapi.com"
  engagements_url <- httr::modify_url(base_url, path = "/engagements/v1/engagements/paged")
  engagements <- list()
  n <- 0
  do <- TRUE
  offset <- 0

  while (do & n < max_iter) {
    res <- httr::GET(engagements_url,
      query =
        list(
          offset = offset,
          hapikey = apikey,
          limit = 250
        )
    )
    n <- n + 1
    res_content <- httr::content(res)
    engagements[n] <- list(res_content$engagements)
    do <- res_content$`has-more`
    offset <- res_content$offset
  }

  engagements <- flatten(engagements)
  engagements <- set_names(engagements, map_dbl(engagements, "id"))
}
