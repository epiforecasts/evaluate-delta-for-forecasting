filter_obs <- function(observations, location) {
  if (missing(location)) {
    location <- NULL
  }
  obs <- data.table::copy(observations)

  if (!is.null(location)) {
    loc <- location
    obs <- obs[location_name %in% loc]
  }
  return(obs)
}
