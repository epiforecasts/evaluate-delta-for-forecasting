load_obs <- function(location, path = "data/observations",
                     source = "covariants") {
  if (missing(location)) {
    location <- NULL
  }
  sources <- c("germany", "covariants")
  source <- match.arg(source,
    choices = sources
  )
  source_path <- here::here(path, paste0(source, ".csv"))
  message("Loading observations from: ", source_path)
  obs <- data.table::fread(
    source_path
  )
  if (!is.null(location)) {
    loc <- location
    obs <- obs[location_name %in% loc]
  }
  return(obs)
}
