load_obs <- function(source) {
  source <- match.arg(source,
    choices = "germany"
  )
  data.table::fread(
    here::here("data", paste0(source, ".csv"))
  )
}
