meta_targets <- list(
  # Path to observations
  tar_target(
    observations_file,
    here("data", "observations", "covariants.csv"),
    format = "file", packages = c("here")
  ),
  tar_target(
    observations,
    fread(observations_file),
    packages = c("data.table", "here")
  ),
  # Compile models
  tar_target(
    single_model,
    forecast.vocs::load_model(strains = 1),
    format = "file", deployment = "main",
  ),
  tar_target(
    two_model,
    forecast.vocs::load_model(strains = 2),
    format = "file", deployment = "main",
  ),
  # Arguments passed to `forecast()` to control forecasting
  tar_target(
    forecast_args,
    list(
      horizon = 4, adapt_delta = 0.99, max_treedepth = 15,
      parallel_chains = 1, chains = 2, keep_fit = FALSE,
      probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
      voc_label = "Delta"
    ),
    deployment = "main"
  ),
  # Arguments passed to `forecast()` to control retrospective forecasting
  tar_target(
    retro_args,
    list(
      voc_scale = c(0.5, 0.25)
    )
  ),
  # Data source to use for model validation
  tar_target(
    validation_source,
    "Germany"
  ),
  # Forecast dates to use for model validation
  tar_target(
    validation_dates,
    as.Date(c("2021-07-04", "2021-07-10", "2021-07-24", "2021-08-07"))
  )
)
