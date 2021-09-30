validation_targets <- list(
  # validate both prior and posterior
  tar_target(
    validate_likelihood,
    c(TRUE, FALSE)
  ),
  # load data for validation data source
  tar_target(
    validation_obs,
    load_obs(validation_source)
  ),
  # extract the most up to date version of the validation data
  tar_target(
    current_validation_obs,
    latest_obs(validation_obs)
  ),
  # split data into that available in each forecast week
  tar_target(
    retro_validation_obs,
    filter_by_availability(validation_obs, date = validation_dates),
    map(validation_dates),
    deployment = "worker"
  ),
  # plot prior predictive check
  # plot posterior predictive check
  # Targets producing forecasts for each week of observed data
  # prior and predictive checks for validation data on the single strain model
  tar_target(
    single_predictive_checks,
    do.call(
      forecast_dt,
      c(
        forecast_args,
        retro_args,
        list(
          obs = retro_validation_obs,
          strains = 1,
          model = single_model,
          likelihood = validate_likelihood
        )
      )
    )[, likelihood := validate_likelihood],
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
    cross(retro_validation_obs, validate_likelihood)
  ),
  # prior and predictive checks for validation data on the two strain model
  # stratified by modelled relationship between variants
  tar_target(
    two_predictive_checks,
    do.call(
      forecast_dt,
      c(
        forecast_args,
        retro_args,
        list(
          obs = retro_validation_obs,
          strains = 2,
          variant_relationship = variant_relationship_scenarios,
          model = two_model,
          likelihood = validate_likelihood
        )
      )
    )[, likelihood := validate_likelihood],
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
    cross(retro_validation_obs, variant_relationship_scenarios,
          validate_likelihood)
  )
)