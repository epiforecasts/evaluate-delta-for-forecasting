# Forecaast each scenario for each forecast date
scenario_forecast_targets <- list(
  tar_target(
    two_scenario_forecasts,
    do.call(
      forecast,
      c(
        forecast_args,
        list(
          obs = avail_scenario_obs$avail_obs[[1]],
          strains = 2,
          overdispersion = overdispersion_scenarios,
          variant_relationship = variant_relationship_scenarios,
          model = two_model,
          id = avail_scenario_obs$id[[1]],
          voc_scale = avail_scenario_obs$voc_scale[[1]]
        )
      )
    ),
    deployment = "worker", memory = "transient", garbage_collection = TRUE,
    head(cross(
      avail_scenario_obs, variant_relationship_scenarios,
      overdispersion_scenarios
    ), n = 1)
  )
)
