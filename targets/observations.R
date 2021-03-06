# Targets converting observed data
obs_targets <- list(
  # load data from supplied source
  tar_target(
    obs,
    filter_obs(observations, source)
  ),
  # extract the most up to date version of the data
  tar_target(
    current_obs,
    latest_obs(obs)
  ),
  # define the list of dates to forecast at
  tar_target(
    forecast_dates,
    current_obs[!is.na(seq_available), ]$date[-c(1:3)]
  ),
  # split data into that available in each forecast week
  tar_target(
    retro_obs,
    filter_by_availability(obs, date = forecast_dates),
    map(forecast_dates),
    deployment = "worker",
    iteration = "list"
  ),
  # generate scenario data sets using latest data and scenarios
  tar_target(
    scenario_obs,
    data_availability_scenarios[
      ,
      generated_obs := list(generate_obs_scenario(
        current_obs,
        seq_lag = seq_lag, seq_samples = seq_samples
      ))
    ],
    map(data_availability_scenarios),
    deployment = "worker"
  ),
  # split scenario datasets based on forecast date
  tar_target(
    avail_scenario_obs,
    scenario_obs[, `:=`(
      forecast_date = forecast_dates,
      avail_obs = list(filter_by_availability(
        scenario_obs$generated_obs[[1]], forecast_dates
      ))
    )],
    cross(forecast_dates, scenario_obs),
    deployment = "worker", iteration = "list"
  )
)
