# load targets + parallel packages
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
library(here)
plan(callr)

# should the whole pipeline be run or just the validation steps
validation_only <- TRUE

# datasets of interest
#sources <- list(source = c("Germany", "United Kingdom", "Belgium", "Italy"))
sources <- list(source = "Germany")

# load required packages and watch forecast.vocs for changes
tar_option_set(
  packages = c("forecast.vocs", "purrr", "data.table", "scoringutils"),
  deployment = "worker",
  workspace_on_error = TRUE,
  error = "continue"
)

# load functions
functions <- list.files(here("R"), full.names = TRUE)
purrr::walk(functions, source)

# load target modules
targets <- list.files(here("targets"), full.names = TRUE)
targets <- grep("*\\.R", targets, value = TRUE)
targets <- targets[!grepl("targets/summarise_sources.R", targets)]
purrr::walk(targets, source)


# input and control targets
meta_targets <- list(
  # Path to observations
  tar_target(
    observations,
    fread(here("data", "observations", "covariants.csv")),
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
      parallel_chains = 1, chains = 2, keep_fit = FALSE
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
    as.Date(c("2021-06-26", "2021-07-10", "2021-07-24", "2021-08-07"))
  )
)
# branch targets across data sources (see individual targets scripts in
# targets/ for further details of each step)
combined_targets <- tar_map(
  values = sources,
  c(
    obs_targets,
    forecast_targets,
    scenario_forecast_targets,
    summarise_forecast_targets,
    score_forecast_targets
  ),
  unlist = FALSE
)

# Load summary targets
source(here("targets/summarise_sources.R"))

# Combine, evaluate, and summarise targets
targets_list <- list(
  meta_targets,
  scenario_targets,
  validation_targets
)
if (!validation_only) {
  targets_list <- c(
    combined_targets,
    summarise_source_targets
  )
}
targets_list
