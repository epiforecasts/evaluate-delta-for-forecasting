# load targets + parallel packages
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
library(here)
plan(callr)

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

# datasets of interest
sources <- list(source = c("Germany", "United Kingdom", "France", "Italy"))

# input and control targets
meta_targets <- list(
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
      horizon = 4, adapt_delta = 0.9, max_treedepth = 15,
      parallel_chains = 1, plot = FALSE, chains = 2, keep_fit = FALSE
    ),
    deployment = "main"
  ),
  # Arguments passed to `forecast()` to control retrospective forecasting
  tar_target(
    retro_args,
    list(
      voc_scale = c(0.5, 0.25)
    )
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
list(
  meta_targets,
  scenario_targets,
  combined_targets,
  summarise_source_targets
)
