
plot_single_predictions <- function(forecasts, obs, likelihood = TRUE) {
  sel_lik <- likelihood
  name <- ifelse(sel_lik, "posterior", "prior")
  p <- suppressWarnings(
    forecasts[likelihood == sel_lik] |>
        forecast.vocs::unnest_posterior() |>
        forecast.vocs::plot_cases(obs, log = TRUE) +
        ggplot2::facet_grid(vars(overdispersion), vars(forecast_date)) +
        ggplot2::guides(col = "none", fill = "none")
  )
  file <- suppressWarnings(
    save_plot(
      p,
      here("figures", "validation", paste0("single_", name, "_prediction.png")),
      height = 9, width = 12
    )
  )
  return(file)
}