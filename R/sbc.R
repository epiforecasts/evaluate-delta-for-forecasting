
forecast_data <- function(data, strains, model, ...) {
  inits <- forecast.vocs::fv_inits(data, strains = strains)

  fit <- forecast.vocs::fv_sample(data, init = inits, model = model, ...)

  posterior <- forecast.vocs::fv_posterior(fit)
  return(posterior)
}

rank_sbc <- function(sbc, by = c("strains", "overdispersion",
                                 "variant_relationship", "dataset")) {
  parameters <- sbc[, rbindlist(sbc$parameters), by = by]
  setnames(parameters, "parameter", "variable")
  posterior <- sbc[, rbindlist(forecast), by = by]
  sbc_unnest <- merge(posterior, parameters, by = c(by, "parameter"))
  return(sbc_unnest)
}