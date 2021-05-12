## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  
  out.width='100%',
  fig.align = "center",
  fig.width = 7,
  fig.height = 5,
  
  message = FALSE,
  warning = FALSE
)

## ---- echo=F,  out.width="100%", fig.align='center'---------------------------
knitr::include_graphics("modeltime_ecosystem.jpg")

## ---- echo=F,  out.width="100%", fig.align='center', fig.cap="The Modeltime Workflow"----
knitr::include_graphics("modeltime_workflow.jpg")

## -----------------------------------------------------------------------------
library(tidymodels)
library(bayesmodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(lubridate)
# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE

## -----------------------------------------------------------------------------
# Data
m750 <- m4_monthly %>% filter(id == "M750")

## -----------------------------------------------------------------------------
m750 %>%
  plot_time_series(date, value, .interactive = interactive)

## -----------------------------------------------------------------------------
# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

## ---- message=TRUE------------------------------------------------------------
# Model 1: arima ----
model_fit_arima<- arima_reg(non_seasonal_ar = 0,
                            non_seasonal_differences = 1,
                            non_seasonal_ma = 1,
                            seasonal_period = 12,
                            seasonal_ar = 0,
                            seasonal_differences = 1,
                            seasonal_ma = 1) %>%
    set_engine(engine = "arima") %>%
    fit(value ~ date, data = training(splits))

## ---- message=TRUE------------------------------------------------------------
# Model 2: arima_boost ----
model_fit_arima_bayes<- sarima_reg(non_seasonal_ar = 0,
                                  non_seasonal_differences = 1,
                                  non_seasonal_ma = 1,
                                  seasonal_period = 12,
                                  seasonal_ar = 0,
                                  seasonal_differences = 1,
                                  seasonal_ma = 1,
                                  pred_seed = 100) %>%
    set_engine(engine = "stan") %>%
    fit(value ~ date, data = training(splits))

## -----------------------------------------------------------------------------
plot(model_fit_arima_bayes$fit$models$model_1)

## -----------------------------------------------------------------------------
model_fit_naive <- random_walk_reg(seasonal_random_walk = TRUE, seasonal_period = 12) %>%
                   set_engine("stan") %>%
                   fit(value ~ date + month(date), data = training(splits))

## -----------------------------------------------------------------------------
plot(model_fit_naive$fit$models$model_1)

## ---- paged.print = FALSE-----------------------------------------------------
models_tbl <- modeltime_table(
    model_fit_arima,
    model_fit_arima_bayes,
    model_fit_naive
)
models_tbl

## ---- paged.print = FALSE-----------------------------------------------------
calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))
calibration_tbl

## -----------------------------------------------------------------------------
calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    ) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25, # For mobile screens
      .interactive      = interactive
    )

## -----------------------------------------------------------------------------
calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(
        .interactive = interactive
    )

## ---- paged.print = F, message=F----------------------------------------------
refit_tbl <- calibration_tbl %>%
    modeltime_refit(data = m750)
refit_tbl %>%
    modeltime_forecast(h = "3 years", actual_data = m750) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25, # For mobile screens
      .interactive      = interactive
    )

