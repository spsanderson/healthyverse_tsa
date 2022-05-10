Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
09 May, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 36,351
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,~
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M~
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:~
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4~
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N~
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA~
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N~
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR~
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0~
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", ~
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638~

The last day in the data set is 2022-05-07 20:45:57, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -3820.13
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 36351         |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     24361 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     24361 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     24361 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       21 |          0 |
| country       |      2898 |          0.92 |   2 |   2 |     0 |      107 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-05-07 | 2021-10-17 |      531 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1461956.4 | 1836512.2 | 357 | 16873 | 261762 | 3243241 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8598.7 |   16426.2 |   1 |   202 |   2687 |    8617 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-05-07 20:45:57 | 2021-10-17 09:38:18 |    21141 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 7M 30S |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->![](man/figures/README-initial_ts_plot-2.png)<!-- -->

Now lets take a look at some time series decomposition graphs.

![](man/figures/README-ts_decomp_plt-1.png)<!-- -->![](man/figures/README-ts_decomp_plt-2.png)<!-- -->![](man/figures/README-ts_decomp_plt-3.png)<!-- -->![](man/figures/README-ts_decomp_plt-4.png)<!-- -->

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

Now we are going to do some basic pre-processing.

``` r
data_padded_tbl <- base_data %>%
  pad_by_time(
    .date_var  = date,
    .pad_value = 0
  )

# Get log interval and standardization parameters
log_params  <- liv(data_padded_tbl$value, limit_lower = 0, offset = 1, silent = TRUE)
limit_lower <- log_params$limit_lower
limit_upper <- log_params$limit_upper
offset      <- log_params$offset

data_liv_tbl <- data_padded_tbl %>%
  # Get log interval transform
  mutate(value_trans = liv(value, limit_lower = 0, offset = 1, silent = TRUE)$log_scaled)

# Get Standardization Params
std_params <- standard_vec(data_liv_tbl$value_trans, silent = TRUE)
std_mean   <- std_params$mean
std_sd     <- std_params$sd

data_transformed_tbl <- data_liv_tbl %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  select(-value)
```

Now that we have our full data set and saved our parameters we can
create the full data set.

``` r
horizon         <- 4*7
lag_period      <- 4*7
rolling_periods <- c(7, 14, 28)

data_prepared_full_tbl <- data_transformed_tbl %>%
  group_by(package) %>%
  
  # Add future windows
  bind_rows(
    future_frame(., .date_var = date, .length_out = horizon)
  ) %>%
  
  # Add autocorolated lags
  tk_augment_lags(value_trans, .lags = lag_period) %>%
  
  # Add rolling features
  tk_augment_slidify(
    .value     = value_trans_lag28
    , .f       = median
    , .period  = rolling_periods
    , .align   = "center"
    , .partial = TRUE
  ) %>%
  
  # Format columns
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .)) %>%
  select(date, package, everything()) %>%
  ungroup()

data_prepared_full_tbl %>% 
  group_by(package) %>% 
  pivot_longer(-c(date, package)) %>% 
  plot_time_series(
    .date_var = date
    , .value = value
    , .color_var = name
    , .smooth = FALSE
    , .interactive = FALSE
    , .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-data_prepared_full_tbl-1.png)<!-- -->

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
data_prepared_tbl <- data_prepared_full_tbl %>%
  filter(!is.na(value_trans))

forecast_tbl <- data_prepared_full_tbl %>%
  filter(is.na(value_trans))

nested_data_tbl <- data_prepared_tbl %>%
  nest_timeseries(
    .id_var = package
    , .length_future = horizon
  ) %>%
  split_nested_timeseries(
    .length_test = horizon
  )
```

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ .
  , data = extract_nested_test_split(nested_data_tbl)
  ) %>%
  step_mutate(yr = lubridate::year(date)) %>%
  step_harmonic(yr, frequency = 365/12, cycle_size = 1) %>%
  step_hai_fourier(value_trans, scale_type = "sincos", period = 365/12, order = 1) %>%
  step_lag(value_trans, lag = 1) %>%
  step_impute_knn(contains("lag_"))

recipe_base
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          5
    ## 
    ## Operations:
    ## 
    ## Variable mutation for lubridate::year(date)
    ## Harmonic numeric variables for yr
    ## Fourier Transformation on value_trans
    ## Lagging value_trans
    ## K-nearest neighbor imputation for contains("lag_")

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# Boosted Auto ARIMA ------------------------------------------------------

model_spec_arima_boosted <- arima_boost(
  min_n = 2
  , learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost")

wflw_arima_boosted <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_boosted)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_lm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_lm)

model_spec_glm <- linear_reg(
  penalty = 1,
  mixture = 0.5
) %>%
  set_engine("glmnet")

wflw_glm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_glm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = TRUE,
     cores = n_cores
   ),
  # Add workflows
  wflw_arima_boosted,
  wflw_auto_arima,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)

nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [472|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [464|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [413|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [389|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [203|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [79 x 6]> <tibble>     <split [51|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc            | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:-----------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA W XGBOOST ERRORS | Test  | 0.7316851 |  94.783598 | 0.4811910 | 119.780274 | 0.9251005 | 0.6480108 |
| healthyR.data |         2 | REGRESSION             | Test  | 0.0648084 |   9.066454 | 0.0426211 |   9.014093 | 0.0794758 | 0.9935788 |
| healthyR.data |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |         4 | LM                     | Test  | 0.0680160 |  10.134156 | 0.0447306 |   9.508315 | 0.0865911 | 0.9927475 |
| healthyR.data |         5 | EARTH                  | Test  | 0.0627870 |  10.458689 | 0.0412917 |   8.712556 | 0.1013405 | 0.9888452 |
| healthyR.data |         6 | NNAR                   | Test  | 0.0185437 |   2.814262 | 0.0121953 |   2.788790 | 0.0234263 | 0.9993813 |
| healthyR      |         1 | ARIMA                  | Test  | 0.6295468 | 124.512740 | 0.5502757 | 120.371591 | 0.8514798 | 0.7108049 |
| healthyR      |         2 | REGRESSION             | Test  | 0.0435009 |  11.680513 | 0.0380234 |  10.868509 | 0.0500957 | 0.9978970 |
| healthyR      |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |         4 | LM                     | Test  | 0.0444457 |  11.632216 | 0.0388492 |  10.905629 | 0.0517982 | 0.9979095 |
| healthyR      |         5 | EARTH                  | Test  | 0.0176461 |   2.685901 | 0.0154242 |   2.644270 | 0.0326943 | 0.9993251 |
| healthyR      |         6 | NNAR                   | Test  | 0.0436859 |   4.641396 | 0.0381851 |   4.652302 | 0.1124077 | 0.9885909 |
| healthyR.ts   |         1 | ARIMA                  | Test  | 0.6414476 | 157.880372 | 0.5530541 | 122.191939 | 0.8611757 | 0.7854467 |
| healthyR.ts   |         2 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |         4 | LM                     | Test  | 0.0483886 |  20.207422 | 0.0417205 |  14.030465 | 0.0578489 | 0.9975565 |
| healthyR.ts   |         5 | EARTH                  | Test  | 0.0144923 |   3.253431 | 0.0124952 |   3.170375 | 0.0161402 | 0.9998527 |
| healthyR.ts   |         6 | NNAR                   | Test  | 0.0124788 |   3.261657 | 0.0107592 |   3.160610 | 0.0214801 | 0.9995545 |
| healthyverse  |         1 | ARIMA                  | Test  | 0.5680710 | 138.091435 | 0.5531147 | 134.017247 | 0.7597391 | 0.7286198 |
| healthyverse  |         2 | REGRESSION             | Test  | 0.0337697 |   9.673919 | 0.0328806 |   8.890488 | 0.0396701 | 0.9984845 |
| healthyverse  |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |         4 | LM                     | Test  | 0.0325924 |   9.644606 | 0.0317343 |   8.749484 | 0.0384024 | 0.9984649 |
| healthyverse  |         5 | EARTH                  | Test  | 0.0083656 |   2.498651 | 0.0081453 |   2.491198 | 0.0120180 | 0.9997996 |
| healthyverse  |         6 | NNAR                   | Test  | 0.0097121 |   4.699988 | 0.0094564 |   5.561781 | 0.0146363 | 0.9997291 |
| healthyR.ai   |         1 | ARIMA                  | Test  | 0.6849102 | 147.245569 | 0.5824105 | 123.208371 | 0.9101586 | 0.5903895 |
| healthyR.ai   |         2 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         4 | LM                     | Test  | 0.0435960 |   9.606569 | 0.0370717 |   9.311595 | 0.0596681 | 0.9962064 |
| healthyR.ai   |         5 | EARTH                  | Test  | 0.0152628 |   2.972507 | 0.0129787 |   2.918372 | 0.0179228 | 0.9997185 |
| healthyR.ai   |         6 | NNAR                   | Test  | 0.0196670 |   5.015167 | 0.0167237 |   3.773845 | 0.0483476 | 0.9985741 |
| TidyDensity   |         1 | ARIMA                  | Test  | 0.7012064 | 111.736771 | 0.4597279 | 100.667402 | 0.8883141 | 0.9707579 |
| TidyDensity   |         2 | REGRESSION             | Test  | 0.0544612 |   8.829548 | 0.0357060 |  10.384007 | 0.0632697 | 0.9970919 |
| TidyDensity   |         3 | NULL                   | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| TidyDensity   |         4 | LM                     | Test  | 0.1276233 |  26.713004 | 0.0836729 |  24.430279 | 0.1613309 | 0.9883324 |
| TidyDensity   |         5 | EARTH                  | Test  | 0.0239641 |   2.934858 | 0.0157114 |   2.987429 | 0.0288277 | 0.9993937 |
| TidyDensity   |         6 | NNAR                   | Test  | 0.4495701 |  57.635765 | 0.2947490 |  69.594366 | 0.6186761 | 0.8170108 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)<!-- -->

### Best Model

``` r
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 10
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         6 NNAR        Test  0.0185   2.81 0.0122   2.79 0.0234 0.999
    ## 2 healthyR          5 EARTH       Test  0.0176   2.69 0.0154   2.64 0.0327 0.999
    ## 3 healthyR~         5 EARTH       Test  0.0145   3.25 0.0125   3.17 0.0161 1.00 
    ## 4 healthyv~         5 EARTH       Test  0.00837  2.50 0.00815  2.49 0.0120 1.00 
    ## 5 healthyR~         5 EARTH       Test  0.0153   2.97 0.0130   2.92 0.0179 1.00 
    ## 6 TidyDens~         5 EARTH       Test  0.0240   2.93 0.0157   2.99 0.0288 0.999

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)<!-- -->

## Refitting and Future Forecast

Now that we have the best models, we can make our future forecasts.

``` r
parallel_start(n_cores)
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(
      verbose = TRUE, 
      allow_par = TRUE, 
      cores = n_cores
    )
  )
parallel_stop()

nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [472|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [464|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [413|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [389|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [203|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [79 x 6]> <tibble>     <split [51|28]>  <mdl_time_tbl>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  mutate(across(.value:.conf_hi, .fns = ~ standard_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  )$standard_inverse_value)) %>%
  mutate(across(.value:.conf_hi, .fns = ~ liiv(
    x = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )$rescaled_v)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
