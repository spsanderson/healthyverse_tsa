Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
16 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 41,731
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2022-07-14 23:46:36, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -283.81
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 41731         |
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
| r_version     |     28017 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     28017 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     28017 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3319 |          0.92 |   2 |   2 |     0 |      113 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-14 | 2021-11-08 |      599 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1419459.39 | 1792153.63 | 357 | 16873 | 289681 | 2974296 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8745.47 |   16748.42 |   1 |   174 |   2632 |    8827 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-14 23:46:36 | 2021-11-08 06:50:37 |    24402 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 22M 48S |       60 |

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

``` r
recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

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
  add_recipe(recipe = recipe_date) %>%
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
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl…
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR      <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 2 healthyR.data <tibble>     <tibble [28 × 6]> <split [306|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [307|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [271|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [119|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR      |         1 | REGRESSION  | Test  | 0.0551853 |  11.677032 | 0.0374714 |  10.770848 | 0.0683956 | 0.9966174 |
| healthyR      |         2 | GLMNET      | Test  | 0.6186076 |  67.556494 | 0.4200404 | 104.262275 | 0.8069482 | 0.9966079 |
| healthyR      |         3 | LM          | Test  | 0.0587110 |  14.389801 | 0.0398653 |  13.629482 | 0.0690329 | 0.9966017 |
| healthyR      |         4 | EARTH       | Test  | 0.0485020 |   4.710038 | 0.0329333 |   4.695734 | 0.0894673 | 0.9966079 |
| healthyR      |         5 | NNAR        | Test  | 0.0298580 |   2.497876 | 0.0202738 |   2.399321 | 0.1096866 | 0.9924394 |
| healthyR.data |         1 | REGRESSION  | Test  | 0.0487059 |   9.575213 | 0.0411937 |  10.031862 | 0.0601549 | 0.9969754 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4766250 |  58.982565 | 0.4031122 |  90.442987 | 0.5959634 | 0.9986897 |
| healthyR.data |         3 | LM          | Test  | 0.0474218 |   9.258746 | 0.0401076 |   9.619051 | 0.0584461 | 0.9969803 |
| healthyR.data |         4 | EARTH       | Test  | 0.0227067 |   4.297508 | 0.0192045 |   4.288685 | 0.0308841 | 0.9986897 |
| healthyR.data |         5 | NNAR        | Test  | 0.0084217 |   1.463096 | 0.0071227 |   1.448082 | 0.0132874 | 0.9997658 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0546233 | 174.808307 | 0.0450647 |  38.457881 | 0.0628821 | 0.9975422 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5190464 | 297.813943 | 0.4282182 | 105.096236 | 0.6681511 | 0.9981332 |
| healthyR.ts   |         3 | LM          | Test  | 0.0581502 | 202.478259 | 0.0479745 |  40.086171 | 0.0653860 | 0.9975171 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0286433 |  23.236382 | 0.0236310 |  15.793945 | 0.0488155 | 0.9981332 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0128673 |   8.791520 | 0.0106156 |   8.575789 | 0.0294455 | 0.9993614 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0398796 |  65.046945 | 0.0332851 |  22.028079 | 0.0529833 | 0.9961607 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4716462 | 366.273542 | 0.3936551 | 105.526983 | 0.6278777 | 0.9955970 |
| healthyverse  |         3 | LM          | Test  | 0.0472564 |  77.958932 | 0.0394421 |  23.270988 | 0.0579284 | 0.9962232 |
| healthyverse  |         4 | EARTH       | Test  | 0.0103393 |   9.365327 | 0.0086296 |   7.175454 | 0.0151624 | 0.9997190 |
| healthyverse  |         5 | NNAR        | Test  | 0.0101637 |   5.077797 | 0.0084830 |   5.170652 | 0.0256556 | 0.9991311 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0440067 |  32.653379 | 0.0423360 |  14.367169 | 0.0536699 | 0.9965724 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4704887 | 106.952213 | 0.4526267 | 102.939433 | 0.6015314 | 0.9968784 |
| healthyR.ai   |         3 | LM          | Test  | 0.0447802 |  35.516928 | 0.0430801 |  15.112944 | 0.0538399 | 0.9965764 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0117369 |   4.503485 | 0.0112913 |   5.593745 | 0.0141068 | 0.9997673 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0042527 |   1.642321 | 0.0040913 |   1.845576 | 0.0073451 | 0.9999727 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0272964 |  10.220757 | 0.0228056 |   8.058207 | 0.0326089 | 0.9987473 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4893315 | 190.486326 | 0.4088266 | 110.791480 | 0.6439532 | 0.9988616 |
| TidyDensity   |         3 | LM          | Test  | 0.0302201 |  22.973982 | 0.0252483 |  13.507219 | 0.0349493 | 0.9986724 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0093640 |   3.467053 | 0.0078234 |   3.196779 | 0.0125202 | 0.9998407 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0086234 |   3.926570 | 0.0072047 |   4.764705 | 0.0165958 | 0.9996783 |

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
    ##   # A tibble: 6 × 10
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthyR         1 REGRESSION  Test  0.0552  11.7  0.0375  10.8  0.0684  0.997
    ## 2 healthy…         5 NNAR        Test  0.00842  1.46 0.00712  1.45 0.0133  1.00 
    ## 3 healthy…         5 NNAR        Test  0.0129   8.79 0.0106   8.58 0.0294  0.999
    ## 4 healthy…         4 EARTH       Test  0.0103   9.37 0.00863  7.18 0.0152  1.00 
    ## 5 healthy…         5 NNAR        Test  0.00425  1.64 0.00409  1.85 0.00735 1.00 
    ## 6 TidyDen…         4 EARTH       Test  0.00936  3.47 0.00782  3.20 0.0125  1.00

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
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(
      verbose = TRUE, 
      allow_par = FALSE
    )
  )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl…
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR      <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 2 healthyR.data <tibble>     <tibble [28 × 6]> <split [306|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [307|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [309|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [271|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [119|28]> <mdl_time_tbl>

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
