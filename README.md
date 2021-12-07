Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
07 December, 2021

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 24,246
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

The last day in the data set is 2021-12-05 23:43:35, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -151.09
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 24246          |
| Number of columns                                | 11             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| character                                        | 6              |
| Date                                             | 1              |
| numeric                                          | 2              |
| POSIXct                                          | 1              |
| Timespan                                         | 1              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| r\_version     |      16316 |           0.33 |   5 |   5 |     0 |        28 |          0 |
| r\_arch        |      16316 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      16316 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        12 |          0 |
| country        |       2061 |           0.91 |   2 |   2 |     0 |        98 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-12-05 | 2021-07-21 |       378 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |        p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|-----------:|--------:|:------|
| size           |          0 |              1 | 1528822.17 | 1877151.58 | 357 | 27383 | 238433 | 3245270.00 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8057.54 |   15012.48 |   1 |   228 |   2957 |    8387.75 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-12-05 23:43:35 | 2021-07-21 14:59:01 |     14157 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   26.5 |        60 |

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

### Prophet Regression

We will first make a `progphet_reg`

``` r
rec_prophet <- recipe(value_trans ~ date, extract_nested_test_split(nested_data_tbl))

wflw_prophet <- workflow() %>%
  add_model(
    prophet_reg(
      mode = "regression",
      seasonality_yearly = "auto",
      seasonality_weekly = "auto",
      seasonality_daily  = "auto"
    ) %>%
      set_engine("prophet")
  ) %>%
  add_recipe(rec_prophet)

wflw_prophet
```

    ## == Workflow ====================================================================
    ## Preprocessor: Recipe
    ## Model: prophet_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## 0 Recipe Steps
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## PROPHET Regression Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   seasonality_yearly = auto
    ##   seasonality_weekly = auto
    ##   seasonality_daily = auto
    ## 
    ## Computational engine: prophet

### XGBoost Regression

We will use the `boost_tree` function

``` r
rec_xgboost <- recipe(value_trans ~ date, extract_nested_test_split(nested_data_tbl)) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgboost <- workflow() %>%
  add_model(
    boost_tree(
      mode = "regression"
    ) %>%
      set_engine("xgboost")
  ) %>%
  add_recipe(rec_xgboost)

wflw_xgboost
```

    ## == Workflow ====================================================================
    ## Preprocessor: Recipe
    ## Model: boost_tree()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## 4 Recipe Steps
    ## 
    ## * step_timeseries_signature()
    ## * step_rm()
    ## * step_zv()
    ## * step_dummy()
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Boosted Tree Model Specification (regression)
    ## 
    ## Computational engine: xgboost

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
  
  # Add workflows
  wflw_prophet,
  wflw_xgboost
)

nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 5 x 5
    ##   package       .actual_data       .future_data      .splits   .modeltime_tables
    ##   <chr>         <list>             <list>            <list>    <list>           
    ## 1 healthyR.data <tibble [349 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [2~
    ## 2 healthyR      <tibble [339 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [2~
    ## 3 healthyR.ts   <tibble [290 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [2~
    ## 4 healthyverse  <tibble [264 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [2~
    ## 5 healthyR.ai   <tibble [79 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [2~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc | .type |       mae |      mape |      mase |    smape |     rmse |       rsq |
|:--------------|-----------:|:-------------|:------|----------:|----------:|----------:|---------:|---------:|----------:|
| healthyR.data |          1 | PROPHET      | Test  | 1.1415191 | 111.42945 | 0.8302722 | 181.0703 | 1.395687 | 0.0040466 |
| healthyR.data |          2 | XGBOOST      | Test  | 0.9920373 |  87.44117 | 0.7215481 | 106.4650 | 1.427851 | 0.0075000 |
| healthyR      |          1 | PROPHET      | Test  | 0.8599852 |  97.75891 | 0.7979565 | 146.5521 | 1.068145 | 0.1416318 |
| healthyR      |          2 | XGBOOST      | Test  | 0.9234790 | 111.05149 | 0.8568706 | 140.4310 | 1.140220 | 0.0349146 |
| healthyR.ts   |          1 | PROPHET      | Test  | 1.1246913 | 170.12914 | 0.9451407 | 151.7039 | 1.353283 | 0.0451605 |
| healthyR.ts   |          2 | XGBOOST      | Test  | 1.0364172 | 167.97110 | 0.8709590 | 138.3264 | 1.324223 | 0.0143878 |
| healthyverse  |          1 | PROPHET      | Test  | 1.1285437 | 203.60371 | 0.9172508 | 159.7343 | 1.309591 | 0.0405480 |
| healthyverse  |          2 | XGBOOST      | Test  | 1.0375025 | 212.47197 | 0.8432549 | 143.2175 | 1.266420 | 0.0380458 |
| healthyR.ai   |          1 | PROPHET      | Test  | 0.8894954 | 183.62895 | 0.8343032 | 133.6834 | 1.109850 | 0.0445042 |
| healthyR.ai   |          2 | XGBOOST      | Test  | 0.9526583 | 173.71083 | 0.8935469 | 150.9063 | 1.165931 | 0.0033050 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = .2
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
    ##   # A tibble: 5 x 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <chr>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da~         1 PROPHET     Test  1.14  111.  0.830  181.  1.40 0.00405
    ## 2 healthyR             1 PROPHET     Test  0.860  97.8 0.798  147.  1.07 0.142  
    ## 3 healthyR.ts          2 XGBOOST     Test  1.04  168.  0.871  138.  1.32 0.0144 
    ## 4 healthyverse         2 XGBOOST     Test  1.04  212.  0.843  143.  1.27 0.0380 
    ## 5 healthyR.ai          1 PROPHET     Test  0.889 184.  0.834  134.  1.11 0.0445

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2
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
    control = control_nested_refit(verbose = TRUE)
  )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 5 x 5
    ##   package       .actual_data       .future_data      .splits   .modeltime_tables
    ##   <chr>         <list>             <list>            <list>    <list>           
    ## 1 healthyR.data <tibble [349 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [339 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [290 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [264 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [79 x 6]>  <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
    .conf_interval_alpha = 0.2
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
