Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
17 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,945
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

The last day in the data set is 2022-01-15 21:51:41, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1133.22
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26945          |
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
| r\_version     |      18042 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18042 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18042 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2257 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-15 | 2021-08-09 |       419 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532375.05 | 1878335.66 | 357 | 16906 | 238827 | 3247050 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8056.48 |   15274.63 |   1 |   221 |   2806 |    8256 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-15 21:51:41 | 2021-08-09 11:44:58 |     15707 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 51M 45S |        60 |

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
  step_rm(yr) %>%
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
    ## Variable mutation
    ## Harmonic numeric variables for yr
    ## Delete terms yr
    ## Fourier transformation on value_trans
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

# ETS ---------------------------------------------------------------------

model_spec_ets <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "ets") 

wflw_ets <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_ets)

model_spec_croston <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "croston")

wflw_croston <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_croston)

model_spec_theta <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "theta")

wflw_theta <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_theta)


# STLM ETS ----------------------------------------------------------------

model_spec_stlm_ets <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("stlm_ets")

wflw_stlm_ets <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_ets)

model_spec_stlm_tbats <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("tbats")

wflw_stlm_tbats <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_tbats)

model_spec_stlm_arima <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("stlm_arima")

wflw_stlm_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_stlm_arima)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)


# Prophet -----------------------------------------------------------------

model_spec_prophet <- prophet_reg(
  seasonality_yearly = "auto",
  seasonality_weekly = "auto",
  seasonality_daily = "auto"
) %>%
  set_engine(engine = "prophet")

wflw_prophet <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_prophet)

model_spec_prophet_boost <- prophet_boost(
  learn_rate = 0.1
  , trees = 10
  , seasonality_yearly = FALSE
  , seasonality_weekly = FALSE
  , seasonality_daily  = FALSE
) %>% 
  set_engine("prophet_xgboost") 

wflw_prophet_boost <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_prophet_boost)

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

# XGBoost -----------------------------------------------------------------

model_spec_xgboost <- boost_tree(
  mode  = "regression",
  mtry  = 10,
  trees = 100,
  min_n = 5,
  tree_depth = 3,
  learn_rate = 0.3,
  loss_reduction = 0.01
) %>%
  set_engine("xgboost")

wflw_xgboost <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_xgboost)
```

### Nested Modeltime Tables

``` r
parallel_start(n_cores)
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
  wflw_croston,
  wflw_ets,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar,
  wflw_prophet,
  wflw_prophet_boost,
  wflw_stlm_arima,
  wflw_stlm_ets,
  wflw_stlm_tbats,
  wflw_theta,
  wflw_xgboost
)
parallel_stop()

nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 5 x 5
    ##   package       .actual_data       .future_data      .splits   .modeltime_tables
    ##   <chr>         <list>             <list>            <list>    <list>           
    ## 1 healthyR.data <tibble [390 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [380 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [330 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [305 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [119 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.6964504 | 225.193154 | 0.5480304 | 101.434391 | 0.8966956 | 0.5054122 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1418342 |  26.342350 | 0.1116080 |  21.135445 | 0.3366502 | 0.9202522 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8497526 | 247.561343 | 0.6686626 | 100.370460 | 1.1441563 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0832098 |  21.561791 | 0.0654770 |  17.737159 | 0.1030643 | 0.9951380 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0223730 |   7.101279 | 0.0176051 |   5.438190 | 0.0384205 | 0.9987719 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0193523 |   8.318393 | 0.0152281 |   6.400785 | 0.0283001 | 0.9994465 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0873358 |  21.529060 | 0.0687238 |  18.087899 | 0.1181978 | 0.9929202 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4339916 | 158.512488 | 0.3415040 |  75.478849 | 0.5471709 | 0.9953117 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0649383 | 494.090290 | 0.8379902 |  95.035961 | 1.3431706 | 0.5957269 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2015657 | 421.114896 | 0.9455010 | 130.422558 | 1.3838266 | 0.2062122 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8147211 | 167.558868 | 0.6410966 | 110.358435 | 1.0443878 | 0.0031349 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8486786 | 246.861413 | 0.6678174 | 100.368180 | 1.1426142 | 0.0944584 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7922986 | 348.444487 | 0.8488047 | 150.749797 | 0.8874363 | 0.5663844 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0359277 |  11.629452 | 0.0384900 |  15.127618 | 0.0446455 | 0.9992680 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.7397751 | 255.374404 | 0.7925352 | 151.760215 | 0.8899231 | 0.0471799 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0386461 |  16.395978 | 0.0414023 |  19.331450 | 0.0490262 | 0.9993000 |
| healthyR      |          7 | EARTH                      | Test  | 0.0186722 |   6.428228 | 0.0200039 |   5.929360 | 0.0214286 | 0.9994378 |
| healthyR      |          8 | NNAR                       | Test  | 0.0941588 |  13.101858 | 0.1008741 |  11.741420 | 0.2573897 | 0.9600418 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0412828 |  16.223363 | 0.0442271 |  18.024574 | 0.0568578 | 0.9986177 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4099474 | 217.056997 | 0.4391846 | 117.917533 | 0.4676876 | 0.9973916 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2549058 | 706.804764 | 1.3444047 | 129.042337 | 1.5063728 | 0.5292334 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1262345 | 565.154326 | 1.2065566 | 126.549463 | 1.4638523 | 0.2466167 |
| healthyR      |         13 | TBATS                      | Test  | 0.6458528 | 201.282038 | 0.6919144 | 150.027241 | 0.7944889 | 0.1364356 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7851090 | 328.486030 | 0.8411023 | 145.420838 | 0.9421707 | 0.0379720 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7907525 | 397.957650 | 0.5243987 | 109.967446 | 0.9203216 | 0.5872139 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0636695 |  43.982523 | 0.0422233 |  20.525143 | 0.0732749 | 0.9959554 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.9632839 | 466.057240 | 0.6388153 | 115.556647 | 1.1280215 | 0.1649103 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0771369 |  43.886172 | 0.0511544 |  21.686559 | 0.0953701 | 0.9961868 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0344196 |   3.358643 | 0.0228258 |   3.277775 | 0.0781014 | 0.9954795 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0372289 |   8.437738 | 0.0246889 |   6.573085 | 0.0832692 | 0.9958453 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1060147 |  61.754809 | 0.0703051 |  31.211687 | 0.1301886 | 0.9950252 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4312800 | 251.390826 | 0.2860094 |  86.102030 | 0.5120211 | 0.9878025 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2302096 | 620.118585 | 0.8158308 | 121.828255 | 1.4827159 | 0.5404514 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2928269 | 745.229544 | 0.8573564 | 122.359057 | 1.5989545 | 0.1547812 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6737585 | 182.479446 | 0.4468124 | 106.012036 | 0.8440235 | 0.4770819 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.0245504 | 557.294940 | 0.6794451 | 113.537154 | 1.2436044 | 0.0920560 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.7849591 | 211.345342 | 0.7196409 | 150.401159 | 0.9812205 | 0.3597429 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0620519 |  22.852612 | 0.0568884 |  18.011028 | 0.0767161 | 0.9963595 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7229130 | 145.412150 | 0.6627578 | 147.945993 | 0.8822393 | 0.2112308 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0660802 |  20.910266 | 0.0605815 |  16.830661 | 0.0775882 | 0.9966522 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0108058 |   2.853305 | 0.0099066 |   2.751562 | 0.0155698 | 0.9997557 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0074667 |   1.508493 | 0.0068454 |   1.472704 | 0.0153759 | 0.9996695 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0599060 |  17.495603 | 0.0549210 |  14.394759 | 0.0711126 | 0.9965768 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5293268 | 192.017685 | 0.4852803 | 121.734668 | 0.6132144 | 0.9842284 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2269677 | 633.765603 | 1.1248689 | 123.423560 | 1.4385398 | 0.5351838 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1302808 | 625.810569 | 1.0362275 | 122.117741 | 1.4217643 | 0.2078532 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5555406 | 142.867861 | 0.5093128 | 116.996941 | 0.6914213 | 0.3453446 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8244308 | 173.567112 | 0.7558280 | 164.657037 | 1.0242345 | 0.0622356 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8254764 | 124.755311 | 0.4910942 | 107.643982 | 1.0480553 | 0.5472648 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1187659 |  18.038533 | 0.0706564 |  20.242438 | 0.1339638 | 0.9980645 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9427046 | 133.995106 | 0.5608358 | 117.250993 | 1.1439469 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1197479 |  18.156420 | 0.0712407 |  20.413710 | 0.1347602 | 0.9980893 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0169062 |   1.857490 | 0.0100578 |   1.841573 | 0.0223710 | 0.9995875 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2534245 |  37.839672 | 0.1507679 |  44.725541 | 0.3535827 | 0.9715437 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1593335 |  24.564396 | 0.0947910 |  25.842368 | 0.1764654 | 0.9962297 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8826573 | 153.607672 | 0.5251123 | 112.441788 | 1.0384746 | 0.8986071 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8946762 | 141.944424 | 0.5322626 |  76.403377 | 1.1375884 | 0.6716872 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1559423 | 204.497644 | 0.6876956 | 130.256706 | 1.3516364 | 0.1862956 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8828532 | 137.705658 | 0.5252289 | 117.655663 | 1.0770141 | 0.1053970 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9890711 | 152.138435 | 0.5884202 | 113.869617 | 1.2180547 | 0.0013949 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |

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
    ##   # A tibble: 5 x 10
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         8 NNAR        Test  0.0194   8.32 0.0152   6.40 0.0283 0.999
    ## 2 healthyR          7 EARTH       Test  0.0187   6.43 0.0200   5.93 0.0214 0.999
    ## 3 healthyR~         2 REGRESSION  Test  0.0637  44.0  0.0422  20.5  0.0733 0.996
    ## 4 healthyv~         8 NNAR        Test  0.00747  1.51 0.00685  1.47 0.0154 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0169   1.86 0.0101   1.84 0.0224 1.00

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
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
    ##   # A tibble: 5 x 5
    ##   package       .actual_data       .future_data      .splits   .modeltime_tables
    ##   <chr>         <list>             <list>            <list>    <list>           
    ## 1 healthyR.data <tibble [390 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [380 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [330 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [305 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [119 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
