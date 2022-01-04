Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
04 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,168
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

The last day in the data set is 2022-01-02 19:16:30, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -818.63
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26168          |
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
| r\_version     |      17544 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17544 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17544 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        14 |          0 |
| country        |       2215 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-02 | 2021-08-04 |       406 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size           |          0 |              1 | 1529290.43 | 1878094.59 | 357 | 17597 | 238594.5 | 3246425 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8022.12 |   15240.77 |   1 |   202 |   2806.0 |    8154 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-02 19:16:30 | 2021-08-04 07:18:44 |     15246 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     14 |        60 |

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
    ## 1 healthyR.data <tibble [377 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [367 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [317 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [292 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [106 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8264988 | 172.059996 | 0.6088752 |  90.112046 | 1.1995958 | 0.6156747 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.2090033 |  15.373874 | 0.1539711 |  14.510407 | 0.7158281 | 0.7802784 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0170432 | 185.455008 | 0.7492478 | 102.095116 | 1.4414799 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.2083894 |  12.580995 | 0.1535188 |  15.355936 | 0.7083106 | 0.7782101 |
| healthyR.data |          7 | EARTH                      | Test  | 0.2049835 |   6.473045 | 0.1510098 |   7.034069 | 0.6680224 | 0.7862708 |
| healthyR.data |          8 | NNAR                       | Test  | 0.2590303 |   8.770046 | 0.1908256 |   8.857821 | 0.7791039 | 0.7156131 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.2063438 |  11.163237 | 0.1520119 |  12.557597 | 0.7018800 | 0.7839244 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4861442 |  58.219706 | 0.3581386 |  60.324923 | 0.9262581 | 0.7530648 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9515928 | 272.726342 | 0.7010310 |  98.252196 | 1.0762244 | 0.6328260 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1597465 | 190.173768 | 0.8543762 | 120.803977 | 1.3962538 | 0.1997153 |
| healthyR.data |         13 | TBATS                      | Test  | 1.0654513 | 212.722822 | 0.7849096 | 101.629597 | 1.4495184 | 0.0233023 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0131282 | 182.814836 | 0.7463636 | 102.069072 | 1.4395947 | 0.2480599 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8720834 | 207.140423 | 0.8454155 | 127.542516 | 0.9797004 | 0.8159602 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0848439 |   8.826552 | 0.0822494 |   8.711045 | 0.2207010 | 0.9722598 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.0572425 | 230.178390 | 1.0249125 | 133.873230 | 1.2644640 | 0.0167883 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0854959 |   9.669378 | 0.0828815 |   9.456308 | 0.2196962 | 0.9722509 |
| healthyR      |          7 | EARTH                      | Test  | 0.0362720 |   2.359387 | 0.0351629 |   2.275696 | 0.1041327 | 0.9948054 |
| healthyR      |          8 | NNAR                       | Test  | 0.0883417 |   6.815084 | 0.0856403 |   6.256933 | 0.2104555 | 0.9872907 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0857711 |   8.115165 | 0.0831482 |   7.958188 | 0.2169439 | 0.9733361 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4086816 |  83.234630 | 0.3961843 |  90.399517 | 0.5136302 | 0.9855164 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0129193 | 247.884789 | 0.9819447 | 105.031389 | 1.2429386 | 0.6389417 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1874556 | 285.398803 | 1.1511439 | 119.455954 | 1.4853143 | 0.2126298 |
| healthyR      |         13 | TBATS                      | Test  | 0.9278006 | 176.179348 | 0.8994290 | 134.410898 | 1.1562899 | 0.1093137 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.0397285 | 213.274585 | 1.0079341 | 134.898662 | 1.2469644 | 0.4543042 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.9569952 | 121.660996 | 0.5897933 | 105.024874 | 1.1461182 | 0.6088394 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.1263305 |   8.546283 | 0.0778571 |   8.519529 | 0.3359062 | 0.9545983 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.0887568 | 132.733959 | 0.6709975 | 113.035149 | 1.3243195 | 0.1124765 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.1262863 |   8.404759 | 0.0778299 |   8.429467 | 0.3372911 | 0.9548699 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.1087260 |   4.838735 | 0.0670075 |   4.724278 | 0.2115052 | 0.9782129 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.1357586 |   7.950937 | 0.0836676 |   7.389794 | 0.2791046 | 0.9639168 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1315489 |  10.065548 | 0.0810732 |   9.779670 | 0.3334396 | 0.9557945 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4861839 |  38.842995 | 0.2996336 |  51.387580 | 0.6699273 | 0.9658511 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1227661 | 211.409331 | 0.6919573 | 101.482190 | 1.3465780 | 0.5943112 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4260632 | 230.936335 | 0.8788784 | 124.167800 | 1.6595670 | 0.1109286 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.2366738 | 114.675021 | 0.7621583 | 168.141565 | 1.4611819 | 0.1203030 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.1331758 | 135.771494 | 0.6983728 | 116.081583 | 1.3989876 | 0.3657998 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.7948330 | 195.667536 | 0.7635143 | 139.793678 | 0.9526283 | 0.7882015 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.1070333 |  20.266331 | 0.1028159 |  22.417621 | 0.2019242 | 0.9802562 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.0514163 | 302.250006 | 1.0099876 | 148.832644 | 1.2393029 | 0.2483217 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.1255806 |  29.332104 | 0.1206324 |  29.090479 | 0.2125013 | 0.9800793 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0535406 |   5.362789 | 0.0514309 |   5.231001 | 0.1199709 | 0.9932036 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0409965 |   3.222371 | 0.0393811 |   3.107459 | 0.1031258 | 0.9953709 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1132459 |  25.327906 | 0.1087837 |  24.693040 | 0.2013443 | 0.9814426 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4059199 |  70.992408 | 0.3899255 |  76.952569 | 0.5416791 | 0.9865641 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.7775282 | 303.986758 | 0.7468914 |  95.476594 | 0.9382752 | 0.7952841 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1959858 | 610.064174 | 1.1488606 | 127.940612 | 1.3779744 | 0.2730596 |
| healthyverse  |         13 | TBATS                      | Test  | 0.7777781 | 114.797962 | 0.7471315 | 135.439615 | 1.0019021 | 0.6696761 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.0496481 | 219.205714 | 1.0082891 | 151.410031 | 1.3019448 | 0.4723342 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA                      | Test  | 0.8500534 |  99.894929 | 0.5999333 | 112.911949 | 1.0696021 | 0.9047920 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1505931 |  17.153783 | 0.1062825 |  19.994066 | 0.2207568 | 0.9737449 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 1.1523340 |  99.513142 | 0.8132707 | 181.463732 | 1.4011461 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1541818 |  17.369916 | 0.1088153 |  19.794370 | 0.2260358 | 0.9721552 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0551080 |   4.485714 | 0.0388930 |   4.459413 | 0.1153032 | 0.9919713 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2965477 |  32.256346 | 0.2092914 |  39.377839 | 0.4062426 | 0.9502331 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1465298 |  17.953200 | 0.1034148 |  20.132639 | 0.2221923 | 0.9707306 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6517349 | 116.038984 | 0.4599682 |  91.137113 | 0.8261524 | 0.8734054 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9525282 | 154.946444 | 0.6722559 |  92.576834 | 1.1973978 | 0.6275532 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSAAN     | Test  | 1.2264032 | 195.337474 | 0.8655458 | 124.378556 | 1.4063998 | 0.1601017 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.0309576 | 108.658672 | 0.7276082 | 147.566535 | 1.2669414 | 0.0776803 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0489237 | 116.699210 | 0.7402879 | 132.550220 | 1.2895934 | 0.2503800 |
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
    ##   package      .model_id .model_desc .type    mae  mape   mase smape  rmse   rsq
    ##   <chr>            <int> <chr>       <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
    ## 1 healthyR.da~         7 EARTH       Test  0.205   6.47 0.151   7.03 0.668 0.786
    ## 2 healthyR             7 EARTH       Test  0.0363  2.36 0.0352  2.28 0.104 0.995
    ## 3 healthyR.ts          7 EARTH       Test  0.109   4.84 0.0670  4.72 0.212 0.978
    ## 4 healthyverse         8 NNAR        Test  0.0410  3.22 0.0394  3.11 0.103 0.995
    ## 5 healthyR.ai          7 EARTH       Test  0.0551  4.49 0.0389  4.46 0.115 0.992

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
    ## 1 healthyR.data <tibble [377 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [367 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [317 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [292 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [106 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
