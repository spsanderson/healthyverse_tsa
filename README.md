Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
12 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,929
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

The last day in the data set is 2022-02-10 17:36:43, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1752.97
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28929          |
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
| r\_version     |      19208 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19208 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19208 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2406 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-10 | 2021-08-25 |       445 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1520810.50 | 1872541.54 | 357 | 23389 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8264.78 |   15728.02 |   1 |   308 |   2806 |    8310 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-10 17:36:43 | 2021-08-25 10:04:06 |     16895 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 56M 25S |        60 |

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [388|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [378|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [328|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [303|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [117|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1380128 |  865.067841 | 0.7999238 | 137.432846 | 1.4014473 | 0.0585076 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0628193 |   95.207918 | 0.0441565 |  14.782575 | 0.0820412 | 0.9943842 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9710041 |  353.861129 | 0.6825312 | 158.082549 | 1.1651004 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0635869 |   76.283808 | 0.0446961 |  14.053433 | 0.0892484 | 0.9936519 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0484645 |   44.171390 | 0.0340663 |  11.002441 | 0.1075123 | 0.9888565 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0118831 |    2.538392 | 0.0083528 |   2.768377 | 0.0171639 | 0.9997626 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0716516 |   69.597121 | 0.0503648 |  15.741347 | 0.1021984 | 0.9914909 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5908737 |  744.847853 | 0.4153327 |  86.258548 | 0.7056316 | 0.9910879 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1835557 | 2861.938567 | 0.8319365 | 103.082921 | 1.4956902 | 0.3512538 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5736137 | 3156.247649 | 1.1061133 | 146.207511 | 1.8798413 | 0.0021324 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9690556 |  454.383767 | 0.6811616 | 128.095940 | 1.2432739 | 0.1709892 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9707750 |  350.192613 | 0.6823702 | 158.356122 | 1.1642697 | 0.0037370 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8613490 | 1977.151751 | 1.0067741 | 160.915815 | 1.0453382 | 0.4038148 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0628835 |  128.294100 | 0.0735004 |  25.619232 | 0.0745807 | 0.9946771 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5832228 |  927.015385 | 0.6816907 | 108.086675 | 0.8331087 | 0.2486578 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0590804 |  119.410510 | 0.0690552 |  28.353825 | 0.0714586 | 0.9946747 |
| healthyR      |          7 | EARTH                      | Test  | 0.0343131 |   30.727897 | 0.0401063 |  15.419310 | 0.0777668 | 0.9932348 |
| healthyR      |          8 | NNAR                       | Test  | 0.0083207 |   65.927853 | 0.0097255 |  11.221811 | 0.0156282 | 0.9997702 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0671033 |  170.641537 | 0.0784326 |  28.195850 | 0.0794976 | 0.9934470 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4830447 | 1346.418521 | 0.5645992 | 110.970633 | 0.5850149 | 0.9762277 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0949869 | 2033.002444 | 1.2798581 | 101.984446 | 1.4008357 | 0.5479498 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1218079 | 2621.805815 | 1.3112074 | 132.159196 | 1.3749923 | 0.1627362 |
| healthyR      |         13 | TBATS                      | Test  | 0.5861620 |  816.531243 | 0.6851262 | 106.966386 | 0.8146448 | 0.3285364 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7589686 |  466.486297 | 0.8871084 | 168.493417 | 0.9658452 | 0.0361675 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.4295285 | 3041.825309 | 1.3345993 | 158.224868 | 1.6356888 | 0.4929148 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0786873 |  194.217498 | 0.0734620 |  23.423603 | 0.0964508 | 0.9917691 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.4687453 | 3378.174697 | 1.3712119 | 160.295655 | 1.6840438 | 0.1163471 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0750405 |  169.949906 | 0.0700573 |  23.293991 | 0.0899822 | 0.9920353 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0149222 |    5.749911 | 0.0139312 |   6.846348 | 0.0215283 | 0.9995501 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0231344 |   17.990604 | 0.0215981 |  10.744708 | 0.0478210 | 0.9984708 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0811891 |  188.770845 | 0.0757977 |  22.537255 | 0.1000394 | 0.9904500 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7661179 | 1871.518649 | 0.7152432 | 134.050547 | 0.8713254 | 0.9756448 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1009648 |  976.865926 | 1.0278543 | 115.230122 | 1.4228582 | 0.4911075 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.6773600 | 4386.174496 | 1.5659734 | 147.982941 | 2.0502263 | 0.1291366 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.0677812 | 1969.090443 | 0.9968742 | 148.409980 | 1.3269196 | 0.3008127 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.4623686 | 3252.283267 | 1.3652587 | 154.948580 | 1.7408933 | 0.0688050 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.0300401 |  117.486871 | 0.9491625 | 161.929448 | 1.2672967 | 0.2969541 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0652027 |    8.353877 | 0.0600830 |   7.951007 | 0.0898890 | 0.9912874 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8554562 |  153.358557 | 0.7882868 | 124.672877 | 1.0117671 | 0.0149975 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0624138 |    7.878873 | 0.0575131 |   7.470407 | 0.0917885 | 0.9912633 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0196060 |    1.878503 | 0.0180665 |   1.889267 | 0.0437090 | 0.9987259 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0170569 |    2.833801 | 0.0157176 |   3.662619 | 0.0380435 | 0.9985953 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1059339 |   24.299370 | 0.0976161 |  24.263794 | 0.1173146 | 0.9910395 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6372148 |   88.993504 | 0.5871814 | 102.601628 | 0.7592517 | 0.9661126 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1878431 |  264.146622 | 1.0945751 | 109.374332 | 1.3880484 | 0.3092681 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3502779 |  265.774291 | 1.2442556 | 140.955619 | 1.6456543 | 0.0124626 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9613878 |  109.722281 | 0.8859007 | 136.244439 | 1.1929408 | 0.0111624 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8308688 |  122.967170 | 0.7656299 | 110.420415 | 1.0504469 | 0.0537574 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1745669 | 2646.078393 | 1.0178394 | 154.953086 | 1.3619103 | 0.4121781 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0714530 |  149.048800 | 0.0619187 |  24.927536 | 0.0873317 | 0.9937025 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 1.0167699 | 1550.727155 | 0.8810979 | 158.957959 | 1.1872403 | 0.0709539 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0620721 |   70.622392 | 0.0537895 |  19.163038 | 0.0817390 | 0.9937514 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0171462 |    4.197014 | 0.0148583 |   3.903182 | 0.0300982 | 0.9991168 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0529357 |   63.808949 | 0.0458723 |  20.193469 | 0.1018353 | 0.9904443 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0759911 |  190.392595 | 0.0658513 |  27.251392 | 0.0946716 | 0.9921043 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.0808274 | 3347.559370 | 0.9366080 | 160.056408 | 1.1724757 | 0.8684942 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1789076 | 1084.302952 | 1.0216009 | 131.001914 | 1.4365778 | 0.4161693 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3573986 | 1571.282180 | 1.1762751 | 140.465598 | 1.7451046 | 0.0823603 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9969320 | 1843.990963 | 0.8639071 | 158.299213 | 1.1542962 | 0.1167879 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0676495 | 1680.796544 | 0.9251884 | 155.606445 | 1.2940451 | 0.0237185 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |

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
    ## 1 healthyR~         8 NNAR        Test  0.0119   2.54 0.00835  2.77 0.0172 1.00 
    ## 2 healthyR          8 NNAR        Test  0.00832 65.9  0.00973 11.2  0.0156 1.00 
    ## 3 healthyR~         7 EARTH       Test  0.0149   5.75 0.0139   6.85 0.0215 1.00 
    ## 4 healthyv~         8 NNAR        Test  0.0171   2.83 0.0157   3.66 0.0380 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0171   4.20 0.0149   3.90 0.0301 0.999

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
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl~
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [388|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [378|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [328|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [303|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [117|28]> <mdl_time_tbl>

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
