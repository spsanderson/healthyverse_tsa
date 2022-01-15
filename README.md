Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
15 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,815
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

The last day in the data set is 2022-01-13 21:40:12, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1085.03
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26815          |
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
| r\_version     |      17968 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17968 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17968 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2244 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-13 | 2021-08-07 |       417 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1531320.61 | 1878066.50 | 357 | 16906 | 238786 | 3246676 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8065.77 |   15290.64 |   1 |   219 |   2802 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-13 21:40:12 | 2021-08-07 18:11:32 |     15632 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 51M 53S |        60 |

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
    ## 1 healthyR.data <tibble [388 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [378 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [328 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [303 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [117 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.7151682 | 231.789471 | 0.5527157 |  98.631235 | 0.9230270 | 0.5157920 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1462021 |  27.035529 | 0.1129919 |  21.652901 | 0.3363198 | 0.9177413 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9683078 | 316.151141 | 0.7483539 | 102.360248 | 1.2776275 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0895973 |  23.291411 | 0.0692450 |  18.543423 | 0.1112258 | 0.9942020 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0223410 |   7.306123 | 0.0172662 |   5.564735 | 0.0302824 | 0.9992809 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0222625 |   3.428011 | 0.0172055 |   3.410631 | 0.0365064 | 0.9986942 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0902449 |  21.989194 | 0.0697455 |  18.852084 | 0.1216822 | 0.9921284 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5056851 | 195.295327 | 0.3908172 |  84.767044 | 0.6213566 | 0.9898783 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0433304 | 490.043704 | 0.8063349 |  92.983493 | 1.3393350 | 0.5898662 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2755002 | 378.238768 | 0.9857666 | 125.155433 | 1.4959740 | 0.2019983 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8276333 | 212.389382 | 0.6396340 |  99.645924 | 1.1018240 | 0.1048396 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9671857 | 315.644081 | 0.7474867 | 102.320583 | 1.2764986 | 0.0126661 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.7955415 | 286.326441 | 0.8082734 | 149.421440 | 0.8903698 | 0.5555414 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0354320 |  13.477523 | 0.0359991 |  15.982153 | 0.0429389 | 0.9992088 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8821857 | 293.562163 | 0.8963043 | 148.345383 | 1.0280148 | 0.0470122 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0381681 |  16.535572 | 0.0387789 |  17.985161 | 0.0478972 | 0.9992213 |
| healthyR      |          7 | EARTH                      | Test  | 0.0085248 |   1.449136 | 0.0086612 |   1.449174 | 0.0145478 | 0.9997070 |
| healthyR      |          8 | NNAR                       | Test  | 0.0974489 |  15.907817 | 0.0990085 |  17.175176 | 0.2644256 | 0.9532388 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0426647 |  18.720998 | 0.0433476 |  19.321105 | 0.0576510 | 0.9984543 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4176405 | 171.692175 | 0.4243245 | 120.606022 | 0.4758038 | 0.9958821 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2778655 | 611.733713 | 1.2983166 | 129.154365 | 1.5524510 | 0.5179155 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1867522 | 532.199788 | 1.2057451 | 129.598912 | 1.5384417 | 0.2427481 |
| healthyR      |         13 | TBATS                      | Test  | 0.7020733 | 205.359692 | 0.7133094 | 144.202683 | 0.8532882 | 0.1357060 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.9556179 | 364.107422 | 0.9709117 | 147.135904 | 1.1070359 | 0.0137790 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2095921 | 673.682496 | 0.7977475 | 117.593412 | 1.4158958 | 0.6514979 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0632991 |  43.076603 | 0.0417469 |  20.415372 | 0.0725859 | 0.9961303 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.0862876 | 556.228951 | 0.7164260 | 116.842371 | 1.2641593 | 0.1595548 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0763108 |  43.010787 | 0.0503284 |  20.877097 | 0.0967455 | 0.9963674 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0345978 |   3.192944 | 0.0228179 |   3.135426 | 0.0731750 | 0.9959747 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0356592 |   9.033203 | 0.0235179 |  12.683137 | 0.0566177 | 0.9979848 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1090697 |  64.404191 | 0.0719334 |  31.264747 | 0.1348023 | 0.9949105 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4184782 | 229.832299 | 0.2759938 |  84.275451 | 0.4990859 | 0.9899761 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2991035 | 637.478019 | 0.8567819 | 120.347235 | 1.5523527 | 0.5300908 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4195175 | 760.144380 | 0.9361971 | 138.352420 | 1.7098338 | 0.1500103 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.7233007 | 188.748758 | 0.4770297 | 105.002654 | 0.9060965 | 0.3635399 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.1455738 | 661.302656 | 0.7555263 | 113.750192 | 1.4004605 | 0.0980862 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8929837 | 276.252987 | 0.8073976 | 157.323801 | 1.0462363 | 0.6321221 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0664544 |  24.607823 | 0.0600853 |  19.557050 | 0.0799946 | 0.9972291 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.9599623 | 270.557529 | 0.8679569 | 160.866965 | 1.1334533 | 0.2062492 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0732352 |  24.148442 | 0.0662161 |  19.142282 | 0.0860255 | 0.9978069 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0102716 |   2.685078 | 0.0092872 |   2.602039 | 0.0149565 | 0.9997587 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0048473 |   1.315638 | 0.0043827 |   1.387900 | 0.0073746 | 0.9999032 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0622289 |  18.820327 | 0.0562647 |  14.842735 | 0.0748439 | 0.9973915 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5430941 | 211.718983 | 0.4910425 | 124.409446 | 0.6142758 | 0.9809797 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0808744 | 522.895821 | 0.9772804 | 119.540511 | 1.2780310 | 0.5429541 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1741517 | 573.374591 | 1.0616178 | 131.255666 | 1.5123592 | 0.1976129 |
| healthyverse  |         13 | TBATS                      | Test  | 0.7830366 | 190.379189 | 0.7079882 | 158.255553 | 0.9427708 | 0.3100253 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.0601609 | 332.913860 | 0.9585521 | 156.291380 | 1.2760417 | 0.0057521 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8466972 | 163.656580 | 0.5364687 | 109.262957 | 1.0909444 | 0.5475025 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1220764 |  19.678949 | 0.0773478 |  21.483618 | 0.1393428 | 0.9975285 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9694487 | 172.246999 | 0.6142442 | 118.215019 | 1.1773510 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1216396 |  19.650890 | 0.0770711 |  21.516316 | 0.1386525 | 0.9976084 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0307601 |   3.501751 | 0.0194897 |   3.444498 | 0.0472103 | 0.9983050 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2441707 |  27.483959 | 0.1547070 |  36.244346 | 0.3138722 | 0.9741492 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1599056 |  25.580817 | 0.1013164 |  26.434560 | 0.1800431 | 0.9943933 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9388868 | 197.339033 | 0.5948802 | 116.655722 | 1.0883546 | 0.8812965 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1491459 | 195.211865 | 0.7281007 | 116.014415 | 1.3257559 | 0.5931285 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1794754 | 224.582577 | 0.7473175 | 125.655616 | 1.4112526 | 0.1916577 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8912247 | 171.484076 | 0.5646814 | 119.130111 | 1.1034427 | 0.1314345 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0164144 | 195.188875 | 0.6440018 | 114.514808 | 1.2724070 | 0.0115907 |
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
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         7 EARTH       Test  0.0223   7.31 0.0173   5.56 0.0303  0.999
    ## 2 healthyR         7 EARTH       Test  0.00852  1.45 0.00866  1.45 0.0145  1.00 
    ## 3 healthy~         8 NNAR        Test  0.0357   9.03 0.0235  12.7  0.0566  0.998
    ## 4 healthy~         8 NNAR        Test  0.00485  1.32 0.00438  1.39 0.00737 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0308   3.50 0.0195   3.44 0.0472  0.998

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
    ## 1 healthyR.data <tibble [388 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [378 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [328 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [303 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [117 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
