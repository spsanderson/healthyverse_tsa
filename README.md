Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
18 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,976
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

The last day in the data set is 2022-01-16 17:21:06, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1152.71
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26976          |
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
| r\_version     |      18060 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18060 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18060 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2258 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-16 | 2021-08-09 |       420 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532442.81 | 1878253.45 | 357 | 16906 | 238827 | 3247068 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8049.26 |   15267.51 |   1 |   227 |   2802 |    8244 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-16 17:21:06 | 2021-08-09 12:10:14 |     15726 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   32.5 |        60 |

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
    ## 1 healthyR.data <tibble [391 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [381 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [331 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [306 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [120 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.7152006 | 284.4970261 | 0.5324859 | 103.8031471 | 0.9103782 | 0.4822482 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1418399 |  23.9577034 | 0.1056036 |  25.2993334 | 0.3363827 | 0.9194735 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.8669863 | 280.6605441 | 0.6454944 | 102.6279577 | 1.1546004 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0804233 |  23.3303309 | 0.0598773 |  18.0208670 | 0.0998431 | 0.9945294 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0299082 |   9.0772368 | 0.0222675 |   6.6565072 | 0.0460942 | 0.9980244 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0252081 |  11.4703512 | 0.0187681 |   8.3777331 | 0.0364346 | 0.9990352 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0838144 |  22.8754833 | 0.0624020 |  18.4398397 | 0.1147350 | 0.9921337 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4691130 | 193.3024646 | 0.3492671 |  81.3014424 | 0.5822599 | 0.9954854 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0197261 | 440.8793455 | 0.7592132 |  95.7120394 | 1.2651798 | 0.6048236 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2315730 | 319.9611841 | 0.9169389 | 135.1247728 | 1.3860615 | 0.2097818 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8098210 | 196.5101080 | 0.6029333 | 107.8357361 | 1.0555793 | 0.0103167 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.8658609 | 279.9207326 | 0.6446565 | 102.6090059 | 1.1531515 | 0.0640759 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8947030 | 434.6672379 | 1.0119173 | 158.8576333 | 0.9764062 | 0.7242426 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0342829 |  12.5020696 | 0.0387743 |  15.3000440 | 0.0435016 | 0.9989606 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.8594582 | 404.1018116 | 0.9720551 | 151.8204146 | 1.0115786 | 0.1156605 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0359059 |  17.2687757 | 0.0406099 |  19.8029536 | 0.0465323 | 0.9990809 |
| healthyR      |          7 | EARTH                      | Test  | 0.0174120 |   6.9217522 | 0.0196932 |   6.3174054 | 0.0192077 | 0.9996279 |
| healthyR      |          8 | NNAR                       | Test  | 0.0106648 |   3.3293979 | 0.0120620 |   3.1579269 | 0.0183072 | 0.9992417 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0379816 |  16.2019375 | 0.0429575 |  17.2220792 | 0.0539774 | 0.9982090 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4851345 | 297.5052164 | 0.5486916 | 133.5416177 | 0.5379893 | 0.9932776 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2332863 | 901.2197257 | 1.3948581 | 129.0277323 | 1.4574905 | 0.6411937 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1567713 | 607.6855179 | 1.3083190 | 129.7632596 | 1.5182820 | 0.4232668 |
| healthyR      |         13 | TBATS                      | Test  | 0.8752360 | 389.8685149 | 0.9898999 | 154.8010621 | 1.0141966 | 0.2402118 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8871370 | 466.6458404 | 1.0033601 | 150.5449757 | 1.0438567 | 0.0032079 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7034415 | 192.7719390 | 0.4722017 | 107.2903800 | 0.8840626 | 0.4221058 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0640216 |  50.0382220 | 0.0429760 |  20.7672314 | 0.0747212 | 0.9961713 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.8117624 | 308.7755131 | 0.5449146 | 116.0425846 | 1.0071501 | 0.1650231 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0787409 |  49.3175046 | 0.0528567 |  21.9950090 | 0.0967533 | 0.9964592 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0339916 |   3.0364150 | 0.0228176 |   2.9904714 | 0.0702749 | 0.9963491 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0326506 |   9.9282980 | 0.0219175 |   8.3520446 | 0.0587031 | 0.9971402 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0989099 |  64.2010944 | 0.0663956 |  27.1784473 | 0.1237073 | 0.9956982 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4299405 | 283.9536574 | 0.2886077 |  85.4891547 | 0.5251498 | 0.9894965 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2626845 | 707.8092703 | 0.8476067 | 118.2726874 | 1.5275187 | 0.5320020 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2947255 | 841.0520464 | 0.8691150 | 138.5223507 | 1.5896785 | 0.1530870 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6656982 | 187.0941747 | 0.4468656 | 113.7413203 | 0.8582389 | 0.4899421 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9091033 | 411.8758918 | 0.6102570 | 117.5671368 | 1.1082513 | 0.0051342 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9359336 | 321.2266919 | 0.8296245 | 161.6519681 | 1.1133204 | 0.4704246 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0626645 |  24.8504724 | 0.0555467 |  18.6754099 | 0.0765613 | 0.9962825 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7912050 | 163.3503361 | 0.7013350 | 154.7646133 | 0.9832369 | 0.2726853 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0660343 |  23.0286638 | 0.0585337 |  17.8088065 | 0.0776315 | 0.9966652 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0112192 |   3.3736668 | 0.0099448 |   3.2030214 | 0.0159923 | 0.9997337 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0029536 |   0.6787806 | 0.0026181 |   0.6796055 | 0.0041796 | 0.9999776 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0614646 |  21.0773838 | 0.0544830 |  16.5038241 | 0.0729027 | 0.9962972 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5902847 | 242.2657263 | 0.5232365 | 133.4089485 | 0.6672101 | 0.9840358 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2157725 | 774.4301728 | 1.0776775 | 125.0604654 | 1.3806616 | 0.5545146 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1581978 | 706.5998084 | 1.0266426 | 125.1765408 | 1.4633934 | 0.2392710 |
| healthyverse  |         13 | TBATS                      | Test  | 0.6466622 | 131.2709068 | 0.5732103 | 132.6448153 | 0.8217414 | 0.4639710 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8646930 | 216.7588396 | 0.7664759 | 166.4663672 | 1.0663220 | 0.0231557 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8165448 |  97.5797036 | 0.4790342 | 115.6307003 | 1.0080351 | 0.4101779 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1275797 |  19.6581502 | 0.0748459 |  21.4508122 | 0.1428044 | 0.9985708 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9472596 | 120.3291599 | 0.5557193 | 125.7356296 | 1.1269287 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1269641 |  19.5197591 | 0.0744848 |  21.4201648 | 0.1418591 | 0.9986180 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0276191 |   2.7570685 | 0.0162030 |   2.7217724 | 0.0398705 | 0.9988006 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2637932 |  32.5021420 | 0.1547569 |  37.9647911 | 0.3232246 | 0.9663288 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1682274 |  26.0792395 | 0.0986923 |  27.0804618 | 0.1843118 | 0.9972941 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8459330 | 150.8656390 | 0.4962751 | 110.3005459 | 1.0115710 | 0.9301504 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0667048 | 171.7170288 | 0.6257931 | 103.9393628 | 1.2837824 | 0.6064259 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1450369 | 194.9610669 | 0.6717474 | 130.0313036 | 1.3440080 | 0.1887057 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8902977 | 112.2152806 | 0.5223021 | 125.3738292 | 1.0706091 | 0.1021073 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9677053 | 132.3514799 | 0.5677140 | 118.2892648 | 1.1832282 | 0.0225583 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |

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
    ##   package .model_id .model_desc .type     mae   mape    mase smape    rmse   rsq
    ##   <chr>       <int> <chr>       <chr>   <dbl>  <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 health~         8 NNAR        Test  0.0252  11.5   0.0188  8.38  0.0364  0.999
    ## 2 health~         8 NNAR        Test  0.0107   3.33  0.0121  3.16  0.0183  0.999
    ## 3 health~         8 NNAR        Test  0.0327   9.93  0.0219  8.35  0.0587  0.997
    ## 4 health~         8 NNAR        Test  0.00295  0.679 0.00262 0.680 0.00418 1.00 
    ## 5 health~         7 EARTH       Test  0.0276   2.76  0.0162  2.72  0.0399  0.999

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
    ## 1 healthyR.data <tibble [391 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [381 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [331 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [306 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [120 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
