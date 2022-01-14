Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
14 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 26,686
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

The last day in the data set is 2022-01-12 22:01:30, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1061.38
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 26686          |
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
| r\_version     |      17873 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      17873 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      17873 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2239 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-12 | 2021-08-06 |       416 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1532190.58 | 1878493.01 | 357 | 16906.00 | 238780 | 3246663 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8083.58 |   15303.71 |   1 |   213.75 |   2806 |    8265 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-12 22:01:30 | 2021-08-06 06:15:25 |     15567 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     45 |        60 |

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
    ## 1 healthyR.data <tibble [387 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [377 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [327 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [302 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [116 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.7053898 | 269.934896 | 0.5757687 |  99.152574 | 0.9071242 | 0.4612870 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0774360 |  20.633448 | 0.0632065 |  25.643869 | 0.1000770 | 0.9925086 |
| healthyR.data |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0152196 | 411.318661 | 0.8286647 | 105.386721 | 1.3077718 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0944642 |  31.635165 | 0.0771056 |  25.173724 | 0.1145140 | 0.9938806 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0285894 |   9.474145 | 0.0233359 |   7.096918 | 0.0453124 | 0.9980498 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0138187 |   3.608275 | 0.0112794 |   4.342082 | 0.0210175 | 0.9995582 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0963013 |  30.035985 | 0.0786052 |  25.552742 | 0.1253024 | 0.9918187 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4785165 | 215.898133 | 0.3905852 |  85.012660 | 0.5841556 | 0.9924022 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0927295 | 548.098724 | 0.8919316 | 102.356662 | 1.3745670 | 0.5636420 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3111258 | 390.475167 | 1.0701958 | 131.672091 | 1.5682726 | 0.1967222 |
| healthyR.data |         13 | TBATS                      | Test  | 0.8672534 | 312.881312 | 0.7078885 | 100.621285 | 1.1546931 | 0.1417424 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0142785 | 410.818921 | 0.8278966 | 105.356429 | 1.3067821 | 0.0000007 |
| healthyR.data |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8363173 | 321.379867 | 0.9034270 | 150.814155 | 0.9267675 | 0.5350043 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0356065 |  13.705062 | 0.0384637 |  16.483041 | 0.0429508 | 0.9991646 |
| healthyR      |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 1.0800576 | 425.844108 | 1.1667261 | 153.139999 | 1.2077095 | 0.0512075 |
| healthyR      |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0383497 |  16.934705 | 0.0414270 |  17.879543 | 0.0476960 | 0.9991816 |
| healthyR      |          7 | EARTH                      | Test  | 0.0181362 |   5.610443 | 0.0195915 |   5.229538 | 0.0211549 | 0.9994209 |
| healthyR      |          8 | NNAR                       | Test  | 0.1101558 |  16.494875 | 0.1189951 |  13.437812 | 0.2884727 | 0.9456204 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0416505 |  17.924368 | 0.0449927 |  18.591988 | 0.0558972 | 0.9983010 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4661421 | 214.135462 | 0.5035473 | 126.773615 | 0.5212607 | 0.9942826 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.3005310 | 662.560962 | 1.4048911 | 131.570089 | 1.5656100 | 0.5134125 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2228586 | 536.756448 | 1.3209860 | 126.153322 | 1.6141644 | 0.2444005 |
| healthyR      |         13 | TBATS                      | Test  | 0.7730732 | 248.755945 | 0.8351079 | 147.148279 | 0.9117422 | 0.1063755 |
| healthyR      |         14 | THETA METHOD               | Test  | 1.0821389 | 463.907135 | 1.1689744 | 150.265284 | 1.2256332 | 0.0006891 |
| healthyR      |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7157463 | 307.165538 | 0.4652561 | 104.526190 | 0.9143586 | 0.4612031 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0625478 |  47.953670 | 0.0406579 |  20.535222 | 0.0724642 | 0.9964291 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.0710587 | 606.049167 | 0.6962196 | 115.783033 | 1.2645643 | 0.1352172 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0815393 |  47.654155 | 0.0530030 |  21.305705 | 0.1003493 | 0.9967072 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0273406 |   3.063727 | 0.0177722 |   3.032518 | 0.0474320 | 0.9979422 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0429619 |   8.181223 | 0.0279265 |   6.929893 | 0.1116167 | 0.9884470 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.1067587 |  66.980103 | 0.0693963 |  30.607442 | 0.1321951 | 0.9950793 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.3807850 | 224.262356 | 0.2475214 |  78.800702 | 0.4509173 | 0.9890941 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2609139 | 699.542756 | 0.8196310 | 120.275507 | 1.4655197 | 0.5253688 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4328787 | 841.540394 | 0.9314132 | 132.681018 | 1.6975440 | 0.1261516 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6960845 | 204.359176 | 0.4524754 | 107.516810 | 0.8813657 | 0.3209159 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.0849021 | 682.728069 | 0.7052182 | 112.741229 | 1.3317979 | 0.0141247 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1297541 | 400.165720 | 1.0711067 | 160.505708 | 1.2884230 | 0.5236065 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0650127 |  26.040917 | 0.0616378 |  19.697389 | 0.0792513 | 0.9969793 |
| healthyverse  |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 1.1694017 | 388.936582 | 1.1086962 | 162.540766 | 1.3250634 | 0.2239018 |
| healthyverse  |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0747898 |  26.672174 | 0.0709074 |  20.230996 | 0.0875423 | 0.9978031 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0092253 |   2.695100 | 0.0087464 |   2.588305 | 0.0136345 | 0.9997855 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0070673 |   2.102594 | 0.0067004 |   2.175650 | 0.0120265 | 0.9998406 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0641537 |  21.561165 | 0.0608234 |  16.096176 | 0.0768794 | 0.9973077 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6131804 | 260.719572 | 0.5813493 | 135.351683 | 0.6874895 | 0.9774697 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1885398 | 625.354355 | 1.1268408 | 123.936818 | 1.3899336 | 0.5085317 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2727464 | 563.535561 | 1.2066761 | 124.170142 | 1.6553683 | 0.2010508 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5789747 | 156.642098 | 0.5489192 | 142.893511 | 0.6803510 | 0.3696412 |
| healthyverse  |         14 | THETA METHOD               | Test  | 1.2236061 | 441.067616 | 1.1600868 | 159.886390 | 1.4244168 | 0.0005533 |
| healthyverse  |         15 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8572900 | 169.833439 | 0.5592536 | 109.600160 | 1.1036405 | 0.5320108 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.1258140 |  20.964729 | 0.0820748 |  22.469788 | 0.1424843 | 0.9973345 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9370905 | 170.576223 | 0.6113115 | 116.025118 | 1.1600489 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.1238854 |  20.766535 | 0.0808167 |  22.464283 | 0.1401553 | 0.9975350 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0246379 |   2.906270 | 0.0160726 |   2.877158 | 0.0339500 | 0.9989065 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.3401294 |  39.858051 | 0.2218836 |  52.125132 | 0.4246320 | 0.9468803 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1656337 |  27.163884 | 0.1080512 |  27.763190 | 0.1861720 | 0.9940188 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9256979 | 196.056761 | 0.6038796 | 117.108519 | 1.0659581 | 0.8808375 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0574881 | 190.617676 | 0.6898530 |  98.239730 | 1.2819340 | 0.6105992 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2139339 | 242.570814 | 0.7919105 | 117.650265 | 1.4368069 | 0.1576505 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9499884 | 181.450625 | 0.6197255 | 120.074721 | 1.1540751 | 0.0485836 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9937532 | 194.948612 | 0.6482755 | 112.906008 | 1.2655233 | 0.0004356 |
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
    ## 1 healthyR~         8 NNAR        Test  0.0138   3.61 0.0113   4.34 0.0210 1.00 
    ## 2 healthyR          7 EARTH       Test  0.0181   5.61 0.0196   5.23 0.0212 0.999
    ## 3 healthyR~         7 EARTH       Test  0.0273   3.06 0.0178   3.03 0.0474 0.998
    ## 4 healthyv~         8 NNAR        Test  0.00707  2.10 0.00670  2.18 0.0120 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0246   2.91 0.0161   2.88 0.0340 0.999

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
    ## 1 healthyR.data <tibble [387 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [377 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [327 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [302 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [116 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
