Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
01 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,047
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

The last day in the data set is 2022-01-30 22:31:29, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1493.88
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28047          |
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
| r\_version     |      18655 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18655 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18655 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2350 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-30 | 2021-08-19 |       434 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1526780.12 | 1875475.31 | 357 | 22980 | 259869 | 3247923 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8199.01 |   15674.04 |   1 |   237 |   2806 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-30 22:31:29 | 2021-08-19 16:42:39 |     16426 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 54M 13S |        60 |

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
    ## 1 healthyR.data <tibble [405 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [395 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [345 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [320 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [134 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.9676951 |  121.583018 | 0.6707150 | 121.117491 | 1.2142849 | 0.5412677 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1011723 |   17.508254 | 0.0701231 |  20.618645 | 0.1603332 | 0.9763002 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9455938 |   97.559209 | 0.6553964 | 126.081862 | 1.1935045 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0616412 |    9.320181 | 0.0427239 |   9.636378 | 0.0757170 | 0.9947451 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0286990 |    3.699374 | 0.0198915 |   3.687093 | 0.0382094 | 0.9988032 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0096781 |    1.338991 | 0.0067079 |   1.355490 | 0.0155391 | 0.9997800 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0625289 |    9.688861 | 0.0433391 |  10.278996 | 0.0853141 | 0.9933655 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5701837 |   73.735402 | 0.3951976 |  90.877121 | 0.6853670 | 0.9940820 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1028625 |  162.273446 | 0.7644003 | 103.350561 | 1.3531647 | 0.5119807 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3043373 |  207.512023 | 0.9040436 | 119.070504 | 1.6861600 | 0.0942324 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9517872 |  102.596181 | 0.6596892 | 119.405243 | 1.2328777 | 0.0928725 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9450378 |   97.465278 | 0.6550111 | 126.146478 | 1.1924724 | 0.1264333 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.6710279 |  553.171712 | 0.8234284 | 155.776375 | 0.8012411 | 0.5465541 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0404190 |   30.872658 | 0.0495987 |  19.461402 | 0.0474414 | 0.9989703 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.4970073 |  423.137174 | 0.6098851 | 122.116378 | 0.5995674 | 0.2011514 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0375221 |   30.767101 | 0.0460439 |  25.464910 | 0.0431829 | 0.9991140 |
| healthyR      |          7 | EARTH                      | Test  | 0.0167550 |   11.761141 | 0.0205604 |   9.294712 | 0.0189938 | 0.9995084 |
| healthyR      |          8 | NNAR                       | Test  | 0.0092527 |    8.262401 | 0.0113542 |  11.001990 | 0.0131202 | 0.9997073 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0410225 |   32.849516 | 0.0503393 |  19.442530 | 0.0507403 | 0.9983009 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4272113 |  476.906683 | 0.5242373 | 122.568134 | 0.4774871 | 0.9970209 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2079961 | 1638.461431 | 1.4823501 | 127.253171 | 1.4250501 | 0.6289212 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0152006 | 1714.269985 | 1.2457679 | 120.915898 | 1.2199204 | 0.3797030 |
| healthyR      |         13 | TBATS                      | Test  | 0.4768023 |  455.562908 | 0.5850912 | 116.723058 | 0.5855644 | 0.2294986 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.5347792 |  357.366698 | 0.6562356 | 127.179663 | 0.6687936 | 0.1360394 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7712930 |  559.945303 | 0.6299909 | 131.340668 | 0.9541620 | 0.2890769 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0716350 |  138.869867 | 0.0585114 |  28.597917 | 0.0830688 | 0.9953148 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7242514 |  430.845667 | 0.5915674 | 138.073707 | 0.9273737 | 0.1934185 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0681659 |  118.575110 | 0.0556778 |  28.002301 | 0.0790563 | 0.9956613 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0306577 |    3.068957 | 0.0250412 |   3.017600 | 0.0711580 | 0.9964486 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0222649 |   12.110696 | 0.0181860 |  12.677475 | 0.0554863 | 0.9970496 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0734920 |  144.179521 | 0.0600282 |  29.009644 | 0.0897021 | 0.9944558 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4592335 |  634.189522 | 0.3751012 | 106.102974 | 0.5229952 | 0.9787504 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2379878 | 1128.760220 | 1.0111864 | 129.025059 | 1.4687574 | 0.5637598 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1580437 | 1502.764207 | 0.9458882 | 127.739172 | 1.4314294 | 0.1946769 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.7361754 |  450.725482 | 0.6013069 | 140.307160 | 0.8971174 | 0.2707263 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8216507 |  358.790141 | 0.6711230 | 158.601968 | 1.0222878 | 0.1330511 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.6998466 |  139.814070 | 0.6450954 | 148.809213 | 0.8890763 | 0.4038487 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0556346 |   15.274564 | 0.0512821 |  11.954093 | 0.0700419 | 0.9929558 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.6450872 |  451.826271 | 0.5946200 | 117.668261 | 0.7446301 | 0.1001196 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0503603 |    9.662526 | 0.0464204 |   9.022402 | 0.0667899 | 0.9929558 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0142449 |    3.707395 | 0.0131305 |   3.336714 | 0.0280767 | 0.9993563 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0053444 |    1.694326 | 0.0049263 |   1.513550 | 0.0167735 | 0.9996539 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0895586 |   23.856727 | 0.0825522 |  27.274755 | 0.1009875 | 0.9920587 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4943930 |  210.956939 | 0.4557151 | 111.426398 | 0.5867130 | 0.9783775 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9783803 |  772.856348 | 0.9018385 | 112.602613 | 1.1350701 | 0.4890140 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2673897 | 1260.165036 | 1.1682377 | 132.961400 | 1.4824476 | 0.1116178 |
| healthyverse  |         13 | TBATS                      | Test  | 0.6802783 |  301.277271 | 0.6270580 | 135.315922 | 0.8393311 | 0.0564831 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.6167603 |  359.270673 | 0.5685092 |  97.955721 | 0.7817281 | 0.1279528 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0196607 |  536.454842 | 0.7290473 | 140.676490 | 1.1881225 | 0.2189913 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0519515 |   36.901733 | 0.0371448 |  17.511408 | 0.0618135 | 0.9988850 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.8407075 |  293.332055 | 0.6010975 | 150.470024 | 0.9713938 | 0.0542400 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0477998 |   28.956362 | 0.0341764 |  15.719915 | 0.0554766 | 0.9989137 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0120246 |    2.723365 | 0.0085975 |   2.548768 | 0.0161331 | 0.9997324 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0574267 |   11.874330 | 0.0410596 |  12.728300 | 0.0756416 | 0.9938988 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0581256 |   53.834602 | 0.0415592 |  22.600070 | 0.0694911 | 0.9977962 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6949949 |  521.920769 | 0.4969145 | 126.693307 | 0.7990219 | 0.9779146 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1686900 |  570.289008 | 0.8356018 | 120.602108 | 1.3999545 | 0.4465258 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2434751 |  600.288439 | 0.8890724 | 139.111104 | 1.5348606 | 0.0894402 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9183612 |  417.403140 | 0.6566192 | 165.622129 | 1.0256703 | 0.0190630 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.8750521 |  277.869172 | 0.6256536 | 143.144044 | 1.0570028 | 0.1373873 |
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
    ## 1 healthyR~         8 NNAR        Test  0.00968  1.34 0.00671  1.36 0.0155 1.00 
    ## 2 healthyR          8 NNAR        Test  0.00925  8.26 0.0114  11.0  0.0131 1.00 
    ## 3 healthyR~         8 NNAR        Test  0.0223  12.1  0.0182  12.7  0.0555 0.997
    ## 4 healthyv~         8 NNAR        Test  0.00534  1.69 0.00493  1.51 0.0168 1.00 
    ## 5 healthyR~         7 EARTH       Test  0.0120   2.72 0.00860  2.55 0.0161 1.00

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
    ## 1 healthyR.data <tibble [405 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [395 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [345 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [320 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [134 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
