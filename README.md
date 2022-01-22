Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
22 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,152
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

The last day in the data set is 2022-01-20 19:43:44, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1251.09
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27152          |
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
| r\_version     |      18138 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18138 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18138 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2279 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-20 | 2021-08-11 |       424 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size           |          0 |              1 | 1534294.27 | 1878437.41 | 357 | 16923 | 238829.5 | 3247728 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8052.32 |   15270.46 |   1 |   212 |   2798.0 |    8244 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-20 19:43:44 | 2021-08-11 11:07:54 |     15859 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     28 |        60 |

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
    ## 1 healthyR.data <tibble [395 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [385 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [335 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [310 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [124 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.8654860 |  419.4151035 | 0.6179133 | 120.906167 | 1.0610195 | 0.4064366 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1173262 |   51.0483859 | 0.0837650 |  29.930626 | 0.1348381 | 0.9931699 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0519419 |  433.6568575 | 0.7510335 | 120.438054 | 1.3250752 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0642251 |   20.8556266 | 0.0458535 |  17.801414 | 0.0816956 | 0.9940020 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0305368 |   12.6281769 | 0.0218017 |   7.691057 | 0.0458489 | 0.9980643 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0419556 |   48.6980474 | 0.0299541 |  13.852489 | 0.0767772 | 0.9948636 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0688804 |   21.5002673 | 0.0491771 |  18.454846 | 0.0959373 | 0.9917805 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5655228 |  280.2499328 | 0.4037547 | 100.305939 | 0.6600176 | 0.9948274 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0922161 |  670.1208122 | 0.7797872 | 104.901276 | 1.3391945 | 0.4739742 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4175054 |  468.1664020 | 1.0120274 | 143.992622 | 1.6518452 | 0.0965293 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9725495 |  329.9037303 | 0.6943513 | 115.693669 | 1.2602541 | 0.0038574 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0516260 |  433.4737993 | 0.7508079 | 120.428246 | 1.3247515 | 0.0251381 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8287838 |  537.6437202 | 0.9632561 | 158.565548 | 0.9139181 | 0.7278531 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0321075 |   13.0389276 | 0.0373170 |  14.467943 | 0.0399182 | 0.9987766 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.7171325 |  341.7181731 | 0.8334891 | 154.184130 | 0.8779257 | 0.0876169 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0316617 |   18.0723320 | 0.0367989 |  20.469801 | 0.0400038 | 0.9989490 |
| healthyR      |          7 | EARTH                      | Test  | 0.0185378 |    8.6331495 | 0.0215456 |   7.434736 | 0.0203170 | 0.9995892 |
| healthyR      |          8 | NNAR                       | Test  | 0.0090435 |    3.7208633 | 0.0105109 |   4.226955 | 0.0129752 | 0.9997431 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0355433 |   14.8123277 | 0.0413103 |  14.685101 | 0.0504457 | 0.9978395 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4858246 |  385.9913949 | 0.5646508 | 137.735649 | 0.5336897 | 0.9931454 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1689011 | 1207.7359497 | 1.3585582 | 123.382496 | 1.3859016 | 0.6827968 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0138262 |  939.3390716 | 1.1783221 | 121.816842 | 1.2496756 | 0.4731280 |
| healthyR      |         13 | TBATS                      | Test  | 0.6836916 |  288.1337797 | 0.7946223 | 158.953806 | 0.8444474 | 0.1067462 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8085445 |  509.8321851 | 0.9397329 | 151.817454 | 0.9650750 | 0.0301368 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7653732 |  317.3426135 | 0.5070719 | 121.279632 | 0.9232682 | 0.2740670 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0664745 |   77.1339551 | 0.0440404 |  21.501564 | 0.0770479 | 0.9958008 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.7730065 |  451.5952824 | 0.5121291 | 110.854105 | 0.9743535 | 0.2396757 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0735929 |   73.0929671 | 0.0487565 |  22.159414 | 0.0888753 | 0.9961145 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0328011 |    3.0572813 | 0.0217312 |   2.995597 | 0.0747353 | 0.9962060 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0290877 |   10.6682314 | 0.0192710 |  12.666795 | 0.0653587 | 0.9963320 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0856203 |   85.4016619 | 0.0567248 |  21.317097 | 0.1124131 | 0.9951756 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4489318 |  407.0042927 | 0.2974245 |  86.241418 | 0.5310007 | 0.9781826 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1969516 |  618.9830694 | 0.7929995 | 121.118862 | 1.4334175 | 0.5465021 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2471180 |  813.2256253 | 0.8262355 | 141.623277 | 1.4702810 | 0.1649872 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6852761 |  248.3832284 | 0.4540063 | 114.779785 | 0.8296841 | 0.4155342 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.8926627 |  653.2750528 | 0.5914032 | 111.678141 | 1.1238911 | 0.0420036 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0602783 |  430.9648031 | 0.9573066 | 171.503883 | 1.2098211 | 0.6588330 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0672633 |   31.7723955 | 0.0607309 |  19.780453 | 0.0792503 | 0.9961858 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7630644 |  151.3621427 | 0.6889574 | 144.704412 | 0.9337935 | 0.1917637 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0669936 |   26.8118419 | 0.0604873 |  18.026864 | 0.0776418 | 0.9965983 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0115687 |    3.7391104 | 0.0104451 |   3.429883 | 0.0166410 | 0.9997072 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0042981 |    0.9516885 | 0.0038807 |   0.964275 | 0.0058670 | 0.9999691 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0739794 |   28.1647520 | 0.0667947 |  23.572458 | 0.0849698 | 0.9959032 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6234926 |  307.1179824 | 0.5629405 | 137.373665 | 0.6962600 | 0.9828518 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2025682 | 1016.9069833 | 1.0857776 | 120.231427 | 1.3786654 | 0.5136307 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1228586 |  990.7571046 | 1.0138092 | 123.919799 | 1.3806348 | 0.1960697 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5996453 |  187.9888171 | 0.5414091 | 122.478398 | 0.7283107 | 0.3697433 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8767031 |  208.6995619 | 0.7915598 | 176.974244 | 1.0562692 | 0.0012691 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9635096 |  155.3467464 | 0.5357887 | 112.310187 | 1.2084412 | 0.6566969 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0537272 |    7.5632800 | 0.0298766 |   7.411339 | 0.0626260 | 0.9987165 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSAAN                     | Test  | 1.1118983 |  167.3882989 | 0.6183047 | 120.579005 | 1.3425277 | 0.0029035 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0523932 |    7.2831161 | 0.0291348 |   7.063199 | 0.0608263 | 0.9987358 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0169699 |    1.7284106 | 0.0094366 |   1.716610 | 0.0225829 | 0.9996211 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.1414886 |   16.0987918 | 0.0786790 |  19.240986 | 0.2125978 | 0.9854754 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0549236 |    8.0820797 | 0.0305419 |   7.625591 | 0.0664840 | 0.9981418 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.9071548 |  153.8509505 | 0.5044509 | 117.003839 | 1.0652820 | 0.9174454 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1303780 |  194.9857492 | 0.6285809 | 117.133733 | 1.2939042 | 0.5256909 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2024383 |  223.1782355 | 0.6686522 | 130.937206 | 1.4540216 | 0.1061024 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9722556 |  141.7175189 | 0.5406522 | 131.415227 | 1.1231009 | 0.0546659 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0090664 |  138.5419724 | 0.5611219 | 119.862030 | 1.2267843 | 0.0029035 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |           NA |        NA |         NA |        NA |        NA |

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
    ##   package      .model_id .model_desc .type     mae   mape    mase  smape    rmse
    ##   <chr>            <int> <chr>       <chr>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
    ## 1 healthyR.da~         7 EARTH       Test  0.0305  12.6   0.0218   7.69  0.0458 
    ## 2 healthyR             8 NNAR        Test  0.00904  3.72  0.0105   4.23  0.0130 
    ## 3 healthyR.ts          8 NNAR        Test  0.0291  10.7   0.0193  12.7   0.0654 
    ## 4 healthyverse         8 NNAR        Test  0.00430  0.952 0.00388  0.964 0.00587
    ## 5 healthyR.ai          7 EARTH       Test  0.0170   1.73  0.00944  1.72  0.0226 
    ## # ... with 1 more variable: rsq <dbl>

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
    ## 1 healthyR.data <tibble [395 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [385 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [335 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [310 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [124 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
