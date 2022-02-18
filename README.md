Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
18 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 29,276
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

The last day in the data set is 2022-02-16 21:22:50, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1900.74
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 29276          |
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
| r\_version     |      19491 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19491 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19491 |           0.33 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2419 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-16 | 2021-08-26 |       451 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size           |          0 |              1 | 1515128.40 | 1870670.69 | 357 | 19045 | 261432.5 | 3247923 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8287.05 |   15796.77 |   1 |   307 |   2770.0 |    8308 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-16 21:22:50 | 2021-08-26 20:21:52 |     17065 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   54.5 |        60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [393|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [384|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [333|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [309|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [123|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.1421280 | 303.6637727 | 0.9234819 | 147.9002794 | 1.4081839 | 0.0084064 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0631100 |  33.6164477 | 0.0510284 |  17.8164586 | 0.0835989 | 0.9938190 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9199001 | 100.7821353 | 0.7437967 | 188.0658363 | 1.0682706 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0695328 |  31.8848826 | 0.0562216 |  19.3671501 | 0.0973482 | 0.9927059 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0737177 |  33.4985589 | 0.0596053 |  19.9557121 | 0.1386967 | 0.9818726 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0142354 |   5.0967006 | 0.0115102 |   6.8301787 | 0.0208853 | 0.9995551 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0718566 |  28.9729332 | 0.0581005 |  19.8732083 | 0.1058334 | 0.9914215 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5017448 | 199.6033114 | 0.4056920 |  79.8048099 | 0.6229119 | 0.9909761 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1376677 | 775.0808289 | 0.9198755 |  97.4472473 | 1.4399444 | 0.3746600 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5141331 | 903.4438255 | 1.2242713 | 141.7571894 | 1.7933410 | 0.0072193 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9902733 | 170.3725901 | 0.8006979 | 147.0209859 | 1.2402078 | 0.0084904 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9192493 |  99.3113136 | 0.7432705 | 188.6506325 | 1.0668387 | 0.0088502 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8318501 | 351.4142882 | 0.9335573 | 153.2840628 | 1.0066049 | 0.3418062 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0639359 |  25.9204423 | 0.0717532 |  18.0713891 | 0.0750741 | 0.9948272 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.6388753 | 144.3683715 | 0.7169882 | 115.8421065 | 0.8683624 | 0.2199267 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0590575 |  22.1713753 | 0.0662783 |  17.7032947 | 0.0707837 | 0.9948359 |
| healthyR      |          7 | EARTH                      | Test  | 0.0341417 |   7.8382130 | 0.0383160 |   9.4170893 | 0.0758025 | 0.9934150 |
| healthyR      |          8 | NNAR                       | Test  | 0.0076706 |   4.5985443 | 0.0086085 |   5.9540768 | 0.0138508 | 0.9998529 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0640121 |  34.4520116 | 0.0718386 |  22.2462152 | 0.0771145 | 0.9938281 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5582232 | 367.6344102 | 0.6264751 | 123.1014539 | 0.6475447 | 0.9739218 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9518487 | 337.7892725 | 1.0682277 |  97.5246831 | 1.2423858 | 0.5456621 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0562422 | 406.0542730 | 1.1853850 | 128.8683445 | 1.3325895 | 0.1210836 |
| healthyR      |         13 | TBATS                      | Test  | 0.6831506 | 178.8462631 | 0.7666768 | 124.1279234 | 0.8884998 | 0.2364581 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7834137 | 132.3232095 | 0.8791988 | 177.1722962 | 0.9819475 | 0.0695106 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2926677 | 767.3607862 | 1.5444833 | 165.4445546 | 1.4953950 | 0.1522557 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0799218 |  54.7688187 | 0.0954908 |  20.2437359 | 0.0972304 | 0.9896503 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.9085845 | 488.2947595 | 1.0855795 | 150.5036567 | 1.1595885 | 0.2645020 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0754812 |  50.7978038 | 0.0901852 |  22.3578020 | 0.0896808 | 0.9900159 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0157957 |   4.1870592 | 0.0188728 |   3.9573288 | 0.0219191 | 0.9994366 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0142281 |   3.2188880 | 0.0169998 |   2.9750377 | 0.0307834 | 0.9990844 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0879044 |  58.2359812 | 0.1050285 |  25.6684761 | 0.1040513 | 0.9877026 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7387231 | 549.4859158 | 0.8826286 | 141.2119690 | 0.8271107 | 0.9727136 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0014961 | 332.8708553 | 1.1965905 |  87.6460807 | 1.4042398 | 0.6019031 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0774173 | 671.3447370 | 1.2873015 | 115.0371874 | 1.5375534 | 0.2632745 |
| healthyR.ts   |         13 | TBATS                      | Test  | 1.0030611 | 445.3442911 | 1.1984604 | 161.7429165 | 1.2149432 | 0.0985859 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9712103 | 419.6154382 | 1.1604050 | 157.9202500 | 1.2364807 | 0.0691533 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.1717638 | 124.0681933 | 1.1540865 | 163.4699192 | 1.3841013 | 0.0947541 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0623339 |   6.9734431 | 0.0613935 |   6.7323539 | 0.0872868 | 0.9905916 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7861977 | 137.5221030 | 0.7743371 | 103.9688553 | 0.9345010 | 0.0310605 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0597434 |   6.4541916 | 0.0588421 |   6.2787319 | 0.0886039 | 0.9905787 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0215177 |   1.9062702 | 0.0211931 |   1.9140685 | 0.0434216 | 0.9985957 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0133854 |   0.7667204 | 0.0131835 |   0.7731224 | 0.0333418 | 0.9991410 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0780679 |  12.6165072 | 0.0768902 |  11.3976306 | 0.0963001 | 0.9900710 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5800539 |  70.6522485 | 0.5713032 |  89.9402703 | 0.6988783 | 0.9626643 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8939681 | 193.1061908 | 0.8804817 |  91.5254198 | 1.0650767 | 0.4064021 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2143955 | 253.5902516 | 1.1960751 | 133.1056311 | 1.4552945 | 0.0256564 |
| healthyverse  |         13 | TBATS                      | Test  | 1.1028163 | 150.3776912 | 1.0861792 | 158.5223425 | 1.2565157 | 0.0072384 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7913056 | 121.4057667 | 0.7793679 |  99.3333129 | 0.9743360 | 0.0011975 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.8028755 | 340.4965205 | 0.8369839 | 161.3203093 | 0.9840242 | 0.2693838 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0779678 |  49.7186469 | 0.0812800 |  28.6736388 | 0.0909842 | 0.9922539 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.8136100 | 393.8437864 | 0.8481744 | 163.4155060 | 0.9810841 | 0.1705969 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0658716 |  30.3137948 | 0.0686700 |  23.6289254 | 0.0840614 | 0.9922575 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0160102 |   2.3196990 | 0.0166903 |   2.2932105 | 0.0293355 | 0.9989751 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0276351 |  10.4541377 | 0.0288091 |   9.7950400 | 0.0470989 | 0.9974060 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0795124 |  64.0579415 | 0.0828903 |  29.8279460 | 0.0969121 | 0.9901396 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8943958 | 876.0234146 | 0.9323922 | 163.0816375 | 0.9691339 | 0.9502330 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0759479 | 727.9026251 | 1.1216572 | 130.5879535 | 1.3193679 | 0.5282352 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1098407 | 522.7494368 | 1.1569899 | 133.2726042 | 1.5300482 | 0.1554505 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.8371087 | 474.1581494 | 0.8726714 | 161.3228880 | 0.9882017 | 0.1799052 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.8747105 | 341.5907437 | 0.9118707 | 162.4347815 | 1.0939694 | 0.0422379 |
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
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         8 NNAR        Test  0.0142  5.10  0.0115  6.83  0.0209 1.00 
    ## 2 healthyR          8 NNAR        Test  0.00767 4.60  0.00861 5.95  0.0139 1.00 
    ## 3 healthyR~         7 EARTH       Test  0.0158  4.19  0.0189  3.96  0.0219 0.999
    ## 4 healthyv~         8 NNAR        Test  0.0134  0.767 0.0132  0.773 0.0333 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0160  2.32  0.0167  2.29  0.0293 0.999

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [393|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [384|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [333|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [309|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [123|28]> <mdl_time_tbl>

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
