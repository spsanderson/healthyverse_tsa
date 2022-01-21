Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
21 January, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 27,094
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

The last day in the data set is 2022-01-19 19:13:09, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1226.58
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 27094          |
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
| r\_version     |      18084 |           0.33 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18084 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18084 |           0.33 |   7 |  15 |     0 |        10 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        15 |          0 |
| country        |       2279 |           0.92 |   2 |   2 |     0 |        99 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-01-19 | 2021-08-10 |       423 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1535433.0 | 1878707.19 | 357 | 16923 | 238833 | 3247734 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8063.8 |   15277.09 |   1 |   221 |   2805 |    8264 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-01-19 19:13:09 | 2021-08-10 18:24:25 |     15829 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     53 |        60 |

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
    ## 1 healthyR.data <tibble [394 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [384 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [334 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [309 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [123 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 0.8420271 |  425.386911 | 0.5877403 | 117.136079 | 1.0295785 | 0.4154508 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.1166500 |   50.282716 | 0.0814224 |  30.575608 | 0.1343231 | 0.9927042 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0183016 |  431.779581 | 0.7107809 | 116.315017 | 1.2938167 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0626018 |   21.017654 | 0.0436965 |  17.671307 | 0.0805764 | 0.9937169 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0296780 |   12.145223 | 0.0207154 |   7.568189 | 0.0453753 | 0.9979799 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0193664 |    7.278563 | 0.0135179 |   5.647108 | 0.0284800 | 0.9991737 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0677730 |   18.264749 | 0.0473060 |  19.121147 | 0.0940367 | 0.9912187 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5827801 |  292.581366 | 0.4067842 | 101.247201 | 0.6773786 | 0.9917577 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0795683 |  645.155698 | 0.7535455 | 109.858751 | 1.3284165 | 0.4527114 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3846395 |  454.698058 | 0.9664872 | 142.101018 | 1.6303693 | 0.0818601 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9673734 |  352.645083 | 0.6752328 | 114.519869 | 1.2351498 | 0.0885164 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0180415 |  431.633775 | 0.7105994 | 116.306712 | 1.2935423 | 0.0141853 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8377146 |  541.755685 | 0.9623128 | 156.414306 | 0.9213222 | 0.7228519 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0329902 |   13.911976 | 0.0378971 |  15.189723 | 0.0412725 | 0.9986950 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.7543693 |  399.064088 | 0.8665711 | 150.453651 | 0.9058020 | 0.1561977 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0333706 |   19.182858 | 0.0383340 |  21.571909 | 0.0422103 | 0.9988696 |
| healthyR      |          7 | EARTH                      | Test  | 0.0171062 |    8.840012 | 0.0196505 |   7.465842 | 0.0189717 | 0.9995979 |
| healthyR      |          8 | NNAR                       | Test  | 0.0142989 |    7.745195 | 0.0164257 |   6.831613 | 0.0220035 | 0.9988015 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0352544 |   15.330369 | 0.0404980 |  15.225004 | 0.0503194 | 0.9979752 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4707369 |  369.394381 | 0.5407524 | 132.428554 | 0.5195972 | 0.9943525 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1711932 | 1158.552552 | 1.3453917 | 129.817274 | 1.3872946 | 0.6668902 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0076866 |  933.864980 | 1.1575658 | 122.596594 | 1.2369440 | 0.4433520 |
| healthyR      |         13 | TBATS                      | Test  | 0.6730281 |  297.063715 | 0.7731315 | 150.313637 | 0.8270319 | 0.1729690 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.8029103 |  517.878921 | 0.9223318 | 147.601519 | 0.9670499 | 0.0278576 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.7507339 |  287.505300 | 0.5002952 | 119.106480 | 0.9132591 | 0.2871632 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0636195 |   72.637936 | 0.0423966 |  21.204880 | 0.0748515 | 0.9959677 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.8660224 |  566.427831 | 0.5771243 | 112.912490 | 1.0588218 | 0.2143943 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0729738 |   68.877507 | 0.0486303 |  22.015612 | 0.0897254 | 0.9962981 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0326825 |    3.086849 | 0.0217799 |   3.025568 | 0.0742732 | 0.9962197 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0216755 |    4.981590 | 0.0144447 |   4.573537 | 0.0482783 | 0.9979060 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0894045 |   84.601440 | 0.0595799 |  22.659232 | 0.1168986 | 0.9953970 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4615907 |  393.234985 | 0.3076078 |  88.686252 | 0.5467333 | 0.9754907 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2053353 |  601.889438 | 0.8032452 | 122.749812 | 1.4593604 | 0.5310125 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3381811 |  796.985916 | 0.8917748 | 140.147037 | 1.5842764 | 0.1500596 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.6870735 |  239.232800 | 0.4578713 | 113.712656 | 0.8344165 | 0.4121465 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9240048 |  677.073346 | 0.6157643 | 113.059999 | 1.1517238 | 0.0285131 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.0316854 |  431.646804 | 0.9060020 | 168.184083 | 1.1829018 | 0.6610524 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0654960 |   31.906162 | 0.0575171 |  20.199250 | 0.0782000 | 0.9961705 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.6908326 |  169.522048 | 0.6066730 | 136.380754 | 0.8582180 | 0.2263060 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0643510 |   26.029405 | 0.0565115 |  17.757030 | 0.0762533 | 0.9966147 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0100766 |    3.305269 | 0.0088491 |   3.073393 | 0.0149410 | 0.9997520 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0057248 |    2.125437 | 0.0050273 |   1.975058 | 0.0078114 | 0.9999627 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0692047 |   28.147984 | 0.0607739 |  24.686545 | 0.0798975 | 0.9961934 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5610047 |  275.868577 | 0.4926611 | 130.975321 | 0.6297122 | 0.9852965 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.9740557 |  782.485781 | 0.8553929 | 116.646466 | 1.1536388 | 0.5447659 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1062992 | 1000.506797 | 0.9715260 | 122.307656 | 1.3734366 | 0.1747982 |
| healthyverse  |         13 | TBATS                      | Test  | 0.5705685 |  181.409206 | 0.5010599 | 120.610299 | 0.6997069 | 0.4286474 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8210730 |  195.101870 | 0.7210471 | 171.196867 | 1.0146252 | 0.0020815 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9014201 |  131.612199 | 0.5017084 | 107.555239 | 1.1452004 | 0.6638403 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0760759 |   11.506552 | 0.0423420 |  12.020537 | 0.0902984 | 0.9986679 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANN                     | Test  | 0.9467027 |  109.984803 | 0.5269115 | 121.621240 | 1.1422664 |        NA |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0749524 |   11.301620 | 0.0417167 |  11.722621 | 0.0891532 | 0.9987073 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0288473 |    2.679466 | 0.0160557 |   2.634594 | 0.0474715 | 0.9985634 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.2658488 |   27.561176 | 0.1479650 |  33.936256 | 0.3319694 | 0.9757718 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.1076822 |   15.551442 | 0.0599333 |  16.344413 | 0.1283553 | 0.9975217 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.8673024 |  130.630483 | 0.4827193 | 112.303595 | 1.0333684 | 0.9033607 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 0.8800017 |  120.661720 | 0.4897874 |  87.147217 | 1.0785915 | 0.6678803 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1719183 |  176.868646 | 0.6522610 | 127.420932 | 1.4037468 | 0.1383827 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9427256 |  125.025994 | 0.5246980 | 127.744939 | 1.1059831 | 0.0728183 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9863383 |  122.917265 | 0.5489717 | 117.648682 | 1.2039422 | 0.0103329 |
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
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy~         8 NNAR        Test  0.0194   7.28 0.0135   5.65 0.0285  0.999
    ## 2 healthyR         7 EARTH       Test  0.0171   8.84 0.0197   7.47 0.0190  1.00 
    ## 3 healthy~         8 NNAR        Test  0.0217   4.98 0.0144   4.57 0.0483  0.998
    ## 4 healthy~         8 NNAR        Test  0.00572  2.13 0.00503  1.98 0.00781 1.00 
    ## 5 healthy~         7 EARTH       Test  0.0288   2.68 0.0161   2.63 0.0475  0.999

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
    ## 1 healthyR.data <tibble [394 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [384 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [334 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [309 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [123 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
