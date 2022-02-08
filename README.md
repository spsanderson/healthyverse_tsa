Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
08 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,492
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

The last day in the data set is 2022-02-06 23:54:53, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1663.27
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28492          |
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
| r\_version     |      18886 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18886 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18886 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2391 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-06 | 2021-08-23 |       441 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1526122.90 | 1874595.15 | 357 | 26285 | 271098 | 3247947 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8253.66 |   15772.69 |   1 |   270 |   2806 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-06 23:54:53 | 2021-08-23 10:56:11 |     16704 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |   52.5 |        60 |

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
    ## 1 healthyR.data <tibble [412 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [402 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [352 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [327 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [141 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.0599389 | 1484.412652 | 0.6926477 | 135.409619 | 1.3030506 | 0.2130390 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0577944 |  145.440768 | 0.0377674 |  13.866407 | 0.0685169 | 0.9959629 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0016210 |  767.155221 | 0.6545381 | 149.600979 | 1.1954874 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0584667 |  114.957598 | 0.0382068 |  13.424981 | 0.0745624 | 0.9954972 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0297426 |   69.932928 | 0.0194362 |  10.313896 | 0.0385640 | 0.9987100 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0206202 |   44.474226 | 0.0134749 |   9.436616 | 0.0554527 | 0.9971760 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0671775 |  105.741155 | 0.0438991 |  15.233156 | 0.0882642 | 0.9939045 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6152159 | 1327.363000 | 0.4020306 |  95.600494 | 0.7140382 | 0.9941122 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0142240 | 4768.588239 | 0.6627740 |  80.657133 | 1.3677653 | 0.4796158 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3642292 | 4610.300198 | 0.8914950 | 135.381213 | 1.7014903 | 0.0557495 |
| healthyR.data |         13 | TBATS                      | Test  | 0.9993988 |  786.634949 | 0.6530860 | 133.379053 | 1.2478758 | 0.1469013 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0011469 |  760.741397 | 0.6542284 | 149.771328 | 1.1944603 | 0.0443492 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8441111 | 3399.444119 | 0.9704818 | 169.354000 | 1.0355364 | 0.4976187 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0541106 |  131.574274 | 0.0622114 |  25.875706 | 0.0689794 | 0.9936863 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5869443 | 1518.340176 | 0.6748149 | 124.694877 | 0.7603941 | 0.2416941 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0505759 |  119.685263 | 0.0581476 |  25.034902 | 0.0653657 | 0.9937510 |
| healthyR      |          7 | EARTH                      | Test  | 0.0287977 |   54.078456 | 0.0331090 |  24.921837 | 0.0696855 | 0.9933614 |
| healthyR      |          8 | NNAR                       | Test  | 0.0077324 |   17.667887 | 0.0088900 |  16.148658 | 0.0163569 | 0.9995925 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0581174 |  189.266696 | 0.0668181 |  29.106095 | 0.0732438 | 0.9923900 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5230402 | 2472.988594 | 0.6013438 | 127.861368 | 0.6129445 | 0.9662460 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1448039 | 4119.123346 | 1.3161909 | 108.645739 | 1.3927967 | 0.5664429 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0076114 | 4380.803686 | 1.1584595 | 123.034882 | 1.2942270 | 0.2557856 |
| healthyR      |         13 | TBATS                      | Test  | 0.5798524 |  798.917609 | 0.6666613 | 123.382464 | 0.7805557 | 0.3529795 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6429502 | 1356.446246 | 0.7392053 | 139.238212 | 0.8361948 | 0.1134189 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.1204799 | 2841.027048 | 0.8767176 | 141.233938 | 1.3955679 | 0.5737598 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0790672 |  467.927172 | 0.0618660 |  22.337697 | 0.0979996 | 0.9936029 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.1760979 | 2720.821670 | 0.9202358 | 150.520136 | 1.4153225 | 0.1717891 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0762053 |  400.277913 | 0.0596267 |  22.928488 | 0.0918022 | 0.9939152 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0291211 |    6.306049 | 0.0227857 |   5.498337 | 0.0671896 | 0.9969707 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0404597 |  184.868790 | 0.0316576 |  16.468851 | 0.1384353 | 0.9885774 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0851056 |  546.790914 | 0.0665908 |  23.545673 | 0.1051964 | 0.9923794 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6380245 | 2490.209661 | 0.4992212 | 117.648488 | 0.7596537 | 0.9837588 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1033342 | 4164.172788 | 0.8633020 | 111.331810 | 1.3806261 | 0.6206791 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3841100 | 4993.742687 | 1.0829945 | 131.682261 | 1.7281150 | 0.2042533 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9582141 | 1464.514048 | 0.7497530 | 140.168782 | 1.2233862 | 0.4057667 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.1659790 | 3411.385464 | 0.9123183 | 145.803760 | 1.4524380 | 0.3540350 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.1376866 |  142.168993 | 1.0294825 | 168.606954 | 1.3512032 | 0.3914593 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0692099 |    8.815706 | 0.0626275 |   8.395781 | 0.0905046 | 0.9906894 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8708877 |  118.888001 | 0.7880586 | 131.387438 | 1.0588322 | 0.0639023 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0640059 |    8.010549 | 0.0579184 |   7.613185 | 0.0908900 | 0.9906812 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0215943 |    1.948423 | 0.0195405 |   1.964950 | 0.0438741 | 0.9989037 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0164279 |    2.236764 | 0.0148655 |   2.011886 | 0.0368694 | 0.9989535 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1167954 |   23.872028 | 0.1056871 |  26.680165 | 0.1284513 | 0.9888299 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6297623 |   85.387456 | 0.5698664 | 103.135265 | 0.7499073 | 0.9645354 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2442271 |  300.354326 | 1.1258902 | 110.871917 | 1.3963238 | 0.4050413 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4169503 |  301.342834 | 1.2821858 | 138.428503 | 1.6538329 | 0.0747436 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9294060 |  102.829939 | 0.8410113 | 138.156549 | 1.1467385 | 0.1184462 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8253968 |  110.910414 | 0.7468943 | 109.353188 | 1.0418514 | 0.2274794 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.2700386 | 5069.195804 | 0.9967595 | 161.346102 | 1.4440576 | 0.4679332 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0641523 |  230.633825 | 0.0503484 |  25.642818 | 0.0790201 | 0.9944245 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 1.0462486 | 2838.662463 | 0.8211232 | 166.594544 | 1.1826085 | 0.0929131 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0556745 |  103.145986 | 0.0436948 |  20.057905 | 0.0713723 | 0.9946501 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0128133 |    5.598962 | 0.0100562 |   6.884150 | 0.0169729 | 0.9996627 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0819383 |  190.582925 | 0.0643073 |  30.337295 | 0.1190814 | 0.9879000 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0660417 |  271.334396 | 0.0518313 |  27.641769 | 0.0845560 | 0.9927862 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 1.1721860 | 6197.251166 | 0.9199623 | 165.600602 | 1.2792656 | 0.8018313 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1257563 | 1039.866022 | 0.8835230 | 129.073697 | 1.3579750 | 0.4815929 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3004615 | 3467.255261 | 1.0206362 | 136.337435 | 1.6526598 | 0.1229147 |
| healthyR.ai   |         13 | TBATS                      | Test  | 1.0646827 | 3438.515417 | 0.8355908 | 166.108075 | 1.2060584 | 0.1036221 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 1.0949207 | 2805.040927 | 0.8593224 | 162.628391 | 1.2999737 | 0.1189809 |
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
    ## 1 healthyR~         7 EARTH       Test  0.0297  69.9  0.0194  10.3  0.0386 0.999
    ## 2 healthyR          8 NNAR        Test  0.00773 17.7  0.00889 16.1  0.0164 1.00 
    ## 3 healthyR~         7 EARTH       Test  0.0291   6.31 0.0228   5.50 0.0672 0.997
    ## 4 healthyv~         8 NNAR        Test  0.0164   2.24 0.0149   2.01 0.0369 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0128   5.60 0.0101   6.88 0.0170 1.00

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
    ## 1 healthyR.data <tibble [412 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [402 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [352 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [327 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [141 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
