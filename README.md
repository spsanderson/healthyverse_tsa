Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
04 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,297
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

The last day in the data set is 2022-02-02 23:33:20, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1566.91
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28297          |
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
| r\_version     |      18795 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      18795 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      18795 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2375 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-02 | 2021-08-22 |       437 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1526446.1 | 1874966.10 | 357 | 24837 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8259.6 |   15783.18 |   1 |   252 |   2806 |    8288 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-02 23:33:20 | 2021-08-22 15:43:54 |     16583 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 54M 47S |        60 |

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
    ## 1 healthyR.data <tibble [408 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [398 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [348 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [323 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [137 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |        mape |      mase |      smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|------------:|----------:|-----------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.0080115 |  101.547704 | 0.6911964 | 125.398147 | 1.2608513 | 0.4818978 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0608940 |    7.509616 | 0.0417552 |   7.431769 | 0.0726096 | 0.9956837 |
| healthyR.data |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 1.0013529 |   91.723643 | 0.6866306 | 132.693408 | 1.2357948 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0598609 |    7.132801 | 0.0410468 |   6.956000 | 0.0741130 | 0.9958452 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0289320 |    3.225468 | 0.0198387 |   3.199381 | 0.0384635 | 0.9988405 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0120186 |    1.314006 | 0.0082412 |   1.301467 | 0.0196237 | 0.9996812 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0683680 |    8.666435 | 0.0468802 |   8.723594 | 0.0871661 | 0.9941343 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5909166 |   65.679714 | 0.4051933 |  85.625335 | 0.7057586 | 0.9928269 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1089145 |  161.029444 | 0.7603860 | 101.992262 | 1.4082519 | 0.4359797 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3036127 |  189.434693 | 0.8938911 | 132.737941 | 1.6455687 | 0.0571601 |
| healthyR.data |         13 | TBATS                      | Test  | 1.0611521 |  101.831027 | 0.7276351 | 132.069959 | 1.3176569 | 0.0015218 |
| healthyR.data |         14 | THETA METHOD               | Test  | 1.0007145 |   91.650008 | 0.6861929 | 132.787464 | 1.2346308 | 0.1256818 |
| healthyR.data |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8018150 |  451.226662 | 0.8891147 | 163.168285 | 1.0024590 | 0.3736720 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0486994 |   30.560860 | 0.0540017 |  19.403435 | 0.0649120 | 0.9942144 |
| healthyR      |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5908031 |  315.818183 | 0.6551284 | 124.387475 | 0.7747203 | 0.2130845 |
| healthyR      |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0461208 |   30.329787 | 0.0511423 |  25.422426 | 0.0626254 | 0.9942588 |
| healthyR      |          7 | EARTH                      | Test  | 0.0290154 |   11.427953 | 0.0321745 |   9.296987 | 0.0698237 | 0.9934454 |
| healthyR      |          8 | NNAR                       | Test  | 0.0095186 |    7.924856 | 0.0105550 |   6.286633 | 0.0216745 | 0.9993637 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0498635 |   31.129814 | 0.0552925 |  19.352266 | 0.0693529 | 0.9930660 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5436156 |  554.752192 | 0.6028032 | 130.340762 | 0.6371759 | 0.9655303 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1872316 | 1430.391297 | 1.3164946 | 118.862666 | 1.4172498 | 0.5169301 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1139481 | 1640.181543 | 1.2352321 | 125.965618 | 1.3513571 | 0.1838188 |
| healthyR      |         13 | TBATS                      | Test  | 0.6149476 |  527.127045 | 0.6819017 | 132.501491 | 0.8073536 | 0.0507242 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.6593918 |  210.263120 | 0.7311848 | 154.451873 | 0.8553728 | 0.1192998 |
| healthyR      |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA                      | Test  | 0.8370751 |  285.621412 | 0.6442527 | 132.605873 | 1.0542481 | 0.4670678 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0781308 |  104.256932 | 0.0601332 |  22.411402 | 0.0951242 | 0.9940612 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 0.8500861 |  300.262246 | 0.6542666 | 151.980458 | 1.0808964 | 0.1903033 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0787592 |   94.658830 | 0.0606168 |  22.965399 | 0.0944085 | 0.9944855 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0349940 |    3.151221 | 0.0269331 |   3.089271 | 0.0784143 | 0.9962491 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0382481 |    5.002533 | 0.0294375 |   5.639745 | 0.0932930 | 0.9936190 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0863737 |  123.408737 | 0.0664773 |  23.971107 | 0.1070654 | 0.9928973 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4654645 |  382.330088 | 0.3582436 |  92.024097 | 0.5773919 | 0.9835190 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1478762 |  815.118128 | 0.8834600 | 114.989762 | 1.4056334 | 0.6130683 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.1737951 | 1015.249551 | 0.9034084 | 118.569431 | 1.4649256 | 0.1954314 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.8638277 |  362.113051 | 0.6648428 | 144.785062 | 1.0947478 | 0.1124064 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 0.9261438 |  105.450643 | 0.7128042 | 186.516682 | 1.1772669 | 0.3670430 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 0.9778904 |  158.097556 | 0.8789013 | 162.985552 | 1.2000457 | 0.1273843 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0634428 |   12.245197 | 0.0570207 |  10.801429 | 0.0824774 | 0.9911153 |
| healthyverse  |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.7302134 |  477.185279 | 0.6562960 | 114.761532 | 0.8597200 | 0.0427152 |
| healthyverse  |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0578870 |   10.681918 | 0.0520273 |  11.130049 | 0.0813056 | 0.9911063 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0192606 |    4.166441 | 0.0173109 |   3.725574 | 0.0374785 | 0.9991994 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0095571 |    1.172745 | 0.0085897 |   1.147346 | 0.0254807 | 0.9994500 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.1035844 |   25.738327 | 0.0930988 |  29.646837 | 0.1165030 | 0.9896670 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5648996 |  206.584942 | 0.5077164 | 110.633428 | 0.6780837 | 0.9665780 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0520420 |  779.245390 | 0.9455468 | 114.542610 | 1.1840710 | 0.3732701 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3770518 | 1321.533392 | 1.2376567 | 135.092977 | 1.6071550 | 0.0257214 |
| healthyverse  |         13 | TBATS                      | Test  | 0.8168844 |  394.313347 | 0.7341935 | 139.889232 | 1.0202797 | 0.0039355 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.7075007 |  304.102100 | 0.6358824 | 104.346033 | 0.8959002 | 0.1905507 |
| healthyverse  |         15 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.9786260 |  391.737206 | 0.6959564 | 148.475783 | 1.1729140 | 0.2052961 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0585412 |   32.900688 | 0.0416320 |  16.272638 | 0.0734025 | 0.9960013 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.8828414 |  218.551317 | 0.6278386 | 162.647776 | 1.0166273 | 0.1045779 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |          NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0538415 |   23.628678 | 0.0382897 |  14.533425 | 0.0690575 | 0.9961635 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0208318 |    5.528622 | 0.0148147 |   4.631873 | 0.0358234 | 0.9990303 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0548472 |   11.861445 | 0.0390049 |  12.997428 | 0.0811952 | 0.9962912 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0611160 |   40.275759 | 0.0434631 |  19.221965 | 0.0800451 | 0.9948732 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6281499 |  411.016429 | 0.4467130 | 115.023448 | 0.7503067 | 0.9676168 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1676902 |  479.472614 | 0.8304107 | 125.036501 | 1.4044734 | 0.4860340 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2478244 |  461.504330 | 0.8873987 | 137.019515 | 1.5668871 | 0.1008003 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9263397 |  336.874858 | 0.6587727 | 160.893538 | 1.0650257 | 0.0928590 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9001175 |  164.665608 | 0.6401246 | 161.864081 | 1.1229234 | 0.2988217 |
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
    ## 1 healthyR~         8 NNAR        Test  0.0120   1.31 0.00824  1.30 0.0196 1.00 
    ## 2 healthyR          8 NNAR        Test  0.00952  7.92 0.0106   6.29 0.0217 0.999
    ## 3 healthyR~         7 EARTH       Test  0.0350   3.15 0.0269   3.09 0.0784 0.996
    ## 4 healthyv~         8 NNAR        Test  0.00956  1.17 0.00859  1.15 0.0255 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0208   5.53 0.0148   4.63 0.0358 0.999

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
    ## 1 healthyR.data <tibble [408 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 2 healthyR      <tibble [398 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 3 healthyR.ts   <tibble [348 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 4 healthyverse  <tibble [323 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~
    ## 5 healthyR.ai   <tibble [137 x 6]> <tibble [28 x 6]> <split [~ <mdl_time_tbl [1~

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
