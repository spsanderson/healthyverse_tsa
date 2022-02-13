Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
13 February, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 28,957
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

The last day in the data set is 2022-02-11 22:10:58, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -1781.54
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 28957          |
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
| r\_version     |      19210 |           0.34 |   5 |   5 |     0 |        29 |          0 |
| r\_arch        |      19210 |           0.34 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      19210 |           0.34 |   7 |  15 |     0 |        11 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         6 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        16 |          0 |
| country        |       2406 |           0.92 |   2 |   2 |     0 |       101 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2022-02-11 | 2021-08-25 |       446 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size           |          0 |              1 | 1521609.94 | 1872684.51 | 357 | 23626 | 271097 | 3247946 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8265.32 |   15730.37 |   1 |   319 |   2806 |    8358 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2022-02-11 22:10:58 | 2021-08-25 10:22:35 |     16913 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max |      median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|------------:|----------:|
| time           |          0 |              1 |   0 |  59 | 10H 56M 29S |        60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [389|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [379|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [329|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [304|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [118|28]> <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model\_id | .model\_desc               | .type |       mae |         mape |      mase |       smape |      rmse |       rsq |
|:--------------|-----------:|:---------------------------|:------|----------:|-------------:|----------:|------------:|----------:|----------:|
| healthyR.data |          1 | ARIMA                      | Test  | 1.0378724 | 1101.2427263 | 0.7540809 | 134.6873444 | 1.3032625 | 0.1539334 |
| healthyR.data |          2 | REGRESSION                 | Test  | 0.0657682 |  122.0851395 | 0.0477848 |  17.8054376 | 0.0865179 | 0.9936222 |
| healthyR.data |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          4 | ETSANN                     | Test  | 0.9697609 |  454.9673454 | 0.7045936 | 154.8227387 | 1.1722819 |        NA |
| healthyR.data |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.data |          6 | LM                         | Test  | 0.0673382 |   99.4390956 | 0.0489255 |  17.4831754 | 0.0959374 | 0.9925586 |
| healthyR.data |          7 | EARTH                      | Test  | 0.0654519 |   70.7762792 | 0.0475550 |  17.0829523 | 0.1422621 | 0.9815139 |
| healthyR.data |          8 | NNAR                       | Test  | 0.0110769 |   15.9108593 | 0.0080481 |   8.4053021 | 0.0206599 | 0.9995896 |
| healthyR.data |          9 | PROPHET W REGRESSORS       | Test  | 0.0752494 |   88.2964114 | 0.0546736 |  19.2346782 | 0.1071712 | 0.9906137 |
| healthyR.data |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.5548956 |  836.0826152 | 0.4031673 |  83.5725027 | 0.6730520 | 0.9918126 |
| healthyR.data |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.2167241 | 3580.6501918 | 0.8840282 | 111.8177170 | 1.4829593 | 0.3557791 |
| healthyR.data |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.5639003 | 3892.1848067 | 1.1362739 | 141.2673647 | 1.8709147 | 0.0029760 |
| healthyR.data |         13 | BATS                       | Test  | 0.9952574 |  852.3805705 | 0.7231184 | 134.1939407 | 1.2600652 | 0.0614930 |
| healthyR.data |         14 | THETA METHOD               | Test  | 0.9693181 |  449.5311673 | 0.7042718 | 155.0771434 | 1.1711972 | 0.0202657 |
| healthyR.data |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          1 | ARIMA                      | Test  | 0.8583289 | 1608.2691798 | 0.9718983 | 160.5195594 | 1.0410796 | 0.4024758 |
| healthyR      |          2 | REGRESSION                 | Test  | 0.0606218 |   91.5099492 | 0.0686429 |  22.4610405 | 0.0731132 | 0.9948665 |
| healthyR      |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          4 | ETSANA                     | Test  | 0.5806048 |  360.5116659 | 0.6574272 | 105.1515050 | 0.8437168 | 0.2513616 |
| healthyR      |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR      |          6 | LM                         | Test  | 0.0571926 |   71.4484981 | 0.0647600 |  19.4526771 | 0.0700725 | 0.9948456 |
| healthyR      |          7 | EARTH                      | Test  | 0.0342873 |   25.3816213 | 0.0388240 |  17.9407012 | 0.0769722 | 0.9933085 |
| healthyR      |          8 | NNAR                       | Test  | 0.0100814 |    9.3596769 | 0.0114154 |   8.3218056 | 0.0221543 | 0.9994853 |
| healthyR      |          9 | PROPHET W REGRESSORS       | Test  | 0.0633800 |  134.4166690 | 0.0717661 |  22.4464439 | 0.0775974 | 0.9935880 |
| healthyR      |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.4997249 | 1202.8999967 | 0.5658458 | 114.0001723 | 0.5992410 | 0.9760510 |
| healthyR      |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.0553080 |  660.0913591 | 1.1949406 | 103.8328890 | 1.3607896 | 0.5570135 |
| healthyR      |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.0673194 |  901.4016413 | 1.2085412 | 124.6155142 | 1.3542965 | 0.1645962 |
| healthyR      |         13 | TBATS                      | Test  | 0.6183040 |  481.0736659 | 0.7001146 | 118.6166702 | 0.8469605 | 0.3192747 |
| healthyR      |         14 | THETA METHOD               | Test  | 0.7623259 |  357.1002973 | 0.8631927 | 170.6726456 | 0.9671889 | 0.0484604 |
| healthyR      |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 1.3144979 | 3399.7289331 | 1.2540739 | 155.7364352 | 1.5330871 | 0.4393937 |
| healthyR.ts   |          2 | REGRESSION                 | Test  | 0.0779969 |  239.7225431 | 0.0744116 |  22.3745634 | 0.0957540 | 0.9919048 |
| healthyR.ts   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          4 | ETSANA                     | Test  | 1.2583249 | 3248.1741126 | 1.2004831 | 158.4498537 | 1.4785520 | 0.1235240 |
| healthyR.ts   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ts   |          6 | LM                         | Test  | 0.0745308 |  210.9080329 | 0.0711048 |  22.8525843 | 0.0894399 | 0.9921749 |
| healthyR.ts   |          7 | EARTH                      | Test  | 0.0150746 |    7.5391383 | 0.0143816 |   6.0702405 | 0.0214926 | 0.9995448 |
| healthyR.ts   |          8 | NNAR                       | Test  | 0.0117775 |   23.8033660 | 0.0112361 |  12.7563914 | 0.0259361 | 0.9996588 |
| healthyR.ts   |          9 | PROPHET W REGRESSORS       | Test  | 0.0815992 |  237.8716518 | 0.0778483 |  24.0933753 | 0.0999048 | 0.9906161 |
| healthyR.ts   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.7064236 | 2085.8503673 | 0.6739511 | 127.9378760 | 0.8116713 | 0.9719290 |
| healthyR.ts   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1045211 | 1383.6916225 | 1.0537492 | 100.3522188 | 1.4610618 | 0.4957301 |
| healthyR.ts   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.4567440 | 4587.7166724 | 1.3897814 | 135.1791183 | 1.8668294 | 0.1353884 |
| healthyR.ts   |         13 | TBATS                      | Test  | 0.9858240 | 2081.8056470 | 0.9405082 | 137.7469285 | 1.2581960 | 0.2694060 |
| healthyR.ts   |         14 | THETA METHOD               | Test  | 1.2692144 | 3119.9371048 | 1.2108720 | 150.9133525 | 1.5577000 | 0.0732107 |
| healthyR.ts   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          1 | ARIMA                      | Test  | 1.0183639 |  102.9071924 | 0.9579209 | 159.7711006 | 1.2559398 | 0.2885052 |
| healthyverse  |          2 | REGRESSION                 | Test  | 0.0647472 |    8.3621888 | 0.0609043 |   7.9513468 | 0.0887555 | 0.9912463 |
| healthyverse  |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          4 | ETSANA                     | Test  | 0.8760624 |  138.0801467 | 0.8240654 | 129.9284857 | 1.0477250 | 0.0190530 |
| healthyverse  |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyverse  |          6 | LM                         | Test  | 0.0615694 |    7.6036724 | 0.0579151 |   7.2032447 | 0.0906632 | 0.9912149 |
| healthyverse  |          7 | EARTH                      | Test  | 0.0195963 |    1.7985021 | 0.0184332 |   1.8096253 | 0.0433141 | 0.9987095 |
| healthyverse  |          8 | NNAR                       | Test  | 0.0158111 |    0.8689583 | 0.0148727 |   0.8804616 | 0.0383524 | 0.9988990 |
| healthyverse  |          9 | PROPHET W REGRESSORS       | Test  | 0.0914673 |   18.8138001 | 0.0860385 |  17.7010818 | 0.1045386 | 0.9911581 |
| healthyverse  |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6350086 |   83.9508718 | 0.5973189 |  97.0188829 | 0.7538613 | 0.9660031 |
| healthyverse  |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1094977 |  237.8394447 | 1.0436456 | 102.4778580 | 1.3376330 | 0.3254190 |
| healthyverse  |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.3115616 |  240.2997463 | 1.2337163 | 127.0647021 | 1.6389329 | 0.0152872 |
| healthyverse  |         13 | TBATS                      | Test  | 0.9834986 |  112.2968140 | 0.9251249 | 140.5032618 | 1.1975779 | 0.0117277 |
| healthyverse  |         14 | THETA METHOD               | Test  | 0.8543985 |  117.7291511 | 0.8036873 | 114.6599679 | 1.0692689 | 0.0312678 |
| healthyverse  |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          1 | ARIMA W XGBOOST ERRORS     | Test  | 0.7541505 |  771.4796165 | 0.6578913 | 149.4215139 | 0.9276396 | 0.4263510 |
| healthyR.ai   |          2 | REGRESSION                 | Test  | 0.0704864 |  180.5070428 | 0.0614896 |  26.4733354 | 0.0869226 | 0.9934488 |
| healthyR.ai   |          3 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          4 | ETSANA                     | Test  | 0.9004228 | 1310.5788650 | 0.7854935 | 165.4945934 | 1.0660709 | 0.0911754 |
| healthyR.ai   |          5 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |
| healthyR.ai   |          6 | LM                         | Test  | 0.0626015 |   87.5463602 | 0.0546111 |  26.3634893 | 0.0818444 | 0.9935791 |
| healthyR.ai   |          7 | EARTH                      | Test  | 0.0168342 |    3.4106266 | 0.0146855 |   3.4976095 | 0.0298529 | 0.9991160 |
| healthyR.ai   |          8 | NNAR                       | Test  | 0.0461963 |   49.9537425 | 0.0402999 |  23.6138833 | 0.0790400 | 0.9936518 |
| healthyR.ai   |          9 | PROPHET W REGRESSORS       | Test  | 0.0724977 |  217.8278272 | 0.0632442 |  28.7946832 | 0.0920186 | 0.9918331 |
| healthyR.ai   |         10 | PROPHET W XGBOOST ERRORS   | Test  | 0.6528763 | 2451.6926979 | 0.5695437 | 127.5324570 | 0.7541174 | 0.9623771 |
| healthyR.ai   |         11 | SEASONAL DECOMP REGRESSION | Test  | 1.1709067 | 1025.1976336 | 1.0214531 | 130.2047563 | 1.4722511 | 0.4223273 |
| healthyR.ai   |         12 | SEASONAL DECOMP ETSANN     | Test  | 1.2533332 | 1214.0747877 | 1.0933588 | 135.1148363 | 1.6802699 | 0.0908947 |
| healthyR.ai   |         13 | TBATS                      | Test  | 0.9445103 | 1609.3002581 | 0.8239538 | 161.0650025 | 1.1017120 | 0.0946338 |
| healthyR.ai   |         14 | THETA METHOD               | Test  | 0.9707709 | 1657.9577739 | 0.8468625 | 155.8350554 | 1.2018595 | 0.0168643 |
| healthyR.ai   |         15 | NULL                       | NA    |        NA |           NA |        NA |          NA |        NA |        NA |

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
    ##   package   .model_id .model_desc .type    mae   mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>  <dbl>  <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         8 NNAR        Test  0.0111 15.9   0.00805 8.41  0.0207 1.00 
    ## 2 healthyR          8 NNAR        Test  0.0101  9.36  0.0114  8.32  0.0222 0.999
    ## 3 healthyR~         7 EARTH       Test  0.0151  7.54  0.0144  6.07  0.0215 1.00 
    ## 4 healthyv~         8 NNAR        Test  0.0158  0.869 0.0149  0.880 0.0384 0.999
    ## 5 healthyR~         7 EARTH       Test  0.0168  3.41  0.0147  3.50  0.0299 0.999

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
    ## 1 healthyR.data <tibble>     <tibble [28 x 6]> <split [389|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 x 6]> <split [379|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 x 6]> <split [329|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 x 6]> <split [304|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 x 6]> <split [118|28]> <mdl_time_tbl>

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
