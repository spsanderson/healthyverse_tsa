Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
18 June, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 39,632
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2022-06-16 21:27:39, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4780.82
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 39632         |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     26733 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     26733 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     26733 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       23 |          0 |
| country       |      3140 |          0.92 |   2 |   2 |     0 |      109 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-06-16 | 2021-11-01 |      571 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |        sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|----------:|----:|---------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1433644.61 | 1811026.2 | 357 | 16739.00 | 274440 | 3193860 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8724.39 |   16787.6 |   1 |   169.75 |   2659 |    8617 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-06-16 21:27:39 | 2021-11-01 02:27:37 |    22999 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      5 |       60 |

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
    ## Variable mutation for lubridate::year(date)
    ## Harmonic numeric variables for yr
    ## Fourier Transformation on value_trans
    ## Lagging value_trans
    ## K-nearest neighbor imputation for contains("lag_")

``` r
recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

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
  add_recipe(recipe = recipe_date) %>%
  add_model(model_spec_glm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_glm,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl…
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [511|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [504|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [452|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [428|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [243|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [91|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0456873 |  17.0734979 | 0.0366117 |  16.0577189 | 0.0557069 | 0.9964651 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4618216 |  86.1981559 | 0.3700824 | 108.6650720 | 0.5678116 | 0.9993303 |
| healthyR.data |         3 | LM          | Test  | 0.0483655 |  18.0648229 | 0.0387578 |  13.9820443 | 0.0607835 | 0.9959545 |
| healthyR.data |         4 | EARTH       | Test  | 0.0172581 |   4.5429392 | 0.0138298 |   4.5469007 | 0.0317366 | 0.9987472 |
| healthyR.data |         5 | NNAR        | Test  | 0.0173991 |   5.9111026 | 0.0139428 |   6.5172885 | 0.0215062 | 0.9992588 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0490715 |  11.5864815 | 0.0396972 |  13.2703169 | 0.0660884 | 0.9949520 |
| healthyR      |         2 | GLMNET      | Test  | 0.5418178 |  73.1981170 | 0.4383119 | 110.1356173 | 0.6760930 | 0.9946424 |
| healthyR      |         3 | LM          | Test  | 0.0497140 |  11.9552513 | 0.0402169 |  13.8987334 | 0.0667191 | 0.9949328 |
| healthyR      |         4 | EARTH       | Test  | 0.0283841 |   2.7084243 | 0.0229617 |   2.6565010 | 0.0710524 | 0.9950011 |
| healthyR      |         5 | NNAR        | Test  | 0.0109471 |   2.0212350 | 0.0088559 |   1.9592755 | 0.0180757 | 0.9996133 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0470719 |   7.8979738 | 0.0349217 |   7.2890504 | 0.0565484 | 0.9981429 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5325671 |  64.7461761 | 0.3951013 |  96.2903648 | 0.6328326 | 0.9983668 |
| healthyR.ts   |         3 | LM          | Test  | 0.0485590 |   7.9664300 | 0.0360250 |   7.5599946 | 0.0582748 | 0.9981429 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0165075 |   2.1993874 | 0.0122466 |   2.1829108 | 0.0244264 | 0.9994360 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0089871 |   1.6561239 | 0.0066674 |   1.6302054 | 0.0139342 | 0.9998081 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0389194 |   9.2614455 | 0.0335438 |   8.3874900 | 0.0455043 | 0.9989682 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4246383 | 100.4432468 | 0.3659868 | 107.9053069 | 0.5062877 | 0.9992980 |
| healthyverse  |         3 | LM          | Test  | 0.0380919 |  10.6743610 | 0.0328306 |   9.4782199 | 0.0439084 | 0.9989514 |
| healthyverse  |         4 | EARTH       | Test  | 0.0096021 |   2.4816768 | 0.0082759 |   2.4864545 | 0.0141855 | 0.9996917 |
| healthyverse  |         5 | NNAR        | Test  | 0.0031227 |   0.8284558 | 0.0026913 |   0.8331572 | 0.0041775 | 0.9999755 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0429143 |   8.6469545 | 0.0352902 |   8.7808935 | 0.0525143 | 0.9982410 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4844566 |  62.2049125 | 0.3983885 |  90.8234985 | 0.5843751 | 0.9985656 |
| healthyR.ai   |         3 | LM          | Test  | 0.0437098 |   8.8874365 | 0.0359443 |   9.1712068 | 0.0539367 | 0.9982207 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0134056 |   2.1983366 | 0.0110239 |   2.1722450 | 0.0152084 | 0.9997636 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0067021 |   1.1016172 | 0.0055114 |   1.0670010 | 0.0144516 | 0.9997681 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0362959 |   7.1110596 | 0.0373035 |   6.6985809 | 0.0442716 | 0.9986644 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4314560 |  61.7573032 | 0.4434341 |  89.3588821 | 0.5199220 | 0.9988052 |
| TidyDensity   |         3 | LM          | Test  | 0.0331433 |   5.7938187 | 0.0340635 |   5.5818822 | 0.0396853 | 0.9986190 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0084733 |   1.6823260 | 0.0087085 |   1.6685755 | 0.0115269 | 0.9998580 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0226157 |   4.0429445 | 0.0232435 |   4.1050702 | 0.0337394 | 0.9990807 |

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
    ##   # A tibble: 6 × 10
    ##   package  .model_id .model_desc .type     mae  mape    mase smape    rmse   rsq
    ##   <chr>        <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>   <dbl> <dbl>
    ## 1 healthy…         5 NNAR        Test  0.0174  5.91  0.0139  6.52  0.0215  0.999
    ## 2 healthyR         5 NNAR        Test  0.0109  2.02  0.00886 1.96  0.0181  1.00 
    ## 3 healthy…         5 NNAR        Test  0.00899 1.66  0.00667 1.63  0.0139  1.00 
    ## 4 healthy…         5 NNAR        Test  0.00312 0.828 0.00269 0.833 0.00418 1.00 
    ## 5 healthy…         5 NNAR        Test  0.00670 1.10  0.00551 1.07  0.0145  1.00 
    ## 6 TidyDen…         4 EARTH       Test  0.00847 1.68  0.00871 1.67  0.0115  1.00

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(!is.na(.model_id)) %>%
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
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(
      verbose = TRUE, 
      allow_par = FALSE
    )
  )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data .future_data      .splits          .modeltime_tabl…
    ##   <chr>         <list>       <list>            <list>           <list>          
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [511|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [504|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [452|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [428|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [243|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [91|28]>  <mdl_time_tbl>

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
