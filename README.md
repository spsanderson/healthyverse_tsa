Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
19 July, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 41,963
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

The last day in the data set is 2022-07-17 23:35:45, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -355.62
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 41963         |
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
| r_version     |     28210 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     28210 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     28210 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       24 |          0 |
| country       |      3365 |          0.92 |   2 |   2 |     0 |      114 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-17 | 2021-11-08 |      602 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1418350.81 | 1790369.21 | 357 | 16873 | 289681 | 2971375 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8721.54 |   16720.77 |   1 |   167 |   2580 |    8798 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-17 23:35:45 | 2021-11-08 17:07:30 |    24550 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 23M 33S |       60 |

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
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <chr>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [337 × 6]> <tibble>     <split [309|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [334 × 6]> <tibble>     <split [306|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [335 × 6]> <tibble>     <split [307|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [337 × 6]> <tibble>     <split [309|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [302 × 6]> <tibble>     <split [274|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [150 × 6]> <tibble>     <split [122|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |        mape |      mase |       smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|------------:|----------:|------------:|----------:|----------:|
| healthyR      |         1 | REGRESSION  | Test  | 0.0556802 |  10.8266075 | 0.0417755 |   9.7822912 | 0.0676270 | 0.9961500 |
| healthyR      |         2 | GLMNET      | Test  | 0.6086222 |  66.5753782 | 0.4566345 | 103.1015067 | 0.7626602 | 0.9963166 |
| healthyR      |         3 | LM          | Test  | 0.0628930 |  13.6065948 | 0.0471871 |  12.7075637 | 0.0726708 | 0.9959903 |
| healthyR      |         4 | EARTH       | Test  | 0.0426408 |   4.5337510 | 0.0319924 |   4.4934249 | 0.0773993 | 0.9963166 |
| healthyR      |         5 | NNAR        | Test  | 0.0342882 |   2.3040576 | 0.0257256 |   2.1727029 | 0.1254688 | 0.9889417 |
| healthyR.data |         1 | REGRESSION  | Test  | 0.0538578 |  10.2671126 | 0.0496066 |  10.3838718 | 0.0673403 | 0.9961093 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4842011 |  59.4245156 | 0.4459819 |  90.7263746 | 0.6039872 | 0.9985939 |
| healthyR.data |         3 | LM          | Test  | 0.0518711 |   9.9287277 | 0.0477768 |   9.9288408 | 0.0646270 | 0.9961324 |
| healthyR.data |         4 | EARTH       | Test  | 0.0272793 |   4.7478882 | 0.0251261 |   4.7426515 | 0.0372207 | 0.9979981 |
| healthyR.data |         5 | NNAR        | Test  | 0.0068191 |   1.5906585 | 0.0062808 |   1.6359542 | 0.0095441 | 0.9998557 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0565227 | 138.4632048 | 0.0512013 |  40.0152820 | 0.0646056 | 0.9972930 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5059861 | 265.7773568 | 0.4583492 | 105.8108654 | 0.6495585 | 0.9980614 |
| healthyR.ts   |         3 | LM          | Test  | 0.0612379 | 139.4321565 | 0.0554726 |  37.6182883 | 0.0703111 | 0.9972069 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0268251 |  22.2006325 | 0.0242996 |  15.3546013 | 0.0475657 | 0.9980614 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0061637 |  17.0787952 | 0.0055834 |  23.9566747 | 0.0087867 | 0.9999567 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0405227 |  42.9750507 | 0.0384557 |  23.6885892 | 0.0532830 | 0.9956930 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4445451 | 340.0799718 | 0.4218703 |  99.8239056 | 0.5822621 | 0.9964317 |
| healthyverse  |         3 | LM          | Test  | 0.0511086 |  30.1769760 | 0.0485017 |  16.3987121 | 0.0636796 | 0.9957042 |
| healthyverse  |         4 | EARTH       | Test  | 0.0090636 |   8.7048159 | 0.0086013 |   6.7971876 | 0.0125298 | 0.9997515 |
| healthyverse  |         5 | NNAR        | Test  | 0.0094888 |  76.5376619 | 0.0090048 |   8.0979386 | 0.0242348 | 0.9990756 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0463580 |   8.7387293 | 0.0487060 |   9.1572607 | 0.0552714 | 0.9966397 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.4839322 |  63.5661576 | 0.5084428 |  98.1138584 | 0.5989754 | 0.9969313 |
| healthyR.ai   |         3 | LM          | Test  | 0.0470550 |   9.0768545 | 0.0494383 |   9.5646090 | 0.0555512 | 0.9966349 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0125015 |   2.3634941 | 0.0131347 |   2.3222354 | 0.0145077 | 0.9997438 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0030952 |   0.4637091 | 0.0032520 |   0.4635051 | 0.0045757 | 0.9999837 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0299030 |  11.4950215 | 0.0276186 |  10.0667034 | 0.0351136 | 0.9986067 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4921805 | 159.7103960 | 0.4545820 | 110.8505258 | 0.6294670 | 0.9989341 |
| TidyDensity   |         3 | LM          | Test  | 0.0328441 |  17.3769048 | 0.0303351 |  11.5529107 | 0.0378906 | 0.9985179 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0087256 |   2.5814768 | 0.0080591 |   2.4826898 | 0.0111323 | 0.9998627 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0072132 |   2.7052630 | 0.0066622 |   2.5151259 | 0.0105860 | 0.9998741 |

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
    ##   package      .mode…¹ .mode…² .type     mae   mape    mase  smape    rmse   rsq
    ##   <chr>          <int> <chr>   <chr>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl> <dbl>
    ## 1 healthyR           1 REGRES… Test  0.0557  10.8   0.0418   9.78  0.0676  0.996
    ## 2 healthyR.da…       5 NNAR    Test  0.00682  1.59  0.00628  1.64  0.00954 1.00 
    ## 3 healthyR.ts        5 NNAR    Test  0.00616 17.1   0.00558 24.0   0.00879 1.00 
    ## 4 healthyverse       4 EARTH   Test  0.00906  8.70  0.00860  6.80  0.0125  1.00 
    ## 5 healthyR.ai        5 NNAR    Test  0.00310  0.464 0.00325  0.464 0.00458 1.00 
    ## 6 TidyDensity        5 NNAR    Test  0.00721  2.71  0.00666  2.52  0.0106  1.00 
    ## # … with abbreviated variable names ¹​.model_id, ²​.model_desc

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
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <chr>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [337 × 6]> <tibble>     <split [309|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [334 × 6]> <tibble>     <split [306|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [335 × 6]> <tibble>     <split [307|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [337 × 6]> <tibble>     <split [309|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [302 × 6]> <tibble>     <split [274|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [150 × 6]> <tibble>     <split [122|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

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
