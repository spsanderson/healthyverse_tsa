Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
23 May, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 37,612
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

The last day in the data set is 2022-05-21 18:07:33, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4153.49
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 37612         |
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
| r_version     |     25232 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     25232 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     25232 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       21 |          0 |
| country       |      3026 |          0.92 |   2 |   2 |     0 |      108 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-05-21 | 2021-10-26 |      545 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1452088.4 | 1826486.58 | 357 | 16873 | 271489 | 3241547 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8714.3 |   16690.43 |   1 |   186 |   2659 |    8647 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-05-21 18:07:33 | 2021-10-26 02:44:32 |    21877 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   38.5 |       60 |

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
  add_recipe(recipe = recipe_base) %>%
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
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [486|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [478|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [427|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [403|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [217|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [93 x 6]> <tibble>     <split [65|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0612392 |   8.919434 | 0.0394404 |  8.785478 | 0.0783412 | 0.9938822 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.0733140 |  11.619485 | 0.0472170 | 11.174721 | 0.0887140 | 0.9926415 |
| healthyR.data |         4 | EARTH       | Test  | 0.0452290 |   8.006757 | 0.0291292 |  7.012317 | 0.0750845 | 0.9947132 |
| healthyR.data |         5 | NNAR        | Test  | 0.0458839 |   5.966831 | 0.0295510 |  5.713683 | 0.0947191 | 0.9913117 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0403653 |  12.968937 | 0.0305070 | 10.877432 | 0.0468202 | 0.9981010 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.0410729 |  13.618539 | 0.0310418 | 11.343860 | 0.0476575 | 0.9981027 |
| healthyR      |         4 | EARTH       | Test  | 0.0159814 |   3.127437 | 0.0120783 |  3.053572 | 0.0232441 | 0.9997854 |
| healthyR      |         5 | NNAR        | Test  | 0.0337322 |   9.962935 | 0.0254938 |  9.857488 | 0.0637164 | 0.9978945 |
| healthyR.ts   |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.0415235 |   8.351488 | 0.0299537 |  7.715523 | 0.0520576 | 0.9976590 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0153763 |   2.978254 | 0.0110920 |  2.912195 | 0.0181193 | 0.9998916 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0163208 |   4.087236 | 0.0117733 |  4.572501 | 0.0254260 | 0.9994665 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0356422 |   9.884107 | 0.0302154 |  9.038239 | 0.0439347 | 0.9980694 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.0345712 |   9.407018 | 0.0293074 |  8.476617 | 0.0428914 | 0.9980695 |
| healthyverse  |         4 | EARTH       | Test  | 0.0108272 |   3.428655 | 0.0091787 |  3.478617 | 0.0150473 | 0.9997246 |
| healthyverse  |         5 | NNAR        | Test  | 0.0061834 |   1.391969 | 0.0052419 |  1.363270 | 0.0108634 | 0.9999117 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0415489 |   7.397238 | 0.0303709 |  7.055555 | 0.0591565 | 0.9966095 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.0423026 |   7.786202 | 0.0309218 |  7.426781 | 0.0601876 | 0.9966189 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0162923 |   2.689262 | 0.0119091 |  2.649767 | 0.0204348 | 0.9996874 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0185436 |   1.849837 | 0.0135548 |  1.845556 | 0.0481817 | 0.9987811 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0493365 |  15.112894 | 0.0342653 | 13.889090 | 0.0591304 | 0.9972546 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.1390996 |  56.862872 | 0.0966078 | 45.065777 | 0.1546509 | 0.9953936 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0132748 |   1.341953 | 0.0092197 |  1.339936 | 0.0191220 | 0.9996921 |
| TidyDensity   |         5 | NNAR        | Test  | 0.4876149 | 168.044941 | 0.3386595 | 90.393325 | 0.5865604 | 0.9111610 |

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
    ##   # A tibble: 6 x 10
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR~         4 EARTH       Test  0.0452   8.01 0.0291   7.01 0.0751 0.995
    ## 2 healthyR          4 EARTH       Test  0.0160   3.13 0.0121   3.05 0.0232 1.00 
    ## 3 healthyR~         4 EARTH       Test  0.0154   2.98 0.0111   2.91 0.0181 1.00 
    ## 4 healthyv~         5 NNAR        Test  0.00618  1.39 0.00524  1.36 0.0109 1.00 
    ## 5 healthyR~         4 EARTH       Test  0.0163   2.69 0.0119   2.65 0.0204 1.00 
    ## 6 TidyDens~         4 EARTH       Test  0.0133   1.34 0.00922  1.34 0.0191 1.00

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
    ##   # A tibble: 6 x 5
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl~
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [486|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [478|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [427|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [403|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [217|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [93 x 6]> <tibble>     <split [65|28]>  <mdl_time_tbl>

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
