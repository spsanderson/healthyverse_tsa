Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
12 June, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 39,007
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

The last day in the data set is 2022-06-10 20:53:41, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4636.25
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 39007         |
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
| r_version     |     26243 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     26243 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     26243 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       23 |          0 |
| country       |      3098 |          0.92 |   2 |   2 |     0 |      109 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-06-10 | 2021-10-28 |      565 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1437481.31 | 1815980.90 | 357 | 16740 | 271509 | 3194712 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8747.93 |   16837.81 |   1 |   168 |   2659 |    8617 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-06-10 20:53:41 | 2021-10-28 22:57:33 |    22694 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 16M 48S |       60 |

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
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [505|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [498|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [446|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [422|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [237|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [85|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0490156 |  13.361744 | 0.0353094 |  15.261785 | 0.0579395 | 0.9971806 |
| healthyR.data |         2 | GLMNET      | Test  | 0.4710844 |  75.087887 | 0.3393558 | 100.378372 | 0.5833684 | 0.9985633 |
| healthyR.data |         3 | LM          | Test  | 0.0539305 |  17.097581 | 0.0388500 |  13.264642 | 0.0650357 | 0.9962450 |
| healthyR.data |         4 | EARTH       | Test  | 0.0318217 |   7.376392 | 0.0229234 |   7.984532 | 0.0466283 | 0.9974876 |
| healthyR.data |         5 | NNAR        | Test  | 0.0547612 |   7.205831 | 0.0394484 |   6.445794 | 0.1797362 | 0.9695978 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0478079 |  15.013426 | 0.0340476 |  16.047724 | 0.0650874 | 0.9956835 |
| healthyR      |         2 | GLMNET      | Test  | 0.5336768 |  70.274675 | 0.3800712 | 104.199024 | 0.6917739 | 0.9949871 |
| healthyR      |         3 | LM          | Test  | 0.0484649 |  15.497703 | 0.0345155 |  16.882902 | 0.0656247 | 0.9956778 |
| healthyR      |         4 | EARTH       | Test  | 0.0272311 |   2.993653 | 0.0193933 |   2.922189 | 0.0717144 | 0.9954075 |
| healthyR      |         5 | NNAR        | Test  | 0.0071148 |   1.873017 | 0.0050670 |   1.918643 | 0.0132196 | 0.9998511 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0432528 |   7.761588 | 0.0291863 |   7.115222 | 0.0530564 | 0.9981350 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5379029 |  63.269031 | 0.3629685 |  93.828222 | 0.6482772 | 0.9979912 |
| healthyR.ts   |         3 | LM          | Test  | 0.0450916 |   7.938993 | 0.0304271 |   7.423365 | 0.0556123 | 0.9981270 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0169409 |   2.503950 | 0.0114315 |   2.472001 | 0.0248697 | 0.9994387 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0063890 |   1.967643 | 0.0043112 |   1.809346 | 0.0077432 | 0.9999502 |
| healthyverse  |         1 | REGRESSION  | Test  | 0.0383558 |   9.115255 | 0.0309110 |   8.260293 | 0.0449376 | 0.9984288 |
| healthyverse  |         2 | GLMNET      | Test  | 0.4430883 | 112.000540 | 0.3570857 | 112.110156 | 0.5366596 | 0.9984806 |
| healthyverse  |         3 | LM          | Test  | 0.0384939 |  10.754435 | 0.0310223 |   9.563301 | 0.0446096 | 0.9984313 |
| healthyverse  |         4 | EARTH       | Test  | 0.0104230 |   2.827565 | 0.0083999 |   2.848221 | 0.0149469 | 0.9996714 |
| healthyverse  |         5 | NNAR        | Test  | 0.0057077 |   1.633552 | 0.0045999 |   1.625035 | 0.0107946 | 0.9998198 |
| healthyR.ai   |         1 | NULL        | NA    |        NA |         NA |        NA |         NA |        NA |        NA |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.5198754 |  62.151056 | 0.3544576 |  91.601988 | 0.6323501 | 0.9984455 |
| healthyR.ai   |         3 | LM          | Test  | 0.0417452 |   8.713040 | 0.0284624 |   9.109184 | 0.0510699 | 0.9984433 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0145483 |   2.319230 | 0.0099192 |   2.288521 | 0.0169219 | 0.9997817 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0047075 |   1.240079 | 0.0032096 |   1.311591 | 0.0080971 | 0.9999352 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0377509 |   7.565958 | 0.0342115 |   7.274025 | 0.0466743 | 0.9987160 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4390301 |  61.606340 | 0.3978683 |  89.105225 | 0.5395283 | 0.9989870 |
| TidyDensity   |         3 | LM          | Test  | 0.0352572 |   6.871961 | 0.0319516 |   6.592238 | 0.0434346 | 0.9987387 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0096043 |   1.178754 | 0.0087039 |   1.173269 | 0.0151792 | 0.9997790 |
| TidyDensity   |         5 | NNAR        | Test  | 0.0472249 |   9.944691 | 0.0427973 |   9.375567 | 0.0603010 | 0.9958311 |

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
    ## 1 healthy…         4 EARTH       Test  0.0318   7.38 0.0229   7.98 0.0466  0.997
    ## 2 healthyR         5 NNAR        Test  0.00711  1.87 0.00507  1.92 0.0132  1.00 
    ## 3 healthy…         5 NNAR        Test  0.00639  1.97 0.00431  1.81 0.00774 1.00 
    ## 4 healthy…         5 NNAR        Test  0.00571  1.63 0.00460  1.63 0.0108  1.00 
    ## 5 healthy…         5 NNAR        Test  0.00471  1.24 0.00321  1.31 0.00810 1.00 
    ## 6 TidyDen…         4 EARTH       Test  0.00960  1.18 0.00870  1.17 0.0152  1.00

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
    ## 1 healthyR.data <tibble>     <tibble [28 × 6]> <split [505|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>     <tibble [28 × 6]> <split [498|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>     <tibble [28 × 6]> <split [446|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>     <tibble [28 × 6]> <split [422|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>     <tibble [28 × 6]> <split [237|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble>     <tibble [28 × 6]> <split [85|28]>  <mdl_time_tbl>

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
