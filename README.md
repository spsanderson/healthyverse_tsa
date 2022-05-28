Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
28 May, 2022

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 37,974
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

The last day in the data set is 2022-05-26 23:43:39, the file was
birthed on: 2021-11-29 11:38:26, and at report knit time is -4279.09
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 37974         |
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
| r_version     |     25478 |          0.33 |   5 |   5 |     0 |       31 |          0 |
| r_arch        |     25478 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     25478 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       21 |          0 |
| country       |      3039 |          0.92 |   2 |   2 |     0 |      109 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-05-26 | 2021-10-27 |      550 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1449175.67 | 1824005.05 | 357 | 16873.00 | 271502 | 3241513 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8716.51 |   16725.41 |   1 |   181.25 |   2659 |    8628 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-05-26 23:43:39 | 2021-10-27 00:19:44 |    22080 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     44 |       60 |

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
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl…
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [491|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [483|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [432|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [408|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [222|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [98 × 6]> <tibble>     <split [70|28]>  <mdl_time_tbl>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |      smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|-----------:|----------:|----------:|
| healthyR.data |         1 | REGRESSION  | Test  | 0.0518446 |  8.620934 | 0.0322221 |   9.461325 | 0.0727826 | 0.9952063 |
| healthyR.data |         2 | GLMNET      | Test  | 0.6248806 | 80.176323 | 0.3883713 | 111.951391 | 0.7786093 | 0.9962122 |
| healthyR.data |         3 | LM          | Test  | 0.0634695 |  9.789676 | 0.0394471 |  10.161339 | 0.0837029 | 0.9940840 |
| healthyR.data |         4 | EARTH       | Test  | 0.0408613 |  6.500074 | 0.0253958 |   7.066010 | 0.0731826 | 0.9955653 |
| healthyR.data |         5 | NNAR        | Test  | 0.0592730 |  7.420194 | 0.0368390 |   6.534991 | 0.1810752 | 0.9762391 |
| healthyR      |         1 | REGRESSION  | Test  | 0.0476050 | 14.343465 | 0.0341308 |  14.180286 | 0.0645734 | 0.9969005 |
| healthyR      |         2 | GLMNET      | Test  | 0.5934062 | 70.555861 | 0.4254475 | 107.154947 | 0.7999486 | 0.9967399 |
| healthyR      |         3 | LM          | Test  | 0.0478334 | 14.546210 | 0.0342946 |  14.557382 | 0.0646219 | 0.9969060 |
| healthyR      |         4 | EARTH       | Test  | 0.0263854 |  3.369541 | 0.0189172 |   3.268093 | 0.0656868 | 0.9981879 |
| healthyR      |         5 | NNAR        | Test  | 0.0361314 |  5.785827 | 0.0259047 |   5.439465 | 0.0637464 | 0.9978636 |
| healthyR.ts   |         1 | REGRESSION  | Test  | 0.0414178 |  6.665976 | 0.0282846 |   6.291911 | 0.0510691 | 0.9980645 |
| healthyR.ts   |         2 | GLMNET      | Test  | 0.5858742 | 61.570394 | 0.4000997 |  90.156299 | 0.7387380 | 0.9978164 |
| healthyR.ts   |         3 | LM          | Test  | 0.0405089 |  7.075402 | 0.0276640 |   6.635012 | 0.0495413 | 0.9980945 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.0154123 |  2.733809 | 0.0105252 |   2.675925 | 0.0181827 | 0.9998997 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.0113960 |  3.229985 | 0.0077824 |   3.336421 | 0.0162539 | 0.9998124 |
| healthyverse  |         1 | NULL        | NA    |        NA |        NA |        NA |         NA |        NA |        NA |
| healthyverse  |         2 | GLMNET      | Test  | 0.5343906 | 92.735010 | 0.4176460 | 105.209508 | 0.6475751 | 0.9979942 |
| healthyverse  |         3 | LM          | Test  | 0.0375316 |  6.433416 | 0.0293323 |   6.083968 | 0.0457862 | 0.9982325 |
| healthyverse  |         4 | EARTH       | Test  | 0.0125600 |  2.408478 | 0.0098161 |   2.421110 | 0.0169972 | 0.9996766 |
| healthyverse  |         5 | NNAR        | Test  | 0.0074619 |  1.349571 | 0.0058317 |   1.349086 | 0.0118677 | 0.9999035 |
| healthyR.ai   |         1 | REGRESSION  | Test  | 0.0459818 |  6.774269 | 0.0315026 |   6.482517 | 0.0610130 | 0.9966710 |
| healthyR.ai   |         2 | GLMNET      | Test  | 0.5744782 | 62.161793 | 0.3935805 |  91.540215 | 0.7280952 | 0.9965254 |
| healthyR.ai   |         3 | LM          | Test  | 0.0460409 |  6.880308 | 0.0315431 |   6.599296 | 0.0610506 | 0.9966919 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.0170780 |  2.310238 | 0.0117003 |   2.282490 | 0.0209514 | 0.9996785 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.0202891 |  1.282886 | 0.0139003 |   1.257712 | 0.0554845 | 0.9982439 |
| TidyDensity   |         1 | REGRESSION  | Test  | 0.0442549 | 14.068103 | 0.0351568 |  12.099374 | 0.0547558 | 0.9989276 |
| TidyDensity   |         2 | GLMNET      | Test  | 0.4591645 | 60.752556 | 0.3647678 |  87.349603 | 0.5558746 | 0.9992068 |
| TidyDensity   |         3 | LM          | Test  | 0.0711131 | 26.645518 | 0.0564934 |  24.483392 | 0.0834568 | 0.9985663 |
| TidyDensity   |         4 | EARTH       | Test  | 0.0115490 |  1.232013 | 0.0091747 |   1.229119 | 0.0177613 | 0.9997033 |
| TidyDensity   |         5 | NNAR        | Test  | 0.2741019 | 63.066855 | 0.2177511 |  71.107044 | 0.3269211 | 0.9481951 |

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
    ##   package   .model_id .model_desc .type     mae  mape    mase smape   rmse   rsq
    ##   <chr>         <int> <chr>       <chr>   <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl>
    ## 1 healthyR…         1 REGRESSION  Test  0.0518   8.62 0.0322   9.46 0.0728 0.995
    ## 2 healthyR          5 NNAR        Test  0.0361   5.79 0.0259   5.44 0.0637 0.998
    ## 3 healthyR…         5 NNAR        Test  0.0114   3.23 0.00778  3.34 0.0163 1.00 
    ## 4 healthyv…         5 NNAR        Test  0.00746  1.35 0.00583  1.35 0.0119 1.00 
    ## 5 healthyR…         4 EARTH       Test  0.0171   2.31 0.0117   2.28 0.0210 1.00 
    ## 6 TidyDens…         4 EARTH       Test  0.0115   1.23 0.00917  1.23 0.0178 1.00

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
    ##   package       .actual_data      .future_data .splits          .modeltime_tabl…
    ##   <chr>         <list>            <list>       <list>           <list>          
    ## 1 healthyR.data <tibble>          <tibble>     <split [491|28]> <mdl_time_tbl>  
    ## 2 healthyR      <tibble>          <tibble>     <split [483|28]> <mdl_time_tbl>  
    ## 3 healthyR.ts   <tibble>          <tibble>     <split [432|28]> <mdl_time_tbl>  
    ## 4 healthyverse  <tibble>          <tibble>     <split [408|28]> <mdl_time_tbl>  
    ## 5 healthyR.ai   <tibble>          <tibble>     <split [222|28]> <mdl_time_tbl>  
    ## 6 TidyDensity   <tibble [98 × 6]> <tibble>     <split [70|28]>  <mdl_time_tbl>

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
