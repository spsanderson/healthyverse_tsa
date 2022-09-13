Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
12 September, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 46,290
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

The last day in the data set is 2022-09-10 21:57:14, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -1673.98
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 46290         |
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
| r_version     |     31469 |          0.32 |   5 |   5 |     0 |       33 |          0 |
| r_arch        |     31469 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     31469 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       28 |          0 |
| country       |      3633 |          0.92 |   2 |   2 |     0 |      117 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-09-10 | 2021-11-21 |      657 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1383602.9 | 1755977.30 | 357 | 16241.00 | 289911 | 2914338 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8873.8 |   16759.01 |   1 |   173.25 |   2473 |    9268 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-09-10 21:57:14 | 2021-11-21 19:11:34 |    27039 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   53.5 |       60 |

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

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
horizon <- 4*7

nested_data_tbl <- data_transformed_tbl %>%
    
    # 1. Extending: We'll predict n days into the future.
    extend_timeseries(
        .id_var        = package,
        .date_var      = date,
        .length_future = horizon
    ) %>%
    
    # 2. Nesting: We'll group by id, and create a future dataset
    #    that forecasts n days of extended data and
    #    an actual dataset that contains n*2 days
    nest_timeseries(
        .id_var        = package,
        .length_future = horizon
        #.length_actual = horizon*2
    ) %>%
    
   # 3. Splitting: We'll take the actual data and create splits
   #    for accuracy and confidence interval estimation of n das (test)
   #    and the rest is training data
    split_nested_timeseries(
        .length_test = horizon
    )

nested_data_tbl
```

    ## # A tibble: 6 × 4
    ##   package       .actual_data       .future_data      .splits         
    ##   <fct>         <list>             <list>            <list>          
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 2 healthyR.data <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 5 healthyR.ai   <tibble [385 × 2]> <tibble [28 × 2]> <split [357|28]>
    ## 6 TidyDensity   <tibble [233 × 2]> <tibble [28 × 2]> <split [205|28]>

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base
```

    ## Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          1

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
    ##   <fct>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [549 × 2]> <tibble>     <split [521|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble>     <split [519|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [549 × 2]> <tibble>     <split [521|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [385 × 2]> <tibble>     <split [357|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [233 × 2]> <tibble>     <split [205|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7234691 | 127.64384 | 0.7187095 | 157.2786 | 0.8832434 | 0.0277436 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7234947 | 102.53601 | 0.7187349 | 167.5478 | 0.9080746 | 0.0273504 |
| healthyR      |         4 | EARTH       | Test  | 2.1289288 | 738.23178 | 2.1149228 | 148.2999 | 2.3810451 | 0.0273504 |
| healthyR      |         5 | NNAR        | Test  | 0.7283065 | 119.00490 | 0.7235150 | 133.8460 | 0.9652981 | 0.0101651 |
| healthyR.data |         1 | ARIMA       | Test  | 0.8734881 | 123.02201 | 0.6634073 | 114.2067 | 1.0044462 | 0.0402680 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.9065410 | 108.48005 | 0.6885107 | 131.8300 | 1.0213946 | 0.0035276 |
| healthyR.data |         4 | EARTH       | Test  | 0.8798958 | 112.27637 | 0.6682739 | 122.3516 | 0.9910091 | 0.0035276 |
| healthyR.data |         5 | NNAR        | Test  | 1.0981863 |  99.74983 | 0.8340637 | 158.6153 | 1.3172894 | 0.0065963 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7404041 | 102.87804 | 0.6197386 | 162.9031 | 0.8787895 | 0.1748541 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8200872 | 111.68338 | 0.6864356 | 148.0143 | 0.9765949 | 0.0182632 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.9018746 | 138.75182 | 0.7548938 | 143.9590 | 1.0544605 | 0.0182632 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8494850 | 118.20627 | 0.7110423 | 164.0177 | 1.0180348 | 0.0067382 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8344650 | 282.88340 | 0.7152885 | 144.0150 | 0.9577155 | 0.0077764 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.7782888 | 324.23689 | 0.6671353 | 127.4709 | 0.8928539 | 0.0044211 |
| healthyverse  |         4 | EARTH       | Test  | 0.7298948 | 413.55961 | 0.6256528 | 111.1867 | 0.8399468 | 0.0044211 |
| healthyverse  |         5 | NNAR        | Test  | 0.7984958 | 333.37487 | 0.6844564 | 123.6042 | 1.0037053 | 0.0000063 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8142240 | 123.19572 | 0.6685819 | 174.0420 | 0.9608777 | 0.0012656 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7977876 | 102.60564 | 0.6550855 | 196.0238 | 0.9627338 | 0.0005926 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.9270828 | 318.85607 | 0.7612534 | 122.0551 | 1.0850674 | 0.0005926 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7410387 | 116.92526 | 0.6084874 | 138.1465 | 0.9280345 | 0.0935260 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.8235705 | 110.68466 | 0.6969485 | 173.6702 | 1.0393295 | 0.1457360 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7715000 | 110.10165 | 0.6528837 | 140.9549 | 0.9813837 | 0.0040202 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7604247 | 105.35827 | 0.6435112 | 153.8817 | 0.9520182 | 0.0040202 |
| TidyDensity   |         5 | NNAR        | Test  | 0.7624994 | 128.70082 | 0.6452669 | 143.8897 | 0.9233941 | 0.0321224 |

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
    ##   package       .model_id .model_d…¹ .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>             <int> <chr>      <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR              1 ARIMA      Test  0.723  128. 0.719  157. 0.883 0.0277 
    ## 2 healthyR.data         4 EARTH      Test  0.880  112. 0.668  122. 0.991 0.00353
    ## 3 healthyR.ts           1 ARIMA      Test  0.740  103. 0.620  163. 0.879 0.175  
    ## 4 healthyverse          4 EARTH      Test  0.730  414. 0.626  111. 0.840 0.00442
    ## 5 healthyR.ai           5 NNAR       Test  0.741  117. 0.608  138. 0.928 0.0935 
    ## 6 TidyDensity           5 NNAR       Test  0.762  129. 0.645  144. 0.923 0.0321 
    ## # … with abbreviated variable name ¹​.model_desc

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
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
        control = control_nested_refit(verbose = TRUE)
    )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 6 × 5
    ##   package       .actual_data       .future_data .splits          .modeltime_ta…¹
    ##   <fct>         <list>             <list>       <list>           <list>         
    ## 1 healthyR      <tibble [549 × 2]> <tibble>     <split [521|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble>     <split [519|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [549 × 2]> <tibble>     <split [521|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [385 × 2]> <tibble>     <split [357|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [233 × 2]> <tibble>     <split [205|28]> <mdl_time_tbl> 
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
