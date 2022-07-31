Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
31 July, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 43,265
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

The last day in the data set is 2022-07-29 23:16:29, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -643.3 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 43265         |
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
| r_version     |     29133 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     29133 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     29133 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       26 |          0 |
| country       |      3445 |          0.92 |   2 |   2 |     0 |      116 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-29 | 2021-11-10 |      614 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1410233.82 | 1778625.10 | 357 | 16873 | 289858 | 2970510 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8812.55 |   16814.89 |   1 |   163 |   2580 |    8956 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-29 23:16:29 | 2021-11-10 15:15:22 |    25370 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 27M 40S |       60 |

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
    ## 1 healthyR      <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 2 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 4 healthyverse  <tibble [527 × 2]> <tibble [28 × 2]> <split [499|28]>
    ## 5 healthyR.ai   <tibble [342 × 2]> <tibble [28 × 2]> <split [314|28]>
    ## 6 TidyDensity   <tibble [190 × 2]> <tibble [28 × 2]> <split [162|28]>

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
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [527 × 2]> <tibble>     <split [499|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [342 × 2]> <tibble>     <split [314|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [190 × 2]> <tibble>     <split [162|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7920423 | 104.69505 | 0.6749289 | 168.6077 | 0.9985333 | 0.1668133 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8453489 | 105.61115 | 0.7203534 | 180.6568 | 1.0586992 | 0.1257938 |
| healthyR      |         4 | EARTH       | Test  | 0.8241884 |  99.31292 | 0.7023217 | 195.7274 | 1.0357419 | 0.1257938 |
| healthyR      |         5 | NNAR        | Test  | 0.8594991 | 148.62748 | 0.7324114 | 159.7548 | 1.0717251 | 0.0671025 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7677249 |  94.43754 | 0.5815428 | 110.3600 | 0.9275904 | 0.0005628 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.7753152 |  89.48967 | 0.5872923 | 119.1077 | 0.9331943 | 0.0035888 |
| healthyR.data |         4 | EARTH       | Test  | 0.7717842 |  89.46428 | 0.5846177 | 117.4395 | 0.9308355 | 0.0035888 |
| healthyR.data |         5 | NNAR        | Test  | 0.9257960 | 113.35081 | 0.7012798 | 151.1001 | 1.0921284 | 0.0073101 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7728344 | 111.38774 | 0.6007185 | 155.6035 | 0.9947237 | 0.0824189 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7914402 | 104.00352 | 0.6151806 | 186.7332 | 1.0329453 | 0.0679661 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.7661607 | 146.36212 | 0.5955310 | 134.2459 | 1.0013356 | 0.0679661 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8466739 | 153.44290 | 0.6581133 | 159.5652 | 1.0818683 | 0.0227471 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7059174 | 144.85626 | 0.6334868 | 130.3142 | 0.8704413 | 0.0022365 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.6754743 | 178.76851 | 0.6061673 | 113.1501 | 0.8369149 | 0.0117835 |
| healthyverse  |         4 | EARTH       | Test  | 0.6930674 | 233.71492 | 0.6219553 | 105.9522 | 0.8457282 | 0.0117835 |
| healthyverse  |         5 | NNAR        | Test  | 0.7696236 | 147.61217 | 0.6906563 | 142.6603 | 0.9329231 | 0.0013527 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6579819 | 105.54695 | 0.6878912 | 160.1617 | 0.9014015 | 0.0126041 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7313620 | 157.83145 | 0.7646069 | 169.2129 | 0.9488489 | 0.0374305 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.6658535 |  97.71480 | 0.6961206 | 170.1388 | 0.8918257 | 0.0374305 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8310939 | 286.03835 | 0.8688721 | 155.1945 | 1.0859967 | 0.0052046 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.7836958 | 101.54805 | 0.6782283 | 193.4476 | 0.9339225 | 0.1266860 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7395788 |  97.16235 | 0.6400484 | 158.3933 | 0.8816487 | 0.1230842 |
| TidyDensity   |         4 | EARTH       | Test  | 0.8102564 | 112.40150 | 0.7012144 | 164.4120 | 0.9800872 | 0.1230842 |
| TidyDensity   |         5 | NNAR        | Test  | 0.9521399 | 165.40925 | 0.8240037 | 170.1516 | 1.1161403 | 0.0581606 |

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
    ## 1 healthyR              1 ARIMA      Test  0.792 105.  0.675  169. 0.999 1.67e-1
    ## 2 healthyR.data         1 ARIMA      Test  0.768  94.4 0.582  110. 0.928 5.63e-4
    ## 3 healthyR.ts           1 ARIMA      Test  0.773 111.  0.601  156. 0.995 8.24e-2
    ## 4 healthyverse          3 LM         Test  0.675 179.  0.606  113. 0.837 1.18e-2
    ## 5 healthyR.ai           4 EARTH      Test  0.666  97.7 0.696  170. 0.892 3.74e-2
    ## 6 TidyDensity           3 LM         Test  0.740  97.2 0.640  158. 0.882 1.23e-1
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
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [527 × 2]> <tibble>     <split [499|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [342 × 2]> <tibble>     <split [314|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [190 × 2]> <tibble>     <split [162|28]> <mdl_time_tbl> 
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
