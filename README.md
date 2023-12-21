Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
21 December, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 85,316
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

The last day in the data set is 2023-12-18 21:39:36, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is
-1.280969^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 85316         |
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
| r_version     |     58471 |          0.31 |   5 |   5 |     0 |       40 |          0 |
| r_arch        |     58471 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     58471 |          0.31 |   7 |  15 |     0 |       17 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       49 |          0 |
| country       |      7093 |          0.92 |   2 |   2 |     0 |      149 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-12-18 | 2022-07-24 |     1121 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1254834.51 | 1612905.16 | 357 | 16762 | 322854.0 | 2401211 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10284.31 |   18242.36 |   1 |   186 |   2892.5 |   11378 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-12-18 21:39:36 | 2022-07-24 12:14:14 |    51637 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     38 |       60 |

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

    ## # A tibble: 7 × 4
    ##   package       .actual_data       .future_data      .splits         
    ##   <fct>         <list>             <list>            <list>          
    ## 1 TidyDensity   <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 2 healthyR      <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 3 healthyR.ai   <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 4 healthyR.data <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 5 healthyR.ts   <tibble [542 × 2]> <tibble [28 × 2]> <split [514|28]>
    ## 6 healthyverse  <tibble [537 × 2]> <tibble [28 × 2]> <split [509|28]>
    ## 7 tidyAML       <tibble [306 × 2]> <tibble [28 × 2]> <split [278|28]>

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
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl
```

    ## # Nested Modeltime Table
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 TidyDensity   <tibble>     <tibble>     <split [521|28]> <mdl_tm_t [4 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [4 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [4 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [4 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [4 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [4 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [278|28]> <mdl_tm_t [4 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 0.5168899 |  850.56948 | 0.7137336 |  93.34930 | 0.6635675 | 0.0680612 |
| TidyDensity   |         2 | LM          | Test  | 0.5108333 |  764.02029 | 0.7053705 |  95.64464 | 0.6563329 | 0.0049966 |
| TidyDensity   |         3 | EARTH       | Test  | 2.2582666 | 2702.29406 | 3.1182672 | 176.81908 | 2.5755132 | 0.0049966 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5310821 |  335.21113 | 0.7333305 | 133.86159 | 0.6631204 | 0.1077745 |
| healthyR      |         1 | ARIMA       | Test  | 0.6508149 |  136.62673 | 0.6140308 | 163.55382 | 0.8037029 | 0.1942721 |
| healthyR      |         2 | LM          | Test  | 0.6949854 |   98.68739 | 0.6557048 | 192.49064 | 0.8707032 | 0.0544901 |
| healthyR      |         3 | EARTH       | Test  | 0.7086168 |  152.09162 | 0.6685658 | 157.22598 | 0.8849667 | 0.0544901 |
| healthyR      |         4 | NNAR        | Test  | 0.7535887 |  151.78175 | 0.7109959 | 160.03138 | 0.9240960 | 0.0122218 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6323707 |  210.17986 | 0.5693300 | 132.98507 | 0.8679502 | 0.1529371 |
| healthyR.ai   |         2 | LM          | Test  | 0.6790045 |  213.45279 | 0.6113149 | 144.63776 | 0.9056547 | 0.0110635 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.6746387 |  252.93099 | 0.6073843 | 135.91824 | 0.9046209 | 0.0110635 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.6521648 |  195.47032 | 0.5871509 | 143.48782 | 0.8773308 | 0.0952386 |
| healthyR.data |         1 | ARIMA       | Test  | 0.6020555 |  239.61235 | 0.7411742 | 136.03545 | 0.7311679 | 0.0000018 |
| healthyR.data |         2 | LM          | Test  | 0.6214664 |   94.84418 | 0.7650705 | 185.60622 | 0.7355888 | 0.0013260 |
| healthyR.data |         3 | EARTH       | Test  | 0.6199208 |  326.96515 | 0.7631677 | 119.11092 | 0.7691493 | 0.0013260 |
| healthyR.data |         4 | NNAR        | Test  | 0.6444123 |  161.20995 | 0.7933185 | 178.42333 | 0.7480583 | 0.0012016 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6327619 |  574.85228 | 0.5950843 | 103.47309 | 0.9204016 | 0.0494497 |
| healthyR.ts   |         2 | LM          | Test  | 0.6646071 |  833.66733 | 0.6250332 |  98.46924 | 0.9250629 | 0.0072833 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6675460 |  858.94425 | 0.6277972 |  98.12702 | 0.9243559 | 0.0072833 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.6307909 |  382.58753 | 0.5932307 | 122.17233 | 0.9029264 | 0.1485367 |
| healthyverse  |         1 | ARIMA       | Test  | 0.4861129 |   89.63293 | 0.6365011 |  87.07823 | 0.6000089 | 0.2312839 |
| healthyverse  |         2 | LM          | Test  | 0.5238045 |  109.62525 | 0.6858533 |  81.34837 | 0.6658711 | 0.0204578 |
| healthyverse  |         3 | EARTH       | Test  | 0.5349289 |  118.72042 | 0.7004192 |  81.61063 | 0.6704360 | 0.0204578 |
| healthyverse  |         4 | NNAR        | Test  | 0.5424896 |  102.14639 | 0.7103189 |  89.08725 | 0.6880301 | 0.0069197 |
| tidyAML       |         1 | ARIMA       | Test  | 1.0382966 |  185.90528 | 1.3865631 | 156.45818 | 1.2301036 | 0.0431019 |
| tidyAML       |         2 | LM          | Test  | 1.3717939 |  449.03190 | 1.8319224 | 143.61639 | 1.6481914 | 0.0815889 |
| tidyAML       |         3 | EARTH       | Test  | 1.1121393 |  248.63683 | 1.4851742 | 146.90578 | 1.3398544 | 0.0815889 |
| tidyAML       |         4 | NNAR        | Test  | 1.0371945 |  179.05609 | 1.3850913 | 150.48295 | 1.2549726 | 0.0659363 |

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
    ##   # A tibble: 7 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 TidyDensity          2 LM          Test  0.511 764.  0.705  95.6 0.656 5.00e-3
    ## 2 healthyR             1 ARIMA       Test  0.651 137.  0.614 164.  0.804 1.94e-1
    ## 3 healthyR.ai          1 ARIMA       Test  0.632 210.  0.569 133.  0.868 1.53e-1
    ## 4 healthyR.da…         1 ARIMA       Test  0.602 240.  0.741 136.  0.731 1.84e-6
    ## 5 healthyR.ts          4 NNAR        Test  0.631 383.  0.593 122.  0.903 1.49e-1
    ## 6 healthyverse         1 ARIMA       Test  0.486  89.6 0.637  87.1 0.600 2.31e-1
    ## 7 tidyAML              1 ARIMA       Test  1.04  186.  1.39  156.  1.23  4.31e-2

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 TidyDensity   <tibble>     <tibble>     <split [521|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [1 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [278|28]> <mdl_tm_t [1 × 5]>

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
