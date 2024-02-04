Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
04 February, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 91,438
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

The last day in the data set is 2024-02-02 23:18:29, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-1.391534^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 91438         |
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
| r_version     |     63433 |          0.31 |   5 |   5 |     0 |       40 |          0 |
| r_arch        |     63433 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     63433 |          0.31 |   7 |  15 |     0 |       17 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       50 |          0 |
| country       |      7495 |          0.92 |   2 |   2 |     0 |      151 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-02-02 | 2022-09-01 |     1167 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1214498.63 | 1594543.03 | 355 | 14701 | 289681 | 2384924 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10224.37 |   18082.54 |   1 |   258 |   2973 |   11342 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-02-02 23:18:29 | 2022-09-01 08:09:39 |    54913 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   32.5 |       60 |

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
    ## 1 TidyDensity   <tibble [550 × 2]> <tibble [28 × 2]> <split [522|28]>
    ## 2 healthyR      <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 3 healthyR.ai   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyR.data <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 5 healthyR.ts   <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 6 healthyverse  <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]>
    ## 7 tidyAML       <tibble [352 × 2]> <tibble [28 × 2]> <split [324|28]>

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
    ## 1 TidyDensity   <tibble>     <tibble>     <split [522|28]> <mdl_tm_t [4 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [4 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [4 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [520|28]> <mdl_tm_t [4 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [4 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [4 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [324|28]> <mdl_tm_t [4 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |     rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|---------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 1.2153263 | 435.45871 | 1.1881718 | 118.07241 | 1.591050 | 0.0635024 |
| TidyDensity   |         2 | LM          | Test  | 1.1391229 | 570.32060 | 1.1136711 | 103.47850 | 1.524013 | 0.0014013 |
| TidyDensity   |         3 | EARTH       | Test  | 1.2472114 | 391.10198 | 1.2193445 | 124.02813 | 1.635263 | 0.0014013 |
| TidyDensity   |         4 | NNAR        | Test  | 1.3316005 | 258.96350 | 1.3018481 | 144.62278 | 1.717588 | 0.0486765 |
| healthyR      |         1 | ARIMA       | Test  | 1.0915986 | 192.22486 | 0.8214286 | 164.57444 | 1.524395 | 0.1222031 |
| healthyR      |         2 | LM          | Test  | 1.0992646 | 106.62390 | 0.8271974 | 187.93009 | 1.556732 | 0.0345419 |
| healthyR      |         3 | EARTH       | Test  | 1.1071872 | 127.91147 | 0.8331591 | 162.44796 | 1.586700 | 0.0345419 |
| healthyR      |         4 | NNAR        | Test  | 1.1429687 | 237.27968 | 0.8600847 | 154.40828 | 1.610972 | 0.0062545 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.9742751 | 432.86803 | 0.7784529 | 130.95852 | 1.488717 | 0.1169042 |
| healthyR.ai   |         2 | LM          | Test  | 1.0199415 | 390.45809 | 0.8149408 | 143.51495 | 1.507630 | 0.0412232 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.0552663 | 701.59273 | 0.8431655 | 126.27116 | 1.588763 | 0.0412232 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.9312032 | 386.06346 | 0.7440382 | 132.94240 | 1.410092 | 0.1225176 |
| healthyR.data |         1 | ARIMA       | Test  | 1.2311047 | 137.76268 | 0.9202646 | 153.34970 | 1.574162 | 0.0008147 |
| healthyR.data |         2 | LM          | Test  | 1.1262078 | 100.79592 | 0.8418530 | 191.73871 | 1.444417 | 0.0293513 |
| healthyR.data |         3 | EARTH       | Test  | 1.2214285 | 130.66860 | 0.9130316 | 147.45466 | 1.588167 | 0.0293513 |
| healthyR.data |         4 | NNAR        | Test  | 1.1308394 | 101.90841 | 0.8453151 | 169.67921 | 1.460348 | 0.0064248 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.1886555 | 101.27735 | 0.6850736 | 139.22584 | 1.721917 | 0.0850089 |
| healthyR.ts   |         2 | LM          | Test  | 1.1555052 | 109.12229 | 0.6659676 | 113.03027 | 1.703578 | 0.0192034 |
| healthyR.ts   |         3 | EARTH       | Test  | 2.8094365 | 343.25210 | 1.6191998 | 165.08835 | 3.403923 | 0.0192034 |
| healthyR.ts   |         4 | NNAR        | Test  | 1.2355042 |  92.98315 | 0.7120745 | 166.97903 | 1.821657 | 0.0278023 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8826828 | 349.10046 | 0.8002295 |  97.29576 | 1.404193 | 0.2517005 |
| healthyverse  |         2 | LM          | Test  | 0.9620751 | 337.49682 | 0.8722055 | 100.79753 | 1.483884 | 0.0680391 |
| healthyverse  |         3 | EARTH       | Test  | 0.9739670 | 366.27067 | 0.8829866 | 100.17827 | 1.498780 | 0.0680391 |
| healthyverse  |         4 | NNAR        | Test  | 1.0614652 | 414.12135 | 0.9623114 | 100.41332 | 1.630465 | 0.1161201 |
| tidyAML       |         1 | ARIMA       | Test  | 1.2119226 | 104.93059 | 1.3641290 | 140.84909 | 1.619921 | 0.0927306 |
| tidyAML       |         2 | LM          | Test  | 1.2997261 | 114.12242 | 1.4629598 | 136.29112 | 1.745890 | 0.0047468 |
| tidyAML       |         3 | EARTH       | Test  | 1.4076875 | 131.36954 | 1.5844802 | 137.14234 | 1.860826 | 0.0047468 |
| tidyAML       |         4 | NNAR        | Test  | 1.2441003 | 108.86130 | 1.4003478 | 138.65775 | 1.666650 | 0.0362042 |

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
    ## 1 TidyDensity          2 LM          Test  1.14   570. 1.11  103.   1.52 0.00140
    ## 2 healthyR             1 ARIMA       Test  1.09   192. 0.821 165.   1.52 0.122  
    ## 3 healthyR.ai          4 NNAR        Test  0.931  386. 0.744 133.   1.41 0.123  
    ## 4 healthyR.da…         2 LM          Test  1.13   101. 0.842 192.   1.44 0.0294 
    ## 5 healthyR.ts          2 LM          Test  1.16   109. 0.666 113.   1.70 0.0192 
    ## 6 healthyverse         1 ARIMA       Test  0.883  349. 0.800  97.3  1.40 0.252  
    ## 7 tidyAML              1 ARIMA       Test  1.21   105. 1.36  141.   1.62 0.0927

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
    ## 1 TidyDensity   <tibble>     <tibble>     <split [522|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [520|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [1 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [324|28]> <mdl_tm_t [1 × 5]>

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
