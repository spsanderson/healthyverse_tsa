Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
26 January, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 89,758
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

The last day in the data set is 2024-01-24 23:55:20, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-1.369995^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 89758         |
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
| r_version     |     61944 |          0.31 |   5 |   5 |     0 |       40 |          0 |
| r_arch        |     61944 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     61944 |          0.31 |   7 |  15 |     0 |       17 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       50 |          0 |
| country       |      7429 |          0.92 |   2 |   2 |     0 |      151 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-01-24 | 2022-08-19 |     1158 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1228685.89 | 1599387.67 | 355 | 14701.00 | 301755 | 2387558 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10285.01 |   18175.49 |   1 |   217.25 |   2880 |   11412 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-01-24 23:55:20 | 2022-08-19 14:16:21 |    54184 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     30 |       60 |

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
    ## 7 tidyAML       <tibble [343 × 2]> <tibble [28 × 2]> <split [315|28]>

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
    ## 7 tidyAML       <tibble>     <tibble>     <split [315|28]> <mdl_tm_t [4 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |     rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|---------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 0.9573680 | 132.43340 | 1.1710800 | 109.08687 | 1.365283 | 0.0865132 |
| TidyDensity   |         2 | LM          | Test  | 0.9796576 | 160.79893 | 1.1983452 | 107.21571 | 1.356911 | 0.2918744 |
| TidyDensity   |         3 | EARTH       | Test  | 0.9553447 | 165.49081 | 1.1686050 | 104.10413 | 1.315232 | 0.2918744 |
| TidyDensity   |         4 | NNAR        | Test  | 1.0547778 |  94.81307 | 1.2902344 | 129.33208 | 1.509936 | 0.0246521 |
| healthyR      |         1 | ARIMA       | Test  | 0.8865969 | 142.36401 | 0.7363690 | 165.71694 | 1.271507 | 0.1260512 |
| healthyR      |         2 | LM          | Test  | 0.9407435 | 109.66447 | 0.7813409 | 188.98960 | 1.332904 | 0.1303125 |
| healthyR      |         3 | EARTH       | Test  | 0.9107281 | 117.56191 | 0.7564114 | 141.48655 | 1.341556 | 0.1303125 |
| healthyR      |         4 | NNAR        | Test  | 0.8347290 | 141.62626 | 0.6932898 | 139.15064 | 1.228101 | 0.1881086 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8987279 | 185.97704 | 0.7939120 | 146.87891 | 1.283827 | 0.0669415 |
| healthyR.ai   |         2 | LM          | Test  | 0.9185933 | 218.34822 | 0.8114607 | 141.59342 | 1.312987 | 0.1166824 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.9177882 | 212.39312 | 0.8107494 | 141.93053 | 1.310865 | 0.1166824 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.8743851 | 213.77037 | 0.7724083 | 142.52739 | 1.253167 | 0.1136824 |
| healthyR.data |         1 | ARIMA       | Test  | 1.0262181 | 106.96090 | 0.7054750 | 157.44335 | 1.287726 | 0.0510196 |
| healthyR.data |         2 | LM          | Test  | 1.0819215 | 100.23341 | 0.7437684 | 199.19281 | 1.327879 | 0.0972626 |
| healthyR.data |         3 | EARTH       | Test  | 1.0753325 | 120.41598 | 0.7392387 | 144.06408 | 1.342881 | 0.0972626 |
| healthyR.data |         4 | NNAR        | Test  | 1.0255439 |  90.44254 | 0.7050115 | 153.90550 | 1.278004 | 0.1773027 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.8922435 | 384.60136 | 1.1580580 | 136.04871 | 2.172498 | 0.1087298 |
| healthyR.ts   |         2 | LM          | Test  | 1.1440638 | 148.78826 | 0.7001701 | 125.87898 | 1.632539 | 0.1179659 |
| healthyR.ts   |         3 | EARTH       | Test  | 1.1636989 | 156.70465 | 0.7121868 | 126.11482 | 1.637573 | 0.1179659 |
| healthyR.ts   |         4 | NNAR        | Test  | 1.0274739 | 101.79846 | 0.6288167 | 148.65354 | 1.610660 | 0.0319005 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6773083 | 306.60735 | 0.8073533 |  82.70866 | 1.079284 | 0.1065050 |
| healthyverse  |         2 | LM          | Test  | 0.7146963 | 264.43300 | 0.8519199 |  82.83982 | 1.124237 | 0.1577503 |
| healthyverse  |         3 | EARTH       | Test  | 0.7103729 | 267.43802 | 0.8467664 |  82.02244 | 1.122477 | 0.1577503 |
| healthyverse  |         4 | NNAR        | Test  | 0.7649515 | 324.05629 | 0.9118242 |  90.12384 | 1.222605 | 0.0453544 |
| tidyAML       |         1 | ARIMA       | Test  | 0.9516353 | 134.34737 | 1.4583442 | 159.70954 | 1.313645 | 0.0342320 |
| tidyAML       |         2 | LM          | Test  | 1.0530094 | 441.75021 | 1.6136962 | 124.43839 | 1.517283 | 0.3270951 |
| tidyAML       |         3 | EARTH       | Test  | 1.0104625 | 280.27637 | 1.5484947 | 140.33422 | 1.416293 | 0.3270951 |
| tidyAML       |         4 | NNAR        | Test  | 0.9976426 | 316.70167 | 1.5288487 | 137.17331 | 1.410889 | 0.0370564 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 TidyDensity           3 EARTH       Test  0.955 165.  1.17  104.   1.32 0.292 
    ## 2 healthyR              4 NNAR        Test  0.835 142.  0.693 139.   1.23 0.188 
    ## 3 healthyR.ai           4 NNAR        Test  0.874 214.  0.772 143.   1.25 0.114 
    ## 4 healthyR.data         4 NNAR        Test  1.03   90.4 0.705 154.   1.28 0.177 
    ## 5 healthyR.ts           4 NNAR        Test  1.03  102.  0.629 149.   1.61 0.0319
    ## 6 healthyverse          1 ARIMA       Test  0.677 307.  0.807  82.7  1.08 0.107 
    ## 7 tidyAML               1 ARIMA       Test  0.952 134.  1.46  160.   1.31 0.0342

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
    ## 7 tidyAML       <tibble>     <tibble>     <split [315|28]> <mdl_tm_t [1 × 5]>

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
