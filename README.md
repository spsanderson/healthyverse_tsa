Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
02 December, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 83,636
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

The last day in the data set is 2023-11-30 23:48:34, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is
-1.237984^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 83636         |
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
| r_version     |     57374 |          0.31 |   5 |   5 |     0 |       39 |          0 |
| r_arch        |     57374 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     57374 |          0.31 |   7 |  15 |     0 |       17 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       49 |          0 |
| country       |      6951 |          0.92 |   2 |   2 |     0 |      147 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-11-30 | 2022-07-15 |     1103 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1260453.36 | 1617169.95 | 357 | 16847.25 | 322874 | 2429696 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10271.52 |   18218.43 |   1 |   184.00 |   2899 |   11412 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-11-30 23:48:34 | 2022-07-15 19:07:28 |    50540 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     51 |       60 |

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
    ## 4 healthyR.data <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 5 healthyR.ts   <tibble [542 × 2]> <tibble [28 × 2]> <split [514|28]>
    ## 6 healthyverse  <tibble [537 × 2]> <tibble [28 × 2]> <split [509|28]>
    ## 7 tidyAML       <tibble [288 × 2]> <tibble [28 × 2]> <split [260|28]>

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
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [4 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [4 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [4 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [260|28]> <mdl_tm_t [4 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 0.6796987 |  505.96108 | 0.9235829 | 100.99325 | 0.8754368 | 0.1107497 |
| TidyDensity   |         2 | LM          | Test  | 0.7348597 |  470.37200 | 0.9985364 | 108.77928 | 0.9411671 | 0.2481394 |
| TidyDensity   |         3 | EARTH       | Test  | 5.3709826 | 6145.23181 | 7.2981573 | 146.71059 | 5.9581383 | 0.2481394 |
| TidyDensity   |         4 | NNAR        | Test  | 0.7118461 |  242.61432 | 0.9672653 | 130.28580 | 0.9413293 | 0.1724163 |
| healthyR      |         1 | ARIMA       | Test  | 0.6091090 |  119.66201 | 0.5527547 | 143.96117 | 0.7379648 | 0.5146458 |
| healthyR      |         2 | LM          | Test  | 0.7395173 |  105.02823 | 0.6710976 | 198.13336 | 0.9049509 | 0.0000225 |
| healthyR      |         3 | EARTH       | Test  | 0.7192852 |  190.77120 | 0.6527374 | 146.70618 | 0.9059985 | 0.0000225 |
| healthyR      |         4 | NNAR        | Test  | 0.6933730 |  128.00874 | 0.6292225 | 160.82319 | 0.8374558 | 0.1708856 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7270730 |  191.23895 | 0.6274416 | 143.88073 | 0.9151967 | 0.3653932 |
| healthyR.ai   |         2 | LM          | Test  | 0.7710211 |  191.38627 | 0.6653675 | 151.77680 | 0.9745299 | 0.0064395 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.7608267 |  248.77298 | 0.6565701 | 140.17368 | 0.9679174 | 0.0064395 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7459107 |  156.35336 | 0.6436980 | 153.50026 | 0.9194790 | 0.1928014 |
| healthyR.data |         1 | ARIMA       | Test  | 0.6421820 |  245.51445 | 0.8805174 | 133.82596 | 0.7541704 | 0.0645056 |
| healthyR.data |         2 | LM          | Test  | 0.6695574 |   96.44137 | 0.9180527 | 187.75415 | 0.7718094 | 0.0049291 |
| healthyR.data |         3 | EARTH       | Test  | 0.6883625 |  433.43752 | 0.9438371 | 125.32968 | 0.8189929 | 0.0049291 |
| healthyR.data |         4 | NNAR        | Test  | 0.6393800 |  122.64982 | 0.8766756 | 168.93479 | 0.7266487 | 0.1905260 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8383359 |  257.79310 | 0.7292450 | 109.65921 | 1.1484092 | 0.0539207 |
| healthyR.ts   |         2 | LM          | Test  | 0.8368915 |  338.28930 | 0.7279886 | 102.29293 | 1.1286492 | 0.0459793 |
| healthyR.ts   |         3 | EARTH       | Test  | 1.7787674 |  699.20727 | 1.5473000 | 173.91985 | 2.1433727 | 0.0459793 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8719026 |  146.75143 | 0.7584438 | 131.64770 | 1.2005270 | 0.1626047 |
| healthyverse  |         1 | ARIMA       | Test  | 0.4866996 |  398.26274 | 0.6832245 |  86.09955 | 0.6311693 | 0.3488767 |
| healthyverse  |         2 | LM          | Test  | 0.5831935 |  538.47619 | 0.8186817 |  92.66371 | 0.7215907 | 0.0095300 |
| healthyverse  |         3 | EARTH       | Test  | 0.6089132 |  641.33869 | 0.8547867 |  92.24968 | 0.7423738 | 0.0095300 |
| healthyverse  |         4 | NNAR        | Test  | 0.5627061 |  392.87718 | 0.7899216 |  94.57654 | 0.7043305 | 0.0409782 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6886549 |  296.24727 | 0.8684913 | 104.13976 | 0.9314680 | 0.1884987 |
| tidyAML       |         2 | LM          | Test  | 0.8209775 |  515.22613 | 1.0353689 | 100.05065 | 1.1122287 | 0.0573557 |
| tidyAML       |         3 | EARTH       | Test  | 0.7540684 |  366.32729 | 0.9509870 | 104.58815 | 0.9970733 | 0.0573557 |
| tidyAML       |         4 | NNAR        | Test  | 0.7391225 |  253.62481 | 0.9321381 | 118.48682 | 0.9558382 | 0.0373486 |

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
    ## 1 TidyDensity           1 ARIMA       Test  0.680  506. 0.924 101.  0.875 0.111 
    ## 2 healthyR              1 ARIMA       Test  0.609  120. 0.553 144.  0.738 0.515 
    ## 3 healthyR.ai           1 ARIMA       Test  0.727  191. 0.627 144.  0.915 0.365 
    ## 4 healthyR.data         4 NNAR        Test  0.639  123. 0.877 169.  0.727 0.191 
    ## 5 healthyR.ts           2 LM          Test  0.837  338. 0.728 102.  1.13  0.0460
    ## 6 healthyverse          1 ARIMA       Test  0.487  398. 0.683  86.1 0.631 0.349 
    ## 7 tidyAML               1 ARIMA       Test  0.689  296. 0.868 104.  0.931 0.188

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
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [1 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [260|28]> <mdl_tm_t [1 × 5]>

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
