Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
18 November, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 82,439
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

The last day in the data set is 2023-11-16 23:23:43, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is
-1.204342^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 82439         |
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
| r_version     |     56697 |          0.31 |   5 |   5 |     0 |       39 |          0 |
| r_arch        |     56697 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     56697 |          0.31 |   7 |  15 |     0 |       17 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       49 |          0 |
| country       |      6788 |          0.92 |   2 |   2 |     0 |      147 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-11-16 | 2022-07-08 |     1089 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1262029.38 | 1619528.97 | 357 | 16747.5 | 322873 | 2445283 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10221.82 |   18156.91 |   1 |   185.0 |   2889 |   11354 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-11-16 23:23:43 | 2022-07-08 15:46:51 |    49735 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 59M 12S |       60 |

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
    ## 7 tidyAML       <tibble [274 × 2]> <tibble [28 × 2]> <split [246|28]>

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits          .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>           <list>            
    ## 1 TidyDensity   <tibble>     <tibble>     <split [522|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [519|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [5 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [246|28]> <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 0.9864603 |  176.72564 | 1.0815636 | 120.48563 | 1.2469766 | 0.0909697 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.9578794 |  210.23814 | 1.0502272 | 109.51640 | 1.2064218 | 0.0714507 |
| TidyDensity   |         4 | EARTH       | Test  | 1.0467047 |  161.32919 | 1.1476161 | 133.94940 | 1.2986712 | 0.0714507 |
| TidyDensity   |         5 | NNAR        | Test  | 1.0789575 |  135.43796 | 1.1829783 | 147.73424 | 1.3421797 | 0.0158592 |
| healthyR      |         1 | ARIMA       | Test  | 0.6744633 |  107.47064 | 0.7114231 | 174.31872 | 0.8062700 | 0.1535533 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7075579 |   93.54428 | 0.7463313 | 182.50928 | 0.8426749 | 0.0089547 |
| healthyR      |         4 | EARTH       | Test  | 0.7061843 |  201.14894 | 0.7448823 | 145.61730 | 0.8630802 | 0.0089547 |
| healthyR      |         5 | NNAR        | Test  | 0.6573270 |  114.04388 | 0.6933477 | 161.59801 | 0.8059367 | 0.0964519 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6991020 |  191.16935 | 0.6856637 | 145.65613 | 0.8973692 | 0.1078823 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7544432 |  169.77107 | 0.7399410 | 153.58598 | 0.9420389 | 0.0216387 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.7464127 |  231.84668 | 0.7320649 | 142.16815 | 0.9294417 | 0.0216387 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.6627131 |  161.88918 | 0.6499743 | 131.56186 | 0.8616384 | 0.2138804 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7655101 |  201.53276 | 0.7568157 | 139.68088 | 0.9090398 | 0.0195780 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.7568587 |   98.11708 | 0.7482625 | 191.41009 | 0.8839253 | 0.0158900 |
| healthyR.data |         4 | EARTH       | Test  | 0.7321428 |  153.89568 | 0.7238273 | 138.68831 | 0.8570100 | 0.0158900 |
| healthyR.data |         5 | NNAR        | Test  | 0.7435226 |  103.71529 | 0.7350779 | 166.84573 | 0.8736008 | 0.0236712 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8225130 |  247.11860 | 0.7298500 | 111.27615 | 1.0990611 | 0.0432193 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8563339 |  290.62108 | 0.7598607 | 109.48727 | 1.1176327 | 0.0007867 |
| healthyR.ts   |         4 | EARTH       | Test  | 4.8001237 | 2524.73573 | 4.2593494 | 148.40239 | 5.2421078 | 0.0007867 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.7631589 |  260.25473 | 0.6771827 | 112.87915 | 1.0814945 | 0.0873623 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5987269 |  312.19726 | 0.8065179 |  91.13333 | 0.7151872 | 0.0220030 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.5822559 |  289.65200 | 0.7843305 |  89.90766 | 0.7123909 | 0.0286269 |
| healthyverse  |         4 | EARTH       | Test  | 0.6436392 |  372.23891 | 0.8670172 |  93.13332 | 0.7495973 | 0.0286269 |
| healthyverse  |         5 | NNAR        | Test  | 0.5720270 |  184.67079 | 0.7705516 |  95.70974 | 0.7302961 | 0.0000636 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6036044 |  641.80995 | 0.8730740 | 105.63296 | 0.6914531 | 0.0764475 |
| tidyAML       |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 0.6906786 | 1019.75443 | 0.9990213 | 100.40648 | 0.8327250 | 0.0144944 |
| tidyAML       |         4 | EARTH       | Test  | 0.6257267 |  726.66613 | 0.9050725 | 105.41292 | 0.7120151 | 0.0144944 |
| tidyAML       |         5 | NNAR        | Test  | 0.5426753 |  450.92890 | 0.7849441 | 104.38694 | 0.6433479 | 0.2633601 |

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
    ## 1 TidyDensity           3 LM          Test  0.958  210. 1.05  110.  1.21  0.0715
    ## 2 healthyR              5 NNAR        Test  0.657  114. 0.693 162.  0.806 0.0965
    ## 3 healthyR.ai           5 NNAR        Test  0.663  162. 0.650 132.  0.862 0.214 
    ## 4 healthyR.data         4 EARTH       Test  0.732  154. 0.724 139.  0.857 0.0159
    ## 5 healthyR.ts           5 NNAR        Test  0.763  260. 0.677 113.  1.08  0.0874
    ## 6 healthyverse          3 LM          Test  0.582  290. 0.784  89.9 0.712 0.0286
    ## 7 tidyAML               5 NNAR        Test  0.543  451. 0.785 104.  0.643 0.263

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
    ## 7 tidyAML       <tibble>     <tibble>     <split [246|28]> <mdl_tm_t [1 × 5]>

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
