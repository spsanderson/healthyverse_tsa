Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
02 May, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 65,114
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

The last day in the data set is 2023-04-30 23:38:18, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -7243.67
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 65114         |
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
| r_version     |     44261 |          0.32 |   5 |   5 |     0 |       37 |          0 |
| r_arch        |     44261 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     44261 |          0.32 |   7 |  15 |     0 |       14 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       43 |          0 |
| country       |      5491 |          0.92 |   2 |   2 |     0 |      136 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-04-30 | 2022-03-25 |      889 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1312915.28 | 1665455.37 | 357 | 26654.25 | 322853 | 2721880 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9797.29 |   18082.36 |   1 |   126.00 |   2614 |   10756 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-04-30 23:38:18 | 2022-03-25 16:12:27 |    39127 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     20 |       60 |

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
    ## 1 healthyR      <tibble [545 × 2]> <tibble [28 × 2]> <split [517|28]>
    ## 2 healthyR.ai   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 4 healthyR.ts   <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]>
    ## 5 healthyverse  <tibble [535 × 2]> <tibble [28 × 2]> <split [507|28]>
    ## 6 TidyDensity   <tibble [465 × 2]> <tibble [28 × 2]> <split [437|28]>
    ## 7 tidyAML       <tibble [74 × 2]>  <tibble [28 × 2]> <split [46|28]>

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
    ## 1 healthyR      <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [507|28]> <mdl_tm_t [5 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [437|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [46|28]>  <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |        mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|------------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 1.0016857 |   979.49135 | 1.0496864 | 176.4134 | 1.2538078 | 0.0111840 |
| healthyR      |         2 | NULL        | NA    |        NA |          NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 1.0702512 |   919.25379 | 1.1215376 | 173.0508 | 1.3162685 | 0.0000032 |
| healthyR      |         4 | EARTH       | Test  | 4.5859382 | 10770.69660 | 4.8056963 | 177.5953 | 4.9256198 | 0.0000032 |
| healthyR      |         5 | NNAR        | Test  | 0.9240861 |   246.07798 | 0.9683682 | 153.5410 | 1.1928324 | 0.0510630 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.9692725 |   285.51110 | 1.0761661 | 172.1155 | 1.2166964 | 0.0297004 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |          NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.9515434 |   232.93629 | 1.0564818 | 183.7566 | 1.1862652 | 0.0568304 |
| healthyR.ai   |         4 | EARTH       | Test  | 1.1764550 |  1638.91323 | 1.3061971 | 153.1628 | 1.4591333 | 0.0568304 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.9793337 |  1027.09836 | 1.0873369 | 157.7073 | 1.2317847 | 0.0027645 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7498865 |   139.83486 | 0.9065901 | 169.6535 | 0.9006328 | 0.0031436 |
| healthyR.data |         2 | NULL        | NA    |        NA |          NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.7354602 |  1772.09263 | 0.8891491 | 103.2385 | 1.0281571 | 0.0002084 |
| healthyR.data |         4 | EARTH       | Test  | 0.7625381 |   465.15096 | 0.9218855 | 177.2154 | 0.9007455 | 0.0002084 |
| healthyR.data |         5 | NNAR        | Test  | 0.7002609 |   757.52688 | 0.8465942 | 128.8700 | 0.8872580 | 0.0062196 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.1519315 |    97.88995 | 0.8788139 | 150.0871 | 1.4910918 | 0.0064655 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |          NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 1.1679143 |    93.50352 | 0.8910073 | 154.5909 | 1.5048964 | 0.0439216 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.3485188 |   125.34651 | 1.0287913 | 175.9134 | 1.6568650 | 0.0439216 |
| healthyR.ts   |         5 | NNAR        | Test  | 1.2057214 |    98.02242 | 0.9198505 | 168.7702 | 1.5412668 | 0.0342047 |
| healthyverse  |         1 | ARIMA       | Test  | 1.0206474 |   448.22288 | 1.2151870 | 176.9683 | 1.1949040 | 0.0000000 |
| healthyverse  |         2 | NULL        | NA    |        NA |          NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.8716067 |  1852.12102 | 1.0377385 | 105.9688 | 1.1279205 | 0.0641108 |
| healthyverse  |         4 | EARTH       | Test  | 1.1788212 |   775.72645 | 1.4035093 | 161.7209 | 1.3587733 | 0.0641108 |
| healthyverse  |         5 | NNAR        | Test  | 0.8776059 |  1319.95123 | 1.0448812 | 141.6833 | 1.0624836 | 0.0236844 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.8040949 |   990.28106 | 1.0506471 | 168.6152 | 0.9910047 | 0.0546066 |
| TidyDensity   |         2 | NULL        | NA    |        NA |          NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.8175424 |   694.61304 | 1.0682179 | 176.8081 | 1.0266930 | 0.0360831 |
| TidyDensity   |         4 | EARTH       | Test  | 0.8292780 |   508.50613 | 1.0835519 | 183.4972 | 1.0521896 | 0.0360831 |
| TidyDensity   |         5 | NNAR        | Test  | 0.8055435 |  2005.35837 | 1.0525400 | 147.6900 | 1.0115998 | 0.0293871 |
| tidyAML       |         1 | ARIMA       | Test  | 0.9401670 |  1439.98188 | 1.3107545 | 131.3461 | 1.2574522 |        NA |
| tidyAML       |         2 | NULL        | NA    |        NA |          NA |        NA |       NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 1.2741878 |  1854.19946 | 1.7764369 | 131.2191 | 1.6825478 | 0.4511897 |
| tidyAML       |         4 | EARTH       | Test  | 1.6993326 |  2405.95815 | 2.3691618 | 134.6047 | 2.1937806 | 0.4511897 |
| tidyAML       |         5 | NNAR        | Test  | 0.9873122 |  2789.88688 | 1.3764830 | 132.9042 | 1.2915971 | 0.0009440 |

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
    ##   package    .model_id .model_desc .type   mae   mape  mase smape  rmse      rsq
    ##   <fct>          <int> <chr>       <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR           5 NNAR        Test  0.924  246.  0.968  154. 1.19   0.0511 
    ## 2 healthyR.…         3 LM          Test  0.952  233.  1.06   184. 1.19   0.0568 
    ## 3 healthyR.…         5 NNAR        Test  0.700  758.  0.847  129. 0.887  0.00622
    ## 4 healthyR.…         1 ARIMA       Test  1.15    97.9 0.879  150. 1.49   0.00647
    ## 5 healthyve…         5 NNAR        Test  0.878 1320.  1.04   142. 1.06   0.0237 
    ## 6 TidyDensi…         1 ARIMA       Test  0.804  990.  1.05   169. 0.991  0.0546 
    ## 7 tidyAML            1 ARIMA       Test  0.940 1440.  1.31   131. 1.26  NA

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
    ## 1 healthyR      <tibble>     <tibble>     <split [517|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [510|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [507|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [437|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [46|28]>  <mdl_tm_t [1 × 5]>

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
