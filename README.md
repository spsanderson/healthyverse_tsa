Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
23 November, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 82,798
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

The last day in the data set is 2023-11-21 19:49:05, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is
-1.215985^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 82798         |
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
| r_version     |     56861 |          0.31 |   5 |   5 |     0 |       39 |          0 |
| r_arch        |     56861 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     56861 |          0.31 |   7 |  15 |     0 |       17 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       49 |          0 |
| country       |      6856 |          0.92 |   2 |   2 |     0 |      147 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-11-21 | 2022-07-09 |     1094 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1263097.22 | 1618877.00 | 357 | 16873 | 323459 | 2445283 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10224.39 |   18164.47 |   1 |   185 |   2888 |   11354 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-11-21 19:49:05 | 2022-07-09 21:16:33 |    50025 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   29.5 |       60 |

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
    ## 7 tidyAML       <tibble [279 × 2]> <tibble [28 × 2]> <split [251|28]>

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
    ## 7 tidyAML       <tibble>     <tibble>     <split [251|28]> <mdl_tm_t [4 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 1.0067545 |  279.37588 | 1.1279920 | 125.76629 | 1.2676006 | 0.0254979 |
| TidyDensity   |         2 | LM          | Test  | 0.9318373 |  340.58672 | 1.0440530 | 107.73640 | 1.1867990 | 0.0965486 |
| TidyDensity   |         3 | EARTH       | Test  | 1.0772952 |  199.59956 | 1.2070275 | 148.23687 | 1.3363889 | 0.0965486 |
| TidyDensity   |         4 | NNAR        | Test  | 1.1104742 |  176.78388 | 1.2442021 | 162.04641 | 1.3674841 | 0.0264986 |
| healthyR      |         1 | ARIMA       | Test  | 0.5484258 |  123.52691 | 0.5914106 | 116.66574 | 0.6808853 | 0.4646857 |
| healthyR      |         2 | LM          | Test  | 0.7007189 |   97.38912 | 0.7556402 | 189.66400 | 0.8348995 | 0.0365431 |
| healthyR      |         3 | EARTH       | Test  | 0.6875510 |  235.35167 | 0.7414404 | 129.18457 | 0.8850174 | 0.0365431 |
| healthyR      |         4 | NNAR        | Test  | 0.6734005 |  152.14029 | 0.7261807 | 143.86281 | 0.8517355 | 0.0065748 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6976550 |  418.99231 | 0.7290953 | 137.30242 | 0.8817549 | 0.0755256 |
| healthyR.ai   |         2 | LM          | Test  | 0.7352975 |  289.34314 | 0.7684342 | 157.82460 | 0.9082299 | 0.0056588 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.7255683 |  510.94086 | 0.7582666 | 137.27245 | 0.9050374 | 0.0056588 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7170134 |  653.11823 | 0.7493261 | 151.79472 | 0.8915164 | 0.0320911 |
| healthyR.data |         1 | ARIMA       | Test  | 0.6914041 |  265.93698 | 0.7333794 | 129.26844 | 0.8338173 | 0.0077541 |
| healthyR.data |         2 | LM          | Test  | 0.7066619 |   96.43162 | 0.7495634 | 185.94750 | 0.8245839 | 0.0102411 |
| healthyR.data |         3 | EARTH       | Test  | 0.7016756 |  396.53521 | 0.7442744 | 116.72329 | 0.8567051 | 0.0102411 |
| healthyR.data |         4 | NNAR        | Test  | 0.6967809 |  124.37377 | 0.7390826 | 156.51132 | 0.8408804 | 0.0909369 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8388004 |  605.59301 | 0.7997287 | 121.75964 | 1.1081104 | 0.0440004 |
| healthyR.ts   |         2 | LM          | Test  | 0.8490713 |  806.62068 | 0.8095211 | 112.78980 | 1.1138182 | 0.0017285 |
| healthyR.ts   |         3 | EARTH       | Test  | 2.2107225 | 3984.71108 | 2.1077459 | 127.55342 | 2.5192027 | 0.0017285 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.7751350 |  442.09570 | 0.7390289 | 116.71873 | 1.1103269 | 0.0439122 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6075457 |  964.01381 | 0.8564048 |  95.34152 | 0.7302344 | 0.0035097 |
| healthyverse  |         2 | LM          | Test  | 0.5465369 |  804.08232 | 0.7704060 |  91.43430 | 0.6898443 | 0.0024755 |
| healthyverse  |         3 | EARTH       | Test  | 0.6343628 | 1075.93737 | 0.8942065 |  95.94433 | 0.7586188 | 0.0024755 |
| healthyverse  |         4 | NNAR        | Test  | 0.5334457 |  636.59792 | 0.7519524 |  95.06960 | 0.6745484 | 0.0003090 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6795661 | 1575.02072 | 0.9987462 | 105.15272 | 0.7944896 | 0.0356065 |
| tidyAML       |         2 | LM          | Test  | 0.7289441 | 1803.08870 | 1.0713162 | 106.38920 | 0.8521952 | 0.0088291 |
| tidyAML       |         3 | EARTH       | Test  | 0.6212218 | 1335.02978 | 0.9129986 | 105.67424 | 0.7191519 | 0.0088291 |
| tidyAML       |         4 | NNAR        | Test  | 0.5578195 |  986.74302 | 0.8198173 | 105.29976 | 0.6484681 | 0.1732551 |

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
    ## 1 TidyDensity          2 LM          Test  0.932 341.  1.04  108.  1.19  9.65e-2
    ## 2 healthyR             1 ARIMA       Test  0.548 124.  0.591 117.  0.681 4.65e-1
    ## 3 healthyR.ai          1 ARIMA       Test  0.698 419.  0.729 137.  0.882 7.55e-2
    ## 4 healthyR.da…         2 LM          Test  0.707  96.4 0.750 186.  0.825 1.02e-2
    ## 5 healthyR.ts          1 ARIMA       Test  0.839 606.  0.800 122.  1.11  4.40e-2
    ## 6 healthyverse         4 NNAR        Test  0.533 637.  0.752  95.1 0.675 3.09e-4
    ## 7 tidyAML              4 NNAR        Test  0.558 987.  0.820 105.  0.648 1.73e-1

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
    ## 7 tidyAML       <tibble>     <tibble>     <split [251|28]> <mdl_tm_t [1 × 5]>

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
