Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
17 November, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 51,213
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

The last day in the data set is 2022-11-15 21:30:27, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -3257.54
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 51213         |
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
| r_version     |     35096 |          0.31 |   5 |   5 |     0 |       35 |          0 |
| r_arch        |     35096 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     35096 |          0.31 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       32 |          0 |
| country       |      4055 |          0.92 |   2 |   2 |     0 |      121 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-11-15 | 2021-12-22 |      723 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1368575.29 | 1725503.00 | 357 | 21806 | 308100 | 2732883 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8835.06 |   16861.84 |   1 |   135 |   2361 |    9217 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-11-15 21:30:27 | 2021-12-22 10:05:26 |    30204 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 39M 57S |       60 |

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
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 5 healthyR.ai   <tibble [451 × 2]> <tibble [28 × 2]> <split [423|28]>
    ## 6 TidyDensity   <tibble [299 × 2]> <tibble [28 × 2]> <split [271|28]>

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
    ##   package       .actual_data       .future_data      .splits          .modelti…¹
    ##   <fct>         <list>             <list>            <list>           <list>    
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [451 × 2]> <tibble [28 × 2]> <split [423|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [299 × 2]> <tibble [28 × 2]> <split [271|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |     mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|---------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7401915 | 141.3578 | 0.7523808 | 155.5114 | 0.9661668 | 0.0768177 |
| healthyR      |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.6531036 | 102.8559 | 0.6638588 | 177.4108 | 0.8393487 | 0.0068598 |
| healthyR      |         4 | EARTH       | Test  | 0.6648428 | 191.8232 | 0.6757913 | 142.0768 | 0.7786450 | 0.0068598 |
| healthyR      |         5 | NNAR        | Test  | 0.6519723 | 218.5097 | 0.6627088 | 113.6771 | 0.8889268 | 0.0211532 |
| healthyR.data |         1 | ARIMA       | Test  | 0.9186200 | 125.9066 | 0.6515124 | 143.3151 | 1.0762624 | 0.1598144 |
| healthyR.data |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.8057228 | 135.1515 | 0.5714425 | 115.0679 | 0.9329453 | 0.0095987 |
| healthyR.data |         4 | EARTH       | Test  | 0.8039010 | 138.4185 | 0.5701504 | 113.0162 | 0.9354785 | 0.0095987 |
| healthyR.data |         5 | NNAR        | Test  | 0.8111970 | 118.8992 | 0.5753249 | 123.4015 | 1.0520490 | 0.0203210 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8032949 | 138.6103 | 0.6305162 | 148.1099 | 0.9547710 | 0.0016753 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8014965 | 120.4335 | 0.6291047 | 152.6921 | 0.9394429 | 0.0659769 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.7946475 | 110.2315 | 0.6237288 | 157.4253 | 0.9327707 | 0.0659769 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.7910934 | 156.2652 | 0.6209391 | 158.5354 | 0.9150626 | 0.0354562 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8335645 | 155.4472 | 0.7045027 | 137.8605 | 0.9788909 | 0.0115102 |
| healthyverse  |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.8272363 | 180.3768 | 0.6991543 | 127.2048 | 0.9500285 | 0.0181076 |
| healthyverse  |         4 | EARTH       | Test  | 0.8484161 | 223.0360 | 0.7170548 | 119.3246 | 0.9666673 | 0.0181076 |
| healthyverse  |         5 | NNAR        | Test  | 0.8434961 | 186.4687 | 0.7128966 | 154.9945 | 0.9471778 | 0.0429864 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7892818 | 107.6904 | 0.6478124 | 147.9318 | 0.9380816 | 0.0074837 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7523666 | 104.0205 | 0.6175138 | 170.9440 | 0.8527884 | 0.0041014 |
| healthyR.ai   |         4 | EARTH       | Test  | 1.5397001 | 423.4312 | 1.2637272 | 139.5091 | 1.7755249 | 0.0041014 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7881223 | 129.6001 | 0.6468608 | 159.5032 | 0.9071231 | 0.0155631 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.7774678 | 110.2534 | 0.6376286 | 191.1270 | 0.8943376 | 0.0247254 |
| TidyDensity   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7312383 |  99.2766 | 0.5997142 | 145.5096 | 0.8850339 | 0.0023646 |
| TidyDensity   |         4 | EARTH       | Test  | 1.4526161 | 409.4726 | 1.1913414 | 138.3255 | 1.7018040 | 0.0023646 |
| TidyDensity   |         5 | NNAR        | Test  | 0.8504424 | 142.7939 | 0.6974777 | 170.0778 | 0.9574035 | 0.0063295 |

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
    ## 1 healthyR              4 EARTH      Test  0.665 192.  0.676  142. 0.779 0.00686
    ## 2 healthyR.data         3 LM         Test  0.806 135.  0.571  115. 0.933 0.00960
    ## 3 healthyR.ts           5 NNAR       Test  0.791 156.  0.621  159. 0.915 0.0355 
    ## 4 healthyverse          5 NNAR       Test  0.843 186.  0.713  155. 0.947 0.0430 
    ## 5 healthyR.ai           3 LM         Test  0.752 104.  0.618  171. 0.853 0.00410
    ## 6 TidyDensity           3 LM         Test  0.731  99.3 0.600  146. 0.885 0.00236
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
    ##   package       .actual_data       .future_data      .splits          .modelti…¹
    ##   <fct>         <list>             <list>            <list>           <list>    
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [451 × 2]> <tibble [28 × 2]> <split [423|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [299 × 2]> <tibble [28 × 2]> <split [271|28]> <mdl_tm_t>
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
