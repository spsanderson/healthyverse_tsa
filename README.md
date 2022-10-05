Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
04 October, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 47,753
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

The last day in the data set is 2022-10-02 23:11:34, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -2203.22
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 47753         |
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
| r_version     |     32510 |          0.32 |   5 |   5 |     0 |       33 |          0 |
| r_arch        |     32510 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     32510 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       28 |          0 |
| country       |      3724 |          0.92 |   2 |   2 |     0 |      117 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-10-02 | 2021-11-30 |      679 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1380191.11 | 1748208.04 | 357 | 16734 | 293068 | 2910176 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8871.69 |   16796.38 |   1 |   167 |   2412 |    9299 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-10-02 23:11:34 | 2021-11-30 10:00:05 |    27958 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 38M 11S |       60 |

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
    ## 1 healthyR      <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 4 healthyverse  <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 5 healthyR.ai   <tibble [407 × 2]> <tibble [28 × 2]> <split [379|28]>
    ## 6 TidyDensity   <tibble [255 × 2]> <tibble [28 × 2]> <split [227|28]>

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
    ## 1 healthyR      <tibble [548 × 2]> <tibble>     <split [520|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [548 × 2]> <tibble>     <split [520|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [407 × 2]> <tibble>     <split [379|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [255 × 2]> <tibble>     <split [227|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.5890270 | 118.58296 | 0.5663165 | 131.78322 | 0.8071653 | 0.0176484 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.5984956 |  94.31703 | 0.5754200 | 169.88630 | 0.7911845 | 0.0045491 |
| healthyR      |         4 | EARTH       | Test  | 1.1028128 | 512.61576 | 1.0602928 | 132.85036 | 1.3080540 | 0.0045491 |
| healthyR      |         5 | NNAR        | Test  | 0.6072062 | 173.42704 | 0.5837947 | 125.92199 | 0.8458759 | 0.0027409 |
| healthyR.data |         1 | ARIMA       | Test  | 0.9393372 | 140.66615 | 0.6825297 | 113.40010 | 1.1417937 | 0.1451868 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.8633851 | 108.08900 | 0.6273423 | 118.95111 | 1.0339833 | 0.0043093 |
| healthyR.data |         4 | EARTH       | Test  | 0.8820788 | 143.78810 | 0.6409253 | 103.86075 | 1.0835534 | 0.0043093 |
| healthyR.data |         5 | NNAR        | Test  | 1.0069867 | 109.73410 | 0.7316844 | 181.90293 | 1.2198590 | 0.0018644 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6570018 | 137.82509 | 0.6190073 | 155.32926 | 0.8133926 | 0.0070709 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7241210 | 151.63713 | 0.6822450 | 163.58494 | 0.8661164 | 0.0020755 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.7049534 | 137.49976 | 0.6641859 | 169.04877 | 0.8465572 | 0.0020755 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.6573970 | 118.51964 | 0.6193796 | 155.36521 | 0.8216326 | 0.0128452 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7579238 | 251.60308 | 0.6907588 | 115.85666 | 0.8710008 | 0.0005338 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.6662053 | 188.38517 | 0.6071682 | 110.54873 | 0.8082649 | 0.0272329 |
| healthyverse  |         4 | EARTH       | Test  | 0.7004121 | 288.63631 | 0.6383436 |  98.91487 | 0.8562377 | 0.0272329 |
| healthyverse  |         5 | NNAR        | Test  | 0.7403299 | 149.48147 | 0.6747240 | 137.80680 | 0.9124462 | 0.0007303 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6404647 | 143.72810 | 0.5703060 | 140.86626 | 0.8411936 | 0.0592669 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.6750779 | 104.49801 | 0.6011275 | 153.20483 | 0.8742384 | 0.0242066 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.9151847 | 455.36923 | 0.8149322 | 113.26393 | 1.1074003 | 0.0242066 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7184294 | 130.45487 | 0.6397302 | 156.80390 | 0.9008722 | 0.0124481 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6105001 | 131.14759 | 0.6273005 | 168.01107 | 0.7672846 | 0.0034405 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.5984815 | 126.35248 | 0.6149512 | 162.66100 | 0.7623461 | 0.0176581 |
| TidyDensity   |         4 | EARTH       | Test  | 0.5976790 | 125.37136 | 0.6141265 | 163.30326 | 0.7614058 |        NA |
| TidyDensity   |         5 | NNAR        | Test  | 0.5716823 | 182.68919 | 0.5874145 | 129.49373 | 0.7886674 | 0.0073977 |

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
    ##   package       .model_id .model_…¹ .type   mae  mape  mase smape  rmse      rsq
    ##   <fct>             <int> <chr>     <chr> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR              3 LM        Test  0.598  94.3 0.575  170. 0.791  0.00455
    ## 2 healthyR.data         3 LM        Test  0.863 108.  0.627  119. 1.03   0.00431
    ## 3 healthyR.ts           1 ARIMA     Test  0.657 138.  0.619  155. 0.813  0.00707
    ## 4 healthyverse          3 LM        Test  0.666 188.  0.607  111. 0.808  0.0272 
    ## 5 healthyR.ai           1 ARIMA     Test  0.640 144.  0.570  141. 0.841  0.0593 
    ## 6 TidyDensity           4 EARTH     Test  0.598 125.  0.614  163. 0.761 NA      
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
    ## 1 healthyR      <tibble [548 × 2]> <tibble>     <split [520|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [548 × 2]> <tibble>     <split [520|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [407 × 2]> <tibble>     <split [379|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [255 × 2]> <tibble>     <split [227|28]> <mdl_time_tbl> 
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
