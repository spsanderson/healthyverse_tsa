Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
12 February, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 57,214
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

The last day in the data set is 2023-02-10 23:28:41, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -5347.51
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 57214         |
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
| r_version     |     39127 |          0.32 |   5 |   5 |     0 |       35 |          0 |
| r_arch        |     39127 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     39127 |          0.32 |   7 |  15 |     0 |       12 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       39 |          0 |
| country       |      4734 |          0.92 |   2 |   2 |     0 |      129 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-02-10 | 2022-02-08 |      810 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |      p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|---------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1350434.47 | 1695540.38 | 357 | 26548.75 | 323463 | 2730973 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9234.48 |   17407.63 |   1 |   120.00 |   2377 |    9919 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-02-10 23:28:41 | 2022-02-08 12:28:42 |    34098 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      2 |       60 |

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
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyverse  <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]>
    ## 5 healthyR.ai   <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]>
    ## 6 TidyDensity   <tibble [386 × 2]> <tibble [28 × 2]> <split [358|28]>

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
    ## 1 healthyR      <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [386 × 2]> <tibble [28 × 2]> <split [358|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.8820467 |  523.0705 | 1.0925814 | 118.36942 | 1.1609462 | 0.0020332 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7644774 |  358.2327 | 0.9469497 | 122.87223 | 0.9872695 | 0.1267700 |
| healthyR      |         4 | EARTH       | Test  | 1.0883703 |  840.5285 | 1.3481523 | 125.30893 | 1.3634831 | 0.1267700 |
| healthyR      |         5 | NNAR        | Test  | 0.7293036 |  214.7321 | 0.9033803 | 144.57548 | 0.9463647 | 0.0007748 |
| healthyR.data |         1 | ARIMA       | Test  | 1.1399832 |  872.2122 | 1.6205234 | 151.21593 | 1.3072125 | 0.0026866 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 1.1855993 |  938.4817 | 1.6853682 | 151.92261 | 1.3493143 | 0.1395124 |
| healthyR.data |         4 | EARTH       | Test  | 1.1343685 |  875.9267 | 1.6125419 | 151.09469 | 1.3002526 | 0.1395124 |
| healthyR.data |         5 | NNAR        | Test  | 0.7844681 |  389.2612 | 1.1151471 | 168.49288 | 0.9257899 | 0.0086726 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0828028 |  313.9127 | 1.0930498 | 153.80283 | 1.4507439 | 0.1610049 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.9792050 |  242.1022 | 0.9884717 | 175.92001 | 1.2586382 | 0.0084229 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.4554362 | 4383.7449 | 1.4692097 | 121.64245 | 1.6606694 | 0.0084229 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.9901744 |  240.7864 | 0.9995449 | 186.80008 | 1.2856281 | 0.0296280 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7031158 | 1855.3843 | 0.9396891 |  90.76025 | 0.8235349 | 0.0456889 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.6244005 | 1361.4973 | 0.8344888 |  91.01626 | 0.7121659 | 0.0398854 |
| healthyverse  |         4 | EARTH       | Test  | 0.8845255 | 2442.2086 | 1.1821365 |  96.03674 | 1.0511690 | 0.0398854 |
| healthyverse  |         5 | NNAR        | Test  | 0.7273466 |  861.1936 | 0.9720727 | 140.06144 | 0.9253275 | 0.1627950 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.9471774 | 1119.6004 | 1.3837784 | 169.32164 | 1.1940827 | 0.0129010 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8404648 |  533.0949 | 1.2278766 | 176.93982 | 1.1172555 | 0.0007773 |
| healthyR.ai   |         4 | EARTH       | Test  | 2.9625404 | 9238.7702 | 4.3281218 | 149.82192 | 3.3106634 | 0.0007773 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8241511 | 1295.3622 | 1.2040432 | 164.96607 | 1.1016861 | 0.0131195 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6438625 |  309.1595 | 0.8053804 | 157.46644 | 0.7729079 | 0.0395725 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6489584 |  139.6159 | 0.8117546 | 182.74360 | 0.7861644 | 0.0491297 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7978447 |  910.2352 | 0.9979902 | 128.88690 | 1.0390872 | 0.0491297 |
| TidyDensity   |         5 | NNAR        | Test  | 0.6787947 |  365.5971 | 0.8490756 | 152.56099 | 0.8167121 | 0.0000042 |

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
    ## 1 healthyR              5 NNAR       Test  0.729  215. 0.903 145.  0.946 7.75e-4
    ## 2 healthyR.data         5 NNAR       Test  0.784  389. 1.12  168.  0.926 8.67e-3
    ## 3 healthyR.ts           3 LM         Test  0.979  242. 0.988 176.  1.26  8.42e-3
    ## 4 healthyverse          3 LM         Test  0.624 1361. 0.834  91.0 0.712 3.99e-2
    ## 5 healthyR.ai           5 NNAR       Test  0.824 1295. 1.20  165.  1.10  1.31e-2
    ## 6 TidyDensity           1 ARIMA      Test  0.644  309. 0.805 157.  0.773 3.96e-2
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
    ## 1 healthyR      <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]> <mdl_tm_t>
    ## 2 healthyR.data <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]> <mdl_tm_t>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]> <mdl_tm_t>
    ## 4 healthyverse  <tibble [548 × 2]> <tibble [28 × 2]> <split [520|28]> <mdl_tm_t>
    ## 5 healthyR.ai   <tibble [538 × 2]> <tibble [28 × 2]> <split [510|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [386 × 2]> <tibble [28 × 2]> <split [358|28]> <mdl_tm_t>
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
