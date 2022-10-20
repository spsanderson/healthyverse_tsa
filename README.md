Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
19 October, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 49,150
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

The last day in the data set is 2022-10-17 23:53:51, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -2563.93
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 49150         |
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
| r_version     |     33526 |          0.32 |   5 |   5 |     0 |       34 |          0 |
| r_arch        |     33526 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     33526 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       31 |          0 |
| country       |      3869 |          0.92 |   2 |   2 |     0 |      119 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-10-17 | 2021-12-09 |      694 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1375846.49 | 1738439.10 | 357 | 16873 | 301749 | 2909724 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8870.96 |   16828.33 |   1 |   158 |   2400 |    9312 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-10-17 23:53:51 | 2021-12-09 10:43:10 |    28850 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |      9 |       60 |

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
    ## 5 healthyR.ai   <tibble [422 × 2]> <tibble [28 × 2]> <split [394|28]>
    ## 6 TidyDensity   <tibble [270 × 2]> <tibble [28 × 2]> <split [242|28]>

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
    ## 5 healthyR.ai   <tibble [422 × 2]> <tibble>     <split [394|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [270 × 2]> <tibble>     <split [242|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.6081676 | 113.45731 | 0.6637880 | 158.7439 | 0.7891812 | 0.0020183 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.5906294 |  99.24835 | 0.6446458 | 195.2596 | 0.7673176 | 0.0020682 |
| healthyR      |         4 | EARTH       | Test  | 0.5914088 | 111.35860 | 0.6454965 | 164.0160 | 0.7625145 | 0.0020682 |
| healthyR      |         5 | NNAR        | Test  | 0.6321911 | 184.40643 | 0.6900086 | 120.3955 | 0.8862529 | 0.0000577 |
| healthyR.data |         1 | ARIMA       | Test  | 0.8512847 | 221.19435 | 0.6776164 | 116.0895 | 1.0179741 | 0.0111376 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.8230999 | 181.82626 | 0.6551815 | 124.0697 | 0.9537509 | 0.0070341 |
| healthyR.data |         4 | EARTH       | Test  | 0.8306082 | 203.79772 | 0.6611581 | 118.6903 | 0.9776930 | 0.0070341 |
| healthyR.data |         5 | NNAR        | Test  | 0.8649672 | 102.33883 | 0.6885076 | 171.5434 | 1.0397000 | 0.0027866 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7138479 | 134.74453 | 0.6777846 | 159.4059 | 0.8979518 | 0.0083545 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.6640075 | 131.16986 | 0.6304622 | 156.3597 | 0.7900686 | 0.0353577 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.0120857 | 279.69030 | 0.9609556 | 138.3534 | 1.2878587 | 0.0353577 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.6977292 | 122.67649 | 0.6624802 | 154.1618 | 0.8788179 | 0.0063660 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6496175 | 256.09787 | 0.5951118 | 113.3870 | 0.8181341 | 0.2255603 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.7108957 | 249.93667 | 0.6512484 | 117.7478 | 0.8899945 | 0.0001932 |
| healthyverse  |         4 | EARTH       | Test  | 0.7611258 | 365.76333 | 0.6972640 | 108.5100 | 0.9625722 | 0.0001932 |
| healthyverse  |         5 | NNAR        | Test  | 0.7539695 | 159.31613 | 0.6907081 | 141.6879 | 1.0186384 | 0.0418064 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6236401 | 111.60775 | 0.5650732 | 123.6504 | 0.8279147 | 0.3887749 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7347214 | 112.74740 | 0.6657227 | 149.4336 | 0.9715095 | 0.0921591 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.9822790 | 269.44429 | 0.8900319 | 138.3816 | 1.2300530 | 0.0921591 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7729286 | 160.50202 | 0.7003418 | 143.5365 | 1.0209546 | 0.0005937 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.7291830 | 113.91398 | 0.6888748 | 185.3722 | 0.8908877 | 0.0285057 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7001469 | 105.44504 | 0.6614438 | 159.1788 | 0.8746433 | 0.0576410 |
| TidyDensity   |         4 | EARTH       | Test  | 0.8392321 | 190.65185 | 0.7928405 | 162.8354 | 1.0120849 | 0.0576410 |
| TidyDensity   |         5 | NNAR        | Test  | 0.7683552 | 199.55561 | 0.7258817 | 142.6625 | 0.9355776 | 0.0130105 |

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
    ## 1 healthyR              4 EARTH      Test  0.591  111. 0.645  164. 0.763 0.00207
    ## 2 healthyR.data         3 LM         Test  0.823  182. 0.655  124. 0.954 0.00703
    ## 3 healthyR.ts           3 LM         Test  0.664  131. 0.630  156. 0.790 0.0354 
    ## 4 healthyverse          1 ARIMA      Test  0.650  256. 0.595  113. 0.818 0.226  
    ## 5 healthyR.ai           1 ARIMA      Test  0.624  112. 0.565  124. 0.828 0.389  
    ## 6 TidyDensity           3 LM         Test  0.700  105. 0.661  159. 0.875 0.0576 
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
    ## 5 healthyR.ai   <tibble [422 × 2]> <tibble>     <split [394|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [270 × 2]> <tibble>     <split [242|28]> <mdl_time_tbl> 
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
