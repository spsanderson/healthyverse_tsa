Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
18 October, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 48,922
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

The last day in the data set is 2022-10-16 18:36:02, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -2534.63
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 48922         |
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
| r_version     |     33321 |          0.32 |   5 |   5 |     0 |       34 |          0 |
| r_arch        |     33321 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     33321 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       31 |          0 |
| country       |      3865 |          0.92 |   2 |   2 |     0 |      119 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-10-16 | 2021-12-08 |      693 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1374116.40 | 1739684.00 | 357 | 16873 | 293340.5 | 2909930 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8867.49 |   16844.37 |   1 |   153 |   2389.0 |    9286 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-10-16 18:36:02 | 2021-12-08 22:30:22 |    28758 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     25 |       60 |

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
    ## 5 healthyR.ai   <tibble [421 × 2]> <tibble [28 × 2]> <split [393|28]>
    ## 6 TidyDensity   <tibble [269 × 2]> <tibble [28 × 2]> <split [241|28]>

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
    ## 5 healthyR.ai   <tibble [421 × 2]> <tibble>     <split [393|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [269 × 2]> <tibble>     <split [241|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |     mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|---------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.5635756 | 128.5100 | 0.6823899 | 160.4348 | 0.7148251 | 0.0000006 |
| healthyR      |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.5446205 |  99.4010 | 0.6594386 | 191.3494 | 0.6960082 | 0.0410785 |
| healthyR      |         4 | EARTH       | Test  | 0.5432895 | 106.9180 | 0.6578271 | 177.2844 | 0.6926622 | 0.0410785 |
| healthyR      |         5 | NNAR        | Test  | 0.5822913 | 193.7649 | 0.7050513 | 126.1942 | 0.7754404 | 0.0057928 |
| healthyR.data |         1 | ARIMA       | Test  | 0.8187931 | 181.6581 | 0.7013291 | 120.6779 | 0.9442759 | 0.0180542 |
| healthyR.data |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.8178006 | 174.1473 | 0.7004790 | 122.4772 | 0.9408150 | 0.0070763 |
| healthyR.data |         4 | EARTH       | Test  | 0.8177470 | 191.8047 | 0.7004331 | 117.5310 | 0.9460404 | 0.0070763 |
| healthyR.data |         5 | NNAR        | Test  | 0.9012268 | 110.0698 | 0.7719369 | 159.2117 | 1.1235138 | 0.0011443 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6703433 | 132.9559 | 0.6635653 | 159.2496 | 0.8261401 | 0.0000834 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.6531980 | 135.1179 | 0.6465933 | 156.2238 | 0.7689869 | 0.0266148 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.8844265 | 250.3109 | 0.8754838 | 133.1101 | 1.1310409 | 0.0266148 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.6664010 | 137.7069 | 0.6596628 | 159.0006 | 0.8137490 | 0.0001101 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5819666 | 242.9344 | 0.5778683 | 107.9684 | 0.7020706 | 0.3327715 |
| healthyverse  |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.6673718 | 239.2684 | 0.6626720 | 114.8224 | 0.8064869 | 0.0065624 |
| healthyverse  |         4 | EARTH       | Test  | 0.6838858 | 337.8471 | 0.6790698 | 104.0803 | 0.8310724 | 0.0065624 |
| healthyverse  |         5 | NNAR        | Test  | 0.6953317 | 148.6726 | 0.6904350 | 140.1858 | 0.9310520 | 0.0113848 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7261508 | 144.7355 | 0.6896194 | 168.0838 | 0.9088030 | 0.0350812 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7084569 | 108.7681 | 0.6728157 | 150.2957 | 0.9306683 | 0.0806094 |
| healthyR.ai   |         4 | EARTH       | Test  | 1.0629291 | 337.4912 | 1.0094550 | 136.6325 | 1.2962591 | 0.0806094 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.6977601 | 136.4885 | 0.6626571 | 145.5638 | 0.9412954 | 0.0010557 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6792597 | 111.8390 | 0.6946359 | 181.7980 | 0.8401508 | 0.0110500 |
| TidyDensity   |         2 | NULL        | NA    |        NA |       NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6564129 | 108.6134 | 0.6712719 | 157.9807 | 0.8284139 | 0.0287385 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7697461 | 182.7916 | 0.7871706 | 156.0976 | 0.9403554 | 0.0287385 |
| TidyDensity   |         5 | NNAR        | Test  | 0.7470707 | 199.4738 | 0.7639819 | 144.0294 | 0.9269128 | 0.0000492 |

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
    ## 1 healthyR              4 EARTH      Test  0.543  107. 0.658  177. 0.693 0.0411 
    ## 2 healthyR.data         3 LM         Test  0.818  174. 0.700  122. 0.941 0.00708
    ## 3 healthyR.ts           3 LM         Test  0.653  135. 0.647  156. 0.769 0.0266 
    ## 4 healthyverse          1 ARIMA      Test  0.582  243. 0.578  108. 0.702 0.333  
    ## 5 healthyR.ai           1 ARIMA      Test  0.726  145. 0.690  168. 0.909 0.0351 
    ## 6 TidyDensity           3 LM         Test  0.656  109. 0.671  158. 0.828 0.0287 
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
    ## 5 healthyR.ai   <tibble [421 × 2]> <tibble>     <split [393|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [269 × 2]> <tibble>     <split [241|28]> <mdl_time_tbl> 
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
