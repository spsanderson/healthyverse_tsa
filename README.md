Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
03 February, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 56,290
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

The last day in the data set is 2023-02-01 23:58:11, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -5132 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 56290         |
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
| r_version     |     38560 |          0.31 |   5 |   5 |     0 |       35 |          0 |
| r_arch        |     38560 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     38560 |          0.31 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       39 |          0 |
| country       |      4629 |          0.92 |   2 |   2 |     0 |      129 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-02-01 | 2022-02-01 |      801 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1349976.04 | 1698851.63 | 357 | 26198 | 322874 | 2731044 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9136.17 |   17292.45 |   1 |   120 |   2364 |    9641 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-02-01 23:58:11 | 2022-02-01 09:26:42 |    33458 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     43 |       60 |

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
    ## 5 healthyR.ai   <tibble [529 × 2]> <tibble [28 × 2]> <split [501|28]>
    ## 6 TidyDensity   <tibble [377 × 2]> <tibble [28 × 2]> <split [349|28]>

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
    ## 5 healthyR.ai   <tibble [529 × 2]> <tibble [28 × 2]> <split [501|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [377 × 2]> <tibble [28 × 2]> <split [349|28]> <mdl_tm_t>
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7229285 | 1075.1596 | 0.9279650 |  95.04692 | 0.9903450 | 0.0092275 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.6260595 |  478.3188 | 0.8036221 | 108.17704 | 0.8162399 | 0.0928180 |
| healthyR      |         4 | EARTH       | Test  | 0.7558081 | 1099.3591 | 0.9701699 |  98.10594 | 1.0187644 | 0.0928180 |
| healthyR      |         5 | NNAR        | Test  | 0.6864833 |  289.9596 | 0.8811833 | 129.69105 | 0.8721382 | 0.0000330 |
| healthyR.data |         1 | ARIMA       | Test  | 1.1104232 | 1308.3638 | 1.3600806 | 132.44245 | 1.2980910 | 0.0032417 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 1.0168755 | 1217.1299 | 1.2455005 | 130.79582 | 1.1859622 | 0.3655532 |
| healthyR.data |         4 | EARTH       | Test  | 1.2620391 | 1703.3390 | 1.5457845 | 130.17944 | 1.5135009 | 0.3655532 |
| healthyR.data |         5 | NNAR        | Test  | 0.7761234 |  273.8992 | 0.9506199 | 154.54294 | 0.9681272 | 0.0000739 |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0384291 | 1160.8891 | 1.1803611 | 149.42230 | 1.3796105 | 0.0310254 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8654901 |  234.5605 | 0.9837848 | 178.84282 | 1.1386034 | 0.0480148 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.6075624 | 3791.4978 | 1.8272832 | 163.77316 | 1.9179572 | 0.0480148 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8701729 |  227.4567 | 0.9891077 | 169.94732 | 1.1746642 | 0.0062191 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6384778 | 1815.5366 | 0.9271261 |  70.00303 | 0.8277398 | 0.0314636 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.5492346 | 1026.5355 | 0.7975370 |  73.63291 | 0.6226686 | 0.0293933 |
| healthyverse  |         4 | EARTH       | Test  | 0.7043770 | 2000.2192 | 1.0228174 |  72.93332 | 0.8866754 | 0.0293933 |
| healthyverse  |         5 | NNAR        | Test  | 0.7653612 |  652.2716 | 1.1113720 | 126.04661 | 0.9218322 | 0.0735508 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.2683458 | 3459.1406 | 1.3404288 | 151.13624 | 1.4850384 | 0.0083647 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8962946 |  580.0650 | 0.9472331 | 166.77647 | 1.0999955 | 0.0396882 |
| healthyR.ai   |         4 | EARTH       | Test  | 1.2221151 | 2812.4095 | 1.2915707 | 148.08011 | 1.4612752 | 0.0396882 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.9288603 |  191.0386 | 0.9816496 | 159.58291 | 1.1706408 | 0.1094040 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5958617 |  605.7840 | 0.7447125 | 157.26865 | 0.7375307 | 0.0013459 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.5678392 |  289.0113 | 0.7096898 | 170.42309 | 0.7151779 | 0.0066908 |
| TidyDensity   |         4 | EARTH       | Test  | 0.6684793 | 2376.4432 | 0.8354706 | 121.49453 | 0.8983012 | 0.0066908 |
| TidyDensity   |         5 | NNAR        | Test  | 0.5770553 |  655.8273 | 0.7212081 | 156.92842 | 0.7051637 | 0.0216112 |

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
    ## 1 healthyR              3 LM         Test  0.626  478. 0.804 108.  0.816 9.28e-2
    ## 2 healthyR.data         5 NNAR       Test  0.776  274. 0.951 155.  0.968 7.39e-5
    ## 3 healthyR.ts           3 LM         Test  0.865  235. 0.984 179.  1.14  4.80e-2
    ## 4 healthyverse          3 LM         Test  0.549 1027. 0.798  73.6 0.623 2.94e-2
    ## 5 healthyR.ai           3 LM         Test  0.896  580. 0.947 167.  1.10  3.97e-2
    ## 6 TidyDensity           5 NNAR       Test  0.577  656. 0.721 157.  0.705 2.16e-2
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
    ## 5 healthyR.ai   <tibble [529 × 2]> <tibble [28 × 2]> <split [501|28]> <mdl_tm_t>
    ## 6 TidyDensity   <tibble [377 × 2]> <tibble [28 × 2]> <split [349|28]> <mdl_tm_t>
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
