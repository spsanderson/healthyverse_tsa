Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
23 July, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 42,445
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

The last day in the data set is 2022-07-21 22:59:00, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -451.01
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 42445         |
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
| r_version     |     28561 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     28561 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     28561 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       26 |          0 |
| country       |      3381 |          0.92 |   2 |   2 |     0 |      115 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-21 | 2021-11-09 |      606 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1414204.56 | 1786344.61 | 357 | 16873 | 289681 | 2971077 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8771.62 |   16757.59 |   1 |   174 |   2595 |    8949 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-21 22:59:00 | 2021-11-09 04:16:48 |    24818 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 25M 26S |       60 |

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
    ## 1 healthyR      <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 2 healthyR.data <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 3 healthyR.ts   <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 4 healthyverse  <tibble [519 × 2]> <tibble [28 × 2]> <split [491|28]>
    ## 5 healthyR.ai   <tibble [334 × 2]> <tibble [28 × 2]> <split [306|28]>
    ## 6 TidyDensity   <tibble [182 × 2]> <tibble [28 × 2]> <split [154|28]>

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
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [519 × 2]> <tibble>     <split [491|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [334 × 2]> <tibble>     <split [306|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [182 × 2]> <tibble>     <split [154|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.6640077 | 100.16637 | 0.6839733 | 146.4769 | 0.8248532 | 0.2185913 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7302779 |  98.32323 | 0.7522361 | 181.9837 | 0.9026565 | 0.1059366 |
| healthyR      |         4 | EARTH       | Test  | 1.1670074 | 389.21208 | 1.2020974 | 144.8764 | 1.4319044 | 0.1059366 |
| healthyR      |         5 | NNAR        | Test  | 0.8264831 | 180.21547 | 0.8513341 | 148.1731 | 1.0503332 | 0.0021688 |
| healthyR.data |         1 | ARIMA       | Test  | 0.7389546 |  97.89319 | 0.6271023 | 119.3760 | 0.9072417 | 0.0020349 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.7501668 |  95.51597 | 0.6366174 | 126.5550 | 0.9045761 | 0.0018414 |
| healthyR.data |         4 | EARTH       | Test  | 0.7438587 |  98.36746 | 0.6312641 | 122.1980 | 0.8976537 | 0.0018414 |
| healthyR.data |         5 | NNAR        | Test  | 0.8741032 | 105.41710 | 0.7417941 | 138.6625 | 1.1342265 | 0.0141221 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7216278 |  94.76441 | 0.6658523 | 160.4314 | 0.8816383 | 0.1394766 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7568764 |  97.99985 | 0.6983764 | 186.4802 | 0.9168661 | 0.0090653 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.7114887 |  86.10248 | 0.6564968 | 130.2212 | 0.8934441 | 0.0090653 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8328717 | 126.43113 | 0.7684979 | 160.9051 | 0.9996926 | 0.0458887 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6414256 | 188.08405 | 0.6177777 | 126.1948 | 0.8014684 | 0.0006638 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.6488190 | 226.98018 | 0.6248985 | 122.2520 | 0.7928867 | 0.0069030 |
| healthyverse  |         4 | EARTH       | Test  | 0.7331992 | 362.88242 | 0.7061678 | 116.6702 | 0.8500677 | 0.0069030 |
| healthyverse  |         5 | NNAR        | Test  | 0.6965032 | 175.44372 | 0.6708247 | 144.2838 | 0.8592068 | 0.0138789 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6795685 | 156.40984 | 0.7101372 | 163.1827 | 0.8673120 | 0.2446156 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7387422 | 135.92686 | 0.7719726 | 170.4085 | 0.9366965 | 0.0537167 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.7727059 | 158.58614 | 0.8074641 | 167.9399 | 0.9686969 | 0.0537167 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8483301 | 241.33555 | 0.8864900 | 145.5357 | 1.0859058 | 0.0035879 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6651162 |  94.55566 | 0.6701142 | 176.4794 | 0.7761265 | 0.0203045 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.6712782 |  95.30095 | 0.6763225 | 170.0992 | 0.7731009 | 0.0000048 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7476422 | 122.85625 | 0.7532604 | 148.6483 | 0.8845806 | 0.0000048 |
| TidyDensity   |         5 | NNAR        | Test  | 0.6446278 | 114.27882 | 0.6494719 | 132.3615 | 0.7798742 | 0.1670800 |

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
    ## 1 healthyR              1 ARIMA      Test  0.664 100.  0.684  146. 0.825 2.19e-1
    ## 2 healthyR.data         4 EARTH      Test  0.744  98.4 0.631  122. 0.898 1.84e-3
    ## 3 healthyR.ts           1 ARIMA      Test  0.722  94.8 0.666  160. 0.882 1.39e-1
    ## 4 healthyverse          3 LM         Test  0.649 227.  0.625  122. 0.793 6.90e-3
    ## 5 healthyR.ai           1 ARIMA      Test  0.680 156.  0.710  163. 0.867 2.45e-1
    ## 6 TidyDensity           3 LM         Test  0.671  95.3 0.676  170. 0.773 4.78e-6
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
    ## 1 healthyR      <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [543 × 2]> <tibble>     <split [515|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [519 × 2]> <tibble>     <split [491|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [334 × 2]> <tibble>     <split [306|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [182 × 2]> <tibble>     <split [154|28]> <mdl_time_tbl> 
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
