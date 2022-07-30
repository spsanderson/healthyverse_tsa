Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
30 July, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 43,194
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

The last day in the data set is 2022-07-28 22:20:48, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -618.38
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 43194         |
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
| r_version     |     29094 |          0.33 |   5 |   5 |     0 |       32 |          0 |
| r_arch        |     29094 |          0.33 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     29094 |          0.33 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       26 |          0 |
| country       |      3444 |          0.92 |   2 |   2 |     0 |      116 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-07-28 | 2021-11-10 |      613 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1410058.50 | 1779401.71 | 357 | 16873 | 289852 | 2970523 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8799.11 |   16801.81 |   1 |   165 |   2580 |    8956 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-07-28 22:20:48 | 2021-11-10 12:13:49 |    25308 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   28.5 |       60 |

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
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 4 healthyverse  <tibble [526 × 2]> <tibble [28 × 2]> <split [498|28]>
    ## 5 healthyR.ai   <tibble [341 × 2]> <tibble [28 × 2]> <split [313|28]>
    ## 6 TidyDensity   <tibble [189 × 2]> <tibble [28 × 2]> <split [161|28]>

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
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [526 × 2]> <tibble>     <split [498|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [341 × 2]> <tibble>     <split [313|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [189 × 2]> <tibble>     <split [161|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.7873048 | 106.30310 | 0.6478611 | 165.1819 | 0.9928333 | 0.1724978 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8407520 | 103.11937 | 0.6918420 | 181.9260 | 1.0540313 | 0.1512247 |
| healthyR      |         4 | EARTH       | Test  | 0.8175469 |  99.62771 | 0.6727469 | 185.5213 | 1.0277130 | 0.1512247 |
| healthyR      |         5 | NNAR        | Test  | 0.8729761 | 164.21328 | 0.7183587 | 144.8614 | 1.1125283 | 0.0021426 |
| healthyR.data |         1 | ARIMA       | Test  | 0.8409596 |  98.21546 | 0.6469156 | 141.6761 | 0.9666143 | 0.0105349 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.7899824 |  91.19029 | 0.6077009 | 123.3998 | 0.9360540 | 0.0043677 |
| healthyR.data |         4 | EARTH       | Test  | 0.7821663 |  90.97836 | 0.6016883 | 119.9021 | 0.9313908 | 0.0043677 |
| healthyR.data |         5 | NNAR        | Test  | 0.9313329 | 106.66683 | 0.7164360 | 170.4519 | 1.0876337 | 0.0180689 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.7981022 | 110.41081 | 0.6299065 | 161.3377 | 1.0126073 | 0.0189560 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.8062076 | 102.55979 | 0.6363037 | 181.7942 | 1.0341892 | 0.1046865 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.7798015 | 123.27680 | 0.6154626 | 133.6255 | 1.0072705 | 0.1046865 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.8351110 | 138.29084 | 0.6591159 | 155.1519 | 1.0599773 | 0.0478545 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7095250 | 146.13166 | 0.6251392 | 134.1013 | 0.8726681 | 0.0027872 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.6748023 | 181.85575 | 0.5945462 | 113.5291 | 0.8354116 | 0.0128278 |
| healthyverse  |         4 | EARTH       | Test  | 0.6808568 | 164.62619 | 0.5998806 | 118.9353 | 0.8421395 | 0.0128278 |
| healthyverse  |         5 | NNAR        | Test  | 0.7699231 | 147.97632 | 0.6783540 | 142.2463 | 0.9343490 | 0.0008954 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6597030 | 102.19554 | 0.6796219 | 161.5199 | 0.8784535 | 0.0217339 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.7326085 | 146.06504 | 0.7547287 | 166.7290 | 0.9459650 | 0.0086888 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.6763697 |  95.20224 | 0.6967918 | 171.0883 | 0.8920859 | 0.0086888 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.7650900 | 173.42239 | 0.7881909 | 150.7522 | 1.0125270 | 0.0017063 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.7801328 | 105.36105 | 0.6686358 | 195.7232 | 0.9213247 | 0.0757885 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7429926 | 100.63248 | 0.6368037 | 159.9775 | 0.8804184 | 0.1418104 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7930109 | 107.92487 | 0.6796733 | 162.0272 | 0.9697583 | 0.1418104 |
| TidyDensity   |         5 | NNAR        | Test  | 0.8039816 | 147.27252 | 0.6890762 | 147.4677 | 0.9595977 | 0.0233683 |

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
    ## 1 healthyR              1 ARIMA      Test  0.787 106.  0.648  165. 0.993 0.172  
    ## 2 healthyR.data         4 EARTH      Test  0.782  91.0 0.602  120. 0.931 0.00437
    ## 3 healthyR.ts           4 EARTH      Test  0.780 123.  0.615  134. 1.01  0.105  
    ## 4 healthyverse          3 LM         Test  0.675 182.  0.595  114. 0.835 0.0128 
    ## 5 healthyR.ai           1 ARIMA      Test  0.660 102.  0.680  162. 0.878 0.0217 
    ## 6 TidyDensity           3 LM         Test  0.743 101.  0.637  160. 0.880 0.142  
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
    ## 3 healthyR.ts   <tibble [544 × 2]> <tibble>     <split [516|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [526 × 2]> <tibble>     <split [498|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [341 × 2]> <tibble>     <split [313|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [189 × 2]> <tibble>     <split [161|28]> <mdl_time_tbl> 
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
