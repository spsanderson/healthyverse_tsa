Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
20 May, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 67,177
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

The last day in the data set is 2023-05-18 19:53:17, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -7671.92
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 67177         |
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
| r_version     |     45885 |          0.32 |   5 |   5 |     0 |       37 |          0 |
| r_arch        |     45885 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     45885 |          0.32 |   7 |  15 |     0 |       14 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       43 |          0 |
| country       |      5592 |          0.92 |   2 |   2 |     0 |      137 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-05-18 | 2022-04-06 |      907 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1308090.71 | 1662901.78 | 357 | 24837 | 313255 | 2692073 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    9872.66 |   18089.04 |   1 |   132 |   2667 |   10889 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-05-18 19:53:17 | 2022-04-06 15:47:01 |    40289 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 56M 47S |       60 |

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
    ## 1 healthyR      <tibble [544 × 2]> <tibble [28 × 2]> <split [516|28]>
    ## 2 healthyR.ai   <tibble [543 × 2]> <tibble [28 × 2]> <split [515|28]>
    ## 3 healthyR.data <tibble [542 × 2]> <tibble [28 × 2]> <split [514|28]>
    ## 4 healthyR.ts   <tibble [537 × 2]> <tibble [28 × 2]> <split [509|28]>
    ## 5 healthyverse  <tibble [534 × 2]> <tibble [28 × 2]> <split [506|28]>
    ## 6 TidyDensity   <tibble [483 × 2]> <tibble [28 × 2]> <split [455|28]>
    ## 7 tidyAML       <tibble [92 × 2]>  <tibble [28 × 2]> <split [64|28]>

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
    ## 1 healthyR      <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [506|28]> <mdl_tm_t [5 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [455|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [64|28]>  <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |        mae |       mape |      mase |    smape |       rmse |       rsq |
|:--------------|----------:|:------------|:------|-----------:|-----------:|----------:|---------:|-----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  |  0.6924651 |  117.14447 | 0.6108136 | 112.5300 |  0.8770188 | 0.2138932 |
| healthyR      |         2 | NULL        | NA    |         NA |         NA |        NA |       NA |         NA |        NA |
| healthyR      |         3 | LM          | Test  |  0.7737373 |  107.14721 | 0.6825026 | 169.1789 |  0.9355277 | 0.0118303 |
| healthyR      |         4 | EARTH       | Test  |  1.0897452 |  260.82342 | 0.9612487 | 123.5543 |  1.2816374 | 0.0118303 |
| healthyR      |         5 | NNAR        | Test  |  0.8162590 |  122.59108 | 0.7200103 | 160.4691 |  0.9534780 | 0.0002413 |
| healthyR.ai   |         1 | ARIMA       | Test  |  0.6618483 |  189.14168 | 0.6352353 | 127.5028 |  0.8063183 | 0.1710381 |
| healthyR.ai   |         2 | NULL        | NA    |         NA |         NA |        NA |       NA |         NA |        NA |
| healthyR.ai   |         3 | LM          | Test  |  0.7148722 |  156.32437 | 0.6861271 | 152.2875 |  0.8749529 | 0.0055898 |
| healthyR.ai   |         4 | EARTH       | Test  |  1.3731460 | 1055.13812 | 1.3179316 | 128.8483 |  1.6282205 | 0.0055898 |
| healthyR.ai   |         5 | NNAR        | Test  |  0.6626346 |  184.09204 | 0.6359900 | 149.1064 |  0.8211705 | 0.1639644 |
| healthyR.data |         1 | ARIMA       | Test  |  0.9841675 |  148.29707 | 0.8239166 | 173.3143 |  1.2236329 | 0.0020806 |
| healthyR.data |         2 | NULL        | NA    |         NA |         NA |        NA |       NA |         NA |        NA |
| healthyR.data |         3 | LM          | Test  |  1.1617781 |  391.42654 | 0.9726071 | 152.2103 |  1.4459413 | 0.0924897 |
| healthyR.data |         4 | EARTH       | Test  |  0.9520729 |  199.96197 | 0.7970479 | 159.0856 |  1.1657459 | 0.0924897 |
| healthyR.data |         5 | NNAR        | Test  |  0.9804219 |  188.39961 | 0.8207809 | 158.7596 |  1.2463764 | 0.0514359 |
| healthyR.ts   |         1 | ARIMA       | Test  |  0.6892160 |   88.84712 | 0.5418015 | 113.6106 |  0.8768869 | 0.3835038 |
| healthyR.ts   |         2 | NULL        | NA    |         NA |         NA |        NA |       NA |         NA |        NA |
| healthyR.ts   |         3 | LM          | Test  |  0.8073354 |  101.21873 | 0.6346566 | 117.6911 |  1.0783395 | 0.0016954 |
| healthyR.ts   |         4 | EARTH       | Test  | 10.4661559 | 2340.67187 | 8.2275783 | 171.8899 | 11.3748728 | 0.0016954 |
| healthyR.ts   |         5 | NNAR        | Test  |  0.8163005 |   92.84389 | 0.6417042 | 132.9942 |  1.1010824 | 0.0068019 |
| healthyverse  |         1 | ARIMA       | Test  |  0.6470918 |  163.49844 | 0.6762035 | 158.0434 |  0.7831683 | 0.0017775 |
| healthyverse  |         2 | NULL        | NA    |         NA |         NA |        NA |       NA |         NA |        NA |
| healthyverse  |         3 | LM          | Test  |  0.7761081 |  387.24409 | 0.8110240 | 132.2948 |  0.9250525 | 0.0150829 |
| healthyverse  |         4 | EARTH       | Test  |  5.6459936 | 4029.62983 | 5.8999986 | 182.5860 |  6.0683465 | 0.0150829 |
| healthyverse  |         5 | NNAR        | Test  |  0.6418367 |  199.82733 | 0.6707120 | 144.9866 |  0.7484146 | 0.0764203 |
| TidyDensity   |         1 | ARIMA       | Test  |  0.6613942 |  134.83102 | 0.7658457 | 127.6870 |  0.7771532 | 0.0963164 |
| TidyDensity   |         2 | NULL        | NA    |         NA |         NA |        NA |       NA |         NA |        NA |
| TidyDensity   |         3 | LM          | Test  |  0.7275968 |  120.37857 | 0.8425034 | 144.7041 |  0.8605960 | 0.0414195 |
| TidyDensity   |         4 | EARTH       | Test  |  4.7971242 | 2378.65025 | 5.5547159 | 149.4032 |  5.3061538 | 0.0414195 |
| TidyDensity   |         5 | NNAR        | Test  |  0.7581842 |  148.04313 | 0.8779214 | 132.3554 |  0.9477998 | 0.0218482 |
| tidyAML       |         1 | ARIMA       | Test  |  0.6385722 |  170.92516 | 0.8454296 | 115.6647 |  0.8314788 |        NA |
| tidyAML       |         2 | NULL        | NA    |         NA |         NA |        NA |       NA |         NA |        NA |
| tidyAML       |         3 | LM          | Test  |  1.3114565 |  300.09987 | 1.7362863 | 168.0840 |  1.5199015 | 0.3065678 |
| tidyAML       |         4 | EARTH       | Test  |  3.5758687 | 1295.89189 | 4.7342262 | 146.6562 |  4.1381357 | 0.3065678 |
| tidyAML       |         5 | NNAR        | Test  |  1.0265945 |  230.57773 | 1.3591468 | 159.0747 |  1.3341053 | 0.0345241 |

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
    ## 1 healthyR             1 ARIMA       Test  0.692 117.  0.611  113. 0.877  0.214 
    ## 2 healthyR.ai          1 ARIMA       Test  0.662 189.  0.635  128. 0.806  0.171 
    ## 3 healthyR.da…         4 EARTH       Test  0.952 200.  0.797  159. 1.17   0.0925
    ## 4 healthyR.ts          1 ARIMA       Test  0.689  88.8 0.542  114. 0.877  0.384 
    ## 5 healthyverse         5 NNAR        Test  0.642 200.  0.671  145. 0.748  0.0764
    ## 6 TidyDensity          1 ARIMA       Test  0.661 135.  0.766  128. 0.777  0.0963
    ## 7 tidyAML              1 ARIMA       Test  0.639 171.  0.845  116. 0.831 NA

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
    ## 1 healthyR      <tibble>     <tibble>     <split [516|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR.ai   <tibble>     <tibble>     <split [515|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.data <tibble>     <tibble>     <split [514|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.ts   <tibble>     <tibble>     <split [509|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyverse  <tibble>     <tibble>     <split [506|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [455|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [64|28]>  <mdl_tm_t [1 × 5]>

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
