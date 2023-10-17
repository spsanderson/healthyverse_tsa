Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
16 October, 2023

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 79,287
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

The last day in the data set is 2023-10-14 23:59:41, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is
-1.125202^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 79287         |
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
| r_version     |     54758 |          0.31 |   5 |   5 |     0 |       38 |          0 |
| r_arch        |     54758 |          0.31 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     54758 |          0.31 |   7 |  15 |     0 |       15 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |   6 |     0 |       47 |          0 |
| country       |      6526 |          0.92 |   2 |   2 |     0 |      143 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2023-10-14 | 2022-06-17 |     1056 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1268152.05 | 1629175.2 | 357 | 16730 | 313019 | 2445303 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10026.51 |   17890.1 |   1 |   168 |   2846 |   11184 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2023-10-14 23:59:41 | 2022-06-17 11:33:38 |    47607 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 56M 18S |       60 |

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
    ## 1 TidyDensity   <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 2 healthyR      <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 3 healthyR.ai   <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 4 healthyR.data <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 5 healthyR.ts   <tibble [541 × 2]> <tibble [28 × 2]> <split [513|28]>
    ## 6 healthyverse  <tibble [536 × 2]> <tibble [28 × 2]> <split [508|28]>
    ## 7 tidyAML       <tibble [241 × 2]> <tibble [28 × 2]> <split [213|28]>

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
    ## 1 TidyDensity   <tibble>     <tibble>     <split [521|28]> <mdl_tm_t [5 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [5 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [5 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [5 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [513|28]> <mdl_tm_t [5 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [508|28]> <mdl_tm_t [5 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [213|28]> <mdl_tm_t [5 × 5]>

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| TidyDensity   |         1 | ARIMA       | Test  | 0.6996352 | 159.55870 | 0.6589145 | 127.96259 | 0.8901582 | 0.1163226 |
| TidyDensity   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.7472509 | 199.20429 | 0.7037589 | 125.38863 | 0.9359081 | 0.0013025 |
| TidyDensity   |         4 | EARTH       | Test  | 0.7941836 | 244.45870 | 0.7479599 | 124.79387 | 0.9816286 | 0.0013025 |
| TidyDensity   |         5 | NNAR        | Test  | 0.6782699 | 108.21961 | 0.6387927 | 146.57024 | 0.8666784 | 0.1258069 |
| healthyR      |         1 | ARIMA       | Test  | 0.7562346 | 106.66726 | 0.7174454 | 181.95926 | 0.9697970 | 0.0844250 |
| healthyR      |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.7196730 | 102.33765 | 0.6827592 | 164.70490 | 0.9292158 | 0.0020978 |
| healthyR      |         4 | EARTH       | Test  | 0.7365299 | 150.22108 | 0.6987514 | 134.97034 | 0.9589544 | 0.0020978 |
| healthyR      |         5 | NNAR        | Test  | 0.7298894 |  97.04709 | 0.6924515 | 170.39713 | 0.9410475 | 0.0893147 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.9100452 | 104.60451 | 0.6859971 | 176.16411 | 1.1433766 | 0.0002070 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8960452 | 104.53117 | 0.6754438 | 174.90790 | 1.1158359 | 0.0050886 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.8713689 | 127.13964 | 0.6568427 | 150.95075 | 1.0940075 | 0.0050886 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8842706 | 118.06131 | 0.6665681 | 164.76326 | 1.1044270 | 0.0027776 |
| healthyR.data |         1 | ARIMA       | Test  | 0.9822336 | 113.62527 | 0.7424476 | 171.07432 | 1.1280684 | 0.0948895 |
| healthyR.data |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.9080296 |  98.72347 | 0.6863585 | 188.84302 | 1.0565229 | 0.0142762 |
| healthyR.data |         4 | EARTH       | Test  | 0.8487463 | 102.56158 | 0.6415477 | 131.64683 | 0.9848174 | 0.0142762 |
| healthyR.data |         5 | NNAR        | Test  | 0.8731912 |  93.75853 | 0.6600250 | 169.16460 | 1.0262029 | 0.1024436 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.9943118 | 156.16519 | 0.6061391 | 135.49789 | 1.3245761 | 0.1420512 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 1.0284294 | 194.93214 | 0.6269375 | 126.81686 | 1.3693057 | 0.0170677 |
| healthyR.ts   |         4 | EARTH       | Test  | 1.0175914 | 176.51850 | 0.6203306 | 131.89466 | 1.3516345 | 0.0170677 |
| healthyR.ts   |         5 | NNAR        | Test  | 1.0390316 | 152.71685 | 0.6334006 | 149.62458 | 1.3933743 | 0.2676658 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5144827 | 182.95311 | 0.6272559 |  60.86528 | 0.7114786 | 0.0904871 |
| healthyverse  |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.5718858 | 148.04032 | 0.6972416 |  68.96549 | 0.7726217 | 0.0054719 |
| healthyverse  |         4 | EARTH       | Test  | 0.5423967 | 184.73889 | 0.6612885 |  62.75984 | 0.7346568 | 0.0054719 |
| healthyverse  |         5 | NNAR        | Test  | 0.7028073 | 109.38564 | 0.8568607 |  98.73325 | 0.8843239 | 0.0512431 |
| tidyAML       |         1 | ARIMA       | Test  | 0.4701321 | 156.86526 | 0.8608139 |  72.32799 | 0.6174874 | 0.1110404 |
| tidyAML       |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| tidyAML       |         3 | LM          | Test  | 0.4587627 | 255.23901 | 0.8399964 |  62.89040 | 0.5986319 | 0.0003685 |
| tidyAML       |         4 | EARTH       | Test  | 0.4987624 | 127.76062 | 0.9132360 |  81.25249 | 0.6316061 | 0.0003685 |
| tidyAML       |         5 | NNAR        | Test  | 0.4859203 | 189.91861 | 0.8897221 |  72.86493 | 0.6414565 | 0.3854736 |

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
    ## 1 TidyDensity          5 NNAR        Test  0.678  108. 0.639 147.  0.867 1.26e-1
    ## 2 healthyR             3 LM          Test  0.720  102. 0.683 165.  0.929 2.10e-3
    ## 3 healthyR.ai          4 EARTH       Test  0.871  127. 0.657 151.  1.09  5.09e-3
    ## 4 healthyR.da…         4 EARTH       Test  0.849  103. 0.642 132.  0.985 1.43e-2
    ## 5 healthyR.ts          1 ARIMA       Test  0.994  156. 0.606 135.  1.32  1.42e-1
    ## 6 healthyverse         1 ARIMA       Test  0.514  183. 0.627  60.9 0.711 9.05e-2
    ## 7 tidyAML              3 LM          Test  0.459  255. 0.840  62.9 0.599 3.69e-4

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
    ## 1 TidyDensity   <tibble>     <tibble>     <split [521|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ai   <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyR.data <tibble>     <tibble>     <split [518|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ts   <tibble>     <tibble>     <split [513|28]> <mdl_tm_t [1 × 5]>
    ## 6 healthyverse  <tibble>     <tibble>     <split [508|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [213|28]> <mdl_tm_t [1 × 5]>

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
