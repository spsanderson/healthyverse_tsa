Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
20 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 111,401
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

The last day in the data set is 2024-08-18 23:33:56, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -275.97
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 111401        |
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
| r_version     |     78098 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78098 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78098 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9546 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-18 | 2023-01-26 |     1365 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1180192.88 | 1560369.51 | 355 | 14701 | 280112 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10315.52 |   18030.34 |   1 |   317 |   3090 |   11494 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-18 23:33:56 | 2023-01-26 18:20:24 |    67421 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 22S |       60 |

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

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,336 × 2]> <tibble [28 × 2]> <split [1308|28]>
    ## 2 healthyR      <tibble [1,328 × 2]> <tibble [28 × 2]> <split [1300|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,274 × 2]> <tibble [28 × 2]> <split [1246|28]>
    ## 5 healthyverse  <tibble [1,245 × 2]> <tibble [28 × 2]> <split [1217|28]>
    ## 6 healthyR.ai   <tibble [1,071 × 2]> <tibble [28 × 2]> <split [1043|28]>
    ## 7 TidyDensity   <tibble [925 × 2]>   <tibble [28 × 2]> <split [897|28]> 
    ## 8 tidyAML       <tibble [541 × 2]>   <tibble [28 × 2]> <split [513|28]>

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
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl <- nested_modeltime_tbl[!is.na(nested_modeltime_tbl$package),]
```

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.5523186 |  130.62340 | 0.6486277 | 149.07120 | 0.7395564 | 0.0927264 |
| healthyR.data |         2 | LM          | Test  | 0.7868402 |  375.23398 | 0.9240433 | 162.00716 | 0.9236691 | 0.0058548 |
| healthyR.data |         3 | EARTH       | Test  | 1.7231723 | 1085.35398 | 2.0236457 | 166.35428 | 1.9158907 | 0.0058548 |
| healthyR.data |         4 | NNAR        | Test  | 0.5309506 |  150.49486 | 0.6235337 | 129.43077 | 0.7467623 | 0.0070770 |
| healthyR      |         1 | ARIMA       | Test  | 0.6778630 |   87.55943 | 0.8558969 | 142.15568 | 0.8571739 | 0.0807014 |
| healthyR      |         2 | LM          | Test  | 0.7650251 |  103.81198 | 0.9659512 | 166.30788 | 0.9767791 | 0.0334620 |
| healthyR      |         3 | EARTH       | Test  | 0.6826799 |  102.51651 | 0.8619790 | 133.79885 | 0.8547198 | 0.0334620 |
| healthyR      |         4 | NNAR        | Test  | 0.7386628 |  104.50710 | 0.9326652 | 162.47477 | 0.9342626 | 0.0689285 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 2.1284088 |  991.09696 | 1.6800224 | 167.32510 | 2.3752234 | 0.0171570 |
| healthyR.ts   |         2 | LM          | Test  | 0.8447117 |  233.54775 | 0.6667584 | 127.19629 | 1.0710809 | 0.0362516 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.8446625 |  238.90130 | 0.6667196 | 126.45760 | 1.0700984 | 0.0362516 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8732382 |  125.12507 | 0.6892754 | 175.59796 | 1.0912429 | 0.0617849 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7213509 |  501.01113 | 1.0304762 | 113.41620 | 0.8327843 | 0.3184118 |
| healthyverse  |         2 | LM          | Test  | 0.7722658 |  577.19401 | 1.1032102 | 111.66248 | 0.9058082 | 0.0544209 |
| healthyverse  |         3 | EARTH       | Test  | 0.7575542 |  420.87440 | 1.0821941 | 121.79709 | 0.8855909 | 0.0544209 |
| healthyverse  |         4 | NNAR        | Test  | 0.7411559 |  275.79026 | 1.0587685 | 133.01173 | 0.9087674 | 0.0280029 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8700567 |  109.62364 | 0.8635673 | 175.21039 | 1.0840846 | 0.0772733 |
| healthyR.ai   |         2 | LM          | Test  | 0.8388549 |  140.74919 | 0.8325981 | 138.71491 | 1.0536433 | 0.0005049 |
| healthyR.ai   |         3 | EARTH       | Test  | 6.3489132 | 2690.54467 | 6.3015589 | 170.46494 | 6.8425369 | 0.0005049 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.9547327 |  133.95231 | 0.9476117 | 160.25590 | 1.2111118 | 0.0000488 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5884596 |  251.36308 | 0.7058363 |  96.36839 | 0.6970149 | 0.5184902 |
| TidyDensity   |         2 | LM          | Test  | 0.6459248 |  271.24260 | 0.7747638 |  97.34173 | 0.7894694 | 0.0051491 |
| TidyDensity   |         3 | EARTH       | Test  | 0.8362778 |  178.44910 | 1.0030854 | 176.10706 | 1.0189386 | 0.0051491 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6353760 |   98.95438 | 0.7621109 | 137.41338 | 0.7762074 | 0.1982875 |
| tidyAML       |         1 | ARIMA       | Test  | 0.7169321 |   97.25317 | 0.8449954 | 125.81285 | 0.8660562 | 0.3198727 |
| tidyAML       |         2 | LM          | Test  | 0.8125057 |  125.51909 | 0.9576410 | 126.92027 | 0.9636364 | 0.0553453 |
| tidyAML       |         3 | EARTH       | Test  | 2.0394206 |  423.94235 | 2.4037157 | 164.78588 | 2.3020459 | 0.0553453 |
| tidyAML       |         4 | NNAR        | Test  | 0.7451172 |  135.55006 | 0.8782151 | 112.76900 | 0.9029689 | 0.1751615 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.552 131.  0.649 149.  0.740 9.27e-2
    ## 2 healthyR             3 EARTH       Test  0.683 103.  0.862 134.  0.855 3.35e-2
    ## 3 healthyR.ts          3 EARTH       Test  0.845 239.  0.667 126.  1.07  3.63e-2
    ## 4 healthyverse         1 ARIMA       Test  0.721 501.  1.03  113.  0.833 3.18e-1
    ## 5 healthyR.ai          2 LM          Test  0.839 141.  0.833 139.  1.05  5.05e-4
    ## 6 TidyDensity          1 ARIMA       Test  0.588 251.  0.706  96.4 0.697 5.18e-1
    ## 7 tidyAML              1 ARIMA       Test  0.717  97.3 0.845 126.  0.866 3.20e-1

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
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1308|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1300|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1246|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1217|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1043|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [897|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [513|28]>  <mdl_tm_t [1 × 5]>

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
