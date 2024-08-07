Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
07 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 109,915
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

The last day in the data set is 2024-08-05 22:18:28, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is 37.29 hours
old. Consider updating the cran log file from the package-downloads
project.

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 109915        |
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
| r_version     |     76933 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     76933 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     76933 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9457 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-05 | 2023-01-17 |     1352 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |     p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|--------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1182191.01 | 1562717.07 | 355 | 14701.0 | 289680 | 2373577 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10338.89 |   18090.11 |   1 |   307.5 |   3081 |   11494 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-05 22:18:28 | 2023-01-17 20:59:34 |    66526 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 9M 9S |       60 |

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
    ## 1 healthyR.data <tibble [1,323 × 2]> <tibble [28 × 2]> <split [1295|28]>
    ## 2 healthyR      <tibble [1,315 × 2]> <tibble [28 × 2]> <split [1287|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,261 × 2]> <tibble [28 × 2]> <split [1233|28]>
    ## 5 healthyverse  <tibble [1,232 × 2]> <tibble [28 × 2]> <split [1204|28]>
    ## 6 healthyR.ai   <tibble [1,058 × 2]> <tibble [28 × 2]> <split [1030|28]>
    ## 7 TidyDensity   <tibble [912 × 2]>   <tibble [28 × 2]> <split [884|28]> 
    ## 8 tidyAML       <tibble [528 × 2]>   <tibble [28 × 2]> <split [500|28]>

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

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|---------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.7773501 | 192.48325 | 0.6305187 | 147.2215 | 1.0236479 | 0.0872619 |
| healthyR.data |         2 | LM          | Test  | 0.8193365 | 345.13291 | 0.6645743 | 142.5892 | 0.9755599 | 0.0002095 |
| healthyR.data |         3 | EARTH       | Test  | 0.9590944 | 434.40379 | 0.7779338 | 138.1763 | 1.2336611 | 0.0002095 |
| healthyR.data |         4 | NNAR        | Test  | 0.8383212 | 153.23064 | 0.6799731 | 161.9606 | 1.0568905 | 0.0219055 |
| healthyR      |         1 | ARIMA       | Test  | 0.6710313 | 134.34673 | 0.7421227 | 147.6634 | 0.8409941 | 0.0743588 |
| healthyR      |         2 | LM          | Test  | 0.7094679 | 126.91630 | 0.7846314 | 157.8814 | 0.9189713 | 0.0734631 |
| healthyR      |         3 | EARTH       | Test  | 1.4721313 | 774.65506 | 1.6280940 | 138.8072 | 1.7109799 | 0.0734631 |
| healthyR      |         4 | NNAR        | Test  | 0.6458521 | 131.55140 | 0.7142759 | 145.6981 | 0.8502583 | 0.0790273 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0711708 | 156.13494 | 0.7746449 | 129.0993 | 1.3864869 | 0.0273477 |
| healthyR.ts   |         2 | LM          | Test  | 1.1120983 | 184.46737 | 0.8042427 | 158.8080 | 1.2359473 | 0.0533997 |
| healthyR.ts   |         3 | EARTH       | Test  | 1.1138551 | 187.48697 | 0.8055131 | 158.0688 | 1.2369751 | 0.0533997 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.9719632 |  95.61344 | 0.7029003 | 171.8699 | 1.1610802 | 0.1443538 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5731614 | 159.35131 | 0.6811961 |  95.0047 | 0.7357935 | 0.5199945 |
| healthyverse  |         2 | LM          | Test  | 0.7602611 | 271.69351 | 0.9035620 | 100.6453 | 0.9332893 | 0.0336167 |
| healthyverse  |         3 | EARTH       | Test  | 0.7644402 | 210.44799 | 0.9085289 | 112.8525 | 0.9112878 | 0.0336167 |
| healthyverse  |         4 | NNAR        | Test  | 0.8096500 | 156.58898 | 0.9622603 | 134.4579 | 0.9555948 | 0.0109456 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.1094250 | 104.23235 | 0.8162558 | 154.7498 | 1.5095817 | 0.0001284 |
| healthyR.ai   |         2 | LM          | Test  | 1.1244604 | 106.02386 | 0.8273181 | 146.0789 | 1.5258570 | 0.0115201 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.1233273 | 105.65848 | 0.8264843 | 147.1776 | 1.5223561 | 0.0115201 |
| healthyR.ai   |         4 | NNAR        | Test  | 1.0595267 | 101.39073 | 0.7795432 | 151.3466 | 1.4380510 | 0.0845441 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6807538 | 206.53584 | 0.8229302 | 114.3626 | 0.8040054 | 0.1930131 |
| TidyDensity   |         2 | LM          | Test  | 0.7359271 | 243.83937 | 0.8896265 | 114.7257 | 0.8700722 | 0.0338158 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7012939 | 117.95558 | 0.8477602 | 162.1960 | 0.9202336 | 0.0338158 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6117318 |  99.42068 | 0.7394928 | 142.0951 | 0.7840643 | 0.2680497 |
| tidyAML       |         1 | ARIMA       | Test  | 0.8578256 | 141.23993 | 0.9741994 | 119.5196 | 1.0102359 | 0.0301128 |
| tidyAML       |         2 | LM          | Test  | 0.8120781 | 111.36247 | 0.9222457 | 119.2869 | 0.9934248 | 0.0525156 |
| tidyAML       |         3 | EARTH       | Test  | 1.0633231 | 258.72668 | 1.2075750 | 112.0349 | 1.3086771 | 0.0525156 |
| tidyAML       |         4 | NNAR        | Test  | 0.8948440 | 167.48728 | 1.0162398 | 114.4531 | 1.0686943 | 0.0000002 |

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
    ## 1 healthyR.da…         2 LM          Test  0.819 345.  0.665 143.  0.976 2.09e-4
    ## 2 healthyR             1 ARIMA       Test  0.671 134.  0.742 148.  0.841 7.44e-2
    ## 3 healthyR.ts          4 NNAR        Test  0.972  95.6 0.703 172.  1.16  1.44e-1
    ## 4 healthyverse         1 ARIMA       Test  0.573 159.  0.681  95.0 0.736 5.20e-1
    ## 5 healthyR.ai          4 NNAR        Test  1.06  101.  0.780 151.  1.44  8.45e-2
    ## 6 TidyDensity          4 NNAR        Test  0.612  99.4 0.739 142.  0.784 2.68e-1
    ## 7 tidyAML              2 LM          Test  0.812 111.  0.922 119.  0.993 5.25e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1295|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1287|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1233|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1204|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1030|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [884|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [500|28]>  <mdl_tm_t [1 × 5]>

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
