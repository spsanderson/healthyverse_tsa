Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
23 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 111,723
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

The last day in the data set is 2024-08-21 23:59:14, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -348.39
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 111723        |
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
| r_version     |     78351 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78351 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78351 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9560 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-21 | 2023-01-29 |     1368 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1180504.56 | 1560366.81 | 355 | 14701 | 286925 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10309.38 |   18020.89 |   1 |   317 |   3077 |   11485 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-21 23:59:14 | 2023-01-29 04:56:03 |    67636 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |    median | n_unique |
|:--------------|----------:|--------------:|----:|----:|----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 4S |       60 |

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
    ## 1 healthyR.data <tibble [1,339 × 2]> <tibble [28 × 2]> <split [1311|28]>
    ## 2 healthyR      <tibble [1,331 × 2]> <tibble [28 × 2]> <split [1303|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,277 × 2]> <tibble [28 × 2]> <split [1249|28]>
    ## 5 healthyverse  <tibble [1,248 × 2]> <tibble [28 × 2]> <split [1220|28]>
    ## 6 healthyR.ai   <tibble [1,074 × 2]> <tibble [28 × 2]> <split [1046|28]>
    ## 7 TidyDensity   <tibble [928 × 2]>   <tibble [28 × 2]> <split [900|28]> 
    ## 8 tidyAML       <tibble [544 × 2]>   <tibble [28 × 2]> <split [516|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.4781265 |  110.78325 | 0.6189767 | 124.54464 | 0.6596848 | 0.2381722 |
| healthyR.data |         2 | LM          | Test  | 0.7879923 |  310.60405 | 1.0201251 | 161.92124 | 0.9058136 | 0.1802741 |
| healthyR.data |         3 | EARTH       | Test  | 0.5180053 |  150.20307 | 0.6706032 | 113.29056 | 0.7260168 | 0.1802741 |
| healthyR.data |         4 | NNAR        | Test  | 0.5196398 |   90.76534 | 0.6727193 | 118.56408 | 0.7324942 | 0.0048656 |
| healthyR      |         1 | ARIMA       | Test  | 0.5914503 |   92.05564 | 0.8462884 | 126.67242 | 0.7740785 | 0.0170396 |
| healthyR      |         2 | LM          | Test  | 0.6811261 |  106.19543 | 0.9746027 | 168.41827 | 0.8771704 | 0.1641926 |
| healthyR      |         3 | EARTH       | Test  | 0.6146525 |  113.99829 | 0.8794877 | 124.83730 | 0.7684895 | 0.1641926 |
| healthyR      |         4 | NNAR        | Test  | 0.7056548 |  121.06128 | 1.0097001 | 165.31782 | 0.8947843 | 0.0118160 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.6129409 | 1118.64223 | 1.5088181 | 127.53621 | 1.8229286 | 0.1065693 |
| healthyR.ts   |         2 | LM          | Test  | 0.7320642 |  232.06555 | 0.6848061 | 114.63827 | 0.9701355 | 0.1249242 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7310798 |  237.47408 | 0.6838852 | 113.68251 | 0.9698290 | 0.1249242 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8652820 |  167.33446 | 0.8094241 | 176.32254 | 1.0927975 | 0.0014849 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5182886 |  170.85787 | 0.7205393 | 103.84485 | 0.6862302 | 0.3716173 |
| healthyverse  |         2 | LM          | Test  | 0.7003354 |  740.67797 | 0.9736259 | 112.06248 | 0.7946752 | 0.0621337 |
| healthyverse  |         3 | EARTH       | Test  | 0.7150208 |  155.46819 | 0.9940420 | 162.99944 | 0.9689903 | 0.0621337 |
| healthyverse  |         4 | NNAR        | Test  | 0.6766105 |  340.15088 | 0.9406429 | 140.81249 | 0.8264771 | 0.0846034 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8722696 |  121.59644 | 0.8959901 | 176.72960 | 1.1138660 | 0.2444610 |
| healthyR.ai   |         2 | LM          | Test  | 0.7352927 |  138.59984 | 0.7552882 | 134.54786 | 0.9487240 | 0.0638185 |
| healthyR.ai   |         3 | EARTH       | Test  | 4.6027159 | 1894.79666 | 4.7278818 | 173.82986 | 4.8398616 | 0.0638185 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.8499652 |  137.84170 | 0.8730791 | 155.52849 | 1.1004365 | 0.1948602 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5500647 |  252.39288 | 0.7774894 |  91.12196 | 0.6607490 | 0.3589939 |
| TidyDensity   |         2 | LM          | Test  | 0.5987358 |  264.07111 | 0.8462837 |  92.34837 | 0.7377889 | 0.1202615 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7627581 |  146.67822 | 1.0781211 | 181.25520 | 0.9141002 | 0.1202615 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5804609 |   98.24166 | 0.8204531 | 119.51426 | 0.7539237 | 0.0211315 |
| tidyAML       |         1 | ARIMA       | Test  | 0.7543886 |  119.36551 | 0.9106731 | 137.17861 | 0.9313617 | 0.0160067 |
| tidyAML       |         2 | LM          | Test  | 0.6899833 |  109.84348 | 0.8329251 | 113.28801 | 0.8421866 | 0.0011784 |
| tidyAML       |         3 | EARTH       | Test  | 3.7108050 |  810.77461 | 4.4795617 | 184.46450 | 3.9718886 | 0.0011784 |
| tidyAML       |         4 | NNAR        | Test  | 0.6301811 |  107.39584 | 0.7607339 | 107.85522 | 0.8370672 | 0.0542746 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         1 ARIMA       Test  0.478  111. 0.619 125.  0.660 0.238 
    ## 2 healthyR              3 EARTH       Test  0.615  114. 0.879 125.  0.768 0.164 
    ## 3 healthyR.ts           3 EARTH       Test  0.731  237. 0.684 114.  0.970 0.125 
    ## 4 healthyverse          1 ARIMA       Test  0.518  171. 0.721 104.  0.686 0.372 
    ## 5 healthyR.ai           2 LM          Test  0.735  139. 0.755 135.  0.949 0.0638
    ## 6 TidyDensity           1 ARIMA       Test  0.550  252. 0.777  91.1 0.661 0.359 
    ## 7 tidyAML               4 NNAR        Test  0.630  107. 0.761 108.  0.837 0.0543

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1311|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1303|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1249|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1220|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1046|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [900|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [516|28]>  <mdl_tm_t [1 × 5]>

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
