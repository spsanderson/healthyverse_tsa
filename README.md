Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
12 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 110,508
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

The last day in the data set is 2024-08-10 17:30:24, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -77.91 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 110508        |
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
| r_version     |     77388 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     77388 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     77388 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9495 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-10 | 2023-01-19 |     1357 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1181099.5 | 1561688.3 | 355 | 14701 | 289676 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10325.7 |   18065.9 |   1 |   317 |   3091 |   11483 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-10 17:30:24 | 2023-01-19 19:01:18 |    66882 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   14.5 |       60 |

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
    ## 1 healthyR.data <tibble [1,328 × 2]> <tibble [28 × 2]> <split [1300|28]>
    ## 2 healthyR      <tibble [1,320 × 2]> <tibble [28 × 2]> <split [1292|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,266 × 2]> <tibble [28 × 2]> <split [1238|28]>
    ## 5 healthyverse  <tibble [1,237 × 2]> <tibble [28 × 2]> <split [1209|28]>
    ## 6 healthyR.ai   <tibble [1,063 × 2]> <tibble [28 × 2]> <split [1035|28]>
    ## 7 TidyDensity   <tibble [917 × 2]>   <tibble [28 × 2]> <split [889|28]> 
    ## 8 tidyAML       <tibble [533 × 2]>   <tibble [28 × 2]> <split [505|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.6376245 |  144.4056 | 0.5527982 | 142.5573 | 0.8333297 | 0.1189338 |
| healthyR.data |         2 | LM          | Test  | 0.7976422 |  381.1525 | 0.6915279 | 148.5874 | 0.9412024 | 0.0001439 |
| healthyR.data |         3 | EARTH       | Test  | 6.7709578 | 5135.6538 | 5.8701834 | 177.6977 | 7.4769971 | 0.0001439 |
| healthyR.data |         4 | NNAR        | Test  | 0.7198865 |  192.9833 | 0.6241164 | 154.4969 | 0.9258939 | 0.0145278 |
| healthyR      |         1 | ARIMA       | Test  | 0.6869983 |  111.1561 | 0.8385756 | 154.2678 | 0.8214487 | 0.0116162 |
| healthyR      |         2 | LM          | Test  | 0.7116806 |  112.3924 | 0.8687036 | 159.6958 | 0.8967204 | 0.0068453 |
| healthyR      |         3 | EARTH       | Test  | 0.6822408 |  138.1374 | 0.8327683 | 149.2929 | 0.8135830 | 0.0068453 |
| healthyR      |         4 | NNAR        | Test  | 0.6857820 |  111.1938 | 0.8370908 | 150.6346 | 0.8657280 | 0.0166818 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0841669 |  310.7032 | 0.8358561 | 149.8959 | 1.2390140 | 0.0038627 |
| healthyR.ts   |         2 | LM          | Test  | 1.0288789 |  238.4608 | 0.7932309 | 151.6573 | 1.1958655 | 0.0215366 |
| healthyR.ts   |         3 | EARTH       | Test  | 1.0314360 |  243.7357 | 0.7952023 | 151.2135 | 1.1963186 | 0.0215366 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.9830707 |  107.3082 | 0.7579143 | 191.0167 | 1.2060230 | 0.0635555 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8043260 |  573.1098 | 1.0175966 | 115.7054 | 0.9472691 | 0.0125549 |
| healthyverse  |         2 | LM          | Test  | 0.8150355 |  631.0954 | 1.0311458 | 113.5076 | 0.9650909 | 0.0561191 |
| healthyverse  |         3 | EARTH       | Test  | 0.8086403 |  459.9877 | 1.0230549 | 126.2314 | 0.9364967 | 0.0561191 |
| healthyverse  |         4 | NNAR        | Test  | 0.8233624 |  325.9854 | 1.0416807 | 142.0019 | 0.9681851 | 0.0040268 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.1504906 |  111.6770 | 0.8861165 | 155.7082 | 1.5698710 | 0.0533133 |
| healthyR.ai   |         2 | LM          | Test  | 1.1258098 |  106.3855 | 0.8671072 | 144.0777 | 1.5493486 | 0.0607389 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.1255373 |  106.6037 | 0.8668973 | 141.3106 | 1.5560955 | 0.0607389 |
| healthyR.ai   |         4 | NNAR        | Test  | 1.1026476 |  107.0653 | 0.8492674 | 161.6146 | 1.5024232 | 0.0000491 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6513499 |  186.6245 | 0.7506959 | 111.4721 | 0.7845839 | 0.2727206 |
| TidyDensity   |         2 | LM          | Test  | 0.7248396 |  224.4356 | 0.8353945 | 113.3167 | 0.8630467 | 0.0000006 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7643876 |  123.4084 | 0.8809745 | 169.5576 | 0.9608180 | 0.0000006 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6220747 |  106.4787 | 0.7169555 | 145.3256 | 0.7607422 | 0.2894545 |
| tidyAML       |         1 | ARIMA       | Test  | 0.9116094 |  186.3464 | 1.0469988 | 132.5064 | 1.0659233 | 0.0439775 |
| tidyAML       |         2 | LM          | Test  | 0.8351294 |  140.7651 | 0.9591602 | 133.1177 | 1.0031823 | 0.0658905 |
| tidyAML       |         3 | EARTH       | Test  | 1.0895750 |  271.0916 | 1.2513953 | 131.8184 | 1.2570994 | 0.0658905 |
| tidyAML       |         4 | NNAR        | Test  | 0.9732210 |  211.9473 | 1.1177608 | 132.5918 | 1.1241317 | 0.0079347 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.638  144. 0.553  143. 0.833 1.19e-1
    ## 2 healthyR             3 EARTH       Test  0.682  138. 0.833  149. 0.814 6.85e-3
    ## 3 healthyR.ts          2 LM          Test  1.03   238. 0.793  152. 1.20  2.15e-2
    ## 4 healthyverse         3 EARTH       Test  0.809  460. 1.02   126. 0.936 5.61e-2
    ## 5 healthyR.ai          4 NNAR        Test  1.10   107. 0.849  162. 1.50  4.91e-5
    ## 6 TidyDensity          4 NNAR        Test  0.622  106. 0.717  145. 0.761 2.89e-1
    ## 7 tidyAML              2 LM          Test  0.835  141. 0.959  133. 1.00  6.59e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1300|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1292|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1238|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1209|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1035|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [889|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [505|28]>  <mdl_tm_t [1 × 5]>

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
