Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
08 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 110,025
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

The last day in the data set is 2024-08-06 23:32:41, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is 12.05 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 110025        |
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
| r_version     |     77018 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     77018 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     77018 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9475 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-06 | 2023-01-18 |     1353 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |      mean |        sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|----------:|----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1182620.8 | 1562610.0 | 355 | 14701 | 289680 | 2373624 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10338.3 |   18086.8 |   1 |   307 |   3083 |   11494 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-06 23:32:41 | 2023-01-18 01:58:37 |    66622 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 9M 15S |       60 |

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
    ## 1 healthyR.data <tibble [1,324 × 2]> <tibble [28 × 2]> <split [1296|28]>
    ## 2 healthyR      <tibble [1,316 × 2]> <tibble [28 × 2]> <split [1288|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,262 × 2]> <tibble [28 × 2]> <split [1234|28]>
    ## 5 healthyverse  <tibble [1,233 × 2]> <tibble [28 × 2]> <split [1205|28]>
    ## 6 healthyR.ai   <tibble [1,059 × 2]> <tibble [28 × 2]> <split [1031|28]>
    ## 7 TidyDensity   <tibble [913 × 2]>   <tibble [28 × 2]> <split [885|28]> 
    ## 8 tidyAML       <tibble [529 × 2]>   <tibble [28 × 2]> <split [501|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.8134618 | 260.77288 | 0.6262897 | 139.0032 | 1.0572196 | 0.2437147 |
| healthyR.data |         2 | LM          | Test  | 0.8415966 | 343.24351 | 0.6479509 | 142.6175 | 0.9973437 | 0.0063306 |
| healthyR.data |         3 | EARTH       | Test  | 0.8227359 | 194.09954 | 0.6334299 | 155.1393 | 1.0448853 | 0.0063306 |
| healthyR.data |         4 | NNAR        | Test  | 0.8558713 | 199.34530 | 0.6589411 | 170.9265 | 1.0573114 | 0.0412408 |
| healthyR      |         1 | ARIMA       | Test  | 0.6892396 | 142.46387 | 0.7197142 | 150.4746 | 0.8508899 | 0.0389512 |
| healthyR      |         2 | LM          | Test  | 0.7029618 | 126.23884 | 0.7340431 | 158.3353 | 0.9108732 | 0.0240720 |
| healthyR      |         3 | EARTH       | Test  | 1.5466868 | 803.62122 | 1.6150733 | 141.6644 | 1.7647140 | 0.0240720 |
| healthyR      |         4 | NNAR        | Test  | 0.6749804 | 152.41862 | 0.7048245 | 154.3613 | 0.8844245 | 0.0170351 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0577638 | 130.89443 | 0.7811648 | 153.3368 | 1.3149090 | 0.0123242 |
| healthyR.ts   |         2 | LM          | Test  | 1.0892815 | 185.19947 | 0.8044408 | 155.4363 | 1.2254117 | 0.0800991 |
| healthyR.ts   |         3 | EARTH       | Test  | 1.0910396 | 188.45073 | 0.8057392 | 154.7849 | 1.2262016 | 0.0800991 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.9613581 |  96.66424 | 0.7099686 | 173.2327 | 1.1675062 | 0.1366256 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5892801 | 231.67251 | 0.6901465 | 100.7338 | 0.7472895 | 0.5394947 |
| healthyverse  |         2 | LM          | Test  | 0.7864222 | 392.10483 | 0.9210332 | 107.2970 | 0.9463181 | 0.0224304 |
| healthyverse  |         3 | EARTH       | Test  | 0.7800146 | 292.47304 | 0.9135289 | 119.1263 | 0.9173431 | 0.0224304 |
| healthyverse  |         4 | NNAR        | Test  | 0.7952272 | 209.60485 | 0.9313454 | 136.8419 | 0.9493132 | 0.0000070 |
| healthyR.ai   |         1 | ARIMA       | Test  | 1.1159141 | 109.30390 | 0.8398980 | 157.5230 | 1.5165659 | 0.0024596 |
| healthyR.ai   |         2 | LM          | Test  | 1.1254453 | 110.53716 | 0.8470717 | 149.1959 | 1.5272963 | 0.0221958 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.1244380 | 110.57878 | 0.8463136 | 148.5052 | 1.5276934 | 0.0221958 |
| healthyR.ai   |         4 | NNAR        | Test  | 1.0519537 | 104.90763 | 0.7917579 | 157.9644 | 1.4224471 | 0.0846670 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.7064624 | 219.46812 | 0.8609040 | 114.1075 | 0.8393260 | 0.1331750 |
| TidyDensity   |         2 | LM          | Test  | 0.7365611 | 243.41136 | 0.8975827 | 114.7978 | 0.8703236 | 0.0184734 |
| TidyDensity   |         3 | EARTH       | Test  | 0.7356308 | 120.79092 | 0.8964490 | 165.1539 | 0.9571289 | 0.0184734 |
| TidyDensity   |         4 | NNAR        | Test  | 0.6280393 |  99.53984 | 0.7653367 | 146.4827 | 0.8132446 | 0.2562136 |
| tidyAML       |         1 | ARIMA       | Test  | 0.8306727 | 139.42119 | 0.9054579 | 124.1271 | 0.9782956 | 0.0676762 |
| tidyAML       |         2 | LM          | Test  | 0.8219859 | 123.20996 | 0.8959891 | 123.3748 | 0.9954731 | 0.0457783 |
| tidyAML       |         3 | EARTH       | Test  | 0.9596102 | 231.43825 | 1.0460036 | 115.3278 | 1.1476198 | 0.0457783 |
| tidyAML       |         4 | NNAR        | Test  | 0.8889228 | 183.23376 | 0.9689523 | 119.8430 | 1.0477008 | 0.0149512 |

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
    ## 1 healthyR.da…         2 LM          Test  0.842 343.  0.648  143. 0.997 0.00633
    ## 2 healthyR             1 ARIMA       Test  0.689 142.  0.720  150. 0.851 0.0390 
    ## 3 healthyR.ts          4 NNAR        Test  0.961  96.7 0.710  173. 1.17  0.137  
    ## 4 healthyverse         1 ARIMA       Test  0.589 232.  0.690  101. 0.747 0.539  
    ## 5 healthyR.ai          4 NNAR        Test  1.05  105.  0.792  158. 1.42  0.0847 
    ## 6 TidyDensity          4 NNAR        Test  0.628  99.5 0.765  146. 0.813 0.256  
    ## 7 tidyAML              1 ARIMA       Test  0.831 139.  0.905  124. 0.978 0.0677

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1296|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1288|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1234|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1205|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1031|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [885|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [501|28]>  <mdl_tm_t [1 × 5]>

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
