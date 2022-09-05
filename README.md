Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
05 September, 2022

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 45,839
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

The last day in the data set is 2022-09-03 19:03:19, the file was
birthed on: 2022-07-02 23:58:17, and at report knit time is -1503.08
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 45839         |
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
| r_version     |     31122 |          0.32 |   5 |   5 |     0 |       33 |          0 |
| r_arch        |     31122 |          0.32 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     31122 |          0.32 |   7 |  15 |     0 |       11 |          0 |
| package       |         0 |          1.00 |   8 |  13 |     0 |        6 |          0 |
| version       |         0 |          1.00 |   5 |   5 |     0 |       28 |          0 |
| country       |      3603 |          0.92 |   2 |   2 |     0 |      117 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2022-09-03 | 2021-11-19 |      650 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1386486.56 | 1759057.96 | 357 | 16247 | 289858 | 2914365 | 5677952 | ▇▁▂▂▁ |
| ip_id         |         0 |             1 |    8876.97 |   16785.48 |   1 |   174 |   2473 |    9229 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2022-09-03 19:03:19 | 2021-11-19 09:25:27 |    26779 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |      median | n_unique |
|:--------------|----------:|--------------:|----:|----:|------------:|---------:|
| time          |         0 |             1 |   0 |  59 | 11H 33M 45S |       60 |

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
    ## 1 healthyR      <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 2 healthyR.data <tibble [546 × 2]> <tibble [28 × 2]> <split [518|28]>
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble [28 × 2]> <split [519|28]>
    ## 4 healthyverse  <tibble [549 × 2]> <tibble [28 × 2]> <split [521|28]>
    ## 5 healthyR.ai   <tibble [378 × 2]> <tibble [28 × 2]> <split [350|28]>
    ## 6 TidyDensity   <tibble [226 × 2]> <tibble [28 × 2]> <split [198|28]>

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
    ## 1 healthyR      <tibble [549 × 2]> <tibble>     <split [521|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble>     <split [519|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [549 × 2]> <tibble>     <split [521|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [378 × 2]> <tibble>     <split [350|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [226 × 2]> <tibble>     <split [198|28]> <mdl_time_tbl> 
    ## # … with abbreviated variable name ¹​.modeltime_tables

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |    smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|---------:|----------:|----------:|
| healthyR      |         1 | ARIMA       | Test  | 0.8240515 |  113.41311 | 0.8591027 | 152.9641 | 1.1299143 | 0.0001649 |
| healthyR      |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR      |         3 | LM          | Test  | 0.8198387 |  115.85648 | 0.8547107 | 166.4762 | 1.0618266 | 0.0036711 |
| healthyR      |         4 | EARTH       | Test  | 2.4710973 | 1311.75158 | 2.5762061 | 157.4766 | 2.7211656 | 0.0036711 |
| healthyR      |         5 | NNAR        | Test  | 0.9015447 |  170.78730 | 0.9398922 | 153.9908 | 1.1240054 | 0.0108428 |
| healthyR.data |         1 | ARIMA       | Test  | 0.9181614 |  107.68158 | 0.7087789 | 130.2214 | 1.0521142 | 0.1385182 |
| healthyR.data |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.data |         3 | LM          | Test  | 0.9726100 |  110.52326 | 0.7508108 | 134.4132 | 1.1202843 | 0.0078603 |
| healthyR.data |         4 | EARTH       | Test  | 0.9703414 |  111.30774 | 0.7490595 | 133.5246 | 1.1149155 | 0.0078603 |
| healthyR.data |         5 | NNAR        | Test  | 1.1164901 |  109.10574 | 0.8618797 | 149.4452 | 1.3660395 | 0.0065611 |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6315173 |   86.22713 | 0.5505649 | 142.2220 | 0.8150463 | 0.2322422 |
| healthyR.ts   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ts   |         3 | LM          | Test  | 0.7751165 |  104.48147 | 0.6757565 | 147.6861 | 0.9317552 | 0.0896402 |
| healthyR.ts   |         4 | EARTH       | Test  | 0.8087300 |  126.52848 | 0.7050612 | 135.9654 | 0.9648860 | 0.0896402 |
| healthyR.ts   |         5 | NNAR        | Test  | 0.7560718 |  129.19368 | 0.6591531 | 156.4019 | 0.9082031 | 0.0932452 |
| healthyverse  |         1 | ARIMA       | Test  | 0.8985275 |  422.68599 | 0.8153462 | 147.1174 | 1.0949601 | 0.0603858 |
| healthyverse  |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyverse  |         3 | LM          | Test  | 0.8326469 |  613.23842 | 0.7555645 | 125.1148 | 0.9875563 | 0.0133952 |
| healthyverse  |         4 | EARTH       | Test  | 0.8065215 |  799.42410 | 0.7318576 | 115.3557 | 0.9305221 | 0.0133952 |
| healthyverse  |         5 | NNAR        | Test  | 0.9127539 |  704.59238 | 0.8282556 | 136.2778 | 1.1533750 | 0.0371363 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8272031 |  272.70299 | 0.6832520 | 175.8565 | 1.0170974 | 0.0322339 |
| healthyR.ai   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| healthyR.ai   |         3 | LM          | Test  | 0.8333246 |   99.72399 | 0.6883082 | 180.9244 | 1.0437813 | 0.0026201 |
| healthyR.ai   |         4 | EARTH       | Test  | 0.8500357 |  216.45923 | 0.7021112 | 136.8877 | 1.1282332 | 0.0026201 |
| healthyR.ai   |         5 | NNAR        | Test  | 0.8292326 |  214.92342 | 0.6849283 | 166.5535 | 0.9818354 | 0.0354864 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.9781865 |  101.55682 | 0.7272197 | 196.0587 | 1.1719098 | 0.0253106 |
| TidyDensity   |         2 | NULL        | NA    |        NA |         NA |        NA |       NA |        NA |        NA |
| TidyDensity   |         3 | LM          | Test  | 0.9853382 |  102.64770 | 0.7325365 | 139.5659 | 1.2454430 | 0.0039066 |
| TidyDensity   |         4 | EARTH       | Test  | 0.9793318 |   99.99477 | 0.7280711 | 167.4961 | 1.2006739 | 0.0039066 |
| TidyDensity   |         5 | NNAR        | Test  | 0.8125770 |   95.85910 | 0.6040995 | 119.3451 | 1.0119575 | 0.2704673 |

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
    ## 1 healthyR              3 LM         Test  0.820 116.  0.855  166. 1.06  0.00367
    ## 2 healthyR.data         1 ARIMA      Test  0.918 108.  0.709  130. 1.05  0.139  
    ## 3 healthyR.ts           1 ARIMA      Test  0.632  86.2 0.551  142. 0.815 0.232  
    ## 4 healthyverse          4 EARTH      Test  0.807 799.  0.732  115. 0.931 0.0134 
    ## 5 healthyR.ai           5 NNAR       Test  0.829 215.  0.685  167. 0.982 0.0355 
    ## 6 TidyDensity           5 NNAR       Test  0.813  95.9 0.604  119. 1.01  0.270  
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
    ## 1 healthyR      <tibble [549 × 2]> <tibble>     <split [521|28]> <mdl_time_tbl> 
    ## 2 healthyR.data <tibble [546 × 2]> <tibble>     <split [518|28]> <mdl_time_tbl> 
    ## 3 healthyR.ts   <tibble [547 × 2]> <tibble>     <split [519|28]> <mdl_time_tbl> 
    ## 4 healthyverse  <tibble [549 × 2]> <tibble>     <split [521|28]> <mdl_time_tbl> 
    ## 5 healthyR.ai   <tibble [378 × 2]> <tibble>     <split [350|28]> <mdl_time_tbl> 
    ## 6 TidyDensity   <tibble [226 × 2]> <tibble>     <split [198|28]> <mdl_time_tbl> 
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
